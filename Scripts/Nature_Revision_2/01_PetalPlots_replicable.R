## Reproducible version of 01_PetalPlots.R
## Keeps output intent unchanged with cleaner structure.

rm(list = ls())
options(scipen = 999)

required_packages <- c("tidyverse", "ggplot2", "viridis", "data.table", "cowplot", "patchwork")
invisible(lapply(required_packages, require, character.only = TRUE))

source("Inputs/FixedScenarioParams.R")

PATHS <- list(
  scenarios = "Data/NR2/MasterAllScenarios.rds",
  birds = "Data/NR2/OG_baseline_birdsSept24.rds",
  dung_beetles = "Data/NR2/MasterDBPerformance.rds",
  carbon = "Data/NR2/MasterCarbonPerformance_withuncertainty.rds",
  megatrees = "Data/NR2/MasterMegatreePerformance_with_uncertainty.rds",
  profits = "Data/NR2/MasterFinancialPerformance.rds",
  export_dir = "Figures/PetalPlots"
)

# Configure carbon stream once for reproducible variant runs.
CARBON_SETTINGS <- list(
  stream = "scc",                  # "scc" or "stock_year"
  discount_rate = "4%",            # used for SCC stream; set NULL to keep all rates
  slope_trajectory = NULL          # e.g., "linear", "convex", etc.; NULL keeps all
)

dir.create(PATHS$export_dir, recursive = TRUE, showWarnings = FALSE)

habitat_colors <- c(
  "primary" = "#4d9221",
  "once-logged" = "#7fbc41",
  "restored" = "#b8e186",
  "twice-logged" = "#e6f5d0",
  "albizia_current" = "#fde0e6",
  "eucalyptus_current" = "#f1b6da",
  "deforested" = "#c51b7d"
)

all_start_landscape <- all_start_landscape %>%
  left_join(
    tibble(habitat = names(habitat_colors), colors = as.character(habitat_colors)),
    by = "habitat"
  )

prepare_carbon_stream <- function(carbon_df, stream = c("scc", "stock_year"), discount_rate = NULL, slope_trajectory = NULL) {
  stream <- match.arg(stream)

  metric_col <- if (stream == "scc") "TOTcarbon_ACD_mean" else "mean_cum_stock_year"
  stream_outcome <- if (stream == "scc") "carbon_scc" else "carbon_stock_year"

  out <- carbon_df %>%
    mutate(
      discount_rate = as.character(discount_rate),
      twice_logged_slope_trajectory = as.character(twice_logged_slope_trajectory)
    )

  if (!is.null(discount_rate) && "discount_rate" %in% names(out)) {
    out <- out %>% filter(discount_rate == !!discount_rate)
  }

  if (!is.null(slope_trajectory) && "twice_logged_slope_trajectory" %in% names(out)) {
    out <- out %>% filter(twice_logged_slope_trajectory == !!slope_trajectory)
  }

  out %>%
    select(index, production_target, scenarioName, scenarioStart, all_of(metric_col), discount_rate) %>%
    rename(performance = all_of(metric_col), modulator = discount_rate) %>%
    mutate(outcome = stream_outcome)
}

read_and_prepare_master <- function(carbon_stream = "scc", carbon_discount_rate = NULL, carbon_slope_trajectory = NULL) {
  birds <- readRDS(PATHS$birds) %>%
    select(index, production_target, scenarioName, scenarioStart, medianRelativeOccupancy, spp_category, outcome) %>%
    rename(performance = medianRelativeOccupancy, modulator = spp_category)

  dung_beetles <- readRDS(PATHS$dung_beetles) %>%
    select(index, production_target, scenarioName, scenarioStart, medianRelativeOccupancy, spp_category, outcome) %>%
    rename(performance = medianRelativeOccupancy, modulator = spp_category)

  carbon <- readRDS(PATHS$carbon) %>%
    prepare_carbon_stream(
      stream = carbon_stream,
      discount_rate = carbon_discount_rate,
      slope_trajectory = carbon_slope_trajectory
    )

  megatrees <- readRDS(PATHS$megatrees) %>%
    select(index, production_target, scenarioName, scenarioStart, landscape_prop, outcome) %>%
    rename(performance = landscape_prop) %>%
    mutate(modulator = "MT")

  profits_df <- readRDS(PATHS$profits) %>%
    pivot_longer(cols = starts_with("NPV"), names_to = "discount_rate", values_to = "NPV") %>%
    mutate(
      discount_rate = gsub("NPV", "", discount_rate),
      discount_rate = paste0(discount_rate, "%")
    ) %>%
    na.omit() %>%
    select(index, production_target, scenarioName, scenarioStart, NPV, discount_rate, costType, outcome)

  profits <- profits_df %>%
    filter(costType == "HarvestProfits") %>%
    select(-costType) %>%
    rename(performance = NPV, modulator = discount_rate) %>%
    mutate(outcome = "profits")

  costs <- profits_df %>%
    filter(costType == "ProtectionCosts") %>%
    select(-costType) %>%
    rename(performance = NPV, modulator = discount_rate) %>%
    mutate(outcome = "costs")

  carbon_outcome_name <- if (carbon_stream == "scc") "carbon_scc" else "carbon_stock_year"
  outcome_order <- c("profits", "costs", "birds", "dungBeetles", carbon_outcome_name, "megatrees")

  bind_rows(birds, dung_beetles, carbon, megatrees, profits, costs) %>%
    mutate(
      outcome = str_replace_all(outcome, "\\s+", ""),
      outcome = factor(outcome, levels = outcome_order)
    ) %>%
    as.data.frame()
}

normalize_performance <- function(df, production_target_value) {
  max_values <- df %>%
    filter(production_target == production_target_value) %>%
    group_by(outcome) %>%
    filter(performance == max(performance, na.rm = TRUE)) %>%
    select(-c(index, production_target, scenarioName, scenarioStart, modulator)) %>%
    distinct() %>%
    rename(max_performance = performance)

  min_values <- df %>%
    filter(production_target == production_target_value) %>%
    group_by(outcome) %>%
    filter(performance == min(performance, na.rm = TRUE)) %>%
    select(-c(index, production_target, scenarioName, scenarioStart, modulator)) %>%
    distinct() %>%
    rename(min_performance = performance)

  df %>%
    left_join(max_values %>% left_join(min_values, by = "outcome"), by = "outcome") %>%
    distinct() %>%
    mutate(
      diffScenMin = performance - min_performance,
      diffRange = max_performance - min_performance,
      normalised_performance = (diffScenMin / diffRange) + 0.1
    )
}

scenarios <- readRDS(PATHS$scenarios)
scenario_composition <- rbindlist(scenarios, use.names = TRUE)
rm(scenarios)

masterDF <- read_and_prepare_master(
  carbon_stream = CARBON_SETTINGS$stream,
  carbon_discount_rate = CARBON_SETTINGS$discount_rate,
  carbon_slope_trajectory = CARBON_SETTINGS$slope_trajectory
)

petal_plot_fun <- function(df, production_target_value, scenario_start, discount_rate_filter, species_category, legend_position, filter_scenarios = NULL) {
  modulator_filters <- c("MT", species_category)
  if (!is.null(discount_rate_filter)) modulator_filters <- c(modulator_filters, discount_rate_filter)

  plot_df <- df %>%
    filter(modulator %in% modulator_filters) %>%
    filter(str_detect(index, "CY_D")) %>%
    filter(scenarioStart == scenario_start) %>%
    filter(production_target == production_target_value) %>%
    distinct() %>%
    mutate(performance = abs(performance))

  if (!is.null(filter_scenarios) && length(filter_scenarios) > 0) {
    plot_df <- plot_df %>% filter(index %in% filter_scenarios)
  }

  plot_df <- normalize_performance(plot_df, production_target_value)
  indexes <- plot_df %>% select(index, production_target) %>% distinct() %>% arrange(index)
  sl_name <- plot_df %>% pull(scenarioStart) %>% unique() %>% .[[1]]

  petal <- plot_df %>%
    ggplot(aes(x = outcome, y = normalised_performance, fill = outcome)) +
    geom_bar(stat = "identity", width = 0.4, colour = "black", linewidth = 0.1, alpha = 0.7) +
    scale_fill_viridis(discrete = TRUE) +
    ylim(0, 1.1) +
    geom_hline(yintercept = 1.1, linetype = "dashed", color = "black") +
    geom_hline(yintercept = 0.1, linetype = "dashed", color = "black") +
    facet_wrap(~index, ncol = 1) +
    coord_polar(clip = "off") +
    xlab("") +
    ylab("") +
    labs(fill = "Outcome") +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      legend.position = legend_position,
      strip.text = element_blank()
    ) +
    ggtitle(paste("P =", production_target_value, "\nStarting Landscape:", sl_name))

  composition_pies <- indexes %>%
    left_join(scenario_composition, by = c("index", "production_target")) %>%
    arrange(index) %>%
    group_by(index, production_target) %>%
    mutate(
      percentage = num_parcels / 1000,
      habitat = factor(habitat, levels = names(habitat_colors))
    ) %>%
    ggplot(aes(x = "", y = num_parcels, fill = habitat)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    facet_wrap(~index, ncol = 1) +
    scale_fill_manual(values = habitat_colors) +
    labs(fill = "Habitat") +
    theme_bw() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5),
      legend.position = legend_position,
      strip.text = element_blank(),
      plot.margin = unit(c(0, 0, 0, 0), "cm")
    )

  plot_grid(composition_pies, petal, nrow = 2, rel_heights = c(0.4, 1), align = "v", axis = "-1000")
}

a <- petal_plot_fun(
  df = masterDF,
  production_target_value = 0.38,
  scenario_start = "all_primary",
  discount_rate_filter = CARBON_SETTINGS$discount_rate,
  legend_position = "none",
  species_category = "loser",
  filter_scenarios = c(
    "all_primary_CY_D.csv 195",
    "all_primary_CY_D.csv 191",
    "all_primary_CY_D.csv 192",
    "all_primary_CY_D.csv 193"
  )
)

b <- petal_plot_fun(
  df = masterDF,
  production_target_value = 0.38,
  scenario_start = "mostly_1L",
  discount_rate_filter = CARBON_SETTINGS$discount_rate,
  legend_position = "none",
  species_category = "loser",
  filter_scenarios = c(
    "mostly_1L_CY_D.csv 352",
    "mostly_1L_CY_D.csv 347",
    "93079",
    "mostly_1L_CY_D.csv 344",
    "mostly_1L_CY_D.csv 345"
  )
)

c <- petal_plot_fun(
  df = masterDF,
  production_target_value = 0.38,
  scenario_start = "mostly_2L",
  discount_rate_filter = CARBON_SETTINGS$discount_rate,
  legend_position = "none",
  species_category = "loser",
  filter_scenarios = c(
    "mostly_2L_CY_D.csv 274",
    "mostly_2L_CY_D.csv 270",
    "mostly_2L_CY_D.csv 269",
    "mostly_2L_CY_D.csv 268"
  )
)

fig1 <- plot_grid(a, b, c, ncol = 3)

SL_pies <- all_start_landscape %>%
  group_by(scenarioStart) %>%
  mutate(
    percentage = num_parcels / 1000,
    habitat = factor(habitat, levels = names(habitat_colors))
  ) %>%
  ggplot(aes(x = "", y = num_parcels, fill = habitat)) +
  geom_bar(stat = "identity", width = 1, colour = "black") +
  coord_polar("y", start = 0) +
  facet_wrap(~scenarioStart, nrow = 1) +
  scale_fill_manual(values = habitat_colors) +
  labs(fill = "Habitat") +
  theme_bw() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5),
    strip.text = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "cm")
  )

with_legend <- petal_plot_fun(
  df = masterDF,
  production_target_value = 0.26,
  scenario_start = "mostly_1L",
  discount_rate_filter = CARBON_SETTINGS$discount_rate,
  legend_position = "bottom",
  species_category = "loser"
)

width <- 8.27
height <- 11.69

ggsave(fig1, filename = file.path(PATHS$export_dir, "fig2PetalPlotsROutput.pdf"), width = width, height = height, units = "in")
ggsave(SL_pies, filename = file.path(PATHS$export_dir, "StartingLandscapePies.pdf"), width = width, height = height, units = "in")
ggsave(with_legend, filename = file.path(PATHS$export_dir, "legend.pdf"), width = width, height = height, units = "in")
