## Reproducible, single-figure workflow for Figure 2-style panels
## Choose one preset + carbon variants in RUN_SETTINGS.

rm(list = ls())
options(scipen = 999)

required_packages <- c(
  "tidyverse", "ggplot2", "data.table", "cowplot", "stringr", "forcats", "biscale"
)
invisible(lapply(required_packages, require, character.only = TRUE))

source("Inputs/FixedScenarioParams.R")

PATHS <- list(
  birds = "Data/NR2/OG_baseline_birds.rds",
  birds_iucn = "Data/NR2/OG_baseline_birdsIUCN.rds",
  dung_beetles = "Data/NR2/MasterDBPerformance_fastPilot.rds",
  carbon = "Data/NR2/carbon_outcomes_combined__all_trajectories.csv",
  megatrees = "Data/NR2/MasterMegatreePerformance_with_uncertainty.rds",
  financial = "Data/NR2/MasterFinancialPerformance__Sensitivity25pctMoneyParams_LONG.rds",
  all_scenarios = "Inputs/MasterAllScenarios.rds",
  export_dir = "Figures/GeomPointFigs/manuscript_figures"
)
dir.create(PATHS$export_dir, recursive = TRUE, showWarnings = FALSE)

SCALE <- list(
  bil = 1e9,
  hundred_million = 1e8,
  width = 8.27,
  height = 11.69,
  text_size = 13
)

FIGURE_PRESETS <- list(
  fig2_d = list(
    scenario_filter = c("AllPrimary", "Mostly1L", "Mostly2L"),
    output_stub = "Fig2_D"
  ),
  figs3_nd = list(
    scenario_filter = c("AllPrimaryNoDef", "Mostly1LNoDef", "Mostly2LNoDef"),
    output_stub = "FigS3_NoDef"
  ),
  figs6_d_dl = list(
    scenario_filter = c("MostlyPrimary+DL", "Mostly1L+DL", "Mostly2L+DL"),
    output_stub = "FigS6_DeforestedLand"
  )
)

RUN_SETTINGS <- list(
  figure_id = "fig2_d",              # names(FIGURE_PRESETS)
  bird_group = "loser",
  beetle_group = "loser",
  economic_discount_rate = "4%",
  economic_cost_type = "HarvestProfits",
  cashflow_variant = "baseline",     # "baseline", "plus25", "minus25", or NULL
  carbon_stream = "scc",             # "scc" or "stock_year"
  carbon_discount_rate = "4%",       # NULL to keep all
  carbon_slope_variant = NULL        # e.g. "linear", NULL to keep all
)

as_null_or_value <- function(x) {
  if (identical(x, "") || toupper(x) == "NULL") return(NULL)
  x
}

# Optional runtime overrides so you can generate multiple PDFs
# without editing the script each time.
RUN_SETTINGS$figure_id <- as_null_or_value(Sys.getenv("FIGURE_ID", RUN_SETTINGS$figure_id))
RUN_SETTINGS$bird_group <- as_null_or_value(Sys.getenv("BIRD_GROUP", RUN_SETTINGS$bird_group))
RUN_SETTINGS$beetle_group <- as_null_or_value(Sys.getenv("BEETLE_GROUP", RUN_SETTINGS$beetle_group))
RUN_SETTINGS$economic_discount_rate <- as_null_or_value(Sys.getenv("ECON_DR", RUN_SETTINGS$economic_discount_rate))
RUN_SETTINGS$economic_cost_type <- as_null_or_value(Sys.getenv("ECON_COST_TYPE", RUN_SETTINGS$economic_cost_type))
RUN_SETTINGS$cashflow_variant <- as_null_or_value(Sys.getenv("CASHFLOW_VARIANT", RUN_SETTINGS$cashflow_variant))
RUN_SETTINGS$carbon_stream <- as_null_or_value(Sys.getenv("CARBON_STREAM", RUN_SETTINGS$carbon_stream))
RUN_SETTINGS$carbon_discount_rate <- as_null_or_value(Sys.getenv("CARBON_DR", RUN_SETTINGS$carbon_discount_rate))
RUN_SETTINGS$carbon_slope_variant <- as_null_or_value(Sys.getenv("CARBON_SLOPE", "NULL"))

resolve_path <- function(primary, fallback = NULL) {
  if (file.exists(primary)) return(primary)
  if (!is.null(fallback) && file.exists(fallback)) return(fallback)
  stop("Missing required input file. Tried: ", primary, ifelse(is.null(fallback), "", paste0(" | Fallback: ", fallback)))
}

rename_scenario_name <- function(df) {
  df %>%
    mutate(
      scenarioName = case_when(
        scenarioName == "all_primary_CY_D.csv" ~ "AllPrimary",
        scenarioName == "mostly_1L_CY_D.csv" ~ "Mostly1L",
        scenarioName == "mostly_2L_CY_D.csv" ~ "Mostly2L",
        scenarioName == "all_primary_CY_ND.csv" ~ "AllPrimaryNoDef",
        scenarioName == "mostly_1L_CY_ND.csv" ~ "Mostly1LNoDef",
        scenarioName == "mostly_2L_CY_ND.csv" ~ "Mostly2LNoDef",
        scenarioName == "primary_deforested_CY_D.csv" ~ "MostlyPrimary+DL",
        scenarioName == "mostly_1L_deforested_CY_D.csv" ~ "Mostly1L+DL",
        scenarioName == "mostly_2L_deforested_CY_D.csv" ~ "Mostly2L+DL",
        scenarioName == "primary_deforested_CY_ND.csv" ~ "MostlyPrimaryNoDef+DL",
        scenarioName == "mostly_1L_deforested_CY_ND.csv" ~ "Mostly1LNoDef+DL",
        scenarioName == "mostly_2L_deforested_CY_ND.csv" ~ "Mostly2LNoDef+DL",
        TRUE ~ scenarioName
      )
    ) %>%
    mutate(
      scenarioName = fct_relevel(
        scenarioName,
        "AllPrimary", "Mostly1L", "Mostly2L",
        "AllPrimaryNoDef", "Mostly1LNoDef", "Mostly2LNoDef",
        "MostlyPrimary+DL", "Mostly1L+DL", "Mostly2L+DL",
        "MostlyPrimaryNoDef+DL", "Mostly1LNoDef+DL", "Mostly2LNoDef+DL"
      )
    )
}

prop_OG_fun <- function(df, hab_in_start) {
  df %>%
    group_by(index, production_target) %>%
    mutate(
      propOG = sum(num_parcels[habitat == "primary"]) / 1000,
      propPlant = sum(num_parcels[habitat %in% c("eucalyptus_current", "albizia_current", "albizia_future", "eucalyptus_future")]) / 1000,
      propEuc = sum(num_parcels[habitat %in% c("eucalyptus_current", "eucalyptus_future")]) / 1000,
      propAlb = sum(num_parcels[habitat %in% c("albizia_current", "albizia_future")]) / 1000,
      prop1L = sum(num_parcels[habitat == "once-logged"]) / 1000,
      prop2L = sum(num_parcels[habitat == "twice-logged"]) / 1000
    ) %>%
    mutate(
      scenarioStart = scenarioName,
      scenarioStart = str_remove(scenarioStart, "_IY_ND.csv"),
      scenarioStart = str_remove(scenarioStart, "_CY_ND.csv"),
      scenarioStart = str_remove(scenarioStart, "_IY_D.csv"),
      scenarioStart = str_remove(scenarioStart, "_CY_D.csv")
    ) %>%
    ungroup() %>%
    left_join(hab_in_start, by = "scenarioStart") %>%
    mutate(
      remainingOG = propOG / originalOG,
      remaining1L = prop1L / original1L,
      remaining2L = prop2L / original2L
    ) %>%
    mutate(across(c(remainingOG, remaining1L, remaining2L), ~ ifelse(is.infinite(.) | is.nan(.), 0, .))) %>%
    select(
      index, production_target, scenarioName, scenarioStart,
      propOG, propPlant, propAlb, propEuc, prop1L, prop2L,
      remainingOG, remaining1L, remaining2L
    ) %>%
    distinct()
}

add_bivariate_colours <- function(df, cols) {
  df %>%
    select(-any_of(c("bi_class", "hex", "hexP", "hex1L", "hex2L"))) %>%
    bi_class(x = propOG, y = prop1L, dim = 4, style = "equal") %>%
    left_join(cols, by = "bi_class") %>%
    rename(hexP = hex) %>%
    select(-bi_class) %>%
    bi_class(x = remainingOG, y = remaining1L, dim = 4, style = "equal") %>%
    left_join(cols, by = "bi_class") %>%
    rename(hex1L = hex) %>%
    select(-bi_class) %>%
    bi_class(x = remainingOG, y = remaining2L, dim = 4, style = "equal") %>%
    left_join(cols, by = "bi_class") %>%
    rename(hex2L = hex) %>%
    select(-bi_class)
}

add_plantation_type <- function(df) {
  df %>%
    mutate(shape_class = ifelse(propPlant > 0, "Cross", "Point"))
}

get_carbon_spec <- function(stream = c("scc", "stock_year")) {
  stream <- match.arg(stream)
  if (stream == "scc") {
    return(list(metric = "TOTcarbon_ACD_mean", ylab = "Social Carbon Cost\n(USD 1000M)", divisor = SCALE$bil))
  }
  list(metric = "mean_cum_stock_year", ylab = "Carbon Stock Years", divisor = 1)
}

filter_carbon_variant <- function(df, discount_rate = NULL, slope_variant = NULL) {
  out <- df %>%
    mutate(
      discount_rate = as.character(discount_rate),
      twice_logged_slope_trajectory = as.character(twice_logged_slope_trajectory)
    )

  if (!is.null(discount_rate)) out <- out %>% filter(discount_rate == !!discount_rate)
  if (!is.null(slope_variant)) out <- out %>% filter(twice_logged_slope_trajectory == !!slope_variant)
  out
}

filter_cashflow_variant <- function(df, cashflow_variant = NULL) {
  if (is.null(cashflow_variant) || !("cashflow_variant" %in% names(df))) return(df)
  df %>% filter(cashflow_variant == !!cashflow_variant)
}

master_plot_fun <- function(df, y_ggplot, y_geom_point, ylab_text, scenario_filter) {
  df %>%
    mutate(scenarioName = fct_relevel(scenarioName, scenario_filter)) %>%
    ggplot(aes(x = production_target, y = {{y_ggplot}})) +
    geom_point(
      aes(
        y = {{y_geom_point}},
        colour = case_when(
          scenarioStart %in% c("all_primary", "primary_deforested") ~ hexP,
          scenarioStart %in% c("mostly_1L", "mostly_1L_deforested") ~ hex1L,
          scenarioStart %in% c("mostly_2L", "mostly_2L_deforested") ~ hex2L
        ),
        shape = shape_class
      ),
      alpha = 0.5,
      stroke = 0.5,
      position = position_jitter(width = 0.05, height = -0.03)
    ) +
    scale_colour_identity() +
    scale_shape_manual(values = c("Point" = 19, "Cross" = 2)) +
    xlim(0, 1) +
    xlab(element_blank()) +
    ylab(ylab_text) +
    facet_wrap(~scenarioName, ncol = 4) +
    theme_bw(base_size = SCALE$text_size) +
    theme(
      legend.position = "none",
      panel.grid.major = element_line(color = "grey90"),
      panel.grid.minor = element_blank(),
      strip.text = element_blank()
    )
}

build_main_figure <- function(birds, dung_beetles, megatrees, carbon, profits, protection, all_legend, settings, preset) {
  carbon_spec <- get_carbon_spec(settings$carbon_stream)
  scenario_filter <- preset$scenario_filter

  B <- birds %>%
    filter(scenarioName %in% scenario_filter, bird_grp == settings$bird_group) %>%
    master_plot_fun(medianRelativeOccupancy, medianRelativeOccupancy, "Median Relative\nOccupancy", scenario_filter)

  DB <- dung_beetles %>%
    filter(scenarioName %in% scenario_filter, spp_category == settings$beetle_group) %>%
    master_plot_fun(medianRelativeOccupancy, medianRelativeOccupancy, "Median Relative\nAbundance", scenario_filter)

  M <- megatrees %>%
    filter(scenarioName %in% scenario_filter) %>%
    master_plot_fun(landscape_prop, landscape_prop, "Megatree\nyears", scenario_filter)

  C <- carbon %>%
    filter(scenarioName %in% scenario_filter) %>%
    filter_carbon_variant(discount_rate = settings$carbon_discount_rate, slope_variant = settings$carbon_slope_variant) %>%
    master_plot_fun(.data[[carbon_spec$metric]] / carbon_spec$divisor, .data[[carbon_spec$metric]] / carbon_spec$divisor, carbon_spec$ylab, scenario_filter)

  P <- profits %>%
    filter(
      scenarioName %in% scenario_filter,
      costType == settings$economic_cost_type,
      discount_rate == settings$economic_discount_rate
    ) %>%
    filter_cashflow_variant(cashflow_variant = settings$cashflow_variant) %>%
    master_plot_fun(NPV / SCALE$hundred_million, NPV / SCALE$hundred_million, "Harvest NPV\n(USD 100M)", scenario_filter)

  PC <- protection %>%
    filter(scenarioName %in% scenario_filter, discount_rate == settings$economic_discount_rate) %>%
    filter_cashflow_variant(cashflow_variant = settings$cashflow_variant) %>%
    master_plot_fun(NPV / SCALE$hundred_million, NPV / SCALE$hundred_million, "Protection NPV\n(USD 100M)", scenario_filter)

  plot_grid(plot_grid(B, DB, M, C, P, PC, ncol = 1), all_legend, nrow = 2, rel_heights = c(1, 0.2))
}

if (!RUN_SETTINGS$figure_id %in% names(FIGURE_PRESETS)) {
  stop("RUN_SETTINGS$figure_id must be one of: ", paste(names(FIGURE_PRESETS), collapse = ", "))
}
selected_preset <- FIGURE_PRESETS[[RUN_SETTINGS$figure_id]]

birds <- readRDS(resolve_path(PATHS$birds, "Data/OG_baseline_birdsSept24.rds")) %>% distinct() %>% rename(bird_grp = spp_category)
birds_iucn <- readRDS(resolve_path(PATHS$birds_iucn, "Data/OG_baseline_birdsIUCNSept24.rds")) %>% distinct() %>% rename(bird_grp = threatened)
birds <- bind_rows(birds, birds_iucn) %>% mutate(index = as.character(index), production_target = as.numeric(production_target))

dungBeetles <- readRDS(resolve_path(PATHS$dung_beetles, "Data/MasterDBPerformance.rds")) %>% mutate(index = as.character(index), production_target = as.numeric(production_target))
carbon <- read.csv(PATHS$carbon) %>% mutate(index = as.character(index), production_target = as.numeric(production_target))
megatrees <- readRDS(resolve_path(PATHS$megatrees, "Data/MasterMegatreePerformance_with_uncertainty.rds")) %>% mutate(index = as.character(index), production_target = as.numeric(production_target))

profits <- readRDS(resolve_path(PATHS$financial, "Data/MasterFinancialPerformance.rds")) %>%
  distinct() %>%
  pivot_longer(cols = starts_with("NPV"), names_to = "discount_rate", values_to = "NPV") %>%
  mutate(
    discount_rate = gsub("NPV", "", discount_rate),
    discount_rate = paste0(discount_rate, "%"),
    index = as.character(index),
    production_target = as.numeric(production_target)
  ) %>%
  na.omit() %>%
  as.data.table()
protection <- profits %>% filter(costType == "ProtectionCosts") %>% mutate(outcome = "protection")

scenarios <- readRDS(resolve_path(PATHS$all_scenarios, "Inputs/MasterAllScenarios.rds"))
scenario_composition <- rbindlist(scenarios, use.names = TRUE)
rm(scenarios)

habInStart <- all_start_landscape %>%
  select(scenarioStart) %>%
  distinct() %>%
  mutate(
    originalOG = c(1, 0.2, 0.2, 0.8, 0.2, 0.2),
    original1L = c(0, 0.8, 0, 0, 0.6, 0),
    original2L = c(0, 0, 0.8, 0, 0, 0.6)
  )

propOGcomp <- prop_OG_fun(scenario_composition, habInStart) %>%
  ungroup() %>%
  as.data.table() %>%
  mutate(index = as.character(index), production_target = as.numeric(production_target))

dungBeetles <- as.data.table(dungBeetles)
carbon <- as.data.table(carbon)
megatrees <- as.data.table(megatrees)

birds <- propOGcomp[birds, on = .(index, production_target)]
dungBeetles <- propOGcomp[dungBeetles, on = .(index, production_target)]
carbon <- propOGcomp[carbon, on = .(index, production_target)]
megatrees <- propOGcomp[megatrees, on = .(index, production_target)] %>%
  select(-c(scenarioName, scenarioStart)) %>%
  rename(scenarioName = i.scenarioName, scenarioStart = i.scenarioStart)
profits <- propOGcomp[profits, on = .(index, production_target)]
protection <- propOGcomp[protection, on = .(index, production_target)]

COL <- "BlueOr"
cols <- data.frame(bi_pal(COL, dim = 4, preview = FALSE))
colnames(cols) <- "hex"
cols <- cols %>% mutate(bi_class = rownames(.))

primary_legend <- bi_legend(pal = COL, dim = 4, xlab = "Old-growth", ylab = "Once-logged", size = SCALE$text_size)
onceL_legend <- bi_legend(pal = COL, dim = 4, xlab = "Remaining old-growth", ylab = "Remaining once-logged", size = SCALE$text_size)
twiceL_legend <- bi_legend(pal = COL, dim = 4, xlab = "Remaining old-growth", ylab = "Remaining twice-logged", size = SCALE$text_size)
all_legend <- plot_grid(primary_legend, onceL_legend, twiceL_legend, ncol = 3)

birds <- birds %>% add_bivariate_colours(cols) %>% add_plantation_type() %>% rename_scenario_name()
dungBeetles <- dungBeetles %>% add_bivariate_colours(cols) %>% add_plantation_type() %>% rename_scenario_name()
carbon <- carbon %>% add_bivariate_colours(cols) %>% add_plantation_type() %>% rename_scenario_name()
megatrees <- megatrees %>% add_bivariate_colours(cols) %>% add_plantation_type() %>% rename_scenario_name()
profits <- profits %>% add_bivariate_colours(cols) %>% add_plantation_type() %>% rename_scenario_name()
protection <- protection %>% add_bivariate_colours(cols) %>% add_plantation_type() %>% rename_scenario_name()

required_carbon_cols <- c("TOTcarbon_ACD_mean", "mean_cum_stock_year", "discount_rate", "twice_logged_slope_trajectory")
missing_cols <- setdiff(required_carbon_cols, names(carbon))
if (length(missing_cols) > 0) {
  stop("Carbon table is missing required columns: ", paste(missing_cols, collapse = ", "))
}

figure_obj <- build_main_figure(
  birds = birds,
  dung_beetles = dungBeetles,
  megatrees = megatrees,
  carbon = carbon,
  profits = profits,
  protection = protection,
  all_legend = all_legend,
  settings = RUN_SETTINGS,
  preset = selected_preset
)

file_suffix <- paste0(
  selected_preset$output_stub,
  "__carbon-", RUN_SETTINGS$carbon_stream,
  "__cdr-", ifelse(is.null(RUN_SETTINGS$carbon_discount_rate), "all", RUN_SETTINGS$carbon_discount_rate),
  "__slope-", ifelse(is.null(RUN_SETTINGS$carbon_slope_variant), "all", RUN_SETTINGS$carbon_slope_variant),
  "__edr-", RUN_SETTINGS$economic_discount_rate,
  "__cashflow-", ifelse(is.null(RUN_SETTINGS$cashflow_variant), "all", RUN_SETTINGS$cashflow_variant)
)
file_suffix <- gsub("%", "pct", file_suffix)
out_file <- file.path(PATHS$export_dir, paste0(file_suffix, ".pdf"))

ggsave(figure_obj, filename = out_file, width = SCALE$width, height = SCALE$height, units = "in")
message("Saved: ", out_file)

# -------------------------------------------------------------------
# EXAMPLES: copy ONE block into RUN_SETTINGS and run script
# -------------------------------------------------------------------
#Example 1: Main Figure 2, SCC at 4%, all slope variants
RUN_SETTINGS <- list(
  figure_id = "fig2_d",
  bird_group = "loser",
  beetle_group = "loser",
  economic_discount_rate = "6%",
  economic_cost_type = "HarvestProfits",
  cashflow_variant = "baseline",
  carbon_stream = "scc",
  carbon_discount_rate = "6%",
  carbon_slope_variant = 1.2
)
#
# Example 2: Supplement (NoDef), SCC at 6%, a specific slope trajectory
# RUN_SETTINGS <- list(
#   figure_id = "figs3_nd",
#   bird_group = "loser",
#   beetle_group = "loser",
#   economic_discount_rate = "6%",
#   economic_cost_type = "HarvestProfits",
#   cashflow_variant = "plus25",
#   carbon_stream = "scc",
#   carbon_discount_rate = "6%",
#   carbon_slope_variant = "linear"
# )
#
# Example 3: Deforested-land scenarios, carbon stock-years stream
# RUN_SETTINGS <- list(
#   figure_id = "figs6_d_dl",
#   bird_group = "loser",
#   beetle_group = "loser",
#   economic_discount_rate = "4%",
#   economic_cost_type = "HarvestProfits",
#   cashflow_variant = "minus25",
#   carbon_stream = "stock_year",
#   carbon_discount_rate = NULL,
#   carbon_slope_variant = NULL
# )
