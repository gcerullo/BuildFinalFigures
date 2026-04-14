## Replicable Petal Plot Builder (Nature Revision 2)
## --------------------------------------------------
## This script has TWO modes:
##   1) MODE = "inspect" -> open an interactive scenario picker view
##   2) MODE = "build"   -> render manuscript-style petal figure(s)
##
## QUICK START (EDIT THIS BLOCK ONLY)
## ----------------------------------
## MODE <- "inspect"
##   - Use this to explore scenarios and choose scenario IDs (index values).
##
## MODE <- "build"
##   - Use ACTIVE_RUN and RUN_ALL exactly like other replicable scripts.
##
## To render one figure:
##   RUN_ALL <- FALSE
##   ACTIVE_RUN <- "run1_main_petal"
##
## To render all preset figures:
##   RUN_ALL <- TRUE
##
## Output folder:
##   Figures/PetalPlots/NR2
##

rm(list = ls())
options(scipen = 999)

required_packages <- c("tidyverse", "ggplot2", "data.table", "cowplot", "biscale")
invisible(lapply(required_packages, require, character.only = TRUE))

source("Inputs/FixedScenarioParams.R")

`%||%` <- function(x, y) if (is.null(x)) y else x

PATHS <- list(
  scenarios = "Inputs/MasterAllScenarios.rds",
  birds = "Data/NR2/OG_baseline_birds.rds",
  dung_beetles = "Data/NR2/MasterDBPerformance_fastPilot.rds",
  carbon = "Data/NR2/carbon_outcomes__all_trajectories.rds",
  megatrees = "Data/full_nature_scenario_megatree_performance_all_thresholds.rds",
  financial = "Data/NR2/MasterFinancialPerformance__Sensitivity25pctMoneyParams_LONG.rds",
  export_dir = "Figures/PetalPlots/NR2"
)
dir.create(PATHS$export_dir, recursive = TRUE, showWarnings = FALSE)

MODE <- "build" # "inspect" or "build"
RUN_ALL <- FALSE
ACTIVE_RUN <- "run1_fig1_losers_dr4"
mode_env <- trimws(Sys.getenv("MODE", ""))
if (nchar(mode_env) > 0) MODE <- mode_env

## --------------------------------------------------
## Color setup (kept stable for reproducibility)
## --------------------------------------------------
outcome_colors <- c(
  profits = "#8C5AA3",
  costs = "#7B7FB8",
  birds = "#6AA5B8",
  dungBeetles = "#60C2A8",
  carbon = "#9EDB7E",
  megatrees = "#F1E05A"
)

habitat_colors <- c(
  primary = "#4d9221",
  `once-logged` = "#7fbc41",
  restored = "#b8e186",
  `twice-logged` = "#e6f5d0",
  albizia_current = "#fde0e6",
  albizia_future = "#fde0e6",
  eucalyptus_current = "#f1b6da",
  eucalyptus_future = "#f1b6da",
  deforested = "#c51b7d"
)

## --------------------------------------------------
## Run presets
## --------------------------------------------------
FIGURE_PRESETS <- list(
  fig2_d = list(
    scenario_filter = c("AllPrimary", "Mostly1L", "Mostly2L")
  ),
  figs3_nd = list(
    scenario_filter = c("AllPrimaryNoDef", "Mostly1LNoDef", "Mostly2LNoDef")
  ),
  figs6_d_dl = list(
    scenario_filter = c("MostlyPrimary+DL", "Mostly1L+DL", "Mostly2L+DL")
  )
)

OUTCOME_OPTIONS <- c("birds", "dung_beetles", "megatrees", "carbon", "profits", "protection")

BASE_RUN_SETTINGS <- list(
  figure_id = "fig2_d",
  outcomes_to_plot = OUTCOME_OPTIONS,
  compare_mode = FALSE,
  compare_rows_by = c("carbon_discount_rate"),
  compare_values = list(carbon_discount_rate = c("2%", "4%", "6%")),
  errorbar_mode = "none",
  carbon_errorbar_mode = "model_ci",
  financial_errorbar_mode = "sensitivity_range",
  sensitivity_field = "cashflow_variant",
  sensitivity_values = list(cashflow_variant = c("minus25", "baseline", "plus25")),
  sensitivity_center = "baseline",
  uncertainty_level = "80",
  megatree_uncertainty_level = "95",
  output_pdf_name = NULL,
  production_target = 0.38,
  scenario_starts = c("all_primary", "mostly_1L", "mostly_2L"),
  scenario_start_labels = c(
    all_primary = "Fully old-growth",
    mostly_1L = "Mostly low-intensity logged",
    mostly_2L = "Mostly high-intensity logged"
  ),
  selected_scenarios_by_start = list(
    all_primary = c("all_primary_CY_D.csv 195", "all_primary_CY_D.csv 191", "all_primary_CY_D.csv 192", "all_primary_CY_D.csv 193"),
    mostly_1L = c("mostly_1L_CY_D.csv 352", "mostly_1L_CY_D.csv 347", "93079", "mostly_1L_CY_D.csv 344", "mostly_1L_CY_D.csv 345"),
    mostly_2L = c("mostly_2L_CY_D.csv 274", "mostly_2L_CY_D.csv 270", "mostly_2L_CY_D.csv 269", "mostly_2L_CY_D.csv 268")
  ),
  bird_group = "loser",
  beetle_group = "loser",
  carbon_stream = "scc",                 # "scc" or "stock_year"
  carbon_discount_rate = "4%",
  carbon_slope_variant = "1",
  economic_discount_rate = "4%",
  megatree_height_filt = "50",
  include_outcome_legend = TRUE,
  include_habitat_legend = TRUE
)

RUN_LIBRARY <- list(
  run1_fig1_losers_dr4 = list(
    figure_id = "fig2_d",
    outcomes_to_plot = OUTCOME_OPTIONS,
    errorbar_mode = "none",
    carbon_errorbar_mode = "model_ci",
    financial_errorbar_mode = "sensitivity_range",
    sensitivity_field = "cashflow_variant",
    sensitivity_values = list(cashflow_variant = c("minus25", "baseline", "plus25")),
    sensitivity_center = "baseline",
    uncertainty_level = "80",
    megatree_uncertainty_level = "95",
    bird_group = "loser",
    beetle_group = "loser",
    economic_discount_rate = "4%",
    economic_cost_type = "HarvestProfits",
    cashflow_variant = NULL,
    carbon_stream = "scc",
    carbon_discount_rate = "4%",
    carbon_slope_variant = "1",
    output_pdf_name = "Fig1_losers_dr4.pdf"
  ),
  run1_main_petal = list(
    output_pdf_name = "Fig2_Petal_Main_replicable.pdf"
  ),
  run2_stock_year_petal = list(
    carbon_stream = "stock_year",
    carbon_discount_rate = NULL,
    output_pdf_name = "Fig2_Petal_StockYears_replicable.pdf"
  )
)

merge_run_settings <- function(base, override) {
  out <- base
  for (nm in names(override)) out[[nm]] <- override[[nm]]
  out
}

if (!ACTIVE_RUN %in% names(RUN_LIBRARY)) {
  stop("ACTIVE_RUN must be one of: ", paste(names(RUN_LIBRARY), collapse = ", "))
}
RUN_SETTINGS <- merge_run_settings(BASE_RUN_SETTINGS, RUN_LIBRARY[[ACTIVE_RUN]])

## --------------------------------------------------
## Data loaders (shared with current NR2 inputs)
## --------------------------------------------------
parse_slope_from_name <- function(x) {
  value <- sub("^2Ltraj_", "", x)
  value <- gsub("_", ".", value)
  suppressWarnings(num <- as.numeric(value))
  if (!is.na(num)) return(as.character(num))
  value
}

load_carbon_data <- function(path) {
  obj <- readRDS(path)
  if (is.data.frame(obj) || data.table::is.data.table(obj)) return(as.data.table(obj))
  if (is.list(obj) && !is.null(names(obj)) && all(vapply(obj, is.list, logical(1)))) {
    slope_tables <- purrr::imap(obj, function(slope_block, slope_name) {
      block_df <- data.table::rbindlist(slope_block, fill = TRUE)
      block_df[, twice_logged_slope_trajectory := parse_slope_from_name(slope_name)]
      block_df
    })
    return(data.table::rbindlist(slope_tables, fill = TRUE))
  }
  stop("Unsupported carbon RDS structure at: ", path)
}

build_master_outcomes <- function(settings) {
  birds <- readRDS(PATHS$birds) %>%
    mutate(index = as.character(index), production_target = as.numeric(production_target)) %>%
    filter(spp_category == settings$bird_group) %>%
    transmute(index, production_target, scenarioName, scenarioStart, outcome = "birds", performance = medianRelativeOccupancy)

  dung <- readRDS(PATHS$dung_beetles) %>%
    mutate(index = as.character(index), production_target = as.numeric(production_target)) %>%
    filter(spp_category == settings$beetle_group) %>%
    transmute(index, production_target, scenarioName, scenarioStart, outcome = "dungBeetles", performance = medianRelativeOccupancy)

  carbon <- load_carbon_data(PATHS$carbon) %>%
    mutate(index = as.character(index), production_target = as.numeric(production_target))

  if (!is.null(settings$carbon_discount_rate) && "discount_rate" %in% names(carbon)) {
    requested_dr <- as.character(settings$carbon_discount_rate)
    carbon <- carbon %>%
      mutate(discount_rate = as.character(.data$discount_rate)) %>%
      filter(.data$discount_rate %in% requested_dr)
    if (nrow(carbon) == 0) {
      stop("No carbon rows after discount-rate filter. Requested: ", paste(requested_dr, collapse = ", "))
    }
    present_dr <- sort(unique(as.character(carbon$discount_rate)))
    if (!setequal(requested_dr, present_dr)) {
      stop(
        "Carbon discount-rate filter mismatch. Requested: ",
        paste(requested_dr, collapse = ", "),
        " | Present: ",
        paste(present_dr, collapse = ", ")
      )
    }
  }
  if (!is.null(settings$carbon_slope_variant) && "twice_logged_slope_trajectory" %in% names(carbon)) {
    carbon <- carbon[as.character(twice_logged_slope_trajectory) %in% as.character(settings$carbon_slope_variant), ]
  }

  carbon_metric <- if (identical(settings$carbon_stream, "stock_year")) "mean_cum_stock_year" else "TOTcarbon_ACD_mean"
  carbon_divisor <- if (identical(settings$carbon_stream, "stock_year")) 1e9 else 1e9
  carbon <- carbon %>%
    transmute(index, production_target, scenarioName, scenarioStart, outcome = "carbon", performance = .data[[carbon_metric]] / carbon_divisor)

  megatrees <- readRDS(PATHS$megatrees) %>%
    mutate(index = as.character(index), production_target = as.numeric(production_target), height_filt = as.character(height_filt)) %>%
    filter(height_filt == as.character(settings$megatree_height_filt)) %>%
    transmute(index, production_target, scenarioName, scenarioStart, outcome = "megatrees", performance = landscape_prop)

  financial <- readRDS(PATHS$financial) %>%
    distinct() %>%
    pivot_longer(cols = starts_with("NPV"), names_to = "discount_rate", values_to = "NPV") %>%
    mutate(
      discount_rate = paste0(gsub("NPV", "", discount_rate), "%"),
      index = as.character(index),
      production_target = as.numeric(production_target)
    ) %>%
    filter(discount_rate == as.character(settings$economic_discount_rate))

  profits <- financial %>%
    filter(costType == "HarvestProfits") %>%
    transmute(index, production_target, scenarioName, scenarioStart, outcome = "profits", performance = NPV / 1e8)

  costs <- financial %>%
    filter(costType == "ProtectionCosts") %>%
    transmute(index, production_target, scenarioName, scenarioStart, outcome = "costs", performance = NPV / 1e8)

  style_lookup <- propOGcomp %>%
    select(index, production_target, hexP, hex1L, hex2L) %>%
    distinct()

  bind_rows(profits, costs, birds, dung, carbon, megatrees) %>%
    mutate(outcome = factor(outcome, levels = c("profits", "costs", "birds", "dungBeetles", "carbon", "megatrees"))) %>%
    left_join(style_lookup, by = c("index", "production_target"))
}

rename_scenario_name <- function(df) {
  df %>%
    mutate(
      scenarioName = dplyr::case_when(
        scenarioName == "all_primary_CY_D.csv" ~ "AllPrimary",
        scenarioName == "mostly_1L_CY_D.csv" ~ "Mostly1L",
        scenarioName == "mostly_2L_CY_D.csv" ~ "Mostly2L",
        scenarioName == "all_primary_CY_ND.csv" ~ "AllPrimaryNoDef",
        scenarioName == "mostly_1L_CY_ND.csv" ~ "Mostly1LNoDef",
        scenarioName == "mostly_2L_CY_ND.csv" ~ "Mostly2LNoDef",
        scenarioName == "primary_deforested_CY_D.csv" ~ "MostlyPrimary+DL",
        scenarioName == "mostly_1L_deforested_CY_D.csv" ~ "Mostly1L+DL",
        scenarioName == "mostly_2L_deforested_CY_D.csv" ~ "Mostly2L+DL",
        TRUE ~ scenarioName
      )
    )
}

prop_OG_fun <- function(scenario_composition, hab_in_start) {
  scenario_composition %>%
    group_by(index, production_target) %>%
    mutate(
      propOG = sum(num_parcels[habitat == "primary"]) / 1000,
      propPlant = sum(num_parcels[habitat %in% c("eucalyptus_current", "albizia_current", "albizia_future", "eucalyptus_future")]) / 1000,
      prop1L = sum(num_parcels[habitat == "once-logged"]) / 1000,
      prop2L = sum(num_parcels[habitat == "twice-logged"]) / 1000
    ) %>%
    mutate(
      scenarioStart = scenarioName,
      scenarioStart = stringr::str_remove(scenarioStart, "_IY_ND.csv"),
      scenarioStart = stringr::str_remove(scenarioStart, "_CY_ND.csv"),
      scenarioStart = stringr::str_remove(scenarioStart, "_IY_D.csv"),
      scenarioStart = stringr::str_remove(scenarioStart, "_CY_D.csv")
    ) %>%
    ungroup() %>%
    left_join(hab_in_start, by = "scenarioStart") %>%
    mutate(
      remainingOG = propOG / originalOG,
      remaining1L = prop1L / original1L,
      remaining2L = prop2L / original2L
    ) %>%
    mutate(across(c(remainingOG, remaining1L, remaining2L), ~ ifelse(is.infinite(.) | is.nan(.), 0, .))) %>%
    select(index, production_target, scenarioStart, propOG, prop1L, prop2L, remainingOG, remaining1L, remaining2L) %>%
    distinct()
}

add_bivariate_colours <- function(df, cols) {
  df %>%
    select(-any_of(c("bi_class", "hex", "hexP", "hex1L", "hex2L"))) %>%
    biscale::bi_class(x = propOG, y = prop1L, dim = 4, style = "equal") %>%
    left_join(cols, by = "bi_class") %>%
    rename(hexP = hex) %>%
    select(-bi_class) %>%
    biscale::bi_class(x = remainingOG, y = remaining1L, dim = 4, style = "equal") %>%
    left_join(cols, by = "bi_class") %>%
    rename(hex1L = hex) %>%
    select(-bi_class) %>%
    biscale::bi_class(x = remainingOG, y = remaining2L, dim = 4, style = "equal") %>%
    left_join(cols, by = "bi_class") %>%
    rename(hex2L = hex) %>%
    select(-bi_class)
}

build_composition_lookup <- function(scenario_comp) {
  scenario_comp %>%
    mutate(
      habitat_group = dplyr::case_when(
        habitat == "primary" ~ "primary",
        habitat == "once-logged" ~ "once_logged",
        habitat == "twice-logged" ~ "twice_logged",
        habitat %in% c("eucalyptus_current", "eucalyptus_future") ~ "eucalyptus",
        habitat %in% c("albizia_current", "albizia_future") ~ "albizia",
        habitat == "restored" ~ "restored",
        habitat == "deforested" ~ "deforested",
        TRUE ~ "other"
      )
    ) %>%
    group_by(index, production_target, scenarioName, habitat_group) %>%
    summarise(n = sum(num_parcels), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = habitat_group, values_from = n, values_fill = 0) %>%
    mutate(
      prop_primary = primary / 1000,
      prop_once = once_logged / 1000,
      prop_twice = twice_logged / 1000,
      prop_eucalyptus = eucalyptus / 1000,
      prop_albizia = albizia / 1000,
      prop_restored = restored / 1000,
      prop_deforested = deforested / 1000
    )
}

build_inspect_geompoint_figure <- function(master_df, scenario_comp, settings) {
  outcome_map <- c(
    birds = "birds",
    dung_beetles = "dungBeetles",
    megatrees = "megatrees",
    carbon = "carbon",
    profits = "profits",
    protection = "costs"
  )
  selected_outcomes <- outcome_map[intersect(names(outcome_map), settings$outcomes_to_plot)]
  selected_outcomes <- as.character(selected_outcomes)
  selected_preset <- FIGURE_PRESETS[[settings$figure_id]]
  if (is.null(selected_preset)) stop("Unknown figure_id in inspect mode: ", settings$figure_id)

  base <- master_df %>%
    mutate(outcome = as.character(outcome)) %>%
    rename_scenario_name() %>%
    filter(outcome %in% selected_outcomes) %>%
    filter(scenarioName %in% selected_preset$scenario_filter) %>%
    mutate(
      plot_colour = dplyr::case_when(
        scenarioStart %in% c("all_primary", "primary_deforested") ~ hexP,
        scenarioStart %in% c("mostly_1L", "mostly_1L_deforested") ~ hex1L,
        scenarioStart %in% c("mostly_2L", "mostly_2L_deforested") ~ hex2L,
        TRUE ~ "#6e6e6e"
      )
    )

  comp_lookup <- build_composition_lookup(scenario_comp) %>%
    rename_scenario_name() %>%
    select(index, production_target, scenarioName, starts_with("prop_"))

  base <- base %>%
    left_join(comp_lookup, by = c("index", "production_target", "scenarioName")) %>%
    mutate(
      outcome_label = dplyr::case_when(
        outcome == "birds" ~ "Median Relative\nOccupancy",
        outcome == "dungBeetles" ~ "Median Relative\nAbundance",
        outcome == "megatrees" ~ "Megatree\nyears",
        outcome == "carbon" & identical(settings$carbon_stream, "stock_year") ~ "Carbon Stock Years\n(billion)",
        outcome == "carbon" ~ "Social Carbon Cost\n(USD 1000M)",
        outcome == "profits" ~ "Harvest NPV\n(USD 100M)",
        outcome == "costs" ~ "Protection NPV\n(USD 100M)",
        TRUE ~ outcome
      ),
      outcome_label = factor(outcome_label, levels = c(
        "Median Relative\nOccupancy",
        "Median Relative\nAbundance",
        "Megatree\nyears",
        if (identical(settings$carbon_stream, "stock_year")) "Carbon Stock Years\n(billion)" else "Social Carbon Cost\n(USD 1000M)",
        "Harvest NPV\n(USD 100M)",
        "Protection NPV\n(USD 100M)"
      ))
    ) %>%
    mutate(
      inspect_text = paste0(
        "Index: ", index,
        "<br>Scenario: ", scenarioName,
        "<br>Production target: ", round(production_target, 3),
        "<br>Outcome: ", outcome,
        "<br>Value: ", signif(performance, 4),
        "<br>Primary: ", round(prop_primary, 3),
        "<br>Once-logged: ", round(prop_once, 3),
        "<br>Twice-logged: ", round(prop_twice, 3),
        "<br>Eucalyptus: ", round(prop_eucalyptus, 3),
        "<br>Albizia: ", round(prop_albizia, 3),
        "<br>Restored: ", round(prop_restored, 3),
        "<br>Deforested: ", round(prop_deforested, 3)
      )
    )

  p <- ggplot(base, aes(x = production_target, y = performance, text = inspect_text, key = index)) +
    geom_point(aes(color = plot_colour), size = 2.4, alpha = 0.98) +
    scale_color_identity() +
    facet_grid(outcome_label ~ scenarioName, scales = "free_y", switch = "y") +
    theme_bw(base_size = 11) +
    labs(
      title = "Interactive GeomPoint Scenario Inspector",
      subtitle = "Click/hover points to inspect scenario index and composition",
      x = "Production target",
      y = NULL
    ) +
    theme(
      strip.placement = "outside",
      strip.background = element_blank(),
      strip.text.y.left = element_text(angle = 0, hjust = 1)
    )

  p
}

normalize_performance <- function(df) {
  max_values <- df %>% group_by(outcome) %>% summarise(max_performance = max(abs(performance), na.rm = TRUE), .groups = "drop")
  min_values <- df %>% group_by(outcome) %>% summarise(min_performance = min(abs(performance), na.rm = TRUE), .groups = "drop")
  df %>%
    mutate(performance = abs(performance)) %>%
    left_join(max_values, by = "outcome") %>%
    left_join(min_values, by = "outcome") %>%
    mutate(
      diffScenMin = performance - min_performance,
      diffRange = pmax(max_performance - min_performance, 1e-12),
      normalised_performance = pmin(1.1, pmax(0.1, (diffScenMin / diffRange) + 0.1))
    )
}

petal_plot_for_start <- function(master_df, scenario_comp, settings, scenario_start) {
  selected_ids <- settings$selected_scenarios_by_start[[scenario_start]] %||% character(0)
  df <- master_df %>%
    filter(
      production_target == settings$production_target,
      scenarioStart == scenario_start,
      str_detect(index, "CY_D")
    )
  if (length(selected_ids) > 0) df <- df %>% filter(index %in% selected_ids)
  if (nrow(df) == 0) stop("No rows for scenario_start=", scenario_start, " and production_target=", settings$production_target)

  df_norm <- normalize_performance(df)
  scenario_order <- unique(df_norm$index)
  df_norm <- df_norm %>%
    mutate(index = factor(index, levels = scenario_order)) %>%
    select(index, outcome, normalised_performance) %>%
    tidyr::complete(index, outcome, fill = list(normalised_performance = 0.1)) %>%
    mutate(normalised_performance = pmin(1.1, pmax(0.1, normalised_performance)))

  petals <- ggplot(df_norm, aes(x = outcome, y = normalised_performance, fill = outcome)) +
    geom_col(width = 0.42, color = "black", linewidth = 0.15, alpha = 0.78) +
    scale_fill_manual(values = outcome_colors) +
    ylim(0, 1.1) +
    geom_hline(yintercept = 0.1, linetype = "dashed", color = "grey40") +
    geom_hline(yintercept = 1.1, linetype = "dashed", color = "grey40") +
    coord_polar(clip = "off") +
    facet_wrap(~index, ncol = 1) +
    theme_minimal(base_size = 10) +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      panel.grid = element_line(color = "grey90"),
      strip.text = element_blank(),
      legend.position = "none",
      plot.margin = margin(1, 1, 1, 1)
    )

  comp <- scenario_comp %>%
    filter(
      production_target == settings$production_target,
      scenarioStart == scenario_start,
      index %in% as.character(unique(df_norm$index))
    ) %>%
    mutate(habitat = factor(habitat, levels = names(habitat_colors))) %>%
    ggplot(aes(x = "", y = num_parcels, fill = habitat)) +
    geom_col(width = 1, color = "grey30", linewidth = 0.1) +
    coord_polar("y", start = 0) +
    facet_wrap(~index, ncol = 1) +
    scale_fill_manual(values = habitat_colors) +
    theme_void(base_size = 10) +
    theme(legend.position = "none", strip.text = element_blank())

  start_pie <- all_start_landscape %>%
    filter(scenarioStart == scenario_start) %>%
    mutate(habitat = factor(habitat, levels = names(habitat_colors))) %>%
    ggplot(aes(x = "", y = num_parcels, fill = habitat)) +
    geom_col(width = 1, color = "grey30", linewidth = 0.1) +
    coord_polar("y", start = 0) +
    scale_fill_manual(values = habitat_colors) +
    theme_void() +
    theme(legend.position = "none")

  header <- ggdraw() +
    draw_label(
      paste0("P = ", settings$production_target, " | S = ", settings$scenario_start_labels[[scenario_start]] %||% scenario_start),
      x = 0, hjust = 0, size = 10
    )

  plot_grid(start_pie, header, comp, petals, ncol = 1, rel_heights = c(0.35, 0.10, 0.35, 1))
}

build_final_petal_figure <- function(master_df, scenario_comp, settings) {
  col_plots <- lapply(settings$scenario_starts, function(ss) {
    petal_plot_for_start(master_df, scenario_comp, settings, ss)
  })

  panel_labels <- LETTERS[seq_along(col_plots)]
  for (i in seq_along(col_plots)) {
    col_plots[[i]] <- ggdraw(col_plots[[i]]) + draw_label(panel_labels[i], x = 0.03, y = 0.985, hjust = 0, vjust = 1, size = 30, fontface = "bold")
  }

  main <- plot_grid(plotlist = col_plots, ncol = length(col_plots))
  legends <- list()
  if (isTRUE(settings$include_outcome_legend)) {
    legends <- c(legends, list(
      ggplot(data.frame(outcome = names(outcome_colors), y = 1), aes(x = outcome, y = y, fill = outcome)) +
        geom_tile() +
        scale_fill_manual(values = outcome_colors) +
        theme_void() +
        theme(legend.position = "bottom") +
        guides(fill = guide_legend(title = NULL, nrow = 2))
    ))
  }
  if (isTRUE(settings$include_habitat_legend)) {
    legends <- c(legends, list(
      ggplot(data.frame(habitat = names(habitat_colors), y = 1), aes(x = habitat, y = y, fill = habitat)) +
        geom_tile() +
        scale_fill_manual(values = habitat_colors) +
        theme_void() +
        theme(legend.position = "bottom") +
        guides(fill = guide_legend(title = NULL, nrow = 2))
    ))
  }

  if (length(legends) == 0) return(main)
  legend_block <- plot_grid(plotlist = legends, ncol = length(legends))
  plot_grid(main, legend_block, ncol = 1, rel_heights = c(1, 0.14))
}

build_scenario_picker <- function(scenario_comp, production_targets = NULL) {
  df <- scenario_comp
  if (!is.null(production_targets)) {
    df <- df %>% filter(production_target %in% production_targets)
  }
  summary_df <- df %>%
    mutate(hab_grp = case_when(
      habitat == "primary" ~ "primary",
      habitat == "once-logged" ~ "once_logged",
      habitat == "twice-logged" ~ "twice_logged",
      habitat %in% c("eucalyptus_current", "eucalyptus_future") ~ "eucalyptus",
      habitat %in% c("albizia_current", "albizia_future") ~ "albizia",
      habitat == "restored" ~ "restored",
      habitat == "deforested" ~ "deforested",
      TRUE ~ "other"
    )) %>%
    group_by(index, production_target, scenarioStart, scenarioName, hab_grp) %>%
    summarise(n = sum(num_parcels), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = hab_grp, values_from = n, values_fill = 0) %>%
    mutate(
      prop_primary = primary / 1000,
      prop_once = once_logged / 1000,
      prop_twice = twice_logged / 1000,
      label = paste0(index, " | ", scenarioName, " | P=", production_target)
    )

  picker <- ggplot(
    summary_df,
    aes(
      x = prop_once, y = prop_twice, color = prop_primary,
      text = paste0(
        "Index: ", index,
        "<br>Scenario: ", scenarioName,
        "<br>Start: ", scenarioStart,
        "<br>P target: ", production_target,
        "<br>Primary: ", round(prop_primary, 3),
        "<br>Once-logged: ", round(prop_once, 3),
        "<br>Twice-logged: ", round(prop_twice, 3)
      )
    )
  ) +
    geom_point(alpha = 0.8, size = 2.2) +
    scale_color_viridis_c(option = "D") +
    facet_grid(production_target ~ scenarioStart) +
    theme_bw(base_size = 11) +
    labs(
      x = "Proportion once-logged",
      y = "Proportion twice-logged",
      color = "Proportion primary",
      title = "Scenario picker: hover points to identify scenario indices"
    )

  list(summary_df = summary_df, picker_plot = picker)
}

render_and_save <- function(master_df, scenario_comp, settings, run_label) {
  fig <- build_final_petal_figure(master_df, scenario_comp, settings)
  out_file <- file.path(PATHS$export_dir, settings$output_pdf_name %||% paste0(run_label, ".pdf"))
  if (!grepl("\\.pdf$", out_file, ignore.case = TRUE)) out_file <- paste0(out_file, ".pdf")
  ggsave(out_file, fig, width = 8.27, height = 11.69, units = "in")
  message("Saved [", run_label, "]: ", out_file)
}

## --------------------------------------------------
## Execute
## --------------------------------------------------
scenario_comp_list <- readRDS(PATHS$scenarios)
scenario_composition <- rbindlist(scenario_comp_list, use.names = TRUE) %>%
  mutate(index = as.character(index), production_target = as.numeric(production_target))
rm(scenario_comp_list)

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
  mutate(index = as.character(index), production_target = as.numeric(production_target))
cols <- data.frame(biscale::bi_pal("BlueOr", dim = 4, preview = FALSE))
colnames(cols) <- "hex"
cols <- cols %>% mutate(bi_class = rownames(.))
propOGcomp <- add_bivariate_colours(propOGcomp, cols)

if (identical(tolower(MODE), "inspect")) {
  inspect_master <- build_master_outcomes(RUN_SETTINGS)
  inspect_plot <- build_inspect_geompoint_figure(inspect_master, scenario_composition, RUN_SETTINGS)
  inspect_static_file <- file.path(PATHS$export_dir, "inspect_geompoint_static.pdf")
  ggsave(inspect_static_file, inspect_plot, width = 12, height = 9, units = "in")
  message("Saved inspect figure (static): ", inspect_static_file)

  if (requireNamespace("plotly", quietly = TRUE) && requireNamespace("htmlwidgets", quietly = TRUE)) {
    p_interactive <- plotly::ggplotly(inspect_plot, tooltip = "text")
    html_file <- file.path(PATHS$export_dir, "inspect_geompoint_interactive.html")
    htmlwidgets::saveWidget(p_interactive, html_file, selfcontained = TRUE)
    message("Saved inspect figure (interactive HTML): ", html_file)
  } else {
    warning("Install 'plotly' and 'htmlwidgets' for interactive inspect output. Static PDF was saved.")
  }
} else if (identical(tolower(MODE), "build")) {
  masterDF <- build_master_outcomes(RUN_SETTINGS)
  if (isTRUE(RUN_ALL)) {
    message("RUN_ALL=TRUE: generating all petal presets.")
    for (run_key in names(RUN_LIBRARY)) {
      run_settings <- merge_run_settings(BASE_RUN_SETTINGS, RUN_LIBRARY[[run_key]])
      run_master <- build_master_outcomes(run_settings)
      tryCatch(
        render_and_save(run_master, scenario_composition, run_settings, run_key),
        error = function(e) warning("Skipping ", run_key, " due to error: ", conditionMessage(e))
      )
    }
  } else {
    render_and_save(masterDF, scenario_composition, RUN_SETTINGS, ACTIVE_RUN)
  }
} else {
  stop("MODE must be 'inspect' or 'build'.")
}

