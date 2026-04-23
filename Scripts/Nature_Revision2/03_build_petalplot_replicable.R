# ----------------------------------------------------------------------------
# Nature Revision 2 — petal builder (inspect vs build modes)
#
# I run this either to inspect scenarios interactively or to batch the petal catalogue and Nature-style variants described in the long header below.
# Inputs: Inputs/FixedScenarioParams.R; Inputs/MasterAllScenarios.rds and the NR2 performance RDS files in PATHS.
# Outputs: Figures under Figures/NR2/PetalPlots (and static previews when MODE is inspect).
# ----------------------------------------------------------------------------

## Replicable Petal Plot Builder (Nature Revision 2)
## --------------------------------------------------
## This script has TWO modes:
##   1) MODE = "inspect" -> static + optional interactive geom views
##   2) MODE = "build"   -> render manuscript-style petal figure(s)
##
## QUICK START (EDIT THIS BLOCK ONLY)
## ----------------------------------
## MODE <- "inspect"
##   - Use this to explore scenarios and choose scenario IDs (index values).
##
## MODE <- "build"
##   - Use ACTIVE_RUN and RUN_ALL exactly like other replicable scripts.
##   - Catalog (pick scenarios for the main figure): one petal PDF per
##     starting landscape, every scenario at P=0.33 (BASE production_target):
##       ACTIVE_RUN <- "run_catalog_petals_by_start"
##     Writes e.g. Fig2_Petal_AllScenarios_P0p33_by_start_all_primary.pdf
##     (optional: set PRODUCTION_TARGET in the environment to override P)
##
## Nature-style petal (fully old-growth only, P = 0.5): horizontal petals,
## legends + starting landscape on top, merged plantation land cover:
##   ACTIVE_RUN <- "run_nature_fig2_petal_primary_P05"
##
## To render one figure:
##   RUN_ALL <- FALSE
##   ACTIVE_RUN <- "run1_main_petal"
##
## To render all preset figures:
##   RUN_ALL <- TRUE
##
## Output folder:
##   Figures/NR2/PetalPlots
## Batch all NR2 figures (petals + geom): see 00_build_all_nr2_figures.R
##
## Default production target for manual / inherited runs is 0.33
## (BASE_RUN_SETTINGS$production_target). Diverse-range and all-at-target
## presets set their own targets in RUN_LIBRARY.
##

rm(list = ls())
options(scipen = 999)

required_packages <- c("tidyverse", "ggplot2", "data.table", "cowplot", "biscale", "png", "jpeg")
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
  export_dir = "Figures/NR2/PetalPlots"
)
dir.create(PATHS$export_dir, recursive = TRUE, showWarnings = FALSE)

MODE <- "build" # "inspect" or "build"
RUN_ALL <- FALSE
ACTIVE_RUN <- "run1_fig1_losers_dr4"
mode_env <- trimws(Sys.getenv("MODE", ""))
if (nchar(mode_env) > 0) MODE <- mode_env
run_all_env <- trimws(Sys.getenv("RUN_ALL", ""))
if (nchar(run_all_env) > 0) RUN_ALL <- tolower(run_all_env) %in% c("1", "true", "t", "yes", "y")
active_run_env <- trimws(Sys.getenv("ACTIVE_RUN", ""))
if (nchar(active_run_env) > 0) ACTIVE_RUN <- active_run_env

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

outcome_legend_labels <- c(
  profits = "Harvest profits",
  costs = "Protection cost advantage",
  birds = "Birds",
  dungBeetles = "Dung beetles",
  carbon = "Carbon benefit",
  megatrees = "Megatrees"
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

## Land-cover key for figures that merge current/future plantation into one slice each
habitat_colors_display <- c(
  primary = "#4d9221",
  `once-logged` = "#7fbc41",
  restored = "#b8e186",
  `twice-logged` = "#e6f5d0",
  albizia = "#fde0e6",
  eucalyptus = "#f1b6da",
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
  selection_mode = "manual",             # "manual", "auto_diverse_range", or "all_at_target"
  selection_target_tolerance = 0,
  split_output_by_start = FALSE,
  show_index_on_petal = FALSE,
  production_target_range = c(0.31, 0.35),
  scenarios_per_start = 5,
  production_target = 0.33,
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
  cashflow_variant = "baseline",
  megatree_height_filt = "50",
  include_outcome_legend = TRUE,
  include_habitat_legend = TRUE,
  ## "figure_bottom" (default): legends under the full multipanel figure.
  ## "panel_top": starting-landscape pie, header text, and legends in one row above petals.
  petal_legends_position = "figure_bottom",
  ## "vertical" (default): petals stacked. "horizontal": one row (good for few scenarios).
  petal_arrangement = "vertical",
  ## If TRUE, composition pies and habitat legend use merged Albizia / Eucalyptus (no current vs future).
  merge_plantation_habitat_display = FALSE,
  ## If TRUE with horizontal layout, draw a–d on each petal instead of one letter per start column.
  petal_label_each_petal = FALSE,
  ## If TRUE, draw geom-style outcome icons on one petal (see petal_icon_panel_index).
  petal_show_outcome_icons = FALSE,
  petal_icon_panel_index = 1L,
  petal_icon_radius_npc = 0.44,
  petal_icon_size_npc = 0.09,
  petal_icon_carbon_label_pt = 9,
  ## "horizontal" | "vertical" | "stacked_rows" (recommended for panel_top: separate
  ## full-width legend rows, multi-column keys, no overlap).
  petal_legend_layout = "horizontal"
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
  ## Fully old-growth starting landscape only; P = 0.5; four CY_D scenarios in
  ## one horizontal row. Legends + starting landscape at top; merged plantation
  ## land cover (Albizia / Eucalyptus). ~180 mm width, 600 dpi.
  run_nature_fig2_petal_primary_P05 = list(
    output_pdf_name = "Fig2_Nature_Petal_primary_P0p5.pdf",
    selection_mode = "manual",
    production_target = 0.5,
    scenario_starts = c("all_primary"),
    selected_scenarios_by_start = list(
      all_primary = c(
        "all_primary_CY_D.csv 254",
        "all_primary_CY_D.csv 251",
        "all_primary_CY_D.csv 252",
        "all_primary_CY_D.csv 253"
      )
    ),
    split_output_by_start = FALSE,
    show_index_on_petal = FALSE,
    include_outcome_legend = TRUE,
    include_habitat_legend = TRUE,
    petal_legends_position = "panel_top",
    petal_legend_layout = "stacked_rows",
    petal_arrangement = "horizontal",
    merge_plantation_habitat_display = TRUE,
    petal_label_each_petal = TRUE,
    petal_show_outcome_icons = TRUE,
    petal_icon_panel_index = 1L,
    petal_icon_radius_npc = 0.46,
    petal_icon_size_npc = 0.118,
    petal_icon_carbon_label_pt = 10,
    figure_width_in = 7.08,
    figure_height_in = 5.85,
    figure_dpi = 600,
    petal_inner_base_size = 8,
    petal_composition_base_size = 6.5,
    petal_header_title_size = 9,
    petal_header_subtitle_size = 7.5,
    petal_index_label_size = 6.5,
    petal_panel_letter_size = 11,
    petal_legend_base_size = 7.5,
    petal_legend_text_size = 7
  ),
  run2_stock_year_petal = list(
    carbon_stream = "stock_year",
    carbon_discount_rate = NULL,
    output_pdf_name = "Fig2_Petal_StockYears_replicable.pdf"
  ),
  run3_diverse_0p31_0p35 = list(
    output_pdf_name = "Fig2_Petal_diverse_0p31_0p35.pdf",
    selection_mode = "auto_diverse_range",
    production_target_range = c(0.31, 0.35),
    scenarios_per_start = 5,
    scenario_starts = c("all_primary", "mostly_1L", "mostly_2L"),
    selected_scenarios_by_start = list(
      all_primary = character(0),
      mostly_1L = character(0),
      mostly_2L = character(0)
    )
  ),
  ## One PDF per scenario_starts[]: every scenario at production_target (from
  ## BASE_RUN_SETTINGS, or PRODUCTION_TARGET env after merge).
  run_catalog_petals_by_start = list(
    output_pdf_name = "Fig2_Petal_AllScenarios_P0p33_by_start.pdf",
    selection_mode = "all_at_target",
    selection_target_tolerance = 1e-8,
    split_output_by_start = TRUE,
    show_index_on_petal = TRUE,
    scenario_starts = c("all_primary", "mostly_1L", "mostly_2L"),
    carbon_discount_rate = "4%",
    carbon_slope_variant = "1",
    economic_discount_rate = "4%",
    cashflow_variant = "baseline",
    include_outcome_legend = TRUE,
    include_habitat_legend = TRUE
  ),
  # ================================================================
  # TEMPLATE FOR FINAL INTEGRATED FIGURE
  # USER ACTION REQUIRED:
  # - Edit selected_scenarios_by_start below.
  # - The order you provide is petal order: top-to-bottom when petal_arrangement
  #   is vertical, left-to-right when petal_arrangement is horizontal.
  # ================================================================
  run5_selected_indices_template = list(
    output_pdf_name = "Fig2_Petal_SelectedIndices_template.pdf",
    selection_mode = "manual",
    split_output_by_start = FALSE,
    show_index_on_petal = FALSE,
    production_target = 0.33,
    scenario_starts = c("all_primary", "mostly_1L", "mostly_2L"),
    carbon_discount_rate = "4%",
    carbon_slope_variant = "1",
    economic_discount_rate = "4%",
    cashflow_variant = "baseline",
    selected_scenarios_by_start = list(
      # ---- EDIT HERE: all_primary (top -> bottom) ----
      all_primary = c("all_primary_CY_D.csv 251", "all_primary_CY_D.csv 252", "all_primary_CY_D.csv 253", "all_primary_CY_D.csv 254", "all_primary_CY_D.csv 255"),
      # ---- EDIT HERE: mostly_1L (top -> bottom) ----
      mostly_1L = c("mostly_1L_CY_D.csv 441", "mostly_1L_CY_D.csv 442", "mostly_1L_CY_D.csv 443", "mostly_1L_CY_D.csv 444", "mostly_1L_CY_D.csv 445"),
      # ---- EDIT HERE: mostly_2L (top -> bottom) ----
      mostly_2L = c("mostly_2L_CY_D.csv 352", "mostly_2L_CY_D.csv 353", "mostly_2L_CY_D.csv 354", "mostly_2L_CY_D.csv 355", "mostly_2L_CY_D.csv 356")
    ),
    include_outcome_legend = TRUE,
    include_habitat_legend = TRUE
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

prod_target_env <- trimws(Sys.getenv("PRODUCTION_TARGET", ""))
if (nchar(prod_target_env) > 0) {
  pt <- suppressWarnings(as.numeric(prod_target_env))
  if (!is.finite(pt)) {
    stop("PRODUCTION_TARGET must be a finite number, got: ", prod_target_env)
  }
  RUN_SETTINGS$production_target <- pt
}

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
  if ("cashflow_variant" %in% names(financial)) {
    cf_variant <- as.character(settings$cashflow_variant %||% "baseline")
    financial <- financial %>% filter(as.character(.data$cashflow_variant) %in% cf_variant)
    if (nrow(financial) == 0) {
      stop("No financial rows after cashflow_variant filter. Requested: ", paste(cf_variant, collapse = ", "))
    }
  }

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
    group_by(index, production_target, scenarioStart, scenarioName, habitat_group) %>%
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

select_diverse_scenarios <- function(composition_df, scenario_start, n_select = 5) {
  candidates <- composition_df %>%
    filter(scenarioStart == scenario_start) %>%
    distinct(index, production_target, scenarioName, scenarioStart, .keep_all = TRUE)
  if (nrow(candidates) == 0) return(candidates)

  feature_cols <- c("prop_primary", "prop_once", "prop_twice", "prop_eucalyptus", "prop_albizia", "prop_restored", "prop_deforested")
  X <- as.matrix(candidates[, feature_cols])
  if (nrow(X) <= n_select) {
    candidates$selection_rank <- seq_len(nrow(candidates))
    return(candidates)
  }

  # Greedy max-min diversity selection.
  center <- colMeans(X, na.rm = TRUE)
  d_center <- sqrt(rowSums((X - matrix(center, nrow(X), length(center), byrow = TRUE))^2))
  selected <- which.max(d_center)[1]

  while (length(selected) < n_select) {
    remaining <- setdiff(seq_len(nrow(X)), selected)
    d_min <- vapply(remaining, function(i) {
      min(sqrt(rowSums((X[selected, , drop = FALSE] - matrix(X[i, ], nrow = length(selected), ncol(X), byrow = TRUE))^2)))
    }, numeric(1))
    selected <- c(selected, remaining[which.max(d_min)])
  }

  out <- candidates[selected, , drop = FALSE]
  out$selection_rank <- seq_len(nrow(out))
  out
}

auto_select_scenarios_by_composition <- function(scenario_comp, settings) {
  comp <- build_composition_lookup(scenario_comp)
  pt_rng <- settings$production_target_range %||% c(settings$production_target, settings$production_target)
  comp <- comp %>%
    filter(
      scenarioStart %in% settings$scenario_starts,
      production_target >= min(pt_rng),
      production_target <= max(pt_rng),
      stringr::str_detect(scenarioName, "_CY_D\\.csv$")
    )

  selected <- lapply(settings$scenario_starts, function(ss) {
    select_diverse_scenarios(comp, ss, n_select = settings$scenarios_per_start %||% 5)
  })
  bind_rows(selected)
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
    ) %>%
    mutate(
      # For costs and SCC-like outcomes, lower raw values are better.
      normalised_performance = ifelse(
        outcome %in% c("costs", "carbon"),
        1.2 - normalised_performance,
        normalised_performance
      ),
      normalised_performance = pmin(1.1, pmax(0.1, normalised_performance))
    )
}

habitat_labels_display <- c(
  primary = "Primary forest",
  `once-logged` = "Once-logged forest",
  restored = "Restored",
  `twice-logged` = "Twice-logged forest",
  albizia = "Albizia",
  eucalyptus = "Eucalyptus",
  deforested = "Deforested"
)

## Same asset paths as Scripts/Nature_Revision_2/02_build_modified_final_figures.R (geom fig).
PETAL_OUTCOME_ICON_PATHS <- list(
  profits = c("Figures/Thumbnails/dollar-sign.svg", "Figures/Thumbnails/Axe.png"),
  costs = "Figures/Thumbnails/dollar-sign.svg",
  birds = "Figures/Thumbnails/RhinoH.jpg",
  dungBeetles = "Figures/Thumbnails/DB.svg",
  megatrees = "Figures/Thumbnails/Dipterocarp.svg",
  carbon = NA_character_
)

load_petal_icon_grob <- function(icon_path, width_px = 200L, height_px = 200L) {
  if (is.null(icon_path) || length(icon_path) != 1L || is.na(icon_path) || !nzchar(icon_path)) return(NULL)
  if (!file.exists(icon_path)) {
    warning("Petal icon missing, skipping: ", icon_path)
    return(NULL)
  }
  ext <- tolower(tools::file_ext(icon_path))
  raster <- NULL
  if (ext == "svg") {
    if (!requireNamespace("rsvg", quietly = TRUE)) {
      warning("Package 'rsvg' required for SVG icon: ", icon_path)
      return(NULL)
    }
    tmp_png <- tempfile(fileext = ".png")
    rsvg::rsvg_png(icon_path, file = tmp_png, width = width_px, height = height_px)
    raster <- png::readPNG(tmp_png)
  } else if (ext == "png") {
    raster <- png::readPNG(icon_path)
    if (tolower(basename(icon_path)) == "axe.png") {
      raster <- raster[, rev(seq_len(ncol(raster))), , drop = FALSE]
      if (length(dim(raster)) == 3L && dim(raster)[3] == 3L) {
        h <- dim(raster)[1]
        w <- dim(raster)[2]
        alpha <- ifelse(
          (raster[, , 1] > 0.92) & (raster[, , 2] > 0.92) & (raster[, , 3] > 0.92),
          0, 1
        )
        rgba <- array(0, dim = c(h, w, 4))
        rgba[, , 1:3] <- raster
        rgba[, , 4] <- alpha
        raster <- rgba
      } else if (length(dim(raster)) == 3L && dim(raster)[3] == 4L) {
        white_mask <- (raster[, , 1] > 0.92) & (raster[, , 2] > 0.92) & (raster[, , 3] > 0.92)
        raster[, , 4] <- ifelse(white_mask, 0, raster[, , 4])
      }
    }
  } else if (ext %in% c("jpg", "jpeg")) {
    if (!requireNamespace("jpeg", quietly = TRUE)) {
      warning("Package 'jpeg' required for JPG icon: ", icon_path)
      return(NULL)
    }
    raster <- jpeg::readJPEG(icon_path)
    if (tolower(basename(icon_path)) == "rhinoh.jpg" && length(dim(raster)) == 3L) {
      h <- dim(raster)[1]
      w <- dim(raster)[2]
      bg_mask <- (raster[, , 1] > 0.82) & (raster[, , 2] > 0.82) & (raster[, , 3] > 0.82)
      text_zone <- matrix(FALSE, nrow = h, ncol = w)
      text_zone[seq_len(max(1L, floor(h * 0.30))), seq_len(max(1L, floor(w * 0.45)))] <- TRUE
      alpha <- ifelse(bg_mask | text_zone, 0, 1)
      rgba <- array(0, dim = c(h, w, 4))
      rgba[, , 1:3] <- raster
      rgba[, , 4] <- alpha
      raster <- rgba
    }
  } else {
    warning("Unsupported icon extension: ", icon_path)
    return(NULL)
  }
  grid::rasterGrob(raster, interpolate = TRUE)
}

add_petal_outcome_icons_to_draw <- function(drawing, settings) {
  outcomes_ordered <- names(outcome_colors)
  n <- length(outcomes_ordered)
  r <- as.numeric(settings$petal_icon_radius_npc %||% 0.44)
  sz <- as.numeric(settings$petal_icon_size_npc %||% 0.09)
  cpt <- settings$petal_icon_carbon_label_pt %||% 9
  ## Match coord_polar(clip = "off", start = pi/2): first outcome at 12 o'clock, clockwise.
  for (i in seq_len(n)) {
    oc <- outcomes_ordered[[i]]
    theta <- pi / 2 - (i - 0.5) * 2 * pi / n
    xc <- 0.5 + r * cos(theta)
    yc <- 0.5 + r * sin(theta)
    clip_icons <- "on"
    if (oc == "carbon") {
      bw <- sz * 0.5
      bh <- sz * 0.72
      box_grob <- grid::rectGrob(gp = grid::gpar(fill = "white", col = "black", lwd = 1.2))
      drawing <- drawing +
        draw_grob(box_grob, x = xc - bw / 2, y = yc - bh / 2, width = bw, height = bh, clip = clip_icons) +
        draw_label("C", x = xc, y = yc, size = cpt, fontface = "bold")
    } else if (oc == "profits") {
      ## Axe only (geom uses dollar + axe; axe alone avoids duplicate $ next to protection).
      axe_path <- PETAL_OUTCOME_ICON_PATHS[["profits"]][[2]]
      g <- load_petal_icon_grob(axe_path, 240L, 240L)
      if (!is.null(g)) drawing <- drawing + draw_grob(g, x = xc - sz / 2, y = yc - sz / 2, width = sz, height = sz, clip = clip_icons)
    } else {
      paths <- PETAL_OUTCOME_ICON_PATHS[[oc]]
      if (is.null(paths)) next
      if (length(paths) == 1L) {
        g <- load_petal_icon_grob(paths, 240L, 240L)
        if (!is.null(g)) drawing <- drawing + draw_grob(g, x = xc - sz / 2, y = yc - sz / 2, width = sz, height = sz, clip = clip_icons)
      } else {
        g1 <- load_petal_icon_grob(paths[[1]], 200L, 200L)
        g2 <- load_petal_icon_grob(paths[[2]], 200L, 200L)
        hw <- sz * 0.48
        if (!is.null(g1)) drawing <- drawing + draw_grob(g1, x = xc - sz * 0.52, y = yc - sz / 2, width = hw, height = sz, clip = clip_icons)
        if (!is.null(g2)) drawing <- drawing + draw_grob(g2, x = xc + sz * 0.04, y = yc - sz / 2, width = hw, height = sz, clip = clip_icons)
      }
    }
  }
  drawing
}

make_petal_discrete_legend_grob <- function(color_values, labels_map, title, nrow, leg_base, leg_text_pt,
                                            legend_layout = "horizontal") {
  key <- names(color_values)
  legend_df <- tibble(item = factor(key, levels = key), x = 1L, y = 1L)
  ltp <- leg_text_pt %||% 8
  nk <- length(key)
  p_leg <- ggplot(legend_df, aes(x = .data$x, y = .data$y, fill = .data$item)) +
    geom_point(shape = 22, size = 3.4, color = "black", stroke = 0.2) +
    scale_fill_manual(values = color_values, labels = labels_map[key], name = title) +
    theme_void(base_size = leg_base) +
    theme(
      legend.title = element_text(face = "bold", size = ltp + 0.5),
      legend.text = element_text(size = ltp),
      legend.key.spacing.y = grid::unit(0.35, "lines"),
      legend.key.spacing.x = grid::unit(0.5, "lines"),
      legend.margin = margin(4, 4, 4, 4)
    )
  if (identical(legend_layout, "vertical")) {
    p_leg <- p_leg +
      theme(legend.position = "left") +
      guides(fill = guide_legend(ncol = 1L, byrow = TRUE))
  } else if (identical(legend_layout, "stacked_rows")) {
    ## Wide multi-column row: fits full-width legend band without stacking keys on top of each other.
    ncol_leg <- if (nk <= 4L) 2L else if (nk <= 6L) 3L else 3L
    nrow_leg <- as.integer(ceiling(nk / ncol_leg))
    p_leg <- p_leg +
      theme(legend.position = "bottom") +
      guides(fill = guide_legend(direction = "horizontal", nrow = nrow_leg, ncol = ncol_leg, byrow = TRUE))
  } else {
    p_leg <- p_leg +
      theme(legend.position = "bottom") +
      guides(fill = guide_legend(nrow = nrow, byrow = TRUE))
  }
  cowplot::get_legend(p_leg)
}

petal_plot_for_start <- function(master_df, scenario_comp, settings, scenario_start, selected_table = NULL) {
  pt_petal <- settings$petal_inner_base_size %||% 10
  pt_comp <- settings$petal_composition_base_size %||% max(6, pt_petal - 2)
  sz_header_title <- settings$petal_header_title_size %||% 9.5
  sz_header_sub <- settings$petal_header_subtitle_size %||% 8.2
  sz_index <- settings$petal_index_label_size %||% 9

  if (!is.null(selected_table)) {
    selected_for_start <- selected_table %>%
      filter(scenarioStart == scenario_start) %>%
      distinct(index, production_target, scenarioName, scenarioStart, selection_rank)
    df <- master_df %>%
      inner_join(selected_for_start, by = c("index", "production_target", "scenarioName", "scenarioStart"))
  } else {
    selected_ids <- settings$selected_scenarios_by_start[[scenario_start]] %||% character(0)
    df <- master_df %>%
      filter(
        production_target == settings$production_target,
        scenarioStart == scenario_start,
        str_detect(index, "CY_D")
      )
    if (length(selected_ids) > 0) df <- df %>% filter(index %in% selected_ids)
  }
  if (nrow(df) == 0) stop("No rows for scenario_start=", scenario_start)
  if (!("selection_rank" %in% names(df))) df$selection_rank <- NA_real_

  selected_keys <- df %>%
    distinct(index, production_target, scenarioName, scenarioStart, selection_rank)

  # Normalize using ALL scenarios within this starting landscape at the
  # selected production target(s), then subset to selected petals.
  norm_reference <- master_df %>%
    filter(
      scenarioStart == scenario_start,
      production_target %in% unique(df$production_target)
    )
  if (nrow(norm_reference) == 0) norm_reference <- df

  df_norm <- normalize_performance(norm_reference) %>%
    inner_join(selected_keys, by = c("index", "production_target", "scenarioName", "scenarioStart")) %>%
    group_by(index, production_target, scenarioName, scenarioStart, outcome, selection_rank) %>%
    summarise(normalised_performance = mean(normalised_performance, na.rm = TRUE), .groups = "drop") %>%
    mutate(plot_id = paste0(index, " | P=", format(round(production_target, 2), nsmall = 2)))

  df_norm <- df_norm %>%
    select(plot_id, outcome, normalised_performance) %>%
    tidyr::complete(plot_id, outcome, fill = list(normalised_performance = 0.1)) %>%
    mutate(normalised_performance = pmin(1.1, pmax(0.1, normalised_performance)))

  comp_ids <- df %>%
    distinct(index, production_target, scenarioName, scenarioStart, selection_rank) %>%
    mutate(plot_id = paste0(index, " | P=", format(round(production_target, 2), nsmall = 2)))

  comp_df <- scenario_comp %>%
    inner_join(comp_ids, by = c("index", "production_target", "scenarioName", "scenarioStart")) %>%
    mutate(habitat = factor(habitat, levels = names(habitat_colors)))

  merge_disp <- isTRUE(settings$merge_plantation_habitat_display)
  hab_plot_cols <- if (merge_disp) habitat_colors_display else habitat_colors
  if (merge_disp) {
    comp_df_plot <- comp_df %>%
      mutate(
        hab_chr = as.character(habitat),
        habitat_plot = dplyr::case_when(
          hab_chr %in% c("albizia_current", "albizia_future") ~ "albizia",
          hab_chr %in% c("eucalyptus_current", "eucalyptus_future") ~ "eucalyptus",
          TRUE ~ hab_chr
        )
      ) %>%
      group_by(plot_id, index, production_target, scenarioName, scenarioStart, habitat_plot) %>%
      summarise(num_parcels = sum(num_parcels, na.rm = TRUE), .groups = "drop") %>%
      mutate(habitat_plot = factor(habitat_plot, levels = names(hab_plot_cols)))
  } else {
    comp_df_plot <- comp_df %>%
      mutate(habitat_plot = factor(as.character(habitat), levels = names(hab_plot_cols)))
  }

  scenario_metrics <- comp_df %>%
    group_by(plot_id, index, production_target, scenarioName, scenarioStart) %>%
    summarise(
      plantation_share = sum(num_parcels[habitat %in% c("eucalyptus_current", "eucalyptus_future", "albizia_current", "albizia_future")], na.rm = TRUE) / 1000,
      twice_logged_share = sum(num_parcels[habitat == "twice-logged"], na.rm = TRUE) / 1000,
      production_yield = suppressWarnings(max(as.numeric(production_yield), na.rm = TRUE)),
      selection_rank = suppressWarnings(min(as.numeric(selection_rank), na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(
      production_yield = ifelse(is.finite(production_yield), production_yield, NA_real_),
      selection_rank = ifelse(is.finite(selection_rank), selection_rank, NA_real_)
    )

  top_plot_id <- scenario_metrics %>%
    arrange(desc(plantation_share), desc(production_target), plot_id) %>%
    slice(1) %>%
    pull(plot_id)

  has_manual_rank <- any(!is.na(scenario_metrics$selection_rank))
  if (isTRUE(has_manual_rank)) {
    scenario_order <- scenario_metrics %>%
      mutate(selection_rank = ifelse(is.na(selection_rank), Inf, selection_rank)) %>%
      arrange(selection_rank, plot_id) %>%
      pull(plot_id)
  } else {
    scenario_order <- scenario_metrics %>%
      mutate(
        is_top_candidate = plot_id == top_plot_id,
        is_twice_logged = twice_logged_share > 1e-9,
        sort_group = case_when(
          is_top_candidate ~ 1L,
          is_twice_logged ~ 3L,
          TRUE ~ 2L
        )
      ) %>%
      arrange(sort_group, twice_logged_share, desc(plantation_share), plot_id) %>%
      pull(plot_id)
  }

  df_norm <- df_norm %>% mutate(plot_id = factor(plot_id, levels = scenario_order))
  plot_ids <- scenario_order
  arrange_horizontal <- identical(settings$petal_arrangement, "horizontal")
  ncol_petals <- if (arrange_horizontal) length(plot_ids) else 1L
  letter_sz <- settings$petal_panel_letter_size %||% 14

  petal_rows <- lapply(seq_along(plot_ids), function(ji) {
    pid <- plot_ids[[ji]]
    idx_label <- trimws(strsplit(pid, "\\|", fixed = FALSE)[[1]][1])
    petal_one <- df_norm %>%
      filter(as.character(plot_id) == pid) %>%
      mutate(outcome = factor(outcome, levels = names(outcome_colors))) %>%
      ggplot(aes(x = outcome, y = normalised_performance, fill = outcome)) +
      geom_col(width = 0.42, color = "black", linewidth = 0.15, alpha = 0.78) +
      scale_fill_manual(values = outcome_colors) +
      ylim(0, 1.1) +
      geom_hline(yintercept = 0.1, linetype = "dashed", color = "grey40") +
      geom_hline(yintercept = 1.1, linetype = "dashed", color = "grey40") +
      coord_polar(clip = "off", start = pi / 2) +
      theme_minimal(base_size = pt_petal) +
      theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_line(color = "grey90"),
        legend.position = "none",
        plot.margin = margin(2, 2, 2, 2)
      )

    comp_one <- comp_df_plot %>%
      filter(as.character(plot_id) == pid) %>%
      ggplot(aes(x = "", y = num_parcels, fill = habitat_plot)) +
      geom_col(width = 1, color = "grey30", linewidth = 0.1) +
      coord_polar("y", start = 0) +
      scale_fill_manual(values = hab_plot_cols, drop = FALSE) +
      theme_void(base_size = pt_comp) +
      theme(legend.position = "none")

    out_plot <- ggdraw(petal_one) +
      draw_plot(comp_one, x = 0.58, y = 0.55, width = 0.38, height = 0.38)
    if (isTRUE(settings$show_index_on_petal)) {
      out_plot <- out_plot + draw_label(idx_label, x = 0.50, y = 0.50, size = sz_index, fontface = "bold")
    }
    icon_idx <- as.integer(settings$petal_icon_panel_index %||% 1L)
    if (isTRUE(settings$petal_show_outcome_icons) && ji == icon_idx) {
      out_plot <- add_petal_outcome_icons_to_draw(out_plot, settings)
    }
    if (isTRUE(settings$petal_label_each_petal)) {
      pl <- if (length(plot_ids) <= 26) LETTERS[ji] else as.character(ji)
      letter_band <- ggdraw() +
        draw_label(pl, x = 0.5, y = 0.5, hjust = 0.5, vjust = 0.5, size = letter_sz * 0.88, fontface = "bold")
      out_plot <- plot_grid(letter_band, out_plot, ncol = 1L, rel_heights = c(0.065, 1), align = "v")
    }
    out_plot
  })

  petals <- plot_grid(plotlist = petal_rows, ncol = ncol_petals)

  start_pie_df <- all_start_landscape %>% filter(scenarioStart == scenario_start)
  if (merge_disp) {
    start_pie_df <- start_pie_df %>%
      mutate(
        hab_chr = as.character(habitat),
        habitat_plot = dplyr::case_when(
          hab_chr %in% c("albizia_current", "albizia_future") ~ "albizia",
          hab_chr %in% c("eucalyptus_current", "eucalyptus_future") ~ "eucalyptus",
          TRUE ~ hab_chr
        )
      ) %>%
      group_by(scenarioStart, habitat_plot) %>%
      summarise(num_parcels = sum(num_parcels, na.rm = TRUE), .groups = "drop") %>%
      mutate(habitat_plot = factor(habitat_plot, levels = names(hab_plot_cols)))
  } else {
    start_pie_df <- start_pie_df %>%
      mutate(habitat_plot = factor(as.character(habitat), levels = names(hab_plot_cols)))
  }

  start_pie <- ggplot(start_pie_df, aes(x = "", y = num_parcels, fill = habitat_plot)) +
    geom_col(width = 1, color = "grey30", linewidth = 0.1) +
    coord_polar("y", start = 0) +
    scale_fill_manual(values = hab_plot_cols, drop = FALSE) +
    theme_void(base_size = pt_comp) +
    theme(legend.position = "none")

  pt_tgt <- unique(scenario_metrics$production_target)
  pt_tgt <- pt_tgt[is.finite(pt_tgt)]
  pt_line <- if (length(pt_tgt) == 1L) {
    paste0("Timber production target: ", format(round(pt_tgt[[1]], 2), nsmall = 2))
  } else {
    "Timber production target (mixed)"
  }

  header <- ggdraw() +
    draw_label(
      paste0("Starting landscape: ", settings$scenario_start_labels[[scenario_start]] %||% scenario_start),
      x = 0, y = 0.78, hjust = 0, size = sz_header_title
    ) +
    draw_label(pt_line, x = 0, y = 0.48, hjust = 0, size = sz_header_sub) +
    draw_label(
      {
        prod_vals_m <- (scenario_metrics$production_yield %||% numeric(0)) / 1e6
        prod_vals_m <- prod_vals_m[is.finite(prod_vals_m)]
        if (length(prod_vals_m) > 0) {
          med <- median(prod_vals_m, na.rm = TRUE)
          paste0(
            "Timber yield: ",
            format(round(med, 2), nsmall = 2),
            " Mm3 yr-1"
          )
        } else {
          "Scenario timber yields: unavailable"
        }
      },
      x = 0, y = 0.16, hjust = 0, size = sz_header_sub
    )

  leg_here <- identical(settings$petal_legends_position, "panel_top") &&
    (isTRUE(settings$include_outcome_legend) || isTRUE(settings$include_habitat_legend))
  leg_base_top <- settings$petal_legend_base_size %||% 10
  leg_txt_top <- settings$petal_legend_text_size %||% 8
  leg_layout_top <- settings$petal_legend_layout %||% "horizontal"

  if (leg_here) {
    top_legs <- list()
    if (isTRUE(settings$include_outcome_legend)) {
      top_legs <- c(
        top_legs,
        list(make_petal_discrete_legend_grob(
          outcome_colors, outcome_legend_labels, "Outcomes", 2L,
          leg_base_top, leg_txt_top, legend_layout = leg_layout_top
        ))
      )
    }
    if (isTRUE(settings$include_habitat_legend)) {
      hab_lab_top <- if (merge_disp) {
        habitat_labels_display
      } else {
        c(
          primary = "Primary",
          `once-logged` = "Once-logged",
          restored = "Restored",
          `twice-logged` = "Twice-logged",
          albizia_current = "Albizia (current)",
          albizia_future = "Albizia (future)",
          eucalyptus_current = "Eucalyptus (current)",
          eucalyptus_future = "Eucalyptus (future)",
          deforested = "Deforested"
        )
      }
      hab_nrow <- if (merge_disp) 2L else 3L
      leg_title <- if (merge_disp) "Land cover" else "Management regimes"
      top_legs <- c(
        top_legs,
        list(make_petal_discrete_legend_grob(
          hab_plot_cols, hab_lab_top, leg_title, hab_nrow,
          leg_base_top, leg_txt_top, legend_layout = leg_layout_top
        ))
      )
    }
    legend_strip <- if (length(top_legs) == 1) {
      top_legs[[1]]
    } else if (identical(leg_layout_top, "stacked_rows")) {
      ## Each legend is its own full-width row (no horizontal overlap between keys).
      leg_row_out <- ggdraw() + draw_grob(top_legs[[1]], x = 0, y = 0, width = 1, height = 1, hjust = 0, vjust = 0)
      leg_row_hab <- ggdraw() + draw_grob(top_legs[[2]], x = 0, y = 0, width = 1, height = 1, hjust = 0, vjust = 0)
      plot_grid(leg_row_out, leg_row_hab, ncol = 1L, rel_heights = c(1, 1.08), align = "v")
    } else if (identical(leg_layout_top, "vertical")) {
      plot_grid(plotlist = top_legs, ncol = 1L, rel_heights = c(1, 1.05))
    } else {
      plot_grid(plotlist = top_legs, ncol = length(top_legs), rel_widths = rep(1, length(top_legs)))
    }
    top_left <- plot_grid(
      start_pie,
      header,
      nrow = 1L,
      rel_widths = c(0.18, 0.82),
      align = "h",
      axis = "tb"
    )
    if (identical(leg_layout_top, "stacked_rows")) {
      top_block <- plot_grid(
        top_left,
        legend_strip,
        ncol = 1L,
        rel_heights = c(0.42, 0.58),
        align = "v"
      )
      plot_grid(top_block, petals, ncol = 1L, rel_heights = c(0.48, 1))
    } else {
      top_row <- plot_grid(
        top_left,
        legend_strip,
        nrow = 1L,
        rel_widths = if (identical(leg_layout_top, "vertical")) c(0.38, 0.62) else c(0.42, 0.58),
        align = "h",
        axis = "tb"
      )
      plot_grid(top_row, petals, ncol = 1, rel_heights = if (identical(leg_layout_top, "vertical")) c(0.4, 1) else c(0.28, 1))
    }
  } else {
    plot_grid(start_pie, header, petals, ncol = 1, rel_heights = c(0.16, 0.12, 1))
  }
}

build_final_petal_figure <- function(master_df, scenario_comp, settings, selected_table = NULL) {
  col_plots <- lapply(settings$scenario_starts, function(ss) {
    petal_plot_for_start(master_df, scenario_comp, settings, ss, selected_table = selected_table)
  })

  panel_letter_pt <- settings$petal_panel_letter_size %||% 30
  leg_base <- settings$petal_legend_base_size %||% 10
  leg_text_pt <- settings$petal_legend_text_size %||% 8

  if (!isTRUE(settings$petal_label_each_petal)) {
    panel_labels <- LETTERS[seq_along(col_plots)]
    for (i in seq_along(col_plots)) {
      col_plots[[i]] <- ggdraw(col_plots[[i]]) +
        draw_label(panel_labels[i], x = 0.03, y = 0.985, hjust = 0, vjust = 1, size = panel_letter_pt, fontface = "bold")
    }
  }

  main <- plot_grid(plotlist = col_plots, ncol = length(col_plots))

  legends_bottom <- !identical(settings$petal_legends_position, "panel_top")
  if (!legends_bottom) {
    return(main)
  }

  merge_disp <- isTRUE(settings$merge_plantation_habitat_display)
  hab_cols_bot <- if (merge_disp) habitat_colors_display else habitat_colors
  hab_lab_bot <- if (merge_disp) {
    habitat_labels_display
  } else {
    c(
      primary = "Primary",
      `once-logged` = "Once-logged",
      restored = "Restored",
      `twice-logged` = "Twice-logged",
      albizia_current = "Albizia (current)",
      albizia_future = "Albizia (future)",
      eucalyptus_current = "Eucalyptus (current)",
      eucalyptus_future = "Eucalyptus (future)",
      deforested = "Deforested"
    )
  }
  hab_nrow_bot <- if (merge_disp) 2L else 3L
  hab_title_bot <- if (merge_disp) "Land cover" else "Management regimes"

  legends <- list()
  if (isTRUE(settings$include_outcome_legend)) {
    legends <- c(legends, list(make_petal_discrete_legend_grob(
      outcome_colors, outcome_legend_labels, "Outcomes", 2L, leg_base, leg_text_pt,
      legend_layout = settings$petal_legend_layout %||% "horizontal"
    )))
  }
  if (isTRUE(settings$include_habitat_legend)) {
    legends <- c(legends, list(make_petal_discrete_legend_grob(
      hab_cols_bot, hab_lab_bot, hab_title_bot, hab_nrow_bot, leg_base, leg_text_pt,
      legend_layout = settings$petal_legend_layout %||% "horizontal"
    )))
  }

  if (length(legends) == 0) return(main)
  legend_block <- if (length(legends) == 1) legends[[1]] else plot_grid(plotlist = legends, ncol = 1, rel_heights = c(1, 1.2))
  plot_grid(main, legend_block, ncol = 1, rel_heights = c(1, 0.20))
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

prepare_selected_table <- function(scenario_comp, settings) {
  if (identical(settings$selection_mode, "auto_diverse_range")) {
    selected <- auto_select_scenarios_by_composition(scenario_comp, settings)
    expected_n <- length(settings$scenario_starts %||% character(0)) * (settings$scenarios_per_start %||% 5)
    if (nrow(selected) < expected_n) {
      warning("Auto selection returned ", nrow(selected), " scenarios (expected ", expected_n, ").")
    }
    return(selected)
  }

  if (identical(settings$selection_mode, "all_at_target")) {
    tgt <- as.numeric(settings$production_target)
    tol <- as.numeric(settings$selection_target_tolerance %||% 0)
    selected <- scenario_comp %>%
      filter(
        scenarioStart %in% (settings$scenario_starts %||% character(0)),
        abs(as.numeric(production_target) - tgt) <= tol
      ) %>%
      distinct(index, production_target, scenarioStart, scenarioName)
    if (nrow(selected) == 0) {
      stop("No scenarios found for selection_mode='all_at_target' at production_target=", tgt, " with tolerance=", tol)
    }
    return(selected)
  }

  # Manual mode: retain legacy selection behavior at fixed production target.
  bind_rows(lapply(settings$scenario_starts, function(ss) {
    ids <- as.character(settings$selected_scenarios_by_start[[ss]] %||% character(0))
    tibble(
      index = ids,
      production_target = as.numeric(settings$production_target),
      scenarioStart = ss,
      selection_rank = seq_along(ids)
    )
  })) %>%
    left_join(
      scenario_comp %>% distinct(index, production_target, scenarioStart, scenarioName),
      by = c("index", "production_target", "scenarioStart")
    ) %>%
    mutate(scenarioName = coalesce(scenarioName, scenarioStart))
}

render_and_save <- function(master_df, scenario_comp, settings, run_label) {
  selected_table <- prepare_selected_table(scenario_comp, settings)
  if (nrow(selected_table) == 0) stop("No selected scenarios available for run: ", run_label)
  selected_table <- selected_table %>%
    distinct(index, production_target, scenarioName, scenarioStart, .keep_all = TRUE)
  if (!("selection_rank" %in% names(selected_table))) {
    selected_table <- selected_table %>% mutate(selection_rank = NA_real_)
  }

  comp_table <- build_composition_lookup(scenario_comp) %>%
    inner_join(selected_table, by = c("index", "production_target", "scenarioName", "scenarioStart")) %>%
    mutate(selection_rank = dplyr::coalesce(selection_rank, Inf)) %>%
    arrange(scenarioStart, production_target, selection_rank, index)
  comp_file <- file.path(PATHS$export_dir, paste0(run_label, "_selected_scenario_composition.csv"))
  readr::write_csv(comp_table, comp_file)
  message("Saved [", run_label, "] scenario composition table: ", comp_file)

  if (isTRUE(settings$split_output_by_start)) {
    for (ss in settings$scenario_starts %||% character(0)) {
      settings_ss <- settings
      settings_ss$scenario_starts <- ss
      selected_ss <- selected_table %>% filter(scenarioStart == ss)
      if (nrow(selected_ss) == 0) next
      fig_ss <- build_final_petal_figure(master_df, scenario_comp, settings_ss, selected_table = selected_ss)
      n_scen <- nrow(selected_ss %>% distinct(index, production_target, scenarioStart, scenarioName))
      out_base <- settings$output_pdf_name %||% paste0(run_label, ".pdf")
      out_stem <- sub("\\.pdf$", "", out_base, ignore.case = TRUE)
      out_file_ss <- file.path(PATHS$export_dir, paste0(out_stem, "_", ss, ".pdf"))
      has_legends <- isTRUE(settings_ss$include_outcome_legend) || isTRUE(settings_ss$include_habitat_legend)
      fig_width <- if (has_legends) 7.6 else 4.2
      fig_height <- max(10, min(42, 2.6 + (0.45 * n_scen) + ifelse(has_legends, 2.2, 0)))
      ggsave(out_file_ss, fig_ss, width = fig_width, height = fig_height, units = "in")
      message("Saved [", run_label, " | ", ss, "]: ", out_file_ss)
    }
  } else {
    fig <- build_final_petal_figure(master_df, scenario_comp, settings, selected_table = selected_table)
    out_file <- file.path(PATHS$export_dir, settings$output_pdf_name %||% paste0(run_label, ".pdf"))
    if (!grepl("\\.pdf$", out_file, ignore.case = TRUE)) out_file <- paste0(out_file, ".pdf")
    gw <- settings$figure_width_in %||% 8.27
    gh <- settings$figure_height_in %||% 11.69
    gdpi <- settings$figure_dpi %||% NULL
    if (is.null(gdpi)) {
      ggsave(out_file, fig, width = gw, height = gh, units = "in")
    } else {
      ggsave(out_file, fig, width = gw, height = gh, units = "in", dpi = gdpi)
    }
    message("Saved [", run_label, "]: ", out_file)
  }
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

