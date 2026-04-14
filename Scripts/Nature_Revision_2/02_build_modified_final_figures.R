## Modified final figures: focus points + background trajectories
## -----------------------------------------------------------------
## This script keeps the same run-setting workflow as the replicable script,
## but changes visual emphasis:
##  - Foreground: min/max highlights at selected production targets
##  - Background: all non-focus points as thicker, faint lines without CIs
## -----------------------------------------------------------------

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
  carbon = "Data/NR2/carbon_outcomes__all_trajectories.rds",
  megatrees = "Data/full_nature_scenario_megatree_performance_all_thresholds.rds",
  financial = "Data/NR2/MasterFinancialPerformance__Sensitivity25pctMoneyParams_LONG.rds",
  all_scenarios = "Inputs/MasterAllScenarios.rds",
  export_dir = "Figures/GeomPointFigs/manuscript_figures/ModifiedStyle"
)
dir.create(PATHS$export_dir, recursive = TRUE, showWarnings = FALSE)

ICON_PATHS <- list(
  birds = "Figures/Thumbnails/RhinoH.jpg",
  dung_beetles = "Figures/Thumbnails/DB.svg",
  megatrees = "Figures/Thumbnails/Dipterocarp.svg",
  harvest_npv = c("Figures/Thumbnails/dollar-sign.svg", "Figures/Thumbnails/Axe.png"),
  protection_npv = "Figures/Thumbnails/dollar-sign.svg"
)

SCALE <- list(
  bil = 1e9,
  hundred_million = 1e8,
  width = 8.27,
  height = 11.69,
  text_size = 13,
  legend_text_size = 10
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

OUTCOME_OPTIONS <- c("birds", "dung_beetles", "megatrees", "carbon", "profits", "protection")

BASE_RUN_SETTINGS <- list(
  figure_id = "fig2_d",
  outcomes_to_plot = OUTCOME_OPTIONS,
  include_legend = TRUE,
  errorbar_mode = "none",
  carbon_errorbar_mode = "model_ci",
  financial_errorbar_mode = "sensitivity_range",
  megatree_errorbar_mode = "model_ci",
  uncertainty_style = "bars",
  uncertainty_level = "80",
  megatree_uncertainty_level = "95",
  sensitivity_field = "cashflow_variant",
  sensitivity_values = list(
    cashflow_variant = c("baseline", "plus25", "minus25"),
    carbon_slope_variant = c("0.8", "1.0", "1.2"),
    carbon_discount_rate = c("2%", "4%", "6%"),
    megatree_height_filt = c("45", "50", "55")
  ),
  sensitivity_center = "baseline",
  compare_mode = FALSE,
  compare_rows_by = c("carbon_discount_rate"),
  compare_values = list(
    carbon_discount_rate = c("2%", "4%", "6%"),
    carbon_slope_variant = c("0.8", "1.0", "1.2"),
    cashflow_variant = c("baseline", "plus25", "minus25"),
    economic_discount_rate = c("2%", "4%", "6%"),
    megatree_height_filt = c("45", "50", "55"),
    bird_group = c("loser", "Y", "N", "intermediate1L", "intermediate2L", "winner"),
    beetle_group = c("loser", "intermediate1L", "intermediate2L", "winner")
  ),
  bird_group = "loser",
  beetle_group = "loser",
  economic_discount_rate = "4%",
  economic_cost_type = "HarvestProfits",
  cashflow_variant = NULL,
  carbon_stream = "scc",
  carbon_discount_rate = "4%",
  carbon_slope_variant = "1",
  megatree_height_filt = "50",
  focus_targets = c(0, 0.25, 0.5, 0.75, 1),
  focus_tolerance = 0.03,
  extra_highlight_colours_per_target = 2,
  background_point_alpha = 0.12,
  highlight_point_size = 3.5,
  background_jitter_width = 0.01,
  foreground_jitter_width = 0.03,
  background_jitter_y_prop = 0.04,
  output_pdf_name = NULL
)

`%||%` <- function(x, y) if (is.null(x)) y else x

merge_run_settings <- function(base, override) {
  out <- base
  for (nm in names(override)) out[[nm]] <- override[[nm]]
  out
}

# ===================================================================
# RUN LIBRARY (EDIT PRESETS HERE IF NEEDED)
# -------------------------------------------------------------------
# Each run below is a ready-made figure recipe.
# The key (e.g. "run1_fig1_losers_dr4") is what you select via ACTIVE_RUN.
# ===================================================================
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
  run2_fig1_losers_dr4_no_deforestation = list(
    figure_id = "figs3_nd",
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
    output_pdf_name = "Fig1_losers_dr4_no_deforestation.pdf"
  ),
  run3_alternative_dr_scc = list(
    figure_id = "fig2_d",
    outcomes_to_plot = c("carbon"),
    include_legend = FALSE,
    errorbar_mode = "model_ci",
    compare_mode = TRUE,
    compare_rows_by = c("carbon_discount_rate"),
    compare_values = list(carbon_discount_rate = c("2%", "4%", "6%")),
    carbon_stream = "scc",
    carbon_discount_rate = "4%",
    carbon_slope_variant = "1",
    output_pdf_name = "alternative_DR_scc.pdf"
  ),
  run4_alternative_dr_harvest_profs = list(
    figure_id = "fig2_d",
    outcomes_to_plot = c("profits"),
    include_legend = FALSE,
    errorbar_mode = "sensitivity_range",
    sensitivity_field = "cashflow_variant",
    sensitivity_values = list(cashflow_variant = c("minus25", "baseline", "plus25")),
    sensitivity_center = "baseline",
    compare_mode = TRUE,
    compare_rows_by = c("economic_discount_rate"),
    compare_values = list(economic_discount_rate = c("2%", "4%", "6%")),
    economic_cost_type = "HarvestProfits",
    cashflow_variant = NULL,
    carbon_slope_variant = "1",
    output_pdf_name = "alternative_DR_harvestProfs.pdf"
  ),
  run5_losers_dr4_starting_deforested = list(
    figure_id = "figs6_d_dl",
    outcomes_to_plot = OUTCOME_OPTIONS,
    errorbar_mode = "none",
    bird_group = "loser",
    beetle_group = "loser",
    economic_discount_rate = "4%",
    economic_cost_type = "HarvestProfits",
    cashflow_variant = "baseline",
    carbon_stream = "scc",
    carbon_discount_rate = "4%",
    carbon_slope_variant = "1",
    output_pdf_name = "losers_dr4_starting_deforested.pdf"
  ),
  run6_birds_various_cats = list(
    figure_id = "fig2_d",
    outcomes_to_plot = c("birds"),
    include_legend = FALSE,
    errorbar_mode = "none",
    compare_mode = TRUE,
    compare_rows_by = c("bird_group"),
    compare_values = list(bird_group = c("loser", "Y", "N", "intermediate1L", "intermediate2L", "winner")),
    output_pdf_name = "birds_various_cats.pdf"
  ),
  run7_beetles_various_cats = list(
    figure_id = "fig2_d",
    outcomes_to_plot = c("dung_beetles"),
    include_legend = FALSE,
    errorbar_mode = "none",
    compare_mode = TRUE,
    compare_rows_by = c("beetle_group"),
    compare_values = list(beetle_group = c("loser", "intermediate1L", "intermediate2L", "winner")),
    output_pdf_name = "beetls_various_cats.pdf"
  ),
  run8_various_canopy_thresholds = list(
    figure_id = "fig2_d",
    outcomes_to_plot = c("megatrees"),
    include_legend = FALSE,
    errorbar_mode = "model_ci",
    uncertainty_level = "80",
    compare_mode = TRUE,
    compare_rows_by = c("megatree_height_filt"),
    compare_values = list(megatree_height_filt = c("45", "50", "55")),
    megatree_height_filt = "50",
    output_pdf_name = "various_canopy_thresholds.pdf"
  ),
  run9_stock_years = list(
    figure_id = "fig2_d",
    outcomes_to_plot = c("carbon"),
    include_legend = FALSE,
    errorbar_mode = "model_ci",
    carbon_stream = "stock_year",
    carbon_discount_rate = NULL,
    carbon_slope_variant = "1",
    output_pdf_name = "stock_years.pdf"
  ),
  run10_scc_slope_variants = list(
    figure_id = "fig2_d",
    outcomes_to_plot = c("carbon"),
    include_legend = FALSE,
    carbon_errorbar_mode = "model_ci",
    uncertainty_level = "80",
    compare_mode = TRUE,
    compare_rows_by = c("carbon_slope_variant"),
    compare_values = list(carbon_slope_variant = c("0.75", "1", "1.2")),
    carbon_stream = "scc",
    carbon_discount_rate = "4%",
    carbon_slope_variant = "1",
    output_pdf_name = "scc_slope_variants.pdf"
  ),
  run11_stock_year_slope_variants = list(
    figure_id = "fig2_d",
    outcomes_to_plot = c("carbon"),
    include_legend = FALSE,
    carbon_errorbar_mode = "model_ci",
    uncertainty_level = "80",
    compare_mode = TRUE,
    compare_rows_by = c("carbon_slope_variant"),
    compare_values = list(carbon_slope_variant = c("0.75", "1", "1.2")),
    carbon_stream = "stock_year",
    carbon_discount_rate = NULL,
    carbon_slope_variant = "1",
    output_pdf_name = "stock_year_slope_variants.pdf"
  )
)

# ===================================================================
# RUN CONTROL (MAIN USER EDIT BLOCK)
# -------------------------------------------------------------------
# Choose ONE of the following run keys for ACTIVE_RUN:
#   run1_fig1_losers_dr4
#   run2_fig1_losers_dr4_no_deforestation
#   run3_alternative_dr_scc
#   run4_alternative_dr_harvest_profs
#   run5_losers_dr4_starting_deforested
#   run6_birds_various_cats
#   run7_beetles_various_cats
#   run8_various_canopy_thresholds
#   run9_stock_years
#   run10_scc_slope_variants
#   run11_stock_year_slope_variants
#
# HOW TO RUN:
# - Build one figure only:
#     RUN_ALL <- FALSE
#     ACTIVE_RUN <- "run1_fig1_losers_dr4"   # change this key as needed
#
# - Build every figure in RUN_LIBRARY:
#     RUN_ALL <- TRUE
#
# Output files are written to:
#   Figures/GeomPointFigs/manuscript_figures/ModifiedStyle
#
# Optional (advanced): environment variable overrides are supported:
#   ACTIVE_RUN and RUN_ALL
# ===================================================================
ACTIVE_RUN <- "run1_fig1_losers_dr4"
RUN_ALL <- TRUE

active_run_env <- trimws(Sys.getenv("ACTIVE_RUN", ""))
if (nchar(active_run_env) > 0) ACTIVE_RUN <- active_run_env
run_all_env <- trimws(Sys.getenv("RUN_ALL", ""))
if (nchar(run_all_env) > 0) RUN_ALL <- tolower(run_all_env) %in% c("true", "1", "yes", "y")
if (!ACTIVE_RUN %in% names(RUN_LIBRARY)) {
  stop("ACTIVE_RUN must be one of: ", paste(names(RUN_LIBRARY), collapse = ", "))
}
RUN_SETTINGS <- merge_run_settings(BASE_RUN_SETTINGS, RUN_LIBRARY[[ACTIVE_RUN]])

resolve_path <- function(primary) {
  if (file.exists(primary)) return(primary)
  warning("Missing required input file at PATHS: ", primary)
  primary
}

parse_slope_from_name <- function(x) {
  value <- sub("^2Ltraj_", "", x)
  value <- gsub("_", ".", value)
  suppressWarnings(num <- as.numeric(value))
  if (!is.na(num)) return(as.character(num))
  value
}

load_carbon_data <- function(path) {
  obj <- readRDS(resolve_path(path))
  if (is.data.frame(obj) || data.table::is.data.table(obj)) return(obj)
  if (is.list(obj) && !is.null(names(obj)) && all(vapply(obj, is.list, logical(1)))) {
    slope_tables <- purrr::imap(obj, function(slope_block, slope_name) {
      block_df <- data.table::rbindlist(slope_block, fill = TRUE)
      block_df[, twice_logged_slope_trajectory := parse_slope_from_name(slope_name)]
      block_df
    })
    return(data.table::rbindlist(slope_tables, fill = TRUE))
  }
  stop("Unsupported structure in carbon RDS.")
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
        TRUE ~ scenarioName
      ),
      scenarioName = fct_relevel(
        scenarioName,
        "AllPrimary", "Mostly1L", "Mostly2L",
        "AllPrimaryNoDef", "Mostly1LNoDef", "Mostly2LNoDef",
        "MostlyPrimary+DL", "Mostly1L+DL", "Mostly2L+DL"
      )
    )
}

prop_OG_fun <- function(scenario_composition, hab_in_start) {
  scenario_composition %>%
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
    select(index, production_target, scenarioName, scenarioStart, propOG, propPlant, propAlb, propEuc, prop1L, prop2L, remainingOG, remaining1L, remaining2L) %>%
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

add_plantation_type <- function(df) df %>% mutate(shape_class = ifelse(propPlant > 0, "Cross", "Point"))

get_carbon_spec <- function(stream = c("scc", "stock_year")) {
  stream <- match.arg(stream)
  if (stream == "scc") {
    return(list(metric = "TOTcarbon_ACD_mean", ylab = "Social Carbon Cost\n(USD 1000M)", divisor = SCALE$bil, ci_cols = list(`95` = c("TOTcarbon_ACD_lwr95", "TOTcarbon_ACD_upr95"), `80` = c("TOTcarbon_ACD_lwr80", "TOTcarbon_ACD_upr80"))))
  }
  list(metric = "mean_cum_stock_year", ylab = "Carbon Stock Years\n(billion)", divisor = SCALE$bil, ci_cols = list(`95` = c("lwr_cum_stock_year_95", "upr_cum_stock_year_95"), `80` = c("lwr_cum_stock_year_80", "upr_cum_stock_year_80"), `50` = c("lwr_cum_stock_year_50", "upr_cum_stock_year_50")))
}

filter_carbon_variant <- function(df, discount_rate = NULL, slope_variant = NULL) {
  out <- df %>% mutate(discount_rate = as.character(.data$discount_rate), twice_logged_slope_trajectory = as.character(.data$twice_logged_slope_trajectory))
  if (!is.null(discount_rate)) {
    discount_vals <- as.character(discount_rate)
    out <- out %>% filter(.data$discount_rate %in% discount_vals)
  }
  if (!is.null(slope_variant)) {
    slope_vals <- as.character(slope_variant)
    slope_num <- suppressWarnings(as.numeric(slope_vals))
    field_num <- suppressWarnings(as.numeric(out$twice_logged_slope_trajectory))
    if (all(!is.na(slope_num)) && all(!is.na(field_num[!is.na(out$twice_logged_slope_trajectory)]))) {
      out <- out[field_num %in% slope_num, , drop = FALSE]
    } else {
      out <- out %>% filter(.data$twice_logged_slope_trajectory %in% slope_vals)
    }
  }
  out
}

filter_cashflow_variant <- function(df, cashflow_variant = NULL) {
  if (is.null(cashflow_variant) || !("cashflow_variant" %in% names(df))) return(df)
  keep_vals <- as.character(cashflow_variant)
  df %>% filter(as.character(.data$cashflow_variant) %in% keep_vals)
}

get_megatree_ci_cols <- function(level = "80") {
  level <- as.character(level)
  if (identical(level, "95")) {
    return(list(c("landscape_prop_lwr95", "landscape_prop_upr95"), c("landscape_prop_lwr", "landscape_prop_upr")))
  }
  if (identical(level, "80")) {
    return(list(c("landscape_prop_lwr80", "landscape_prop_upr80"), c("landscape_prop_lwr", "landscape_prop_upr")))
  }
  if (identical(level, "50")) {
    return(list(c("landscape_prop_lwr50", "landscape_prop_upr50"), c("landscape_prop_lwr", "landscape_prop_upr")))
  }
  list(c("landscape_prop_lwr80", "landscape_prop_upr80"), c("landscape_prop_lwr", "landscape_prop_upr"))
}

apply_sensitivity_range <- function(df, value_col, sensitivity_field, sensitivity_values = NULL, sensitivity_center = NULL) {
  if (is.null(sensitivity_field) || !(sensitivity_field %in% names(df))) return(df)
  out <- df
  if (!is.null(sensitivity_values) && sensitivity_field %in% names(sensitivity_values)) {
    keep_vals <- as.character(sensitivity_values[[sensitivity_field]])
    out <- out %>% filter(as.character(.data[[sensitivity_field]]) %in% keep_vals)
  }
  if (nrow(out) == 0) return(out)

  group_cols <- c("index", "production_target", "scenarioStart", "scenarioName", "hexP", "hex1L", "hex2L", "shape_class")
  group_cols <- group_cols[group_cols %in% names(out)]

  range_df <- out %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(
      plot_ymin = min(.data[[value_col]], na.rm = TRUE),
      plot_ymax = max(.data[[value_col]], na.rm = TRUE),
      .groups = "drop"
    )

  if (!is.null(sensitivity_center)) {
    center_df <- out %>%
      filter(as.character(.data[[sensitivity_field]]) == as.character(sensitivity_center)) %>%
      group_by(across(all_of(group_cols))) %>%
      summarise(plot_value = mean(.data[[value_col]], na.rm = TRUE), .groups = "drop")
  } else {
    center_df <- out %>%
      group_by(across(all_of(group_cols))) %>%
      summarise(plot_value = mean(.data[[value_col]], na.rm = TRUE), .groups = "drop")
  }
  range_df %>% left_join(center_df, by = group_cols)
}

get_setting_values <- function(settings, field) {
  if (isTRUE(settings$compare_mode) &&
      !is.null(settings$compare_rows_by) &&
      field %in% settings$compare_rows_by &&
      !is.null(settings$compare_values[[field]])) {
    return(settings$compare_values[[field]])
  }
  settings[[field]]
}

range_with_optional_bounds <- function(df, value_col = "plot_value", ymin_col = NULL, ymax_col = NULL) {
  vals <- df[[value_col]]
  if (!is.null(ymin_col) && ymin_col %in% names(df)) vals <- c(vals, df[[ymin_col]])
  if (!is.null(ymax_col) && ymax_col %in% names(df)) vals <- c(vals, df[[ymax_col]])
  vals <- vals[is.finite(vals)]
  if (length(vals) == 0) return(NULL)
  range(vals, na.rm = TRUE)
}

compute_y_limits_map <- function(inputs, settings, preset) {
  scenario_filter <- preset$scenario_filter
  selected_outcomes <- unique(settings$outcomes_to_plot)
  limits <- list()

  if ("birds" %in% selected_outcomes && !is.null(inputs$birds)) {
    bird_vals <- as.character(get_setting_values(settings, "bird_group"))
    B <- inputs$birds %>% filter(scenarioName %in% scenario_filter, bird_grp %in% bird_vals) %>% mutate(plot_value = medianRelativeOccupancy)
    if (nrow(B) > 0) limits$birds <- range_with_optional_bounds(B, value_col = "plot_value")
  }
  if ("dung_beetles" %in% selected_outcomes && !is.null(inputs$dung_beetles)) {
    beetle_vals <- as.character(get_setting_values(settings, "beetle_group"))
    DB <- inputs$dung_beetles %>% filter(scenarioName %in% scenario_filter, spp_category %in% beetle_vals) %>% mutate(plot_value = medianRelativeOccupancy)
    if (nrow(DB) > 0) limits$dung_beetles <- range_with_optional_bounds(DB, value_col = "plot_value")
  }
  if ("megatrees" %in% selected_outcomes && !is.null(inputs$megatrees)) {
    M <- inputs$megatrees %>%
      filter(scenarioName %in% scenario_filter, as.character(height_filt) %in% as.character(get_setting_values(settings, "megatree_height_filt"))) %>%
      mutate(plot_value = landscape_prop)
    if (identical(settings$megatree_errorbar_mode %||% settings$errorbar_mode, "model_ci")) {
      ci_candidates <- get_megatree_ci_cols(settings$megatree_uncertainty_level %||% "95")
      for (cand in ci_candidates) {
        if (all(cand %in% names(M))) {
          M <- M %>% mutate(plot_ymin = .data[[cand[1]]], plot_ymax = .data[[cand[2]]]); break
        }
      }
    }
    if (nrow(M) > 0) limits$megatrees <- range_with_optional_bounds(M, value_col = "plot_value", ymin_col = "plot_ymin", ymax_col = "plot_ymax")
  }
  if ("carbon" %in% selected_outcomes && !is.null(inputs$carbon)) {
    carbon_spec <- get_carbon_spec(settings$carbon_stream)
    C <- inputs$carbon %>%
      filter(scenarioName %in% scenario_filter) %>%
      filter_carbon_variant(discount_rate = get_setting_values(settings, "carbon_discount_rate"), slope_variant = get_setting_values(settings, "carbon_slope_variant")) %>%
      mutate(plot_value = .data[[carbon_spec$metric]] / carbon_spec$divisor)
    if (identical(settings$carbon_errorbar_mode %||% settings$errorbar_mode, "model_ci")) {
      ci_cols <- carbon_spec$ci_cols[[as.character(settings$uncertainty_level %||% "80")]]
      if (!is.null(ci_cols) && all(ci_cols %in% names(C))) C <- C %>% mutate(plot_ymin = .data[[ci_cols[1]]] / carbon_spec$divisor, plot_ymax = .data[[ci_cols[2]]] / carbon_spec$divisor)
    }
    if (nrow(C) > 0) limits$carbon <- range_with_optional_bounds(C, value_col = "plot_value", ymin_col = "plot_ymin", ymax_col = "plot_ymax")
  }
  if ("profits" %in% selected_outcomes && !is.null(inputs$profits)) {
    P <- inputs$profits %>%
      filter(scenarioName %in% scenario_filter, costType %in% as.character(get_setting_values(settings, "economic_cost_type")), discount_rate %in% as.character(get_setting_values(settings, "economic_discount_rate"))) %>%
      filter_cashflow_variant(cashflow_variant = get_setting_values(settings, "cashflow_variant")) %>%
      mutate(plot_value = NPV / SCALE$hundred_million)
    if (identical(settings$financial_errorbar_mode %||% settings$errorbar_mode, "sensitivity_range")) {
      P <- apply_sensitivity_range(P, value_col = "plot_value", sensitivity_field = settings$sensitivity_field, sensitivity_values = settings$sensitivity_values, sensitivity_center = settings$sensitivity_center)
    }
    if (nrow(P) > 0) limits$profits <- range_with_optional_bounds(P, value_col = "plot_value", ymin_col = "plot_ymin", ymax_col = "plot_ymax")
  }
  if ("protection" %in% selected_outcomes && !is.null(inputs$protection)) {
    PC <- inputs$protection %>%
      filter(scenarioName %in% scenario_filter, discount_rate %in% as.character(get_setting_values(settings, "economic_discount_rate"))) %>%
      filter_cashflow_variant(cashflow_variant = get_setting_values(settings, "cashflow_variant")) %>%
      mutate(plot_value = NPV / SCALE$hundred_million)
    if (identical(settings$financial_errorbar_mode %||% settings$errorbar_mode, "sensitivity_range")) {
      PC <- apply_sensitivity_range(PC, value_col = "plot_value", sensitivity_field = settings$sensitivity_field, sensitivity_values = settings$sensitivity_values, sensitivity_center = settings$sensitivity_center)
    }
    if (nrow(PC) > 0) limits$protection <- range_with_optional_bounds(PC, value_col = "plot_value", ymin_col = "plot_ymin", ymax_col = "plot_ymax")
  }
  limits
}

build_comparison_grid <- function(settings) {
  if (!isTRUE(settings$compare_mode)) return(NULL)
  row_fields <- settings$compare_rows_by
  value_list <- lapply(row_fields, function(field) settings$compare_values[[field]])
  names(value_list) <- row_fields
  as.data.frame(do.call(expand.grid, c(value_list, stringsAsFactors = FALSE)))
}

apply_row_settings <- function(base_settings, row_df, i) {
  row <- row_df[i, , drop = FALSE]
  out <- base_settings
  for (field in names(row)) out[[field]] <- row[[field]][[1]]
  out
}

format_row_label <- function(row_df, i) {
  row <- row_df[i, , drop = FALSE]
  label_map <- c(
    carbon_slope_variant = "Slope variant",
    carbon_discount_rate = "Carbon discount rate",
    economic_discount_rate = "Economic discount rate",
    megatree_height_filt = "Megatree height filter"
  )
  parts <- unlist(lapply(names(row), function(field) {
    label_name <- if (field %in% names(label_map)) label_map[[field]] else field
    paste0(label_name, ": ", row[[field]][[1]])
  }))
  paste(parts, collapse = " | ")
}

load_icon_grob <- function(icon_path, width_px = 260, height_px = 260) {
  if (is.null(icon_path) || !file.exists(icon_path)) return(NULL)
  ext <- tolower(tools::file_ext(icon_path))
  raster <- NULL
  if (ext == "svg") {
    if (!requireNamespace("rsvg", quietly = TRUE)) {
      warning("Package 'rsvg' is required for SVG icon rendering. Skipping icon: ", icon_path)
      return(NULL)
    }
    tmp_png <- tempfile(fileext = ".png")
    rsvg::rsvg_png(icon_path, file = tmp_png, width = width_px, height = height_px)
    raster <- png::readPNG(tmp_png)
  } else if (ext == "png") {
    raster <- png::readPNG(icon_path)
    # Remove white background from Axe icon so it overlays cleanly.
    if (tolower(basename(icon_path)) == "axe.png") {
      # Mirror horizontally so axe leans right.
      raster <- raster[, ncol(raster):1, , drop = FALSE]
      if (length(dim(raster)) == 3 && dim(raster)[3] == 3) {
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
      } else if (length(dim(raster)) == 3 && dim(raster)[3] == 4) {
        white_mask <- (raster[, , 1] > 0.92) & (raster[, , 2] > 0.92) & (raster[, , 3] > 0.92)
        raster[, , 4] <- ifelse(white_mask, 0, raster[, , 4])
      }
    }
  } else if (ext %in% c("jpg", "jpeg")) {
    if (!requireNamespace("jpeg", quietly = TRUE)) {
      warning("Package 'jpeg' is required for JPG icon rendering. Skipping icon: ", icon_path)
      return(NULL)
    }
    raster <- jpeg::readJPEG(icon_path)
    # RhinoH includes a light background and text. Convert to RGBA and remove
    # the background/text region so only the bird silhouette remains visible.
    if (tolower(basename(icon_path)) == "rhinoh.jpg" && length(dim(raster)) == 3) {
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
    warning("Unsupported icon extension for: ", icon_path)
    return(NULL)
  }
  grid::rasterGrob(raster, interpolate = TRUE)
}

select_focus_extrema_points <- function(df, focus_targets, focus_tolerance, y_col, extra_highlight_colours_per_target = 2) {
  facets <- unique(as.character(df$scenarioName))
  out <- vector("list", length(facets) * length(focus_targets))
  k <- 1

  for (scn in facets) {
    df_scn <- df %>% filter(as.character(scenarioName) == scn)
    for (tgt in focus_targets) {
      candidates <- df_scn %>%
        mutate(.target = tgt, .dist = abs(production_target - tgt)) %>%
        filter(.dist <= focus_tolerance)

      if (nrow(candidates) == 0) {
        candidates <- df_scn %>%
          mutate(.target = tgt, .dist = abs(production_target - tgt)) %>%
          filter(.dist == min(.dist, na.rm = TRUE))
      }
      if (nrow(candidates) == 0) next

      min_row <- candidates %>% slice_min(order_by = .data[[y_col]], n = 1, with_ties = FALSE)
      max_row <- candidates %>% slice_max(order_by = .data[[y_col]], n = 1, with_ties = FALSE)
      selected <- bind_rows(min_row, max_row) %>% distinct()
      used_cols <- unique(as.character(selected$plot_colour))

      extra_pool <- candidates %>%
        anti_join(
          selected %>% select(index, scenarioName, scenarioStart, production_target, plot_colour, shape_class),
          by = c("index", "scenarioName", "scenarioStart", "production_target", "plot_colour", "shape_class")
        ) %>%
        filter(!(as.character(plot_colour) %in% used_cols))

      if (nrow(extra_pool) == 0) {
        extra_pool <- candidates %>%
          anti_join(
            selected %>% select(index, scenarioName, scenarioStart, production_target, plot_colour, shape_class),
            by = c("index", "scenarioName", "scenarioStart", "production_target", "plot_colour", "shape_class")
          )
      }

      extras <- extra_pool %>%
        group_by(plot_colour) %>%
        slice_min(order_by = .dist, n = 1, with_ties = FALSE) %>%
        ungroup() %>%
        slice_head(n = max(0L, as.integer(extra_highlight_colours_per_target)))

      out[[k]] <- bind_rows(selected, extras) %>% distinct() %>% mutate(production_target = .target) %>% select(-.target, -.dist)
      k <- k + 1
    }
  }

  bind_rows(out) %>% distinct()
}

master_plot_modified <- function(df, y_col, ylab_text, scenario_filter, errorbar_mode = "none", ymin_col = NULL, ymax_col = NULL, settings, icon_paths = NULL, icon_style = "default", y_limits = NULL, show_icon = TRUE) {
  base_df <- df %>%
    mutate(
      scenarioName = fct_relevel(scenarioName, scenario_filter),
      plot_colour = case_when(
        scenarioStart %in% c("all_primary", "primary_deforested") ~ hexP,
        scenarioStart %in% c("mostly_1L", "mostly_1L_deforested") ~ hex1L,
        scenarioStart %in% c("mostly_2L", "mostly_2L_deforested") ~ hex2L
      ),
      shape_bg = ifelse(shape_class == "Point", "Point_bg", "Cross_bg"),
      shape_fg = ifelse(shape_class == "Point", "Point_fg", "Cross_fg")
    )

  rep_df <- select_focus_extrema_points(
    base_df,
    focus_targets = settings$focus_targets %||% c(0, 0.25, 0.5, 0.75, 1),
    focus_tolerance = settings$focus_tolerance %||% 0.03,
    y_col = y_col,
    extra_highlight_colours_per_target = settings$extra_highlight_colours_per_target %||% 2
  )

  set.seed(1)
  y_span <- diff(range(base_df[[y_col]], na.rm = TRUE))
  y_amount <- (settings$background_jitter_y_prop %||% 0.012) * ifelse(is.finite(y_span) && y_span > 0, y_span, 1)
  base_df <- base_df %>%
    mutate(
      x_bg = pmax(0, pmin(1, jitter(production_target, amount = settings$background_jitter_width %||% 0.01))),
      y_bg = jitter(.data[[y_col]], amount = y_amount)
    )
  set.seed(2)
  rep_df <- rep_df %>%
    mutate(x_fg = pmax(0, pmin(1, jitter(production_target, amount = settings$foreground_jitter_width %||% 0.012))))

  p <- ggplot() +
    geom_point(
      data = base_df,
      aes(x = x_bg, y = y_bg, colour = plot_colour, shape = shape_bg),
      size = settings$highlight_point_size %||% 3.5,
      alpha = settings$background_point_alpha %||% 0.12,
      stroke = 0.3
    )

  p <- p +
    geom_point(
      data = rep_df,
      aes(x = x_fg, y = .data[[y_col]], colour = plot_colour, shape = shape_fg),
      size = settings$highlight_point_size %||% 3.5,
      alpha = 0.98,
      stroke = 1.25
    )

  if ((identical(errorbar_mode, "model_ci") || identical(errorbar_mode, "sensitivity_range")) &&
      !is.null(ymin_col) && !is.null(ymax_col) && all(c(ymin_col, ymax_col) %in% names(rep_df))) {
    p <- p +
      geom_errorbar(
        data = rep_df,
        aes(x = x_fg, ymin = .data[[ymin_col]], ymax = .data[[ymax_col]], colour = plot_colour),
        width = 0,
        linewidth = 1.05,
        alpha = 0.95
      )
  }

  p <- p +
    scale_colour_identity() +
    scale_shape_manual(values = c("Point_bg" = 16, "Cross_bg" = 17, "Point_fg" = 1, "Cross_fg" = 2)) +
    xlim(0, 1) +
    xlab(NULL) +
    ylab(ylab_text) +
    facet_wrap(~scenarioName, ncol = 3) +
    theme_bw(base_size = SCALE$text_size) +
    theme(
      legend.position = "none",
      panel.grid.major = element_line(color = "grey90"),
      panel.grid.minor = element_blank(),
      strip.text = element_blank()
    )
  if (!is.null(y_limits) && length(y_limits) == 2 && all(is.finite(y_limits))) {
    p <- p + coord_cartesian(ylim = y_limits)
  }

  # Add icon(s) in the rightmost facet blank area.
  if (isTRUE(show_icon) && identical(icon_style, "carbon_symbol")) {
    box_grob <- grid::rectGrob(gp = grid::gpar(fill = "white", col = "black", lwd = 2))
    return(
      ggdraw(p) +
        # Height is scaled up relative to width so the box appears visually square
        # in this non-square drawing canvas.
        draw_grob(box_grob, x = 0.905, y = 0.205, width = 0.06, height = 0.105) +
        draw_label("C", x = 0.935, y = 0.258, size = 17, fontface = "bold")
    )
  }

  if (isTRUE(show_icon) && !is.null(icon_paths)) {
    paths <- as.character(icon_paths)
    grobs <- lapply(paths, load_icon_grob)
    grobs <- grobs[!vapply(grobs, is.null, logical(1))]
    if (length(grobs) > 0) {
      pg <- ggdraw(p)
      if (length(grobs) == 1 && icon_style %in% c("birds", "megatrees")) {
        pg <- pg + draw_grob(grobs[[1]], x = 0.885, y = 0.64, width = 0.105, height = 0.27)
      } else if (length(grobs) == 1 && icon_style == "protection_npv") {
        pg <- pg + draw_grob(grobs[[1]], x = 0.905, y = 0.69, width = 0.06, height = 0.16)
      } else if (length(grobs) == 1) {
        pg <- pg + draw_grob(grobs[[1]], x = 0.905, y = 0.69, width = 0.06, height = 0.16)
      } else {
        pg <- pg +
          draw_grob(grobs[[1]], x = 0.885, y = 0.69, width = 0.06, height = 0.16) +
          draw_grob(grobs[[2]], x = 0.925, y = 0.69, width = 0.06, height = 0.16)
      }
      return(pg)
    }
  }
  p
}

add_top_headers <- function(main_plot) {
  top_shape_legend <- ggplot() +
    annotate("point", x = 0.40, y = 0.55, shape = 2, size = 4, stroke = 1.1) +
    annotate("text", x = 0.425, y = 0.55, label = "With Plantation", hjust = 0, size = 3.6) +
    annotate("point", x = 0.58, y = 0.55, shape = 1, size = 4, stroke = 1.1) +
    annotate("text", x = 0.605, y = 0.55, label = "No Plantations", hjust = 0, size = 3.6) +
    xlim(0, 1) + ylim(0, 1) + theme_void()

  top_col_labels <- ggdraw() +
    draw_label("Mostly Primary", x = 1 / 6, y = 0.5, fontface = "bold", size = 11) +
    draw_label("Mostly Once-logged", x = 0.5, y = 0.5, fontface = "bold", size = 11) +
    draw_label("Mostly Twice-logged", x = 5 / 6, y = 0.5, fontface = "bold", size = 11)

  plot_grid(top_shape_legend, top_col_labels, main_plot, ncol = 1, rel_heights = c(0.05, 0.05, 1))
}

build_main_figure <- function(birds, dung_beetles, megatrees, carbon, profits, protection, all_legend, settings, preset, y_limits_map = NULL, include_top_headers = TRUE, include_icons = TRUE) {
  carbon_spec <- get_carbon_spec(settings$carbon_stream)
  scenario_filter <- preset$scenario_filter
  selected_outcomes <- unique(settings$outcomes_to_plot)
  panels <- list()
  errorbar_mode <- settings$errorbar_mode %||% "none"
  carbon_errorbar_mode <- settings$carbon_errorbar_mode %||% errorbar_mode
  financial_errorbar_mode <- settings$financial_errorbar_mode %||% errorbar_mode
  megatree_errorbar_mode <- settings$megatree_errorbar_mode %||% errorbar_mode

  if ("birds" %in% selected_outcomes) {
    B <- birds %>% filter(scenarioName %in% scenario_filter, bird_grp == settings$bird_group) %>% mutate(plot_value = medianRelativeOccupancy)
    panels <- c(panels, list(master_plot_modified(B, "plot_value", "Median Relative\nOccupancy", scenario_filter, "none", settings = settings, icon_paths = ICON_PATHS$birds, icon_style = "birds", y_limits = y_limits_map$birds, show_icon = include_icons)))
  }
  if ("dung_beetles" %in% selected_outcomes) {
    DB <- dung_beetles %>% filter(scenarioName %in% scenario_filter, spp_category == settings$beetle_group) %>% mutate(plot_value = medianRelativeOccupancy)
    panels <- c(panels, list(master_plot_modified(DB, "plot_value", "Median Relative\nAbundance", scenario_filter, "none", settings = settings, icon_paths = ICON_PATHS$dung_beetles, icon_style = "default", y_limits = y_limits_map$dung_beetles, show_icon = include_icons)))
  }
  if ("megatrees" %in% selected_outcomes) {
    M <- megatrees %>% filter(scenarioName %in% scenario_filter, as.character(height_filt) == as.character(settings$megatree_height_filt)) %>% mutate(plot_value = landscape_prop)
    if (identical(megatree_errorbar_mode, "model_ci")) {
      ci_candidates <- get_megatree_ci_cols(settings$megatree_uncertainty_level %||% "95")
      for (cand in ci_candidates) {
        if (all(cand %in% names(M))) {
          M <- M %>% mutate(plot_ymin = .data[[cand[1]]], plot_ymax = .data[[cand[2]]]); break
        }
      }
    }
    panels <- c(panels, list(master_plot_modified(M, "plot_value", "Megatree\nyears", scenario_filter, megatree_errorbar_mode, "plot_ymin", "plot_ymax", settings, icon_paths = ICON_PATHS$megatrees, icon_style = "megatrees", y_limits = y_limits_map$megatrees, show_icon = include_icons)))
  }
  if ("carbon" %in% selected_outcomes) {
    C <- carbon %>%
      filter(scenarioName %in% scenario_filter) %>%
      filter_carbon_variant(discount_rate = settings$carbon_discount_rate, slope_variant = settings$carbon_slope_variant) %>%
      mutate(plot_value = .data[[carbon_spec$metric]] / carbon_spec$divisor)

    # Safety check: ensure the plotted carbon subset matches requested discount rate(s).
    if (!is.null(settings$carbon_discount_rate) && ("discount_rate" %in% names(C)) && nrow(C) > 0) {
      requested_dr <- sort(unique(as.character(settings$carbon_discount_rate)))
      plotted_dr <- sort(unique(as.character(C$discount_rate)))
      if (!setequal(requested_dr, plotted_dr)) {
        stop(
          "Carbon discount-rate filter mismatch. Requested: ",
          paste(requested_dr, collapse = ", "),
          " | Plotted: ",
          paste(plotted_dr, collapse = ", ")
        )
      }
    }

    if (identical(carbon_errorbar_mode, "model_ci")) {
      ci_cols <- carbon_spec$ci_cols[[as.character(settings$uncertainty_level %||% "80")]]
      if (!is.null(ci_cols) && all(ci_cols %in% names(C))) {
        C <- C %>% mutate(plot_ymin = .data[[ci_cols[1]]] / carbon_spec$divisor, plot_ymax = .data[[ci_cols[2]]] / carbon_spec$divisor)
      }
    } else if (identical(carbon_errorbar_mode, "sensitivity_range")) {
      C <- apply_sensitivity_range(
        C,
        value_col = "plot_value",
        sensitivity_field = settings$sensitivity_field,
        sensitivity_values = settings$sensitivity_values,
        sensitivity_center = settings$sensitivity_center
      )
    }
    panels <- c(panels, list(master_plot_modified(C, "plot_value", carbon_spec$ylab, scenario_filter, carbon_errorbar_mode, "plot_ymin", "plot_ymax", settings, icon_paths = NULL, icon_style = "carbon_symbol", y_limits = y_limits_map$carbon, show_icon = include_icons)))
  }
  if ("profits" %in% selected_outcomes) {
    P <- profits %>%
      filter(scenarioName %in% scenario_filter, costType == settings$economic_cost_type, discount_rate == settings$economic_discount_rate) %>%
      filter_cashflow_variant(cashflow_variant = settings$cashflow_variant) %>%
      mutate(plot_value = NPV / SCALE$hundred_million)
    if (identical(financial_errorbar_mode, "sensitivity_range")) {
      P <- apply_sensitivity_range(
        P,
        value_col = "plot_value",
        sensitivity_field = settings$sensitivity_field,
        sensitivity_values = settings$sensitivity_values,
        sensitivity_center = settings$sensitivity_center
      )
    }
    panels <- c(panels, list(master_plot_modified(P, "plot_value", "Harvest NPV\n(USD 100M)", scenario_filter, financial_errorbar_mode, "plot_ymin", "plot_ymax", settings, icon_paths = ICON_PATHS$harvest_npv, icon_style = "harvest_npv", y_limits = y_limits_map$profits, show_icon = include_icons)))
  }
  if ("protection" %in% selected_outcomes) {
    PC <- protection %>%
      filter(scenarioName %in% scenario_filter, discount_rate == settings$economic_discount_rate) %>%
      filter_cashflow_variant(cashflow_variant = settings$cashflow_variant) %>%
      mutate(plot_value = NPV / SCALE$hundred_million)
    if (identical(financial_errorbar_mode, "sensitivity_range")) {
      PC <- apply_sensitivity_range(
        PC,
        value_col = "plot_value",
        sensitivity_field = settings$sensitivity_field,
        sensitivity_values = settings$sensitivity_values,
        sensitivity_center = settings$sensitivity_center
      )
    }
    panels <- c(panels, list(master_plot_modified(PC, "plot_value", "Protection NPV\n(USD 100M)", scenario_filter, financial_errorbar_mode, "plot_ymin", "plot_ymax", settings, icon_paths = ICON_PATHS$protection_npv, icon_style = "protection_npv", y_limits = y_limits_map$protection, show_icon = include_icons)))
  }

  aligned_panels <- align_plots(plotlist = panels, align = "hv", axis = "tblr")
  main <- plot_grid(plotlist = aligned_panels, ncol = 1, align = "hv", axis = "tblr")

  if (isTRUE(include_top_headers)) {
    main <- add_top_headers(main)
  }
  if (isTRUE(settings$include_legend)) return(plot_grid(main, all_legend, nrow = 2, rel_heights = c(1, 0.2)))
  main
}

scenarios <- readRDS(resolve_path(PATHS$all_scenarios))
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

COL <- "BlueOr"
cols <- data.frame(bi_pal(COL, dim = 4, preview = FALSE))
colnames(cols) <- "hex"
cols <- cols %>% mutate(bi_class = rownames(.))
primary_legend <- bi_legend(pal = COL, dim = 4, xlab = "Old-growth", ylab = "Once-\nlogged", size = SCALE$legend_text_size)
onceL_legend <- bi_legend(pal = COL, dim = 4, xlab = "Remain.\nold-growth", ylab = "Remain.\nonce-logged", size = SCALE$legend_text_size)
twiceL_legend <- bi_legend(pal = COL, dim = 4, xlab = "Remain.\nold-growth", ylab = "Remain.\ntwice-logged", size = SCALE$legend_text_size)
all_legend <- plot_grid(primary_legend, NULL, onceL_legend, NULL, twiceL_legend, ncol = 5, rel_widths = c(1, 0.1, 1, 0.1, 1))

prep_base <- function(df) propOGcomp[df, on = .(index, production_target)]
prep_styled <- function(df) prep_base(df) %>% add_bivariate_colours(cols) %>% add_plantation_type() %>% rename_scenario_name()

birds <- bind_rows(
  readRDS(resolve_path(PATHS$birds)) %>% distinct() %>% rename(bird_grp = spp_category),
  readRDS(resolve_path(PATHS$birds_iucn)) %>% distinct() %>% rename(bird_grp = threatened)
) %>% mutate(index = as.character(index), production_target = as.numeric(production_target)) %>% as.data.table() %>% prep_styled()

dungBeetles <- readRDS(resolve_path(PATHS$dung_beetles)) %>% mutate(index = as.character(index), production_target = as.numeric(production_target)) %>% as.data.table() %>% prep_styled()
carbon <- load_carbon_data(PATHS$carbon) %>% mutate(index = as.character(index), production_target = as.numeric(production_target)) %>% as.data.table() %>% prep_styled()
megatrees <- readRDS(resolve_path(PATHS$megatrees)) %>%
  mutate(index = as.character(index), production_target = as.numeric(production_target), height_filt = as.character(height_filt)) %>%
  as.data.table()
megatrees <- prep_base(megatrees) %>%
  select(-c(scenarioName, scenarioStart)) %>%
  rename(scenarioName = i.scenarioName, scenarioStart = i.scenarioStart) %>%
  add_bivariate_colours(cols) %>%
  add_plantation_type() %>%
  rename_scenario_name()

profits <- readRDS(resolve_path(PATHS$financial)) %>%
  distinct() %>%
  pivot_longer(cols = starts_with("NPV"), names_to = "discount_rate", values_to = "NPV") %>%
  mutate(
    discount_rate = gsub("NPV", "", discount_rate),
    discount_rate = paste0(discount_rate, "%"),
    index = as.character(index),
    production_target = as.numeric(production_target)
  ) %>%
  na.omit() %>%
  as.data.table() %>%
  prep_styled()
protection <- profits %>% filter(costType == "ProtectionCosts") %>% mutate(outcome = "protection")

render_and_save <- function(settings, run_label) {
  selected_preset <- FIGURE_PRESETS[[settings$figure_id]]
  if (is.null(selected_preset)) stop("Unknown figure_id: ", settings$figure_id)
  inputs <- list(
    birds = birds,
    dung_beetles = dungBeetles,
    megatrees = megatrees,
    carbon = carbon,
    profits = profits,
    protection = protection
  )
  comparison_grid <- build_comparison_grid(settings)

  if (isTRUE(settings$compare_mode)) {
    row_plots <- vector("list", nrow(comparison_grid))
    for (i in seq_len(nrow(comparison_grid))) {
      row_settings <- apply_row_settings(settings, comparison_grid, i)
      row_settings$include_legend <- FALSE
      # In compare mode, each row gets its own y-limits map.
      limits_settings <- row_settings
      limits_settings$compare_mode <- FALSE
      row_y_limits_map <- compute_y_limits_map(inputs, limits_settings, selected_preset)
      row_base <- build_main_figure(
        birds = birds,
        dung_beetles = dungBeetles,
        megatrees = megatrees,
        carbon = carbon,
        profits = profits,
        protection = protection,
        all_legend = all_legend,
        settings = row_settings,
        preset = selected_preset,
        y_limits_map = row_y_limits_map,
        include_top_headers = FALSE,
        include_icons = (i == 1)
      )
      row_label <- format_row_label(comparison_grid, i)
      row_plots[[i]] <- plot_grid(
        ggdraw() + draw_label(row_label, x = 0, hjust = 0, fontface = "bold", size = 10),
        row_base, ncol = 1, rel_heights = c(0.06, 1)
      )
    }
    compare_main <- plot_grid(plotlist = row_plots, ncol = 1)
    compare_main <- add_top_headers(compare_main)
    figure_obj <- if (isTRUE(settings$include_legend)) plot_grid(compare_main, all_legend, nrow = 2, rel_heights = c(1, 0.2)) else compare_main
  } else {
    y_limits_map <- NULL
    figure_obj <- build_main_figure(
      birds = birds,
      dung_beetles = dungBeetles,
      megatrees = megatrees,
      carbon = carbon,
      profits = profits,
      protection = protection,
      all_legend = all_legend,
      settings = settings,
      preset = selected_preset,
      y_limits_map = y_limits_map
    )
  }

  out_file <- file.path(PATHS$export_dir, settings$output_pdf_name %||% paste0(run_label, ".pdf"))
  if (!grepl("\\.pdf$", out_file, ignore.case = TRUE)) out_file <- paste0(out_file, ".pdf")
  ggsave(filename = out_file, plot = figure_obj, device = "pdf", width = SCALE$width, height = SCALE$height, units = "in")
  message("Saved [", run_label, "]: ", out_file)
}

if (isTRUE(RUN_ALL)) {
  message("RUN_ALL=TRUE: generating all RUN_LIBRARY presets in modified style.")
  for (run_key in names(RUN_LIBRARY)) {
    run_settings <- merge_run_settings(BASE_RUN_SETTINGS, RUN_LIBRARY[[run_key]])
    tryCatch(
      render_and_save(run_settings, run_key),
      error = function(e) warning("Skipping ", run_key, " due to error: ", conditionMessage(e))
    )
  }
} else {
  render_and_save(RUN_SETTINGS, ACTIVE_RUN)
}
