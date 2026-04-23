# ----------------------------------------------------------------------------
# Nature Revision 2 — multipanel geom figure (replicable Figure 2-style)
#
# This is the script I actually edit when I change which starting landscapes, carbon stream, or financial sensitivity shows up in the main performance panels.
# Inputs: Inputs/FixedScenarioParams.R; NR2 RDS tables and MasterAllScenarios as listed in PATHS near the top of the file.
# Outputs: PDFs under Figures/NR2/GeomPoint_Replicable.
# ----------------------------------------------------------------------------

## Reproducible, single-figure workflow for Figure 2-style panels
## -----------------------------------------------------------------
## QUICK START
## 1) Edit only RUN_SETTINGS (and optionally FIGURE_PRESETS)
## 2) Run:
##      Rscript "Scripts/Nature_Revision_2/02_BuildMultipanelPerformanceFigure_replicable.R"
## 3) Output PDF is written to:
##      Figures/NR2/GeomPoint_Replicable
##
## WHAT YOU CAN CUSTOMIZE
## - Which starting landscapes are shown (via figure_id -> FIGURE_PRESETS)
## - Which outcomes are plotted (outcomes_to_plot)
## - Carbon stream and filters (SCC vs stock-years, discount rate, slope variant)
## - Financial assumptions (cashflow_variant, econ discount rate, cost type)
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
  carbon_stock_diff = "Data/NR2/stock_diff_vs_baseline__windowed__all_traj.rds",
  megatrees = "Data/full_nature_scenario_megatree_performance_all_thresholds.rds",
  financial = "Data/NR2/MasterFinancialPerformance__Sensitivity25pctMoneyParams_LONG.rds",
  all_scenarios = "Inputs/MasterAllScenarios.rds",
  export_dir = "Figures/NR2/GeomPoint_Replicable"
)
dir.create(PATHS$export_dir, recursive = TRUE, showWarnings = FALSE)

SCALE <- list(
  bil = 1e9,
  hundred_million = 1e8,
  width = 8.27,
  height = 11.69,
  text_size = 13,
  ## Extra vertical space when bivariate legends are appended (inches).
  legend_bivariate_extra_height = 1.85,
  legend_rel_height = 0.28,
  bi_legend_axis_size = 11.8,
  bi_legend_header_size = 11
)

## FIGURE_PRESETS controls scenario sets ("starting landscapes + rule set").
## Pick one preset in RUN_SETTINGS$figure_id.
## You can add your own presets here (example below).
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

## Example custom preset (uncomment to use):
## FIGURE_PRESETS$all_primary_nd <- list(
##   scenario_filter = c("AllPrimaryNoDef"),
##   output_stub = "AllPrimary_NoDef"
## )

OUTCOME_OPTIONS <- c("birds", "dung_beetles", "megatrees", "carbon", "profits", "protection")

BASE_RUN_SETTINGS <- list(
  figure_id = "fig2_d",               # choose from names(FIGURE_PRESETS)
  outcomes_to_plot = OUTCOME_OPTIONS, # any subset: c("carbon"), c("carbon","profits"), etc.
  include_legend = TRUE,
  errorbar_mode = "none",             # "none", "model_ci", "sensitivity_range"
  carbon_errorbar_mode = "model_ci",  # default: always show carbon model CI
  financial_errorbar_mode = "sensitivity_range", # default: show +/- sensitivity for profits/protection
  megatree_errorbar_mode = "model_ci",# default: always show megatree model CI
  uncertainty_style = "bars",         # bars are now the default/only CI style
  uncertainty_level = "80",           # used when errorbar_mode="model_ci": "50", "80", "95"
  megatree_uncertainty_level = "95",  # default megatree CI level
  sensitivity_field = "cashflow_variant",
  sensitivity_values = list(
    cashflow_variant = c("baseline", "plus25", "minus25"),
    carbon_slope_variant = c("0.8", "1.0", "1.2"),
    carbon_discount_rate = c("2%", "4%", "6%"),
    carbon_window_year_end = c(20, 40, 60),
    megatree_height_filt = c("45", "50", "55")
  ),
  sensitivity_center = "baseline",
  compare_mode = FALSE,
  compare_rows_by = c("carbon_discount_rate"),
  compare_values = list(
    carbon_discount_rate = c("2%", "4%", "6%"),
    carbon_slope_variant = c("0.8", "1.0", "1.2"),
    cashflow_variant = c("baseline", "plus25", "minus25"),
    carbon_window_year_end = c(20, 40, 60),
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
  carbon_window_year_end = 60,
  megatree_height_filt = "50",
  variable_scc_discount = NULL,
  output_pdf_name = NULL
)

as_null_or_value <- function(x) {
  if (identical(x, "") || toupper(x) == "NULL") return(NULL)
  x
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

merge_run_settings <- function(base, override) {
  out <- base
  for (nm in names(override)) out[[nm]] <- override[[nm]]
  out
}

# ===================================================================
# SELECT RUN HERE (EDIT THIS SECTION ONLY)
# ===================================================================
# Choose ONE run key below and assign it to ACTIVE_RUN.
# This is the main selector for which manuscript figure to produce.
#
# AVAILABLE RUN KEYS:
# - run1_fig1_losers_dr4
# - run2_fig1_losers_dr4_no_deforestation
# - run3_alternative_dr_scc
# - run4_alternative_dr_harvest_profs
# - run5_losers_dr4_starting_deforested
# - run6_birds_various_cats
# - run7_beetles_various_cats
# - run8_various_canopy_thresholds
# - run9_delta_stock  
# - run10_scc_slope_variants
# - run11_delta_stock_slope_variants
#
# OUTPUT PDF NAME:
# Each run already includes a direct output filename in RUN_LIBRARY$output_pdf_name.
# You usually do NOT need to edit filename logic elsewhere.
# ===================================================================
ACTIVE_RUN <- "run3_alternative_dr_scc"
RUN_ALL <- FALSE  # TRUE = run every preset in RUN_LIBRARY

# -------------------------------------------------------------------
# RUN LIBRARY (PRIMARY MANUSCRIPT PRESETS)
# -------------------------------------------------------------------
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
    errorbar_mode = "none",
    compare_mode = TRUE,
    compare_rows_by = c("bird_group"),
    compare_values = list(bird_group = c("loser", "Y", "N", "intermediate1L", "intermediate2L", "winner")),
    output_pdf_name = "birds_various_cats.pdf"
  ),
  run7_beetles_various_cats = list(
    figure_id = "fig2_d",
    outcomes_to_plot = c("dung_beetles"),
    errorbar_mode = "none",
    compare_mode = TRUE,
    compare_rows_by = c("beetle_group"),
    compare_values = list(beetle_group = c("loser", "intermediate1L", "intermediate2L", "winner")),
    output_pdf_name = "beetls_various_cats.pdf"
  ),
  run8_various_canopy_thresholds = list(
    figure_id = "fig2_d",
    outcomes_to_plot = c("megatrees"),
    errorbar_mode = "model_ci",
    uncertainty_level = "80",
    compare_mode = TRUE,
    compare_rows_by = c("megatree_height_filt"),
    compare_values = list(megatree_height_filt = c("45", "50", "55")),
    megatree_height_filt = "50",
    output_pdf_name = "various_canopy_thresholds.pdf"
  ),
  run9_delta_stock = list(
    figure_id = "fig2_d",
    outcomes_to_plot = c("carbon"),
    errorbar_mode = "model_ci",
    carbon_stream = "stock_diff_vs_baseline",
    carbon_discount_rate = NULL,
    carbon_window_year_end = 60,
    carbon_slope_variant = "1",
    output_pdf_name = "delta_stock.pdf"
  ),
  run10_scc_slope_variants = list(
    figure_id = "fig2_d",
    outcomes_to_plot = c("carbon"),
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
  run11_delta_stock_slope_variants = list(
    figure_id = "fig2_d",
    outcomes_to_plot = c("carbon"),
    carbon_errorbar_mode = "model_ci",
    uncertainty_level = "80",
    compare_mode = TRUE,
    compare_rows_by = c("carbon_slope_variant"),
    compare_values = list(carbon_slope_variant = c("0.75", "1", "1.2")),
    carbon_stream = "stock_diff_vs_baseline",
    carbon_discount_rate = NULL,
    carbon_window_year_end = 60,
    carbon_slope_variant = "1",
    output_pdf_name = "delta_stock_slope_variants.pdf"
  ),
  sum_stock_slope_variants = list(
    figure_id = "fig2_d",
    outcomes_to_plot = c("carbon"),
    errorbar_mode = "model_ci",
    carbon_errorbar_mode = "model_ci",
    uncertainty_level = "80",
    carbon_stream = "stock_year_slope_facets",
    carbon_discount_rate = "4%",
    carbon_slope_variant = NULL,
    carbon_slope_facet_values = c("0.75", "1", "1.2"),
    carbon_window_year_end = NULL,
    output_pdf_name = "sum_stock_slope_variants.pdf"
  )
)

# Optional runtime override (advanced users only).
# If ACTIVE_RUN is set in environment, it replaces the selector above.
active_run_env <- trimws(Sys.getenv("ACTIVE_RUN", ""))
if (nchar(active_run_env) > 0) ACTIVE_RUN <- active_run_env
run_all_env <- trimws(Sys.getenv("RUN_ALL", ""))
if (nchar(run_all_env) > 0) RUN_ALL <- tolower(run_all_env) %in% c("true", "1", "yes", "y")

if (!ACTIVE_RUN %in% names(RUN_LIBRARY)) {
  stop("ACTIVE_RUN must be one of: ", paste(names(RUN_LIBRARY), collapse = ", "))
}
RUN_SETTINGS <- merge_run_settings(BASE_RUN_SETTINGS, RUN_LIBRARY[[ACTIVE_RUN]])
message("Selected run: ", ACTIVE_RUN)
message("Planned output file: ", RUN_SETTINGS$output_pdf_name %||% paste0(ACTIVE_RUN, ".pdf"))

parse_csv_env <- function(x) {
  x <- trimws(x)
  if (identical(x, "")) return(NULL)
  out <- unlist(strsplit(x, ",", fixed = TRUE))
  trimws(out[nchar(trimws(out)) > 0])
}

# Optional runtime overrides so you can generate multiple PDFs
# without editing the script each time.
# Example (PowerShell):
#   $env:ACTIVE_RUN='run3_alternative_dr_scc'
#   $env:OUTPUT_PDF_NAME='my_custom_name.pdf'
#   Rscript "Scripts/Nature_Revision_2/02_BuildMultipanelPerformanceFigure_replicable.R"
RUN_SETTINGS$figure_id <- as_null_or_value(Sys.getenv("FIGURE_ID", RUN_SETTINGS$figure_id))
RUN_SETTINGS$outcomes_to_plot <- {
  env_val <- parse_csv_env(Sys.getenv("OUTCOMES", ""))
  if (is.null(env_val)) RUN_SETTINGS$outcomes_to_plot else env_val
}
RUN_SETTINGS$include_legend <- tolower(Sys.getenv("INCLUDE_LEGEND", ifelse(RUN_SETTINGS$include_legend, "true", "false"))) %in% c("true", "1", "yes", "y")
RUN_SETTINGS$errorbar_mode <- as_null_or_value(Sys.getenv("ERRORBAR_MODE", RUN_SETTINGS$errorbar_mode))
RUN_SETTINGS$carbon_errorbar_mode <- as_null_or_value(Sys.getenv("CARBON_ERRORBAR_MODE", RUN_SETTINGS$carbon_errorbar_mode %||% "NULL"))
RUN_SETTINGS$financial_errorbar_mode <- as_null_or_value(Sys.getenv("FINANCIAL_ERRORBAR_MODE", RUN_SETTINGS$financial_errorbar_mode %||% "NULL"))
RUN_SETTINGS$megatree_errorbar_mode <- as_null_or_value(Sys.getenv("MEGATREE_ERRORBAR_MODE", RUN_SETTINGS$megatree_errorbar_mode %||% "NULL"))
RUN_SETTINGS$uncertainty_style <- as_null_or_value(Sys.getenv("UNCERTAINTY_STYLE", RUN_SETTINGS$uncertainty_style))
RUN_SETTINGS$uncertainty_level <- as_null_or_value(Sys.getenv("UNCERTAINTY_LEVEL", RUN_SETTINGS$uncertainty_level))
RUN_SETTINGS$megatree_uncertainty_level <- as_null_or_value(Sys.getenv("MEGATREE_UNCERTAINTY_LEVEL", RUN_SETTINGS$megatree_uncertainty_level %||% "80"))
sensitivity_field_env <- trimws(Sys.getenv("SENSITIVITY_FIELD", ""))
if (nchar(sensitivity_field_env) > 0) RUN_SETTINGS$sensitivity_field <- as_null_or_value(sensitivity_field_env)
sensitivity_center_env <- trimws(Sys.getenv("SENSITIVITY_CENTER", ""))
if (nchar(sensitivity_center_env) > 0) RUN_SETTINGS$sensitivity_center <- as_null_or_value(sensitivity_center_env)
RUN_SETTINGS$compare_mode <- tolower(Sys.getenv("COMPARE_MODE", ifelse(RUN_SETTINGS$compare_mode, "true", "false"))) %in% c("true", "1", "yes", "y")
RUN_SETTINGS$compare_rows_by <- {
  env_val <- parse_csv_env(Sys.getenv("COMPARE_ROWS_BY", ""))
  if (is.null(env_val)) RUN_SETTINGS$compare_rows_by else env_val
}
RUN_SETTINGS$bird_group <- as_null_or_value(Sys.getenv("BIRD_GROUP", RUN_SETTINGS$bird_group))
RUN_SETTINGS$beetle_group <- as_null_or_value(Sys.getenv("BEETLE_GROUP", RUN_SETTINGS$beetle_group))
RUN_SETTINGS$economic_discount_rate <- as_null_or_value(Sys.getenv("ECON_DR", RUN_SETTINGS$economic_discount_rate))
RUN_SETTINGS$economic_cost_type <- as_null_or_value(Sys.getenv("ECON_COST_TYPE", RUN_SETTINGS$economic_cost_type))
RUN_SETTINGS$cashflow_variant <- as_null_or_value(Sys.getenv("CASHFLOW_VARIANT", RUN_SETTINGS$cashflow_variant %||% "NULL"))
RUN_SETTINGS$carbon_stream <- as_null_or_value(Sys.getenv("CARBON_STREAM", RUN_SETTINGS$carbon_stream))
RUN_SETTINGS$carbon_discount_rate <- as_null_or_value(Sys.getenv("CARBON_DR", RUN_SETTINGS$carbon_discount_rate %||% "NULL"))
RUN_SETTINGS$carbon_slope_variant <- as_null_or_value(Sys.getenv("CARBON_SLOPE", RUN_SETTINGS$carbon_slope_variant %||% "NULL"))
RUN_SETTINGS$megatree_height_filt <- as_null_or_value(Sys.getenv("MEGATREE_HEIGHT_FILT", RUN_SETTINGS$megatree_height_filt %||% "NULL"))
RUN_SETTINGS$variable_scc_discount <- as_null_or_value(Sys.getenv("VARIABLE_SCC_DISCOUNT", "NULL"))
output_name_env <- trimws(Sys.getenv("OUTPUT_PDF_NAME", ""))
if (nchar(output_name_env) > 0) RUN_SETTINGS$output_pdf_name <- as_null_or_value(output_name_env)

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
  resolved <- resolve_path(path)
  ext <- tolower(tools::file_ext(resolved))

  if (identical(ext, "csv")) return(read.csv(resolved))

  if (!identical(ext, "rds")) {
    stop("Unsupported carbon input extension: ", ext, ". Expected .rds or .csv")
  }

  obj <- readRDS(resolved)

  if (is.data.frame(obj) || data.table::is.data.table(obj)) return(obj)

  if (is.list(obj)) {
    if (!is.null(names(obj)) && length(obj) > 0 && all(vapply(obj, is.list, logical(1)))) {
      slope_tables <- purrr::imap(obj, function(slope_block, slope_name) {
        block_df <- data.table::rbindlist(slope_block, fill = TRUE)
        block_df[, twice_logged_slope_trajectory := parse_slope_from_name(slope_name)]
        block_df
      })
      return(data.table::rbindlist(slope_tables, fill = TRUE))
    }
    if (length(obj) > 0 && all(vapply(obj, is.data.frame, logical(1)))) {
      return(data.table::rbindlist(obj, fill = TRUE))
    }
  }

  stop("Unsupported structure in carbon RDS: expected a data frame or list of tables.")
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

stock_year_ci_cols <- list(
  `95` = c("lwr_cum_stock_year_95", "upr_cum_stock_year_95"),
  `80` = c("lwr_cum_stock_year_80", "upr_cum_stock_year_80"),
  `50` = c("lwr_cum_stock_year_50", "upr_cum_stock_year_50")
)

get_carbon_spec <- function(stream = c("scc", "stock_year", "stock_diff_vs_baseline", "stock_year_slope_facets")) {
  stream <- match.arg(stream)
  if (stream == "scc") {
    return(list(
      metric = "TOTcarbon_ACD_mean",
      ylab = "Social Carbon Cost\n(USD 1000M)",
      divisor = SCALE$bil,
      ci_cols = list(
        `95` = c("TOTcarbon_ACD_lwr95", "TOTcarbon_ACD_upr95"),
        `80` = c("TOTcarbon_ACD_lwr80", "TOTcarbon_ACD_upr80")
      )
    ))
  }
  if (stream == "stock_diff_vs_baseline") {
    return(list(
      metric = "mean_cum_stock_diff_vs_baseline",
      ylab = "\u0394 Cumulative C stock\nvs baseline (Mg C ha\u207B\u00B9 yr\u207B\u00B9)",
      divisor = SCALE$bil,
      ci_cols = list(
        `95` = c("lwr95_cum_stock_diff_vs_baseline", "upr95_cum_stock_diff_vs_baseline"),
        `80` = c("lwr80_cum_stock_diff_vs_baseline", "upr80_cum_stock_diff_vs_baseline")
      )
    ))
  }
  if (stream == "stock_year_slope_facets") {
    return(list(
      metric = "mean_cum_stock_year",
      ylab = "Cumulative carbon stock\n(billion C stock-yr)",
      divisor = SCALE$bil,
      ci_cols = stock_year_ci_cols
    ))
  }
  list(
    metric = "mean_cum_stock_year",
    ylab = "Carbon Stock Years\n(billion)",
    divisor = SCALE$bil,
    ci_cols = stock_year_ci_cols
  )
}

filter_carbon_variant <- function(df, discount_rate = NULL, slope_variant = NULL, window_year_end = NULL) {
  out <- as.data.frame(df)
  if (!("discount_rate" %in% names(out))) out$discount_rate <- NA_character_
  if (!("twice_logged_slope_trajectory" %in% names(out))) out$twice_logged_slope_trajectory <- NA_character_
  out$discount_rate <- as.character(out$discount_rate)
  out$twice_logged_slope_trajectory <- as.character(out$twice_logged_slope_trajectory)

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
  if (!is.null(window_year_end) && ("window_year_end" %in% names(out))) {
    window_vals <- as.numeric(window_year_end)
    out <- out %>% filter(as.numeric(.data$window_year_end) %in% window_vals)
  }
  out
}

filter_megatree_variant <- function(df, height_filt = NULL) {
  if (is.null(height_filt) || !("height_filt" %in% names(df))) return(df)
  out <- df %>% mutate(height_filt = as.character(.data$height_filt))
  filt_vals <- as.character(height_filt)
  filt_num <- suppressWarnings(as.numeric(filt_vals))
  field_num <- suppressWarnings(as.numeric(out$height_filt))
  if (all(!is.na(filt_num)) && all(!is.na(field_num[!is.na(out$height_filt)]))) {
    return(out[field_num %in% filt_num, , drop = FALSE])
  }
  out %>% filter(.data$height_filt %in% filt_vals)
}

get_megatree_ci_cols <- function(level = "80") {
  level <- as.character(level)
  # Prefer explicit level-specific columns, but allow legacy names.
  if (identical(level, "95")) {
    return(list(
      c("landscape_prop_lwr95", "landscape_prop_upr95"),
      c("landscape_prop_lwr", "landscape_prop_upr")
    ))
  }
  if (identical(level, "80")) {
    return(list(
      c("landscape_prop_lwr80", "landscape_prop_upr80"),
      c("landscape_prop_lwr", "landscape_prop_upr")
    ))
  }
  if (identical(level, "50")) {
    return(list(
      c("landscape_prop_lwr50", "landscape_prop_upr50"),
      c("landscape_prop_lwr", "landscape_prop_upr")
    ))
  }
  list(c("landscape_prop_lwr80", "landscape_prop_upr80"), c("landscape_prop_lwr", "landscape_prop_upr"))
}

filter_cashflow_variant <- function(df, cashflow_variant = NULL) {
  if (is.null(cashflow_variant) || !("cashflow_variant" %in% names(df))) return(df)
  vals <- as.character(cashflow_variant)
  df %>% filter(as.character(.data$cashflow_variant) %in% vals)
}

apply_sensitivity_range <- function(df, value_col, sensitivity_field, sensitivity_values = NULL, sensitivity_center = NULL) {
  if (is.null(sensitivity_field) || !(sensitivity_field %in% names(df))) return(df)

  out <- df
  if (!is.null(sensitivity_values) && sensitivity_field %in% names(sensitivity_values)) {
    keep_vals <- sensitivity_values[[sensitivity_field]]
    field_vec <- out[[sensitivity_field]]
    keep_num <- suppressWarnings(as.numeric(as.character(keep_vals)))
    field_num <- suppressWarnings(as.numeric(as.character(field_vec)))
    if (all(!is.na(keep_num)) && all(!is.na(field_num[!is.na(field_vec)]))) {
      out <- out[field_num %in% keep_num, , drop = FALSE]
    } else {
      out <- out %>% filter(.data[[sensitivity_field]] %in% keep_vals)
    }
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
      filter(.data[[sensitivity_field]] == sensitivity_center) %>%
      group_by(across(all_of(group_cols))) %>%
      summarise(plot_value = mean(.data[[value_col]], na.rm = TRUE), .groups = "drop")
  } else {
    center_df <- out %>%
      group_by(across(all_of(group_cols))) %>%
      summarise(plot_value = mean(.data[[value_col]], na.rm = TRUE), .groups = "drop")
  }

  range_df %>% left_join(center_df, by = group_cols)
}

build_comparison_grid <- function(settings) {
  if (!isTRUE(settings$compare_mode)) return(NULL)
  if (is.null(settings$compare_rows_by) || length(settings$compare_rows_by) == 0) {
    stop("compare_mode=TRUE requires at least one value in compare_rows_by.")
  }

  row_fields <- settings$compare_rows_by
  value_list <- lapply(row_fields, function(field) settings$compare_values[[field]])
  names(value_list) <- row_fields

  missing_fields <- names(value_list)[vapply(value_list, is.null, logical(1))]
  if (length(missing_fields) > 0) {
    stop("Missing compare_values for: ", paste(missing_fields, collapse = ", "))
  }
  empty_fields <- names(value_list)[vapply(value_list, length, integer(1)) == 0]
  if (length(empty_fields) > 0) {
    stop("Empty compare_values vectors for: ", paste(empty_fields, collapse = ", "))
  }

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
    carbon_window_year_end = "Window end year",
    economic_discount_rate = "Economic discount rate",
    megatree_height_filt = "Megatree height filter"
  )
  parts <- unlist(lapply(names(row), function(field) {
    label_name <- label_map[[field]] %||% field
    paste0(label_name, ": ", row[[field]][[1]])
  }))
  paste(parts, collapse = " | ")
}

get_setting_values <- function(settings, field) {
  if (isTRUE(settings$compare_mode) &&
      !is.null(settings$compare_rows_by) &&
      field %in% settings$compare_rows_by &&
      !is.null(settings$compare_values[[field]])) {
    return(settings$compare_values[[field]])
  }
  if (identical(field, "carbon_slope_variant") && !is.null(settings$carbon_slope_facet_values)) {
    return(settings$carbon_slope_facet_values)
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
    errorbar_mode <- settings$errorbar_mode %||% "none"
    megatree_errorbar_mode <- settings$megatree_errorbar_mode %||% errorbar_mode
    M <- inputs$megatrees %>%
      filter(scenarioName %in% scenario_filter) %>%
      filter_megatree_variant(height_filt = get_setting_values(settings, "megatree_height_filt")) %>%
      mutate(plot_value = landscape_prop)
    if (identical(megatree_errorbar_mode, "model_ci")) {
      ci_candidates <- get_megatree_ci_cols(settings$megatree_uncertainty_level %||% "80")
      for (cand in ci_candidates) {
        if (all(cand %in% names(M))) {
          M <- M %>% mutate(plot_ymin = .data[[cand[1]]], plot_ymax = .data[[cand[2]]])
          break
        }
      }
    }
    if (nrow(M) > 0) limits$megatrees <- range_with_optional_bounds(M, value_col = "plot_value", ymin_col = "plot_ymin", ymax_col = "plot_ymax")
  }

  if ("carbon" %in% selected_outcomes && !is.null(inputs$carbon)) {
    errorbar_mode <- settings$errorbar_mode %||% "none"
    carbon_errorbar_mode <- settings$carbon_errorbar_mode %||% errorbar_mode
    carbon_spec <- get_carbon_spec(settings$carbon_stream)
    carbon_input <- if (identical(settings$carbon_stream, "stock_diff_vs_baseline")) inputs$carbon_stock_diff else inputs$carbon
    C <- carbon_input %>%
      filter(scenarioName %in% scenario_filter) %>%
      filter_carbon_variant(
        discount_rate = get_setting_values(settings, "carbon_discount_rate"),
        slope_variant = get_setting_values(settings, "carbon_slope_variant"),
        window_year_end = get_setting_values(settings, "carbon_window_year_end")
      ) %>%
      mutate(plot_value = .data[[carbon_spec$metric]] / carbon_spec$divisor)
    if (identical(carbon_errorbar_mode, "model_ci")) {
      ci_key <- as.character(settings$uncertainty_level %||% "80")
      ci_cols <- carbon_spec$ci_cols[[ci_key]]
      if (!is.null(ci_cols) && all(ci_cols %in% names(C))) {
        C <- C %>% mutate(plot_ymin = .data[[ci_cols[1]]] / carbon_spec$divisor, plot_ymax = .data[[ci_cols[2]]] / carbon_spec$divisor)
      }
    }
    if (nrow(C) > 0) limits$carbon <- range_with_optional_bounds(C, value_col = "plot_value", ymin_col = "plot_ymin", ymax_col = "plot_ymax")
  }

  if ("profits" %in% selected_outcomes && !is.null(inputs$profits)) {
    errorbar_mode <- settings$errorbar_mode %||% "none"
    financial_errorbar_mode <- settings$financial_errorbar_mode %||% errorbar_mode
    P <- inputs$profits %>%
      filter(
        scenarioName %in% scenario_filter,
        costType %in% as.character(get_setting_values(settings, "economic_cost_type")),
        discount_rate %in% as.character(get_setting_values(settings, "economic_discount_rate"))
      ) %>%
      filter_cashflow_variant(cashflow_variant = get_setting_values(settings, "cashflow_variant")) %>%
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
    if (nrow(P) > 0) limits$profits <- range_with_optional_bounds(P, value_col = "plot_value", ymin_col = "plot_ymin", ymax_col = "plot_ymax")
  }

  if ("protection" %in% selected_outcomes && !is.null(inputs$protection)) {
    errorbar_mode <- settings$errorbar_mode %||% "none"
    financial_errorbar_mode <- settings$financial_errorbar_mode %||% errorbar_mode
    PC <- inputs$protection %>%
      filter(scenarioName %in% scenario_filter, discount_rate %in% as.character(get_setting_values(settings, "economic_discount_rate"))) %>%
      filter_cashflow_variant(cashflow_variant = get_setting_values(settings, "cashflow_variant")) %>%
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
    if (nrow(PC) > 0) limits$protection <- range_with_optional_bounds(PC, value_col = "plot_value", ymin_col = "plot_ymin", ymax_col = "plot_ymax")
  }

  limits
}

master_plot_fun <- function(df, y_col, ylab_text, scenario_filter, errorbar_mode = "none", uncertainty_style = "bars", ymin_col = NULL, ymax_col = NULL, errorbar_alpha = 0.28, errorbar_linewidth = 0.25, y_limits = NULL, show_x_axis_title = FALSE, facet_layout = "scenario_wrap") {
  base_df <- df %>%
    mutate(
      scenarioName = fct_relevel(scenarioName, scenario_filter),
      plot_colour = case_when(
        scenarioStart %in% c("all_primary", "primary_deforested") ~ hexP,
        scenarioStart %in% c("mostly_1L", "mostly_1L_deforested") ~ hex1L,
        scenarioStart %in% c("mostly_2L", "mostly_2L_deforested") ~ hex2L
      )
    )

  # Shared x-jitter for BOTH bars and points keeps each CI aligned to its point.
  point_position <- position_jitter(width = 0.02, height = 0, seed = 1)

  p <- base_df %>%
    ggplot(aes(x = production_target, y = .data[[y_col]]))

  if (identical(errorbar_mode, "model_ci") || identical(errorbar_mode, "sensitivity_range")) {
    if (!is.null(ymin_col) && !is.null(ymax_col) && all(c(ymin_col, ymax_col) %in% names(base_df))) {
      p <- p +
        geom_errorbar(
          aes(
            ymin = .data[[ymin_col]],
            ymax = .data[[ymax_col]],
            colour = plot_colour
          ),
          position = point_position,
          width = 0,
          linewidth = errorbar_linewidth,
          alpha = errorbar_alpha
        )
    }
  }

  facet_lay <- facet_layout %||% "scenario_wrap"
  p_final <- p +
    geom_point(
      aes(
        y = .data[[y_col]],
        colour = plot_colour,
        shape = shape_class
      ),
      position = point_position,
      size = 1.7,
      alpha = 0.82,
      stroke = 0.6
    ) +
    scale_colour_identity() +
    scale_shape_manual(values = c("Point" = 19, "Cross" = 2)) +
    xlim(0, 1) +
    xlab(if (isTRUE(show_x_axis_title)) "Production target (P)" else NULL) +
    ylab(ylab_text)
  if (identical(facet_lay, "scenario_x_slope_grid")) {
    p_final <- p_final + facet_grid(rows = vars(slope_facet), cols = vars(scenarioName), switch = "y")
  } else {
    p_final <- p_final + facet_wrap(~scenarioName, ncol = 4)
  }
  p_final <- p_final +
    theme_bw(base_size = SCALE$text_size) +
    theme(
      legend.position = "none",
      panel.grid.major = element_line(color = "grey90"),
      panel.grid.minor = element_blank()
    )
  if (identical(facet_lay, "scenario_x_slope_grid")) {
    p_final <- p_final + theme(
      strip.text.x = element_blank(),
      strip.text.y = element_text(face = "bold", size = max(8.5, SCALE$text_size - 3.5))
    )
  } else {
    p_final <- p_final + theme(strip.text = element_blank())
  }

  if (!is.null(y_limits) && length(y_limits) == 2 && all(is.finite(y_limits))) {
    p_final <- p_final + coord_cartesian(ylim = y_limits)
  }
  p_final
}

build_main_figure <- function(birds, dung_beetles, megatrees, carbon, carbon_stock_diff, profits, protection, all_legend, settings, preset, y_limits_map = NULL, add_bottom_x_axis_title = TRUE) {
  carbon_spec <- get_carbon_spec(settings$carbon_stream)
  scenario_filter <- preset$scenario_filter
  selected_outcomes <- unique(settings$outcomes_to_plot)
  panel_stack_order <- c("birds", "dung_beetles", "megatrees", "carbon", "profits", "protection")
  last_x_outcome <- tail(intersect(panel_stack_order, selected_outcomes), 1L)
  show_x <- function(outcome) isTRUE(add_bottom_x_axis_title) && length(last_x_outcome) == 1L && identical(outcome, last_x_outcome)
  panels <- list()
  errorbar_mode <- settings$errorbar_mode %||% "none"
  carbon_errorbar_mode <- settings$carbon_errorbar_mode %||% errorbar_mode
  financial_errorbar_mode <- settings$financial_errorbar_mode %||% errorbar_mode
  megatree_errorbar_mode <- settings$megatree_errorbar_mode %||% errorbar_mode
  uncertainty_style <- settings$uncertainty_style %||% "bars"

  if ("birds" %in% selected_outcomes) {
    B <- birds %>%
      filter(scenarioName %in% scenario_filter, bird_grp == settings$bird_group)
    if (nrow(B) == 0) stop("No data for birds panel with current RUN_SETTINGS.")
    B <- B %>% mutate(plot_value = medianRelativeOccupancy)
    panels <- c(panels, list(
      B %>% master_plot_fun(
        y_col = "plot_value",
        ylab_text = "Median Relative\nOccupancy",
        scenario_filter = scenario_filter,
        errorbar_mode = "none",
        y_limits = y_limits_map$birds,
        show_x_axis_title = show_x("birds")
      )
    ))
  }

  if ("dung_beetles" %in% selected_outcomes) {
    DB <- dung_beetles %>%
      filter(scenarioName %in% scenario_filter, spp_category == settings$beetle_group)
    if (nrow(DB) == 0) stop("No data for dung beetles panel with current RUN_SETTINGS.")
    DB <- DB %>% mutate(plot_value = medianRelativeOccupancy)
    panels <- c(panels, list(
      DB %>% master_plot_fun(
        y_col = "plot_value",
        ylab_text = "Median Relative\nAbundance",
        scenario_filter = scenario_filter,
        errorbar_mode = "none",
        y_limits = y_limits_map$dung_beetles,
        show_x_axis_title = show_x("dung_beetles")
      )
    ))
  }

  if ("megatrees" %in% selected_outcomes) {
    M <- megatrees %>%
      filter(scenarioName %in% scenario_filter) %>%
      filter_megatree_variant(height_filt = settings$megatree_height_filt)
    if (nrow(M) == 0) stop("No data for megatrees panel with current RUN_SETTINGS.")
    M <- M %>% mutate(plot_value = landscape_prop)
    if (identical(megatree_errorbar_mode, "model_ci")) {
      ci_candidates <- get_megatree_ci_cols(settings$megatree_uncertainty_level %||% "80")
      selected_ci_cols <- NULL
      for (cand in ci_candidates) {
        if (all(cand %in% names(M))) {
          selected_ci_cols <- cand
          break
        }
      }
      if (!is.null(selected_ci_cols)) {
        M <- M %>% mutate(plot_ymin = .data[[selected_ci_cols[1]]], plot_ymax = .data[[selected_ci_cols[2]]])
      } else {
        warning(
          "Megatree CI requested but CI columns were not found. Checked: ",
          paste(unlist(ci_candidates), collapse = ", ")
        )
      }
    } else if (identical(megatree_errorbar_mode, "sensitivity_range")) {
      M <- apply_sensitivity_range(
        M,
        value_col = "plot_value",
        sensitivity_field = settings$sensitivity_field,
        sensitivity_values = settings$sensitivity_values,
        sensitivity_center = settings$sensitivity_center
      )
    }
    panels <- c(panels, list(
      M %>% master_plot_fun(
        y_col = "plot_value",
        ylab_text = "Megatree\nyears",
        scenario_filter = scenario_filter,
        errorbar_mode = megatree_errorbar_mode,
        uncertainty_style = uncertainty_style,
        ymin_col = if ("plot_ymin" %in% names(M)) "plot_ymin" else NULL,
        ymax_col = if ("plot_ymax" %in% names(M)) "plot_ymax" else NULL,
        errorbar_alpha = 0.45,
        errorbar_linewidth = 0.35,
        y_limits = y_limits_map$megatrees,
        show_x_axis_title = show_x("megatrees")
      )
    ))
  }

  if ("carbon" %in% selected_outcomes) {
    carbon_input <- if (identical(settings$carbon_stream, "stock_diff_vs_baseline")) carbon_stock_diff else carbon
    C <- carbon_input %>%
      filter(scenarioName %in% scenario_filter) %>%
      filter_carbon_variant(
        discount_rate = get_setting_values(settings, "carbon_discount_rate"),
        slope_variant = get_setting_values(settings, "carbon_slope_variant"),
        window_year_end = get_setting_values(settings, "carbon_window_year_end")
      )
    if (nrow(C) == 0) stop("No data for carbon panel with current RUN_SETTINGS.")
    C <- C %>% mutate(plot_value = .data[[carbon_spec$metric]] / carbon_spec$divisor)

    facet_layout_carbon <- settings$carbon_facet_layout %||% "scenario_wrap"
    if (identical(settings$carbon_stream, "stock_year_slope_facets")) {
      sfv <- settings$carbon_slope_facet_values %||% c("0.75", "1", "1.2")
      C <- C %>% mutate(slope_facet = factor(as.character(twice_logged_slope_trajectory), levels = as.character(sfv)))
      facet_layout_carbon <- "scenario_x_slope_grid"
    }

    if (identical(carbon_errorbar_mode, "model_ci")) {
      ci_key <- as.character(settings$uncertainty_level %||% "80")
      ci_cols <- carbon_spec$ci_cols[[ci_key]]
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

    panels <- c(panels, list(
      C %>% master_plot_fun(
        y_col = "plot_value",
        ylab_text = carbon_spec$ylab,
        scenario_filter = scenario_filter,
        errorbar_mode = carbon_errorbar_mode,
        uncertainty_style = uncertainty_style,
        ymin_col = if ("plot_ymin" %in% names(C)) "plot_ymin" else NULL,
        ymax_col = if ("plot_ymax" %in% names(C)) "plot_ymax" else NULL,
        y_limits = y_limits_map$carbon,
        show_x_axis_title = show_x("carbon"),
        facet_layout = facet_layout_carbon
      )
    ))
  }

  if ("profits" %in% selected_outcomes) {
    P <- profits %>%
      filter(
        scenarioName %in% scenario_filter,
        costType == settings$economic_cost_type,
        discount_rate == settings$economic_discount_rate
      ) %>%
      filter_cashflow_variant(cashflow_variant = settings$cashflow_variant)
    if (nrow(P) == 0) stop("No data for profits panel with current RUN_SETTINGS.")
    P <- P %>% mutate(plot_value = NPV / SCALE$hundred_million)
    if (identical(financial_errorbar_mode, "sensitivity_range")) {
      P <- apply_sensitivity_range(
        P,
        value_col = "plot_value",
        sensitivity_field = settings$sensitivity_field,
        sensitivity_values = settings$sensitivity_values,
        sensitivity_center = settings$sensitivity_center
      )
    }
    panels <- c(panels, list(
      P %>% master_plot_fun(
        y_col = "plot_value",
        ylab_text = "Harvest NPV\n(USD 100M)",
        scenario_filter = scenario_filter,
        errorbar_mode = if (identical(financial_errorbar_mode, "sensitivity_range")) "sensitivity_range" else if (identical(financial_errorbar_mode, "model_ci")) "model_ci" else "none",
        uncertainty_style = uncertainty_style,
        ymin_col = if ("plot_ymin" %in% names(P)) "plot_ymin" else NULL,
        ymax_col = if ("plot_ymax" %in% names(P)) "plot_ymax" else NULL,
        y_limits = y_limits_map$profits,
        show_x_axis_title = show_x("profits")
      )
    ))
  }

  if ("protection" %in% selected_outcomes) {
    PC <- protection %>%
      filter(scenarioName %in% scenario_filter, discount_rate == settings$economic_discount_rate) %>%
      filter_cashflow_variant(cashflow_variant = settings$cashflow_variant)
    if (nrow(PC) == 0) stop("No data for protection panel with current RUN_SETTINGS.")
    PC <- PC %>% mutate(plot_value = NPV / SCALE$hundred_million)
    if (identical(financial_errorbar_mode, "sensitivity_range")) {
      PC <- apply_sensitivity_range(
        PC,
        value_col = "plot_value",
        sensitivity_field = settings$sensitivity_field,
        sensitivity_values = settings$sensitivity_values,
        sensitivity_center = settings$sensitivity_center
      )
    }
    panels <- c(panels, list(
      PC %>% master_plot_fun(
        y_col = "plot_value",
        ylab_text = "Protection NPV\n(USD 100M)",
        scenario_filter = scenario_filter,
        errorbar_mode = if (identical(financial_errorbar_mode, "sensitivity_range")) "sensitivity_range" else if (identical(financial_errorbar_mode, "model_ci")) "model_ci" else "none",
        uncertainty_style = uncertainty_style,
        ymin_col = if ("plot_ymin" %in% names(PC)) "plot_ymin" else NULL,
        ymax_col = if ("plot_ymax" %in% names(PC)) "plot_ymax" else NULL,
        y_limits = y_limits_map$protection,
        show_x_axis_title = show_x("protection")
      )
    ))
  }

  if (length(panels) == 0) stop("RUN_SETTINGS$outcomes_to_plot did not select any panels.")
  main <- plot_grid(plotlist = panels, ncol = 1)
  if (isTRUE(settings$include_legend)) {
    leg_h <- settings$legend_rel_height %||% SCALE$legend_rel_height %||% 0.28
    return(plot_grid(main, all_legend, nrow = 2, rel_heights = c(1, leg_h)))
  }
  main
}

validate_run_settings <- function(settings, run_label) {
  if (!settings$figure_id %in% names(FIGURE_PRESETS)) {
    stop("RUN_SETTINGS$figure_id must be one of: ", paste(names(FIGURE_PRESETS), collapse = ", "))
  }
  unknown_outcomes <- setdiff(settings$outcomes_to_plot, OUTCOME_OPTIONS)
  if (length(unknown_outcomes) > 0) {
    stop("Unknown outcomes in RUN_SETTINGS$outcomes_to_plot: ", paste(unknown_outcomes, collapse = ", "))
  }
  valid_compare_fields <- c("carbon_discount_rate", "carbon_slope_variant", "carbon_window_year_end", "cashflow_variant", "economic_discount_rate", "economic_cost_type", "carbon_stream", "bird_group", "beetle_group", "megatree_height_filt")
  if (isTRUE(settings$compare_mode)) {
    bad_fields <- setdiff(settings$compare_rows_by, valid_compare_fields)
    if (length(bad_fields) > 0) {
      stop("Unsupported compare_rows_by fields: ", paste(bad_fields, collapse = ", "))
    }
  }
  valid_errorbar_modes <- c("none", "model_ci", "sensitivity_range")
  if (!settings$errorbar_mode %in% valid_errorbar_modes) {
    stop("RUN_SETTINGS$errorbar_mode must be one of: ", paste(valid_errorbar_modes, collapse = ", "))
  }
  if (!is.null(settings$carbon_errorbar_mode) && !settings$carbon_errorbar_mode %in% valid_errorbar_modes) {
    stop("RUN_SETTINGS$carbon_errorbar_mode must be one of: ", paste(valid_errorbar_modes, collapse = ", "), " (or NULL)")
  }
  if (!is.null(settings$financial_errorbar_mode) && !settings$financial_errorbar_mode %in% valid_errorbar_modes) {
    stop("RUN_SETTINGS$financial_errorbar_mode must be one of: ", paste(valid_errorbar_modes, collapse = ", "), " (or NULL)")
  }
  if (!is.null(settings$megatree_errorbar_mode) && !settings$megatree_errorbar_mode %in% valid_errorbar_modes) {
    stop("RUN_SETTINGS$megatree_errorbar_mode must be one of: ", paste(valid_errorbar_modes, collapse = ", "), " (or NULL)")
  }
  valid_uncertainty_styles <- c("bars")
  if (!settings$uncertainty_style %in% valid_uncertainty_styles) {
    stop("RUN_SETTINGS$uncertainty_style must be 'bars'.")
  }
  if ((identical(settings$errorbar_mode, "sensitivity_range") ||
       identical(settings$carbon_errorbar_mode, "sensitivity_range") ||
       identical(settings$financial_errorbar_mode, "sensitivity_range")) &&
      is.null(settings$sensitivity_field)) {
    stop("When errorbar_mode='sensitivity_range', set RUN_SETTINGS$sensitivity_field.")
  }
  valid_uncertainty_levels <- c("50", "80", "95")
  if (!as.character(settings$uncertainty_level) %in% valid_uncertainty_levels) {
    stop("RUN_SETTINGS$uncertainty_level must be one of: 50, 80, 95")
  }
  if (!is.null(settings$megatree_uncertainty_level) && !as.character(settings$megatree_uncertainty_level) %in% valid_uncertainty_levels) {
    stop("RUN_SETTINGS$megatree_uncertainty_level must be one of: 50, 80, 95")
  }
  valid_cashflow_variants <- c("baseline", "plus25", "minus25")
  if (!is.null(settings$cashflow_variant) && !settings$cashflow_variant %in% valid_cashflow_variants) {
    stop("RUN_SETTINGS$cashflow_variant must be one of: ", paste(valid_cashflow_variants, collapse = ", "), " (or NULL)")
  }
  valid_megatree_height_filt <- c("45", "50", "55")
  if (!is.null(settings$megatree_height_filt) && !as.character(settings$megatree_height_filt) %in% valid_megatree_height_filt) {
    stop("RUN_SETTINGS$megatree_height_filt must be one of: ", paste(valid_megatree_height_filt, collapse = ", "), " (or NULL)")
  }
}

if (!isTRUE(RUN_ALL)) validate_run_settings(RUN_SETTINGS, ACTIVE_RUN)

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

## Bivariate colour keys aligned with the three scenario columns (axis labels only).
build_bivariate_legend_footer <- function(pal = COL, dim_val = 4L) {
  ax <- SCALE$bi_legend_axis_size %||% 11.5
  primary_legend <- bi_legend(
    pal = pal,
    dim = dim_val,
    xlab = "Old-growth share",
    ylab = "Once-logged share",
    size = ax
  )
  onceL_legend <- bi_legend(
    pal = pal,
    dim = dim_val,
    xlab = "Old-growth\n(relative)",
    ylab = "Once-logged\n(relative)",
    size = ax
  )
  twiceL_legend <- bi_legend(
    pal = pal,
    dim = dim_val,
    xlab = "Old-growth\n(relative)",
    ylab = "Twice-logged\n(relative)",
    size = ax
  )
  plot_grid(
    primary_legend, onceL_legend, twiceL_legend,
    ncol = 3L,
    align = "h",
    axis = "tblr",
    rel_widths = c(1, 1, 1)
  )
}

cols <- data.frame(bi_pal(COL, dim = 4, preview = FALSE))
colnames(cols) <- "hex"
cols <- cols %>% mutate(bi_class = rownames(.))
all_legend <- build_bivariate_legend_footer(pal = COL, dim_val = 4L)

load_inputs_for_settings <- function(settings) {
  selected_outcomes <- settings$outcomes_to_plot
  need_birds <- "birds" %in% selected_outcomes
  need_beetles <- "dung_beetles" %in% selected_outcomes
  need_carbon <- "carbon" %in% selected_outcomes
  need_megatrees <- "megatrees" %in% selected_outcomes
  need_financial <- any(c("profits", "protection") %in% selected_outcomes)
  need_profits <- "profits" %in% selected_outcomes
  need_protection <- "protection" %in% selected_outcomes

  prep_base <- function(df) {
    propOGcomp[df, on = .(index, production_target)]
  }
  prep_styled <- function(df) {
    prep_base(df) %>% add_bivariate_colours(cols) %>% add_plantation_type() %>% rename_scenario_name()
  }

  birds <- NULL
  dungBeetles <- NULL
  carbon <- NULL
  carbon_stock_diff <- NULL
  megatrees <- NULL
  profits <- NULL
  protection <- NULL

  if (need_birds) {
    birds_main <- readRDS(resolve_path(PATHS$birds)) %>% distinct() %>% rename(bird_grp = spp_category)
    birds_iucn <- readRDS(resolve_path(PATHS$birds_iucn)) %>% distinct() %>% rename(bird_grp = threatened)
    birds <- bind_rows(birds_main, birds_iucn) %>%
      mutate(index = as.character(index), production_target = as.numeric(production_target)) %>%
      as.data.table() %>%
      prep_styled()
  }

  if (need_beetles) {
    dungBeetles <- readRDS(resolve_path(PATHS$dung_beetles)) %>%
      mutate(index = as.character(index), production_target = as.numeric(production_target)) %>%
      as.data.table() %>%
      prep_styled()
  }

  if (need_carbon) {
    if (identical(settings$carbon_stream, "stock_diff_vs_baseline")) {
      carbon_stock_diff <- load_carbon_data(PATHS$carbon_stock_diff) %>%
        mutate(index = as.character(index), production_target = as.numeric(production_target)) %>%
        as.data.table() %>%
        prep_styled()
      required_carbon_cols <- c("mean_cum_stock_diff_vs_baseline", "window_year_end", "twice_logged_slope_trajectory")
      missing_cols <- setdiff(required_carbon_cols, names(carbon_stock_diff))
      if (length(missing_cols) > 0) {
        stop("Delta-stock carbon table is missing required columns: ", paste(missing_cols, collapse = ", "))
      }
    } else {
      carbon <- load_carbon_data(PATHS$carbon) %>%
        mutate(index = as.character(index), production_target = as.numeric(production_target)) %>%
        as.data.table() %>%
        prep_styled()
      required_carbon_cols <- c("TOTcarbon_ACD_mean", "mean_cum_stock_year", "discount_rate", "twice_logged_slope_trajectory")
      missing_cols <- setdiff(required_carbon_cols, names(carbon))
      if (length(missing_cols) > 0) {
        stop("Carbon table is missing required columns: ", paste(missing_cols, collapse = ", "))
      }
    }
  }

  if (need_megatrees) {
    megatrees <- readRDS(resolve_path(PATHS$megatrees)) %>%
      mutate(index = as.character(index), production_target = as.numeric(production_target), height_filt = as.character(height_filt)) %>%
      as.data.table()
    megatrees <- prep_base(megatrees) %>%
      select(-c(scenarioName, scenarioStart)) %>%
      rename(scenarioName = i.scenarioName, scenarioStart = i.scenarioStart) %>%
      add_bivariate_colours(cols) %>%
      add_plantation_type() %>%
      rename_scenario_name()
    required_megatree_cols <- c("landscape_prop", "height_filt")
    missing_megatree_cols <- setdiff(required_megatree_cols, names(megatrees))
    if (length(missing_megatree_cols) > 0) {
      stop("Megatree table is missing required columns: ", paste(missing_megatree_cols, collapse = ", "))
    }
  }

  if (need_financial) {
    financial_long <- readRDS(resolve_path(PATHS$financial)) %>%
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

    if (need_profits) {
      profits <- prep_styled(financial_long)
    }
    if (need_protection) {
      protection <- financial_long %>%
        filter(costType == "ProtectionCosts") %>%
        mutate(outcome = "protection") %>%
        as.data.table() %>%
        prep_styled()
    }
  }

  list(
    birds = birds,
    dung_beetles = dungBeetles,
    carbon = carbon,
    carbon_stock_diff = carbon_stock_diff,
    megatrees = megatrees,
    profits = profits,
    protection = protection
  )
}

## Compare-mode row caption: inset top-right for delta-stock rows so it does not
## collide with the rotated y-axis title (strip-above-plot layout otherwise).
assemble_compare_row_plot <- function(row_base, row_label, inset_compare_label) {
  if (isTRUE(inset_compare_label)) {
    ggdraw(row_base) +
      draw_label(
        row_label,
        x = 0.99,
        y = 0.91,
        hjust = 1,
        vjust = 1,
        fontface = "bold",
        size = 9,
        colour = "grey20"
      )
  } else {
    plot_grid(
      ggdraw() + draw_label(row_label, x = 0, hjust = 0, fontface = "bold", size = 10),
      row_base,
      ncol = 1,
      rel_heights = c(0.06, 1)
    )
  }
}

render_and_save <- function(settings, run_label) {
  validate_run_settings(settings, run_label)
  selected_preset <- FIGURE_PRESETS[[settings$figure_id]]
  loaded_inputs <- load_inputs_for_settings(settings)
  y_limits_map <- if (isTRUE(settings$compare_mode)) compute_y_limits_map(loaded_inputs, settings, selected_preset) else NULL
  comparison_grid <- build_comparison_grid(settings)

  if (isTRUE(settings$compare_mode)) {
    row_plots <- vector("list", nrow(comparison_grid))
    for (i in seq_len(nrow(comparison_grid))) {
      row_settings <- apply_row_settings(settings, comparison_grid, i)
      row_settings$include_legend <- FALSE
      ## Per-row filters must use this row's scalars, not the full compare_values grid.
      row_settings_plot <- row_settings
      row_settings_plot$compare_mode <- FALSE
      row_base <- build_main_figure(
        birds = loaded_inputs$birds,
        dung_beetles = loaded_inputs$dung_beetles,
        megatrees = loaded_inputs$megatrees,
        carbon = loaded_inputs$carbon,
        carbon_stock_diff = loaded_inputs$carbon_stock_diff,
        profits = loaded_inputs$profits,
        protection = loaded_inputs$protection,
        all_legend = all_legend,
        settings = row_settings_plot,
        preset = selected_preset,
        y_limits_map = y_limits_map,
        add_bottom_x_axis_title = (i == nrow(comparison_grid))
      )
      row_label <- format_row_label(comparison_grid, i)
      inset_lbl <- identical(settings$carbon_stream, "stock_diff_vs_baseline")
      row_plots[[i]] <- assemble_compare_row_plot(row_base, row_label, inset_lbl)
    }
    compare_main <- plot_grid(plotlist = row_plots, ncol = 1)
    leg_h <- settings$legend_rel_height %||% SCALE$legend_rel_height %||% 0.28
    if (isTRUE(settings$include_legend)) {
      figure_obj <- plot_grid(compare_main, all_legend, nrow = 2, rel_heights = c(1, leg_h))
    } else {
      figure_obj <- compare_main
    }
  } else {
    figure_obj <- build_main_figure(
      birds = loaded_inputs$birds,
      dung_beetles = loaded_inputs$dung_beetles,
      megatrees = loaded_inputs$megatrees,
      carbon = loaded_inputs$carbon,
      carbon_stock_diff = loaded_inputs$carbon_stock_diff,
      profits = loaded_inputs$profits,
      protection = loaded_inputs$protection,
      all_legend = all_legend,
      settings = settings,
      preset = selected_preset,
      y_limits_map = y_limits_map
    )
  }

  default_pdf_name <- paste0(run_label, ".pdf")
  output_pdf_name <- settings$output_pdf_name %||% default_pdf_name
  if (!grepl("\\.pdf$", output_pdf_name, ignore.case = TRUE)) {
    output_pdf_name <- paste0(output_pdf_name, ".pdf")
  }
  out_file <- file.path(PATHS$export_dir, output_pdf_name)

  out_height <- SCALE$height
  if (identical(settings$carbon_stream, "stock_year_slope_facets")) {
    nf <- max(1L, length(settings$carbon_slope_facet_values %||% c("0.75", "1", "1.2")))
    if (nf > 1L) out_height <- out_height + (nf - 1L) * 2.2
  }
  if (isTRUE(settings$include_legend)) {
    out_height <- out_height + (SCALE$legend_bivariate_extra_height %||% 1.85)
  }

  ggsave(
    filename = out_file,
    plot = figure_obj,
    device = "pdf",
    width = SCALE$width,
    height = out_height,
    units = "in"
  )
  message("Saved [", run_label, "]: ", out_file)
}

if (isTRUE(RUN_ALL)) {
  message("RUN_ALL=TRUE: generating all RUN_LIBRARY presets.")
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

# -------------------------------------------------------------------
# LEGACY EXAMPLES (OLDER MANUAL RUN_SETTINGS BLOCKS)
# Primary manuscript presets now live in RUN_LIBRARY above.
# -------------------------------------------------------------------
# Example 1: Full multipanel (all outcomes), baseline cashflow
# RUN_SETTINGS <- list(
#   figure_id = "fig2_d",
#   outcomes_to_plot = OUTCOME_OPTIONS,
#   include_legend = TRUE,
#   bird_group = "loser",
#   beetle_group = "loser",
#   economic_discount_rate = "4%",
#   economic_cost_type = "HarvestProfits",
#   cashflow_variant = "baseline",
#   carbon_stream = "scc",
#   carbon_discount_rate = "4%",
#   carbon_slope_variant = NULL
# )
#
# Example 12: Sensitivity-range error bars across slope assumptions for stock-years
# RUN_SETTINGS <- list(
#   figure_id = "fig2_d",
#   outcomes_to_plot = c("carbon"),
#   include_legend = FALSE,
#   errorbar_mode = "sensitivity_range",
#   uncertainty_level = "80",
#   sensitivity_field = "carbon_slope_variant",
#   sensitivity_values = list(carbon_slope_variant = c("0.8", "1.0", "1.2")),
#   sensitivity_center = "1.0",
#   compare_mode = FALSE,
#   compare_rows_by = c("carbon_discount_rate"),
#   compare_values = list(carbon_discount_rate = c("2%", "4%", "6%")),
#   bird_group = "loser",
#   beetle_group = "loser",
#   economic_discount_rate = "4%",
#   economic_cost_type = "HarvestProfits",
#   cashflow_variant = "baseline",
#   carbon_stream = "stock_year",
#   carbon_discount_rate = NULL,
#   carbon_slope_variant = NULL
# )
#
# Example 10: Add 80% model CI error bars for SCC (carbon only)
# RUN_SETTINGS <- list(
#   figure_id = "fig2_d",
#   outcomes_to_plot = c("carbon"),
#   include_legend = TRUE,
#   errorbar_mode = "model_ci",
#   uncertainty_level = "80",
#   sensitivity_field = NULL,
#   sensitivity_center = NULL,
#   compare_mode = FALSE,
#   compare_rows_by = c("carbon_discount_rate"),
#   compare_values = list(carbon_discount_rate = c("2%", "4%", "6%")),
#   bird_group = "loser",
#   beetle_group = "loser",
#   economic_discount_rate = "4%",
#   economic_cost_type = "HarvestProfits",
#   cashflow_variant = "baseline",
#   carbon_stream = "scc",
#   carbon_discount_rate = "4%",
#   carbon_slope_variant = NULL
# )
#
# Example 11: Sensitivity-range error bars across cashflow assumptions
# (center = baseline, bars = min/max across baseline/plus25/minus25)
# RUN_SETTINGS <- list(
#   figure_id = "fig2_d",
#   outcomes_to_plot = c("profits"),
#   include_legend = FALSE,
#   errorbar_mode = "sensitivity_range",
#   uncertainty_level = "80",
#   sensitivity_field = "cashflow_variant",
#   sensitivity_values = list(cashflow_variant = c("baseline", "plus25", "minus25")),
#   sensitivity_center = "baseline",
#   compare_mode = FALSE,
#   compare_rows_by = c("carbon_discount_rate"),
#   compare_values = list(carbon_discount_rate = c("2%", "4%", "6%")),
#   bird_group = "loser",
#   beetle_group = "loser",
#   economic_discount_rate = "4%",
#   economic_cost_type = "HarvestProfits",
#   cashflow_variant = NULL,
#   carbon_stream = "scc",
#   carbon_discount_rate = "4%",
#   carbon_slope_variant = NULL
# )
#
# Example 2: Carbon + economic only, plus25 cashflow, NoDef scenarios
# RUN_SETTINGS <- list(
#   figure_id = "figs3_nd",
#   outcomes_to_plot = c("carbon", "profits", "protection"),
#   include_legend = FALSE,
#   bird_group = "loser",
#   beetle_group = "loser",
#   economic_discount_rate = "6%",
#   economic_cost_type = "HarvestProfits",
#   cashflow_variant = "plus25",
#   carbon_stream = "scc",
#   carbon_discount_rate = "6%",
#   carbon_slope_variant = NULL
# )
#
# Example 3: Carbon-only stock-years panel, minus25 cashflow context
# RUN_SETTINGS <- list(
#   figure_id = "figs6_d_dl",
#   outcomes_to_plot = c("carbon"),
#   include_legend = FALSE,
#   bird_group = "loser",
#   beetle_group = "loser",
#   economic_discount_rate = "4%",
#   economic_cost_type = "HarvestProfits",
#   cashflow_variant = "minus25",
#   carbon_stream = "stock_year",
#   carbon_discount_rate = NULL,
#   carbon_slope_variant = NULL
# )
#
# Example 4: SCC sensitivity with only carbon panel (run three times)
# RUN_SETTINGS <- list(
#   figure_id = "fig2_d",
#   outcomes_to_plot = c("carbon"),
#   include_legend = FALSE,
#   bird_group = "loser",
#   beetle_group = "loser",
#   economic_discount_rate = "4%",
#   economic_cost_type = "HarvestProfits",
#   cashflow_variant = "baseline",
#   carbon_stream = "scc",
#   carbon_discount_rate = "2%",  # then "4%", then "6%"
#   carbon_slope_variant = NULL
# )
#
# Example 5: AllPrimary + NoDef only, carbon-only, compare slope variants
# Step A: add a preset once:
#   FIGURE_PRESETS$all_primary_nd <- list(
#     scenario_filter = c("AllPrimaryNoDef"),
#     output_stub = "AllPrimary_NoDef"
#   )
# Step B: run this block, changing carbon_slope_variant each run
# RUN_SETTINGS <- list(
#   figure_id = "all_primary_nd",
#   outcomes_to_plot = c("carbon"),
#   include_legend = FALSE,
#   bird_group = "loser",
#   beetle_group = "loser",
#   economic_discount_rate = "4%",
#   economic_cost_type = "HarvestProfits",
#   cashflow_variant = "baseline",
#   carbon_stream = "scc",
#   carbon_discount_rate = "4%",
#   carbon_slope_variant = "0.8"   # then "1.0", then "1.2"
# )
#
# Example 6: SCC across starting landscapes, compare discount rates
# RUN_SETTINGS <- list(
#   figure_id = "fig2_d",          # AllPrimary + Mostly1L + Mostly2L
#   outcomes_to_plot = c("carbon"),
#   include_legend = FALSE,
#   bird_group = "loser",
#   beetle_group = "loser",
#   economic_discount_rate = "4%",
#   economic_cost_type = "HarvestProfits",
#   cashflow_variant = "baseline",
#   carbon_stream = "scc",
#   carbon_discount_rate = "2%",   # then "4%", then "6%"
#   carbon_slope_variant = NULL
# )
#
# Example 7: Single figure, carbon-only, three discount-rate rows (2/4/6)
#  RUN_SETTINGS <- list(
#   figure_id = "fig2_d",
#   outcomes_to_plot = c("carbon"),
#   include_legend = TRUE,
#   compare_mode = TRUE,
#   compare_rows_by = c("carbon_discount_rate"),
#   compare_values = list(carbon_discount_rate = c("2%", "4%", "6%")),
#   bird_group = "loser",
#   beetle_group = "loser",
#   economic_discount_rate = "4%",
#   economic_cost_type = "HarvestProfits",
#   cashflow_variant = "baseline",
#   carbon_stream = "scc",
#   carbon_discount_rate = "4%",
#   carbon_slope_variant = NULL
# )
#
# Example 8: Single figure, carbon-only, three slope-variant rows
# RUN_SETTINGS <- list(
#   figure_id = "fig2_d",
#   outcomes_to_plot = c("carbon"),
#   include_legend = FALSE,
#   compare_mode = TRUE,
#   compare_rows_by = c("carbon_slope_variant"),
#   compare_values = list(carbon_slope_variant = c("0.8", "1", "1.2")),
#   bird_group = "loser",
#   beetle_group = "loser",
#   economic_discount_rate = "4%",
#   economic_cost_type = "HarvestProfits",
#   cashflow_variant = "baseline",
#   carbon_stream = "stock_year",
#   carbon_discount_rate = "4%",
#   carbon_slope_variant = NULL
# )
#
# Example 9: Single figure comparing BOTH slope and cashflow assumptions
# (rows = all combinations of slope x cashflow)
# RUN_SETTINGS <- list(
#   figure_id = "fig2_d",
#   outcomes_to_plot = c("carbon", "profits"),
#   include_legend = FALSE,
#   compare_mode = TRUE,
#   compare_rows_by = c("carbon_slope_variant", "cashflow_variant"),
#   compare_values = list(
#     carbon_slope_variant = c("0.8", "1.2"),
#     cashflow_variant = c("baseline", "plus25", "minus25")
#   ),
#   bird_group = "loser",
#   beetle_group = "loser",
#   economic_discount_rate = "4%",
#   economic_cost_type = "HarvestProfits",
#   cashflow_variant = "baseline",
#   carbon_stream = "scc",
#   carbon_discount_rate = "4%",
#   carbon_slope_variant = NULL
# )
