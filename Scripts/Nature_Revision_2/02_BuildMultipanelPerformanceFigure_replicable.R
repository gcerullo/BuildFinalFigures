## Reproducible, single-figure workflow for Figure 2-style panels
## -----------------------------------------------------------------
## QUICK START
## 1) Edit only RUN_SETTINGS (and optionally FIGURE_PRESETS)
## 2) Run:
##      Rscript "Scripts/Nature_Revision_2/02_BuildMultipanelPerformanceFigure_replicable.R"
## 3) Output PDF is written to:
##      Figures/GeomPointFigs/manuscript_figures
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

RUN_SETTINGS <- list(
  figure_id = "fig2_d",               # choose from names(FIGURE_PRESETS)
  outcomes_to_plot = OUTCOME_OPTIONS, # any subset: c("carbon"), c("carbon","profits"), etc.
  include_legend = TRUE,              # FALSE for compact exports (useful for carbon-only)
  compare_mode = FALSE,               # TRUE = one figure with multiple sensitivity rows
  compare_rows_by = c("carbon_discount_rate"), # parameters to vary across rows
  compare_values = list(              # used only when compare_mode = TRUE
    carbon_discount_rate = c("2%", "4%", "6%"),
    carbon_slope_variant = c("0.8", "1.0", "1.2"),
    cashflow_variant = c("baseline", "plus25", "minus25"),
    economic_discount_rate = c("2%", "4%", "6%")
  ),
  bird_group = "loser",
  beetle_group = "loser",
  economic_discount_rate = "4%",
  economic_cost_type = "HarvestProfits",
  cashflow_variant = "baseline",      # "baseline", "plus25", "minus25", or NULL (= all)
  carbon_stream = "scc",              # "scc" or "stock_year"
  carbon_discount_rate = "4%",        # SCC discount rate; NULL = all rates
  carbon_slope_variant = NULL         # e.g. "1.2"; NULL = all slope variants
)

as_null_or_value <- function(x) {
  if (identical(x, "") || toupper(x) == "NULL") return(NULL)
  x
}

parse_csv_env <- function(x) {
  x <- trimws(x)
  if (identical(x, "")) return(NULL)
  out <- unlist(strsplit(x, ",", fixed = TRUE))
  trimws(out[nchar(trimws(out)) > 0])
}

# Optional runtime overrides so you can generate multiple PDFs
# without editing the script each time.
# Example (PowerShell):
#   $env:FIGURE_ID='figs3_nd'
#   $env:OUTCOMES='carbon,profits'
#   $env:CARBON_STREAM='scc'
#   $env:CARBON_DR='6%'
#   $env:CASHFLOW_VARIANT='plus25'
#   Rscript "Scripts/Nature_Revision_2/02_BuildMultipanelPerformanceFigure_replicable.R"
RUN_SETTINGS$figure_id <- as_null_or_value(Sys.getenv("FIGURE_ID", RUN_SETTINGS$figure_id))
RUN_SETTINGS$outcomes_to_plot <- {
  env_val <- parse_csv_env(Sys.getenv("OUTCOMES", ""))
  if (is.null(env_val)) RUN_SETTINGS$outcomes_to_plot else env_val
}
RUN_SETTINGS$include_legend <- tolower(Sys.getenv("INCLUDE_LEGEND", ifelse(RUN_SETTINGS$include_legend, "true", "false"))) %in% c("true", "1", "yes", "y")
RUN_SETTINGS$compare_mode <- tolower(Sys.getenv("COMPARE_MODE", ifelse(RUN_SETTINGS$compare_mode, "true", "false"))) %in% c("true", "1", "yes", "y")
RUN_SETTINGS$compare_rows_by <- {
  env_val <- parse_csv_env(Sys.getenv("COMPARE_ROWS_BY", ""))
  if (is.null(env_val)) RUN_SETTINGS$compare_rows_by else env_val
}
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
  parts <- unlist(lapply(names(row), function(field) paste0(field, ": ", row[[field]][[1]])))
  paste(parts, collapse = " | ")
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
  selected_outcomes <- unique(settings$outcomes_to_plot)
  panels <- list()

  if ("birds" %in% selected_outcomes) {
    B <- birds %>%
      filter(scenarioName %in% scenario_filter, bird_grp == settings$bird_group)
    if (nrow(B) == 0) stop("No data for birds panel with current RUN_SETTINGS.")
    panels <- c(panels, list(
      B %>% master_plot_fun(medianRelativeOccupancy, medianRelativeOccupancy, "Median Relative\nOccupancy", scenario_filter)
    ))
  }

  if ("dung_beetles" %in% selected_outcomes) {
    DB <- dung_beetles %>%
      filter(scenarioName %in% scenario_filter, spp_category == settings$beetle_group)
    if (nrow(DB) == 0) stop("No data for dung beetles panel with current RUN_SETTINGS.")
    panels <- c(panels, list(
      DB %>% master_plot_fun(medianRelativeOccupancy, medianRelativeOccupancy, "Median Relative\nAbundance", scenario_filter)
    ))
  }

  if ("megatrees" %in% selected_outcomes) {
    M <- megatrees %>%
      filter(scenarioName %in% scenario_filter)
    if (nrow(M) == 0) stop("No data for megatrees panel with current RUN_SETTINGS.")
    panels <- c(panels, list(
      M %>% master_plot_fun(landscape_prop, landscape_prop, "Megatree\nyears", scenario_filter)
    ))
  }

  if ("carbon" %in% selected_outcomes) {
    C <- carbon %>%
      filter(scenarioName %in% scenario_filter) %>%
      filter_carbon_variant(discount_rate = settings$carbon_discount_rate, slope_variant = settings$carbon_slope_variant)
    if (nrow(C) == 0) stop("No data for carbon panel with current RUN_SETTINGS.")
    panels <- c(panels, list(
      C %>% master_plot_fun(.data[[carbon_spec$metric]] / carbon_spec$divisor, .data[[carbon_spec$metric]] / carbon_spec$divisor, carbon_spec$ylab, scenario_filter)
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
    panels <- c(panels, list(
      P %>% master_plot_fun(NPV / SCALE$hundred_million, NPV / SCALE$hundred_million, "Harvest NPV\n(USD 100M)", scenario_filter)
    ))
  }

  if ("protection" %in% selected_outcomes) {
    PC <- protection %>%
      filter(scenarioName %in% scenario_filter, discount_rate == settings$economic_discount_rate) %>%
      filter_cashflow_variant(cashflow_variant = settings$cashflow_variant)
    if (nrow(PC) == 0) stop("No data for protection panel with current RUN_SETTINGS.")
    panels <- c(panels, list(
      PC %>% master_plot_fun(NPV / SCALE$hundred_million, NPV / SCALE$hundred_million, "Protection NPV\n(USD 100M)", scenario_filter)
    ))
  }

  if (length(panels) == 0) stop("RUN_SETTINGS$outcomes_to_plot did not select any panels.")
  main <- plot_grid(plotlist = panels, ncol = 1)
  if (isTRUE(settings$include_legend)) {
    return(plot_grid(main, all_legend, nrow = 2, rel_heights = c(1, 0.2)))
  }
  main
}

if (!RUN_SETTINGS$figure_id %in% names(FIGURE_PRESETS)) {
  stop("RUN_SETTINGS$figure_id must be one of: ", paste(names(FIGURE_PRESETS), collapse = ", "))
}
unknown_outcomes <- setdiff(RUN_SETTINGS$outcomes_to_plot, OUTCOME_OPTIONS)
if (length(unknown_outcomes) > 0) {
  stop("Unknown outcomes in RUN_SETTINGS$outcomes_to_plot: ", paste(unknown_outcomes, collapse = ", "))
}
valid_compare_fields <- c("carbon_discount_rate", "carbon_slope_variant", "cashflow_variant", "economic_discount_rate", "economic_cost_type", "carbon_stream")
if (isTRUE(RUN_SETTINGS$compare_mode)) {
  bad_fields <- setdiff(RUN_SETTINGS$compare_rows_by, valid_compare_fields)
  if (length(bad_fields) > 0) {
    stop("Unsupported compare_rows_by fields: ", paste(bad_fields, collapse = ", "))
  }
}
valid_cashflow_variants <- c("baseline", "plus25", "minus25")
if (!is.null(RUN_SETTINGS$cashflow_variant) && !RUN_SETTINGS$cashflow_variant %in% valid_cashflow_variants) {
  stop("RUN_SETTINGS$cashflow_variant must be one of: ", paste(valid_cashflow_variants, collapse = ", "), " (or NULL)")
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

comparison_grid <- build_comparison_grid(RUN_SETTINGS)
if (isTRUE(RUN_SETTINGS$compare_mode)) {
  row_plots <- vector("list", nrow(comparison_grid))
  for (i in seq_len(nrow(comparison_grid))) {
    row_settings <- apply_row_settings(RUN_SETTINGS, comparison_grid, i)
    row_settings$include_legend <- FALSE
    row_base <- build_main_figure(
      birds = birds,
      dung_beetles = dungBeetles,
      megatrees = megatrees,
      carbon = carbon,
      profits = profits,
      protection = protection,
      all_legend = all_legend,
      settings = row_settings,
      preset = selected_preset
    )
    row_label <- format_row_label(comparison_grid, i)
    row_plots[[i]] <- plot_grid(
      ggdraw() + draw_label(row_label, x = 0, hjust = 0, fontface = "bold", size = 10),
      row_base,
      ncol = 1,
      rel_heights = c(0.06, 1)
    )
  }
  compare_main <- plot_grid(plotlist = row_plots, ncol = 1)
  if (isTRUE(RUN_SETTINGS$include_legend)) {
    figure_obj <- plot_grid(compare_main, all_legend, nrow = 2, rel_heights = c(1, 0.2))
  } else {
    figure_obj <- compare_main
  }
} else {
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
}

short_outcome_codes <- c(
  birds = "birds",
  dung_beetles = "db",
  megatrees = "mt",
  carbon = "carb",
  profits = "prof",
  protection = "prot"
)
outcome_tag <- paste(short_outcome_codes[RUN_SETTINGS$outcomes_to_plot], collapse = "-")
compare_field_codes <- c(
  carbon_discount_rate = "cdr",
  carbon_slope_variant = "csl",
  cashflow_variant = "cfv",
  economic_discount_rate = "edr",
  economic_cost_type = "ect",
  carbon_stream = "cst"
)
compare_tag <- if (isTRUE(RUN_SETTINGS$compare_mode)) {
  paste(compare_field_codes[RUN_SETTINGS$compare_rows_by], collapse = "-")
} else {
  "none"
}
file_suffix <- paste0(
  selected_preset$output_stub,
  "__o-", outcome_tag,
  "__cmp-", compare_tag,
  "__c-", RUN_SETTINGS$carbon_stream,
  "__dr-", ifelse(is.null(RUN_SETTINGS$carbon_discount_rate), "all", RUN_SETTINGS$carbon_discount_rate),
  "__sl-", ifelse(is.null(RUN_SETTINGS$carbon_slope_variant), "all", RUN_SETTINGS$carbon_slope_variant),
  "__e-", RUN_SETTINGS$economic_discount_rate,
  "__cf-", ifelse(is.null(RUN_SETTINGS$cashflow_variant), "all", RUN_SETTINGS$cashflow_variant)
)
file_suffix <- gsub("%", "pct", file_suffix)
out_file <- file.path(PATHS$export_dir, paste0(file_suffix, ".pdf"))

ggsave(figure_obj, filename = out_file, width = SCALE$width, height = SCALE$height, units = "in")
message("Saved: ", out_file)

# -------------------------------------------------------------------
# EXAMPLES: copy ONE block into RUN_SETTINGS and run script
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
RUN_SETTINGS <- list(
  figure_id = "fig2_d",
  outcomes_to_plot = c("carbon"),
  include_legend = FALSE,
  compare_mode = TRUE,
  compare_rows_by = c("carbon_slope_variant"),
  compare_values = list(carbon_slope_variant = c("0.8", "1", "1.2")),
  bird_group = "loser",
  beetle_group = "loser",
  economic_discount_rate = "4%",
  economic_cost_type = "HarvestProfits",
  cashflow_variant = "baseline",
  carbon_stream = "stock_year",
  carbon_discount_rate = "4%",
  carbon_slope_variant = NULL
)
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
