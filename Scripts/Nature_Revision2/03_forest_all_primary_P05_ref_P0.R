# ----------------------------------------------------------------------------
# Nature-style forest plot: all-primary CY_D outcomes at P = 0.5 vs reference
# at P = 0 (no production), same planning map (index) for all series.
#
# Run from BuildFinalFigures project root:
#   Rscript Scripts/Nature_Revision2/03_forest_all_primary_P05_ref_P0.R
#
# Output: Figures/NR2/GeomPoint_Modified/forest_all_primary_P05_refP0.pdf
# ----------------------------------------------------------------------------

rm(list = ls())
options(scipen = 999)

suppressPackageStartupMessages({
  library(tidyverse)
  library(data.table)
})

## Project root (parent of Scripts/)
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[startsWith(args, "--file=")])
script_dir <- if (nzchar(file_arg)) {
  dirname(normalizePath(file_arg))
} else {
  tryCatch(dirname(normalizePath(sys.frames()[[1]]$ofile)), error = function(e) getwd())
}
if (is.na(script_dir) || !nzchar(script_dir)) script_dir <- getwd()
root <- normalizePath(file.path(script_dir, "..", ".."))
if (!file.exists(file.path(root, "Inputs", "FixedScenarioParams.R"))) {
  root <- normalizePath(getwd())
}
setwd(root)

source(file.path(root, "Inputs", "FixedScenarioParams.R"))

PATHS <- list(
  birds = "Data/NR2/OG_baseline_birds.rds",
  birds_iucn = "Data/NR2/OG_baseline_birdsIUCN.rds",
  dung_beetles = "Data/NR2/MasterDBPerformance_fastPilot.rds",
  carbon = "Data/NR2/carbon_outcomes__all_trajectories.rds",
  megatrees = "Data/full_nature_scenario_megatree_performance_all_thresholds.rds",
  financial = "Data/NR2/MasterFinancialPerformance__Sensitivity25pctMoneyParams_LONG.rds",
  all_scenarios = "Inputs/MasterAllScenarios.rds",
  export_dir = "Figures/NR2/GeomPoint_Modified"
)
dir.create(PATHS$export_dir, recursive = TRUE, showWarnings = FALSE)

SCALE <- list(bil = 1e9, hundred_million = 1e8)

resolve_path <- function(rel) {
  p <- file.path(root, rel)
  if (file.exists(p)) return(p)
  stop("Missing file: ", p)
}

parse_slope_from_name <- function(x) {
  value <- sub("^2Ltraj_", "", x)
  value <- gsub("_", ".", value)
  suppressWarnings(num <- as.numeric(value))
  if (!is.na(num)) return(as.character(num))
  value
}

load_carbon_data <- function(path) {
  obj <- readRDS(path)
  if (is.data.frame(obj) || is.data.table(obj)) return(as.data.table(obj))
  if (is.list(obj) && !is.null(names(obj)) && all(vapply(obj, is.list, logical(1)))) {
    slope_tables <- purrr::imap(obj, function(slope_block, slope_name) {
      block_df <- rbindlist(slope_block, fill = TRUE)
      block_df[, twice_logged_slope_trajectory := parse_slope_from_name(slope_name)]
      block_df
    })
    return(rbindlist(slope_tables, fill = TRUE))
  }
  stop("Unsupported carbon RDS structure.")
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
      scenarioName = forcats::fct_relevel(
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
    select(index, production_target, scenarioName, scenarioStart, propOG, propPlant, prop1L, prop2L) %>%
    distinct()
}

scenarios <- readRDS(resolve_path(PATHS$all_scenarios))
scenario_composition <- rbindlist(scenarios, use.names = TRUE)
rm(scenarios)

habInStart <- all_start_landscape %>%
  dplyr::select(scenarioStart) %>%
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

prep_join <- function(df) {
  as.data.table(propOGcomp)[as.data.table(df), on = .(index, production_target)] %>%
    as_tibble() %>%
    rename_scenario_name()
}

birds <- bind_rows(
  readRDS(resolve_path(PATHS$birds)) %>% distinct() %>% rename(bird_grp = spp_category),
  readRDS(resolve_path(PATHS$birds_iucn)) %>% distinct() %>% rename(bird_grp = threatened)
) %>%
  mutate(index = as.character(index), production_target = as.numeric(production_target)) %>%
  prep_join()

dungBeetles <- readRDS(resolve_path(PATHS$dung_beetles)) %>%
  mutate(index = as.character(index), production_target = as.numeric(production_target)) %>%
  prep_join()

carbon <- load_carbon_data(resolve_path(PATHS$carbon)) %>%
  mutate(
    index = as.character(index),
    production_target = as.numeric(production_target),
    twice_logged_slope_trajectory = as.character(twice_logged_slope_trajectory)
  ) %>%
  prep_join()

megatrees <- readRDS(resolve_path(PATHS$megatrees)) %>%
  mutate(
    index = as.character(index),
    production_target = as.numeric(production_target),
    height_filt = as.character(height_filt)
  ) %>%
  as.data.table() %>%
  {
    as.data.table(propOGcomp)[., on = .(index, production_target)]
  } %>%
  as_tibble() %>%
  dplyr::select(-scenarioName, -scenarioStart) %>%
  rename(scenarioName = i.scenarioName, scenarioStart = i.scenarioStart) %>%
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
  prep_join()

## ---- Parameters (match main geom defaults) ----
P_FOCUS <- 0.5
P_REF <- 0
P_TOL <- 1e-7
scenario_col <- "AllPrimary"
start_land <- "all_primary"
bird_level <- "loser"
beetle_level <- "loser"
carbon_discount <- "4%"
carbon_slope <- "1"
meg_height <- "50"
econ_dr <- "4%"
cashflow <- "baseline"
## Intervals from RDS only (no derived SEs)
meg_lwr_col <- "landscape_prop_lwr95"
meg_upr_col <- "landscape_prop_upr95"
carb_lwr_col <- "TOTcarbon_ACD_lwr80"
carb_upr_col <- "TOTcarbon_ACD_upr80"

near_p <- function(x, p) abs(as.numeric(x) - p) < P_TOL

base_f <- function(df) {
  df %>%
    filter(
      scenarioStart == start_land,
      as.character(scenarioName) == scenario_col,
      near_p(production_target, P_FOCUS) | near_p(production_target, P_REF)
    )
}

idx_birds <- birds %>% base_f() %>%
  filter(bird_grp == bird_level) %>%
  distinct(index, production_target)

idx_beetles <- dungBeetles %>% base_f() %>%
  filter(spp_category == beetle_level) %>%
  distinct(index, production_target)

idx_meg <- megatrees %>% base_f() %>%
  filter(as.character(height_filt) == meg_height) %>%
  distinct(index, production_target)

idx_carb <- carbon %>%
  base_f() %>%
  filter(
    discount_rate == carbon_discount,
    abs(as.numeric(twice_logged_slope_trajectory) - as.numeric(carbon_slope)) < 1e-6
  ) %>%
  distinct(index, production_target)

idx_harv <- profits %>%
  base_f() %>%
  filter(costType == "HarvestProfits", discount_rate == econ_dr, cashflow_variant == cashflow) %>%
  distinct(index, production_target)

idx_prot <- profits %>%
  base_f() %>%
  filter(costType == "ProtectionCosts", discount_rate == econ_dr, cashflow_variant == cashflow) %>%
  distinct(index, production_target)

count_p <- function(idxs) {
  idxs %>%
    count(index) %>%
    filter(n == 2L)
}

common <- count_p(idx_birds) %>%
  inner_join(count_p(idx_beetles), by = "index") %>%
  inner_join(count_p(idx_meg), by = "index") %>%
  inner_join(count_p(idx_carb), by = "index") %>%
  inner_join(count_p(idx_harv), by = "index") %>%
  inner_join(count_p(idx_prot), by = "index")

if (nrow(common) == 0L) {
  stop("No index has all outcomes at both P = 0 and P = ", P_FOCUS, " for AllPrimary / all_primary.")
}

## Choose index closest to median plantation share at P_FOCUS among valid indices
plant_at_focus <- birds %>%
  filter(
    scenarioStart == start_land,
    as.character(scenarioName) == scenario_col,
    near_p(production_target, P_FOCUS),
    bird_grp == bird_level,
    index %in% common$index
  ) %>%
  distinct(index, propPlant)

med_pp <- median(plant_at_focus$propPlant, na.rm = TRUE)
chosen_index <- plant_at_focus %>%
  mutate(dist = abs(propPlant - med_pp)) %>%
  slice_min(order_by = dist, n = 1, with_ties = FALSE) %>%
  pull(index)

## ---- Build one row per outcome ----
pick_row <- function(df, ...) {
  df %>% filter(index == chosen_index, ...) %>% distinct(index, production_target, ...)
}

## Birds
b05 <- birds %>%
  pick_row(bird_grp == bird_level, near_p(production_target, P_FOCUS)) %>%
  slice(1)
b0 <- birds %>%
  pick_row(bird_grp == bird_level, near_p(production_target, P_REF)) %>%
  slice(1)

## Beetles
db05 <- dungBeetles %>%
  pick_row(spp_category == beetle_level, near_p(production_target, P_FOCUS)) %>%
  slice(1)
db0 <- dungBeetles %>%
  pick_row(spp_category == beetle_level, near_p(production_target, P_REF)) %>%
  slice(1)

## Megatrees
m05 <- megatrees %>%
  pick_row(as.character(height_filt) == meg_height, near_p(production_target, P_FOCUS)) %>%
  slice(1)
m0 <- megatrees %>%
  pick_row(as.character(height_filt) == meg_height, near_p(production_target, P_REF)) %>%
  slice(1)

## Carbon SCC
c05 <- carbon %>%
  pick_row(
    discount_rate == carbon_discount,
    abs(as.numeric(twice_logged_slope_trajectory) - as.numeric(carbon_slope)) < 1e-6,
    near_p(production_target, P_FOCUS)
  ) %>%
  slice(1)
c0 <- carbon %>%
  pick_row(
    discount_rate == carbon_discount,
    abs(as.numeric(twice_logged_slope_trajectory) - as.numeric(carbon_slope)) < 1e-6,
    near_p(production_target, P_REF)
  ) %>%
  slice(1)

## Profits
h05 <- profits %>%
  pick_row(
    costType == "HarvestProfits",
    discount_rate == econ_dr,
    cashflow_variant == cashflow,
    near_p(production_target, P_FOCUS)
  ) %>%
  slice(1)
h0 <- profits %>%
  pick_row(
    costType == "HarvestProfits",
    discount_rate == econ_dr,
    cashflow_variant == cashflow,
    near_p(production_target, P_REF)
  ) %>%
  slice(1)

p05 <- profits %>%
  pick_row(
    costType == "ProtectionCosts",
    discount_rate == econ_dr,
    cashflow_variant == cashflow,
    near_p(production_target, P_FOCUS)
  ) %>%
  slice(1)
p0 <- profits %>%
  pick_row(
    costType == "ProtectionCosts",
    discount_rate == econ_dr,
    cashflow_variant == cashflow,
    near_p(production_target, P_REF)
  ) %>%
  slice(1)

plot_tbl <- tibble(
  outcome = c(
    "Loser birds\n(median relative occupancy)",
    "Loser dung beetles\n(median relative occupancy)",
    sprintf("Megatrees\n(landscape %% ; height %s m)", meg_height),
    "Social cost of carbon\n(USD billion)",
    "Harvest profits\n(USD 100 million)",
    "Protection costs\n(USD 100 million)"
  ),
  outcome_short = c("Birds", "Dung beetles", "Megatrees", "SCC", "Harvest", "Protection"),
  x_p05 = c(
    b05$medianRelativeOccupancy,
    db05$medianRelativeOccupancy,
    m05$landscape_prop,
    c05$TOTcarbon_ACD_mean / SCALE$bil,
    h05$NPV / SCALE$hundred_million,
    p05$NPV / SCALE$hundred_million
  ),
  x_p0 = c(
    b0$medianRelativeOccupancy,
    db0$medianRelativeOccupancy,
    m0$landscape_prop,
    c0$TOTcarbon_ACD_mean / SCALE$bil,
    h0$NPV / SCALE$hundred_million,
    p0$NPV / SCALE$hundred_million
  ),
  xmin = c(NA_real_, NA_real_, m05[[meg_lwr_col]], c05[[carb_lwr_col]] / SCALE$bil, NA_real_, NA_real_),
  xmax = c(NA_real_, NA_real_, m05[[meg_upr_col]], c05[[carb_upr_col]] / SCALE$bil, NA_real_, NA_real_)
) %>%
  mutate(
    outcome = fct_inorder(factor(outcome)),
    xmin = ifelse(is.finite(xmin), pmin(xmin, xmax, na.rm = TRUE), xmin),
    xmax = ifelse(is.finite(xmax), pmax(xmin, xmax, na.rm = TRUE), xmax)
  )

## ---- Nature-style theme ----
nature_theme <- function(base_size = 11) {
  theme_bw(base_size = base_size) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(colour = "grey40", linewidth = 0.35),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      strip.text.y.left = element_text(angle = 0, hjust = 1, face = "plain", colour = "grey10"),
      strip.background = element_rect(fill = "grey96", colour = "grey85"),
      plot.title = element_text(face = "bold", size = base_size + 1, hjust = 0),
      plot.subtitle = element_text(colour = "grey35", size = base_size - 1, hjust = 0),
      plot.caption = element_text(colour = "grey45", size = base_size - 2, hjust = 0),
      plot.margin = margin(12, 14, 10, 8)
    )
}

accent <- "#117A65"
ref_grey <- "grey45"

p <- ggplot(plot_tbl, aes(x = x_p05, y = 0)) +
  facet_wrap(~outcome, ncol = 1, scales = "free_x", strip.position = "left", dir = "v") +
  geom_vline(aes(xintercept = x_p0), colour = ref_grey, linetype = "dotted", linewidth = 0.45, alpha = 0.95) +
  geom_errorbarh(
    aes(xmin = xmin, xmax = xmax),
    height = 0.22,
    linewidth = 0.38,
    colour = accent,
    alpha = 0.85,
    na.rm = TRUE
  ) +
  geom_point(colour = accent, size = 3.2, stroke = 0) +
  labs(
    x = NULL,
    title = "All-primary plantation outcomes at production target P = 0.5",
    subtitle = sprintf(
      "Dotted vertical line: outcome value at P = 0 (no production), same map (index %s). Error bars: stored 95%% CI (megatrees) or 80%% CI (SCC) at P = 0.5 only.",
      chosen_index
    ),
    caption = sprintf(
      "Scenario: %s · Starting landscape: %s · Harvest & protection: %s discount, %s cashflow · Carbon: %s discount, twice-logged slope %s",
      scenario_col, start_land, econ_dr, cashflow, carbon_discount, carbon_slope
    )
  ) +
  nature_theme(11)

out_pdf <- file.path(root, PATHS$export_dir, "forest_all_primary_P05_refP0.pdf")
ggsave(
  filename = out_pdf,
  plot = p,
  device = "pdf",
  width = 7.2,
  height = 8.4,
  units = "in"
)

message("Saved: ", normalizePath(out_pdf, winslash = "/", mustWork = FALSE))
