# ----------------------------------------------------------------------------
# Forest-style figure: all-primary start, P = 0.5 vs no production (P = 0)
#
# Outcomes: birds, dung beetles, megatrees, delta cumulative C stock vs baseline
#           (window 60, twice-logged slope 1), same scales as modified geom.
#
# Reference: median at P = 0 where rows exist (birds / beetles / megatrees). Delta C stock
# RDS has no P = 0 rows — use median at the minimum P available for the same window/slope
# (lowest production target in the table) as the baseline-landscape reference.
# x = value(P=0.5) - that reference; dotted reference at zero (vline if delta on x, hline if on y).
#
# 80% intervals (horizontal): megatrees & delta C only. Birds & beetles: points only.
# Colour = biscale hexP; shape = filled circle / triangle (no plantation / plantation).
# Bivariate footer: scenario letters (A–E) in grid tiles, matching x-axis scenario order.
#
# Figure layout: 2x2 outcomes (default); optional 1x4 horizontal compact (COMPACT_FOUR_IN_ONE_ROW);
# optional vertical delta on y + 1x4 (VERTICAL_ONE_ROW_FIGURE); or 2x2 vertical (VERTICAL_DELTA_FIGURE).
# Then landscape text (L) | shape key + bivariate.
# Run from BuildFinalFigures project root:
#   Rscript Scripts/Nature_Revision2/03_forest_allprimary_p05_vs_p0.R
# ----------------------------------------------------------------------------

options(scipen = 999)

suppressPackageStartupMessages({
  library(tidyverse)
  library(data.table)
  library(cowplot)
  library(stringr)
  library(forcats)
  library(biscale)
  library(grid)
  library(scales)
  library(ggtext)
})

source("Inputs/FixedScenarioParams.R")

PATHS <- list(
  birds = "Data/NR2/OG_baseline_birds.rds",
  birds_iucn = "Data/NR2/OG_baseline_birdsIUCN.rds",
  dung_beetles = "Data/NR2/MasterDBPerformance_fastPilot.rds",
  carbon_stock_diff = "Data/NR2/stock_diff_vs_baseline__windowed__all_traj.rds",
  megatrees = "Data/full_nature_scenario_megatree_performance_all_thresholds.rds",
  all_scenarios = "Inputs/MasterAllScenarios.rds",
  export_dir = "Figures/NR2/forest_plots"
)

ICON_PATHS <- list(
  birds = "Figures/Thumbnails/RhinoH.jpg",
  dung_beetles = "Figures/Thumbnails/DB.svg",
  megatrees = "Figures/Thumbnails/Dipterocarp.svg"
)

SCALE <- list(
  bil = 1e9,
  hundred_million = 1e8
)
COL <- "BlueOr"

`%||%` <- function(x, y) if (is.null(x)) y else x

## ---- Match modified geom figure --------------------------------------------
P_FOCUS <- 0.5
P_REF <- 0
P_TOL <- 1e-8
SCENARIO_START <- "all_primary"
SCENARIO_NAME_FILTER <- "AllPrimary"
BIRD_GRP <- "loser"
BEETLE_GRP <- "loser"
CARBON_WINDOW <- 60L
CARBON_SLOPE <- "1"
MEGA_HEIGHT <- "50"
## TRUE: delta on y (hline baseline; pinned to panel top/bottom when all effects are <0 or >0),
##       scenarios on x; 2x2 grid; writes ..._vertical_delta.pdf
VERTICAL_DELTA_FIGURE <- FALSE
## TRUE: same vertical delta panels as above, but four outcomes in one row + short strip titles;
##       writes ..._vertical_onerow.pdf (takes precedence over VERTICAL_DELTA_FIGURE for grid only)
VERTICAL_ONE_ROW_FIGURE <- TRUE
## TRUE: horizontal delta, four outcomes in one row (ignored when any vertical forest layout is on)
COMPACT_FOUR_IN_ONE_ROW <- FALSE

vertical_forest_layout <- isTRUE(VERTICAL_DELTA_FIGURE) || isTRUE(VERTICAL_ONE_ROW_FIGURE)
if (isTRUE(vertical_forest_layout) && isTRUE(COMPACT_FOUR_IN_ONE_ROW)) {
  warning("Vertical forest layout is active: COMPACT_FOUR_IN_ONE_ROW ignored.")
  COMPACT_FOUR_IN_ONE_ROW <- FALSE
}
compact_layout <- isTRUE(COMPACT_FOUR_IN_ONE_ROW) && !isTRUE(vertical_forest_layout)
outcomes_one_row <- isTRUE(VERTICAL_ONE_ROW_FIGURE) || isTRUE(compact_layout)
use_short_outcome_labels <- isTRUE(VERTICAL_ONE_ROW_FIGURE) || isTRUE(compact_layout)

OUTPUT_NAME <- if (isTRUE(VERTICAL_ONE_ROW_FIGURE)) {
  "forest_allprimary_p05_vs_p0_four_outcomes_vertical_onerow.pdf"
} else if (isTRUE(VERTICAL_DELTA_FIGURE)) {
  "forest_allprimary_p05_vs_p0_four_outcomes_vertical_delta.pdf"
} else if (isTRUE(compact_layout)) {
  "forest_allprimary_p05_vs_p0_four_outcomes_onerow.pdf"
} else {
  "forest_allprimary_p05_vs_p0_four_outcomes.pdf"
}

## Matches save_plot dimensions below (npc shift for ~1 cm icon nudge on final PDF)
FIG_SAVE_WIDTH_IN <- if (isTRUE(outcomes_one_row)) 11.2 else 7.35
FIG_SAVE_HEIGHT_IN <- if (isTRUE(VERTICAL_ONE_ROW_FIGURE)) {
  7.15
} else if (isTRUE(compact_layout)) {
  6.85
} else {
  8.5
}

## One outcome panel's approx. size (in) for npc: icons sit in ggdraw per panel, npc = panel not page
ICON_GRID_COLS <- if (isTRUE(outcomes_one_row)) 4L else 2L
ICON_GRID_ROWS <- if (isTRUE(outcomes_one_row)) 1L else 2L
ICON_OUTCOME_HFRAC <- if (isTRUE(vertical_forest_layout)) {
  1 / (1 + 0.52)
} else if (isTRUE(compact_layout)) {
  1 / (1 + 0.042 + 0.58)
} else {
  1 / (1 + 0.048 + 0.52)
}
ICON_PANEL_W_IN <- FIG_SAVE_WIDTH_IN / ICON_GRID_COLS
ICON_PANEL_H_IN <- FIG_SAVE_HEIGHT_IN * ICON_OUTCOME_HFRAC / ICON_GRID_ROWS

## Bivariate tile stretch in x (≈1 = square cells in x vs y data units)
BIVARIATE_TILE_X_STRETCH <- 1.02

## Optional footnote under bivariate grid (left empty: composition uses full habitat names in labels).
COMPOSITION_ABBREV_CAPTION <- ""

resolve_path <- function(primary) {
  if (file.exists(primary)) return(primary)
  stop("Missing file: ", primary)
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
  if (is.data.frame(obj) || is.data.table(obj)) return(obj)
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
      propPlant = sum(num_parcels[habitat %in% c(
        "eucalyptus_current", "albizia_current", "albizia_future", "eucalyptus_future"
      )]) / 1000,
      prop1L = sum(num_parcels[habitat == "once-logged"]) / 1000,
      prop2L = sum(num_parcels[habitat == "twice-logged"]) / 1000,
      prop_restored = sum(num_parcels[grepl("restor", as.character(habitat), ignore.case = TRUE)]) / 1000
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
    mutate(
      prop_other = pmax(0, 1 - propOG - prop1L - prop2L - propPlant - prop_restored)
    ) %>%
    select(
      index, production_target, scenarioName, scenarioStart,
      propOG, propPlant, prop1L, prop2L, prop_restored, prop_other,
      remainingOG, remaining1L, remaining2L
    ) %>%
    distinct()
}

add_bivariate_colours <- function(df, cols_tbl) {
  df %>%
    select(-any_of(c("bi_class", "hex", "hexP", "hex1L", "hex2L"))) %>%
    bi_class(x = propOG, y = prop1L, dim = 4, style = "equal") %>%
    left_join(cols_tbl, by = "bi_class") %>%
    rename(hexP = hex) %>%
    select(-bi_class) %>%
    bi_class(x = remainingOG, y = remaining1L, dim = 4, style = "equal") %>%
    left_join(cols_tbl, by = "bi_class") %>%
    rename(hex1L = hex) %>%
    select(-bi_class) %>%
    bi_class(x = remainingOG, y = remaining2L, dim = 4, style = "equal") %>%
    left_join(cols_tbl, by = "bi_class") %>%
    rename(hex2L = hex) %>%
    select(-bi_class)
}

add_plantation_type <- function(df) {
  df %>% mutate(shape_class = ifelse(propPlant > 0, "Cross", "Point"))
}

filter_stock_diff_window <- function(df, window_end = CARBON_WINDOW, slope_variant = CARBON_SLOPE) {
  df %>%
    filter(
      as.numeric(.data$window_year_end) == as.numeric(window_end),
      abs(as.numeric(.data$twice_logged_slope_trajectory) - as.numeric(slope_variant)) < 1e-6
    )
}

## Icons (same assets as modified geom script; carbon = letter C in-panel style)
load_icon_grob <- function(icon_path, width_px = 220L, height_px = 220L) {
  if (is.null(icon_path) || !file.exists(resolve_path(icon_path))) return(NULL)
  path <- resolve_path(icon_path)
  ext <- tolower(tools::file_ext(path))
  raster <- NULL
  if (ext == "svg") {
    if (!requireNamespace("rsvg", quietly = TRUE)) return(NULL)
    tmp_png <- tempfile(fileext = ".png")
    rsvg::rsvg_png(path, file = tmp_png, width = width_px, height = height_px)
    raster <- png::readPNG(tmp_png)
  } else if (ext == "png") {
    raster <- png::readPNG(path)
  } else if (ext %in% c("jpg", "jpeg")) {
    if (!requireNamespace("jpeg", quietly = TRUE)) return(NULL)
    raster <- jpeg::readJPEG(path)
    if (tolower(basename(path)) == "rhinoh.jpg" && length(dim(raster)) == 3) {
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
    return(NULL)
  }
  grid::rasterGrob(raster, interpolate = TRUE)
}

icon_cell <- function(path) {
  g <- load_icon_grob(path)
  if (is.null(g)) {
    return(ggdraw() + theme_void())
  }
  ggdraw() + draw_grob(g, scale = 0.88)
}

icon_carbon_cell <- function() {
  ggdraw() +
    draw_label(
      "C",
      fontface = "bold",
      size = 22,
      colour = "grey15"
    )
}

## Inset icon (npc coords): top-left inside each outcome panel
carbon_icon_grob <- function() {
  grid::textGrob("C", gp = grid::gpar(fontface = "bold", fontsize = 17, col = "grey20"))
}

## Bird / megatree thumbnails: ~1.5× default npc size when VERTICAL_ONE_ROW_FIGURE only
ICON_WH_DEFAULT <- 0.115
ICON_WH_BIRD_TREE <- ICON_WH_DEFAULT * 1.5
ICON_WH_BIRD_MEGA <- if (isTRUE(VERTICAL_ONE_ROW_FIGURE)) ICON_WH_BIRD_TREE else ICON_WH_DEFAULT

## Inset icons: default npc anchor + ~4.5 cm right and ~1 cm down (panel npc; vjust=1 = grob top at anchor)
inset_icon_on_plot <- function(p, grob, x = 0.11, y = 0.86, w = ICON_WH_DEFAULT, h = ICON_WH_DEFAULT) {
  d <- ggdraw() + draw_plot(p, x = 0, y = 0, width = 1, height = 1)
  if (!is.null(grob)) {
    dx <- 4.5 / 2.54 / ICON_PANEL_W_IN
    dy <- 1 / 2.54 / ICON_PANEL_H_IN
    d <- d + draw_grob(
      grob,
      x = x + dx,
      y = y - dy,
      width = w,
      height = h,
      hjust = 0,
      vjust = 1
    )
  }
  d
}

scenarios <- readRDS(resolve_path(PATHS$all_scenarios))
scenario_composition <- rbindlist(scenarios, use.names = TRUE)
rm(scenarios)

## Total timber production (m3) at P = 0.5 for all-primary CY_D plan; caption uses Mm3 (= 1e6 m3)
PROD_SCENARIO_FILE <- "all_primary_CY_D.csv"
production_mm3_p05 <- {
  sc <- as.data.table(scenario_composition)
  py <- sc[
    scenarioStart == SCENARIO_START &
      scenarioName == PROD_SCENARIO_FILE &
      abs(as.numeric(production_target) - as.numeric(P_FOCUS)) < P_TOL
  ][, unique(production_yield)]
  if (length(py) == 0L || all(!is.finite(py))) {
    NA_real_
  } else {
    stats::median(py, na.rm = TRUE) / 1e6
  }
}
if (!is.finite(production_mm3_p05)) {
  warning(
    "production_yield not found at P = ", P_FOCUS, " for ", PROD_SCENARIO_FILE,
    " — caption volume left blank."
  )
}

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
  mutate(
    index = as.character(index),
    production_target = as.numeric(production_target)
  )

cols_tbl <- data.frame(bi_pal(COL, dim = 4, preview = FALSE))
colnames(cols_tbl) <- "hex"
cols_tbl <- cols_tbl %>% mutate(bi_class = rownames(.))

prep_base <- function(df) propOGcomp[as.data.table(df), on = .(index, production_target)]
prep_styled <- function(df) {
  prep_base(df) %>%
    as_tibble() %>%
    add_bivariate_colours(cols_tbl) %>%
    add_plantation_type() %>%
    rename_scenario_name()
}

birds <- bind_rows(
  readRDS(resolve_path(PATHS$birds)) %>% distinct() %>% rename(bird_grp = spp_category),
  readRDS(resolve_path(PATHS$birds_iucn)) %>% distinct() %>% rename(bird_grp = threatened)
) %>%
  mutate(index = as.character(index), production_target = as.numeric(production_target)) %>%
  as.data.table() %>%
  prep_styled()

dungBeetles <- readRDS(resolve_path(PATHS$dung_beetles)) %>%
  mutate(index = as.character(index), production_target = as.numeric(production_target)) %>%
  as.data.table() %>%
  prep_styled()

carbon_stock_diff <- load_carbon_data(PATHS$carbon_stock_diff) %>%
  mutate(
    index = as.character(index),
    production_target = as.numeric(production_target),
    window_year_end = as.numeric(window_year_end),
    twice_logged_slope_trajectory = as.character(twice_logged_slope_trajectory)
  ) %>%
  as.data.table() %>%
  prep_styled()

megatrees <- readRDS(resolve_path(PATHS$megatrees)) %>%
  mutate(
    index = as.character(index),
    production_target = as.numeric(production_target),
    height_filt = as.character(height_filt)
  ) %>%
  as.data.table()
megatrees <- prep_base(megatrees) %>%
  as_tibble() %>%
  select(-scenarioName, -scenarioStart) %>%
  rename(scenarioName = i.scenarioName, scenarioStart = i.scenarioStart) %>%
  add_bivariate_colours(cols_tbl) %>%
  add_plantation_type() %>%
  rename_scenario_name()

plot_colour_primary <- function(df) {
  df %>%
    mutate(
      plot_colour = case_when(
        scenarioStart %in% c("all_primary", "primary_deforested") ~ hexP,
        scenarioStart %in% c("mostly_1L", "mostly_1L_deforested") ~ hex1L,
        scenarioStart %in% c("mostly_2L", "mostly_2L_deforested") ~ hex2L,
        TRUE ~ hexP
      )
    )
}

near_p <- function(pt, p) abs(as.numeric(pt) - as.numeric(p)) < P_TOL

## Facet titles (one strip per outcome panel); short forms for one-row layouts
LAB_BIRDS_FULL <- "Birds (loser)\nDelta median relative occupancy"
LAB_BEETLES_FULL <- "Dung beetles (loser)\nDelta median relative abundance"
LAB_MEGA_FULL <- "Megatrees\nDelta megatree % of landscape (50 m canopy)"
LAB_CARBON_FULL <- "Delta Cumulative C stock vs baseline\n(Billion MgC)"
LAB_BIRDS_SHORT <- "Old-growth birds\nDelta med. occupancy"
LAB_BEETLES_SHORT <- "Old-growth beetles\nDelta med. abundance"
LAB_MEGA_SHORT <- "Megatrees\nDelta % landscape (50 m)"
LAB_CARBON_SHORT <- "Delta cumul. C vs baseline\n(Billion MgC)"
if (isTRUE(use_short_outcome_labels)) {
  LAB_BIRDS <- LAB_BIRDS_SHORT
  LAB_BEETLES <- LAB_BEETLES_SHORT
  LAB_MEGA <- LAB_MEGA_SHORT
  LAB_CARBON <- LAB_CARBON_SHORT
} else {
  LAB_BIRDS <- LAB_BIRDS_FULL
  LAB_BEETLES <- LAB_BEETLES_FULL
  LAB_MEGA <- LAB_MEGA_FULL
  LAB_CARBON <- LAB_CARBON_FULL
}
OUTCOME_LEVELS <- c(LAB_BIRDS, LAB_BEETLES, LAB_MEGA, LAB_CARBON)

## Reference = median at P = 0; optional 80% band on delta from CI columns on P=0.5 rows
ensemble_delta <- function(d0, d05, val_col, ci_lwr = NULL, ci_upr = NULL, ref_fun = stats::median) {
  ref <- ref_fun(d0[[val_col]], na.rm = TRUE)
  if (!is.finite(ref)) {
    return(tibble::tibble())
  }
  out <- d05 %>%
    mutate(
      v0 = ref,
      v05 = .data[[val_col]],
      delta = v05 - v0
    )
  if (!is.null(ci_lwr) && ci_lwr %in% names(out) && !is.null(ci_upr) && ci_upr %in% names(out)) {
    out <- out %>%
      mutate(
        delta_l = .data[[ci_lwr]] - ref,
        delta_u = .data[[ci_upr]] - ref
      )
  } else {
    out <- out %>% mutate(delta_l = NA_real_, delta_u = NA_real_)
  }
  id_tail <- str_extract(as.character(out$index), "\\d+$")
  out %>%
    mutate(
      scenario_lbl = paste0("#", coalesce(id_tail, as.character(index)))
    ) %>%
    filter(is.finite(delta))
}

## ---- Build long plot data ---------------------------------------------------
rows <- list()

B0 <- birds %>%
  as_tibble() %>%
  filter(
    scenarioStart == SCENARIO_START,
    as.character(scenarioName) == SCENARIO_NAME_FILTER,
    bird_grp == BIRD_GRP,
    near_p(production_target, P_REF)
  ) %>%
  plot_colour_primary()
B05 <- birds %>%
  as_tibble() %>%
  filter(
    scenarioStart == SCENARIO_START,
    as.character(scenarioName) == SCENARIO_NAME_FILTER,
    bird_grp == BIRD_GRP,
    near_p(production_target, P_FOCUS)
  ) %>%
  plot_colour_primary()
rows$birds <- ensemble_delta(B0, B05, "medianRelativeOccupancy") %>%
  mutate(outcome_label = LAB_BIRDS)

D0 <- dungBeetles %>%
  as_tibble() %>%
  filter(
    scenarioStart == SCENARIO_START,
    as.character(scenarioName) == SCENARIO_NAME_FILTER,
    spp_category == BEETLE_GRP,
    near_p(production_target, P_REF)
  ) %>%
  plot_colour_primary()
D05 <- dungBeetles %>%
  as_tibble() %>%
  filter(
    scenarioStart == SCENARIO_START,
    as.character(scenarioName) == SCENARIO_NAME_FILTER,
    spp_category == BEETLE_GRP,
    near_p(production_target, P_FOCUS)
  ) %>%
  plot_colour_primary()
rows$dung_beetles <- ensemble_delta(D0, D05, "medianRelativeOccupancy") %>%
  mutate(outcome_label = LAB_BEETLES)

M0 <- megatrees %>%
  as_tibble() %>%
  filter(
    scenarioStart == SCENARIO_START,
    as.character(scenarioName) == SCENARIO_NAME_FILTER,
    height_filt == MEGA_HEIGHT,
    near_p(production_target, P_REF)
  ) %>%
  plot_colour_primary()
M05 <- megatrees %>%
  as_tibble() %>%
  filter(
    scenarioStart == SCENARIO_START,
    as.character(scenarioName) == SCENARIO_NAME_FILTER,
    height_filt == MEGA_HEIGHT,
    near_p(production_target, P_FOCUS)
  ) %>%
  plot_colour_primary()
rows$megatrees <- ensemble_delta(
  M0, M05, "landscape_prop",
  ci_lwr = "landscape_prop_lwr80",
  ci_upr = "landscape_prop_upr80"
) %>%
  mutate(outcome_label = LAB_MEGA)

## Delta stock: no P = 0 in RDS — reference = median at minimum P (same window / slope).
CD_base <- carbon_stock_diff %>%
  as_tibble() %>%
  filter(
    scenarioStart == SCENARIO_START,
    as.character(scenarioName) == SCENARIO_NAME_FILTER
  ) %>%
  filter_stock_diff_window() %>%
  plot_colour_primary() %>%
  mutate(
    val_bil = mean_cum_stock_diff_vs_baseline / SCALE$bil,
    lwr_bil = lwr80_cum_stock_diff_vs_baseline / SCALE$bil,
    upr_bil = upr80_cum_stock_diff_vs_baseline / SCALE$bil
  )
min_p_carbon <- suppressWarnings(min(as.numeric(CD_base$production_target), na.rm = TRUE))
CD0_c <- CD_base %>% filter(abs(as.numeric(production_target) - min_p_carbon) < 1e-6)
CD05_c <- CD_base %>% filter(near_p(production_target, P_FOCUS))
ref_carbon <- stats::median(CD0_c$val_bil, na.rm = TRUE)
if (!is.finite(ref_carbon)) {
  stop("Could not compute delta-stock reference (median at min P).")
}
rows$carbon_dstock <- CD05_c %>%
  mutate(
    v0 = ref_carbon,
    v05 = val_bil,
    delta = v05 - v0,
    delta_l = lwr_bil - ref_carbon,
    delta_u = upr_bil - ref_carbon,
    id_tail = str_extract(as.character(index), "\\d+$"),
    scenario_lbl = paste0("#", dplyr::coalesce(id_tail, as.character(index)))
  ) %>%
  filter(is.finite(delta)) %>%
  select(-id_tail) %>%
  mutate(outcome_label = LAB_CARBON)

plot_df <- bind_rows(rows) %>%
  mutate(
    outcome_label = factor(outcome_label, levels = OUTCOME_LEVELS),
    plant_shape = factor(
      ifelse(shape_class == "Point", "Without plantation", "With plantation"),
      levels = c("Without plantation", "With plantation")
    )
  )

## Fixed x-axis order for vertical one row: plantation (triangle) scenarios first, then others;
## same relative order on all four outcome panels (numeric suffix of index, then id string).
scenario_axis_levels <- if (isTRUE(VERTICAL_ONE_ROW_FIGURE)) {
  plot_df %>%
    dplyr::mutate(idx = as.character(.data$index)) %>%
    dplyr::group_by(.data$idx) %>%
    dplyr::summarize(
      plant_first = any(.data$shape_class != "Point"),
      id_num = {
        z <- suppressWarnings(
          as.integer(stringr::str_extract(dplyr::first(.data$idx), "\\d+$"))
        )
        if (length(z) != 1L || is.na(z)) NA_integer_ else z
      },
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      id_sort = dplyr::if_else(is.na(.data$id_num), Inf, as.numeric(.data$id_num))
    ) %>%
    dplyr::arrange(dplyr::desc(.data$plant_first), .data$id_sort, .data$idx) %>%
    dplyr::pull(.data$idx)
} else {
  character(0L)
}

scenario_x_label_map <- if (isTRUE(VERTICAL_ONE_ROW_FIGURE) && length(scenario_axis_levels) > 0L) {
  stats::setNames(LETTERS[seq_along(scenario_axis_levels)], as.character(scenario_axis_levels))
} else {
  NULL
}

plot_df <- plot_df %>%
  mutate(
    y_id = if (isTRUE(VERTICAL_ONE_ROW_FIGURE) && length(scenario_axis_levels) > 0L) {
      factor(as.character(.data$index), levels = scenario_axis_levels)
    } else {
      forcats::fct_reorder(factor(as.character(.data$index)), .data$delta, .desc = FALSE)
    }
  )

if (nrow(plot_df) == 0) {
  stop("No rows at P = 0.5 after filtering — check AllPrimary / all_primary and tolerances.")
}

## Landscape composition: only non-zero % classes; full habitat names (no abbreviations)
composition_nonzero_pct <- function(
    propOG,
    prop1L,
    prop2L,
    propPlant,
    prop_restored = NA_real_
) {
  pr <- dplyr::coalesce(prop_restored, 0)
  labs <- c(
    "old-growth forest",
    "once-logged forest",
    "twice-logged forest",
    "plantation",
    "strip-planted forest"
  )
  vals <- c(propOG, prop1L, prop2L, propPlant, pr)
  p <- as.integer(round(vals * 100))
  keep <- p > 0L
  if (!any(keep)) {
    return("100%")
  }
  paste(sprintf("%s %d%%", labs[keep], p[keep]), collapse = " / ")
}

## 4x4 bivariate grid (same hex layout as `cols_tbl` / biscale) + scenario letters in tiles (A, B, …) matching x-axis order
bivariate_legend_with_cell_arrows <- function(plot_df, cols_tbl, index_levels_ordered = character(0L)) {
  xs <- BIVARIATE_TILE_X_STRETCH
  stretch_cx <- function(cx) 2.5 + (cx - 2.5) * xs
  grid_df <- cols_tbl %>%
    tidyr::separate(bi_class, into = c("bx", "by"), sep = "-", convert = TRUE) %>%
    dplyr::mutate(
      cx = as.numeric(.data$bx),
      cy = as.numeric(.data$by),
      cx_s = stretch_cx(.data$cx)
    )
  if (length(index_levels_ordered) < 1L) {
    return(ggplot() + theme_void())
  }
  letter_vec <- LETTERS[seq_along(index_levels_ordered)]
  names(letter_vec) <- as.character(index_levels_ordered)
  one_row_per_index <- plot_df %>%
    dplyr::mutate(idx_chr = as.character(.data$index)) %>%
    dplyr::filter(.data$idx_chr %in% names(letter_vec)) %>%
    dplyr::group_by(.data$idx_chr) %>%
    dplyr::slice_head(n = 1L) %>%
    dplyr::ungroup()
  examples <- dplyr::tibble(idx_chr = as.character(index_levels_ordered)) %>%
    dplyr::mutate(letter = unname(letter_vec[.data$idx_chr])) %>%
    dplyr::left_join(one_row_per_index, by = "idx_chr") %>%
    dplyr::left_join(
      grid_df %>% dplyr::select(cell_hex = hex, cx, cy),
      by = c("plot_colour" = "cell_hex")
    ) %>%
    dplyr::filter(is.finite(.data$cx), is.finite(.data$cy), !is.na(.data$letter))
  if (nrow(examples) < 1L) {
    return(ggplot() + theme_void())
  }
  examples <- examples %>%
    dplyr::arrange(dplyr::desc(.data$cy), .data$cx) %>%
    dplyr::mutate(
      prop_restored = dplyr::coalesce(.data$prop_restored, 0),
      caption = purrr::pmap_chr(
        list(
          .data$propOG,
          .data$prop1L,
          .data$prop2L,
          .data$propPlant,
          .data$prop_restored
        ),
        \(
          propOG,
          prop1L,
          prop2L,
          propPlant,
          prop_restored
        ) composition_nonzero_pct(
          propOG,
          prop1L,
          prop2L,
          propPlant,
          prop_restored
        )
      ),
      caption = stringr::str_wrap(.data$caption, width = 52),
      cx_s = stretch_cx(.data$cx)
    )

  ## Several scenarios can share one bivariate cell: show letters side-by-side (e.g. "A B"), not stacked.
  tile_lbl_df <- examples %>%
    dplyr::group_by(.data$cx_s, .data$cy) %>%
    dplyr::summarise(
      tile_letters = paste(sort(unique(.data$letter)), collapse = " "),
      .groups = "drop"
    )

  caption_df <- examples %>%
    dplyr::group_by(.data$cx_s, .data$cy) %>%
    dplyr::summarise(
      letters_comb = paste(sort(unique(.data$letter)), collapse = " "),
      caption_one = dplyr::first(.data$caption),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      caption_lined = stringr::str_wrap(
        paste0(.data$letters_comb, "  ", .data$caption_one),
        width = 40
      )
    ) %>%
    dplyr::arrange(dplyr::desc(.data$cy), .data$cx_s) %>%
    dplyr::mutate(
      kk = dplyr::n(),
      slot = dplyr::row_number(),
      y_lab = 3.92 - (.data$slot - 1L) * (2.82 / max(.data$kk - 1L, 1L)),
      x_text = stretch_cx(4.58) + 0.42
    )
  ggplot() +
    geom_tile(
      data = grid_df,
      aes(x = .data$cx_s, y = .data$cy, fill = .data$hex),
      colour = "grey38",
      linewidth = 0.28,
      width = 0.9 * xs,
      height = 0.9
    ) +
    scale_fill_identity() +
    geom_text(
      data = tile_lbl_df,
      aes(x = .data$cx_s, y = .data$cy, label = .data$tile_letters),
      colour = "black",
      size = 2.72,
      fontface = "bold",
      hjust = 0.5,
      vjust = 0.5
    ) +
    geom_text(
      data = caption_df,
      aes(x = .data$x_text, y = .data$y_lab, label = .data$caption_lined),
      colour = "grey12",
      size = 2.45,
      hjust = 0,
      vjust = 0.5,
      lineheight = 0.96
    ) +
    annotate(
      "text",
      x = 2.5,
      y = 0.22,
      label = "Old-growth share ->",
      size = 3.55,
      colour = "grey12",
      fontface = "plain",
      hjust = 0.5,
      vjust = 0.5
    ) +
    scale_x_continuous(
      name = NULL,
      breaks = NULL,
      expand = expansion(mult = c(0.06, 0.05))
    ) +
    scale_y_continuous(
      "Once-logged share ->",
      breaks = NULL,
      expand = expansion(mult = c(0.06, 0.06), add = c(0.55, 0.12))
    ) +
    labs(caption = stringr::str_wrap(COMPOSITION_ABBREV_CAPTION, width = 88)) +
    coord_cartesian(xlim = c(0.35, 8.55), ylim = c(0.02, 4.55), clip = "off", expand = FALSE) +
    theme_minimal(base_size = 10) +
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_text(
        size = 10,
        face = "plain",
        angle = 90,
        colour = "grey12",
        margin = margin(r = 22, l = 6)
      ),
      plot.caption = element_text(
        hjust = 0.5,
        size = 6.9,
        colour = "grey36",
        lineheight = 1.12,
        margin = margin(t = 8, b = 2)
      ),
      plot.margin = margin(6, 14, 14, 18)
    )
}

## ---- ggplot: four outcome panels (2x2 cowplot), icon inset top-left per panel ----
x_lab_baseline <- function(x) {
  vapply(
    x,
    function(z) {
      if (!is.finite(z)) return("")
      if (abs(z) < 1e-10) return("Baseline")
      as.character(number(z, accuracy = 0.01, trim = TRUE))
    },
    character(1L)
  )
}

## Vertical birds panel: bold "Baseline" via markdown (larger than numeric ticks); numbers unchanged.
y_lab_baseline_markdown <- function(x) {
  vapply(
    x,
    function(z) {
      if (!is.finite(z)) return("")
      if (abs(z) < 1e-10) {
        return("<span style='font-size:9.35pt'>**Baseline**</span>")
      }
      as.character(number(z, accuracy = 0.01, trim = TRUE))
    },
    character(1L)
  )
}

## Horizontal delta axis: no tick label at x = 0 (dotted vline is the baseline cue)
x_lab_horizontal_delta <- function(x) {
  vapply(
    x,
    function(z) {
      if (!is.finite(z)) return("")
      if (abs(z) < 1e-10) return("")
      as.character(number(z, accuracy = 0.01, trim = TRUE))
    },
    character(1L)
  )
}

## y-axis tick labels without the word "baseline" (used on non-left panels, vertical one row)
y_numeric_axis_lbl <- function(x) {
  vapply(
    x,
    function(z) {
      if (!is.finite(z)) return("")
      if (abs(z) < 1e-10) return("")
      as.character(number(z, accuracy = 0.01, trim = TRUE))
    },
    character(1L)
  )
}

prep_one_outcome <- function(df, lab_chr) {
  out <- df %>%
    dplyr::filter(as.character(.data$outcome_label) == lab_chr) %>%
    dplyr::mutate(outcome_label = factor(.data$outcome_label, levels = OUTCOME_LEVELS))
  if (isTRUE(VERTICAL_ONE_ROW_FIGURE) && length(scenario_axis_levels) > 0L) {
    out %>%
      dplyr::mutate(
        y_id = factor(as.character(.data$index), levels = scenario_axis_levels)
      )
  } else {
    out %>%
      dplyr::mutate(
        y_id = forcats::fct_reorder(factor(as.character(.data$index)), .data$delta, .desc = FALSE)
      )
  }
}

forest_ggplot_single <- function(
  dat,
  show_shape_legend,
  vertical = vertical_forest_layout,
  compact = compact_layout,
  narrow_vertical = isTRUE(VERTICAL_ONE_ROW_FIGURE),
  show_axis_baseline_lbl = FALSE,
  scenario_x_label_map = NULL
) {
  dat_ci <- dplyr::filter(dat, is.finite(.data$delta_l), is.finite(.data$delta_u))
  if (!isTRUE(vertical)) {
    cmp <- isTRUE(compact)
    strip_sz <- if (cmp) 6.85 else 9.35
    pt_sz <- if (cmp) 2.62 else 3.05
    stroke_w <- if (cmp) 0.72 else 0.78
    err_w <- if (cmp) 0.28 else 0.35
    err_lw <- if (cmp) 0.36 else 0.45
    ## Wider axis padding in compact row: numeric delta span uses less of each narrow panel width
    x_expand <- if (cmp) expansion(mult = c(0.42, 0.92)) else expansion(mult = c(0.04, 0.15))
    pan_sp <- if (cmp) unit(5, "pt") else unit(10, "pt")
    pl_marg <- if (cmp) margin(3, 5, 4, 4) else margin(4, 10, 4, 6)
    return(
      ggplot(dat, aes(x = delta, y = y_id)) +
        geom_errorbar(
          data = dat_ci,
          aes(xmin = delta_l, xmax = delta_u, y = y_id, colour = plot_colour),
          orientation = "y",
          width = err_w,
          linewidth = err_lw,
          alpha = 0.9,
          na.rm = TRUE
        ) +
        geom_vline(
          xintercept = 0,
          linetype = "dotted",
          colour = "grey10",
          linewidth = 0.85,
          alpha = 0.95,
          show.legend = FALSE
        ) +
        geom_point(
          aes(colour = plot_colour, shape = plant_shape),
          size = pt_sz,
          stroke = stroke_w,
          alpha = 1
        ) +
        scale_colour_identity(guide = "none") +
        scale_shape_manual(
          name = NULL,
          values = c("Without plantation" = 1L, "With plantation" = 2L)
        ) +
        scale_x_continuous(
          labels = x_lab_baseline,
          expand = x_expand
        ) +
        facet_wrap(~outcome_label, nrow = 1L, scales = "free", strip.position = "top") +
        labs(x = NULL, y = NULL, title = NULL, subtitle = NULL) +
        scale_y_discrete(labels = NULL, expand = expansion(add = if (cmp) 0.28 else 0.35)) +
        coord_cartesian(clip = "off") +
        theme_classic(base_size = if (cmp) 9 else 10, base_family = "sans") +
        theme(
          strip.background = element_blank(),
          strip.text = element_text(
            face = "bold",
            size = strip_sz,
            colour = if (cmp) "grey9" else "grey8",
            hjust = 0,
            lineheight = if (cmp) 0.94 else 0.96,
            margin = margin(b = if (cmp) 3 else 5, t = if (cmp) 2 else 3)
          ),
          panel.spacing = pan_sp,
          axis.text.y = element_blank(),
          axis.ticks.y = element_line(colour = "grey45", linewidth = 0.35),
          axis.text.x = if (cmp) element_text(size = 7.2, colour = "grey25") else NULL,
          axis.title.x = element_blank(),
          legend.position = if (show_shape_legend) "top" else "none",
          legend.justification = "left",
          legend.box = "horizontal",
          legend.margin = margin(b = 1, t = 0),
          plot.margin = pl_marg
        )
    )
  }
  dmin <- suppressWarnings(min(c(dat$delta, dat$delta_l, dat$delta_u), na.rm = TRUE))
  dmax <- suppressWarnings(max(c(dat$delta, dat$delta_l, dat$delta_u), na.rm = TRUE))
  if (!is.finite(dmin) || !is.finite(dmax)) {
    dmin <- dmax <- 0
  }
  span <- if (is.finite(dmax - dmin) && dmax > dmin) dmax - dmin else max(0.02, abs(dmax), abs(dmin), 0.01)
  pad <- 0.07 * span
  ## When all effects stay on one side of zero, pin y = 0 to the top or bottom panel edge
  ## so the baseline reads as a dotted line along that edge.
  y_lim <- if (dmax <= 0) {
    c(dmin - pad, 0 + 0.05 * span)
  } else if (dmin >= 0) {
    c(0 - 0.05 * span, dmax + pad)
  } else {
    c(dmin - pad, dmax + pad)
  }
  nv <- isTRUE(narrow_vertical)
  strip_sz_v <- if (nv) 9.35 else 9.0
  pt_sz_v <- if (nv) 3.35 else 3.05
  stroke_v <- if (nv) 0.98 else 0.78
  err_w_v <- if (nv) 0.24 else 0.35
  err_lw_v <- if (nv) 0.44 else 0.45
  ax_y_sz <- if (nv) 6.35 else 7.2
  pan_sp_v <- if (nv) unit(4.5, "pt") else unit(10, "pt")
  x_disc_exp <- if (nv) 0.22 else 0.35
  pl_marg_v <- if (nv) margin(3, 4, 4, 3) else margin(4, 10, 4, 6)
  base_sz_v <- if (nv) 9 else 10
  y_lbl_fn <- if (isTRUE(show_axis_baseline_lbl)) y_lab_baseline_markdown else y_numeric_axis_lbl
  x_lbl_fn <- if (isTRUE(nv) && !is.null(scenario_x_label_map) && length(scenario_x_label_map) > 0L) {
    function(x) {
      nm <- as.character(x)
      out <- unname(scenario_x_label_map[nm])
      ifelse(is.na(out), nm, out)
    }
  } else {
    NULL
  }
  ggplot(dat, aes(x = y_id, y = delta)) +
    geom_errorbar(
      data = dat_ci,
      aes(x = y_id, ymin = delta_l, ymax = delta_u, colour = plot_colour),
      width = err_w_v,
      linewidth = err_lw_v,
      alpha = 0.9,
      na.rm = TRUE
    ) +
    geom_hline(
      yintercept = 0,
      linetype = "dotted",
      colour = "grey10",
      linewidth = 0.85,
      alpha = 0.95,
      show.legend = FALSE
    ) +
    geom_point(
      aes(colour = plot_colour, shape = plant_shape),
      size = pt_sz_v,
      stroke = stroke_v,
      alpha = 1
    ) +
    scale_colour_identity(guide = "none") +
    scale_shape_manual(
      name = NULL,
      values = c("Without plantation" = 1L, "With plantation" = 2L)
    ) +
    scale_x_discrete(
      expand = expansion(add = x_disc_exp),
      labels = x_lbl_fn %||% waiver(),
      drop = FALSE,
      limits = if (!is.null(scenario_x_label_map) && length(scenario_x_label_map) > 0L) {
        names(scenario_x_label_map)
      } else {
        NULL
      }
    ) +
    scale_y_continuous(labels = y_lbl_fn) +
    facet_wrap(~outcome_label, nrow = 1L, scales = "free_y", strip.position = "top") +
    labs(x = NULL, y = NULL, title = NULL, subtitle = NULL) +
    coord_cartesian(ylim = y_lim, clip = "off") +
    theme_classic(base_size = base_sz_v, base_family = "sans") +
    theme(
      strip.background = element_blank(),
      strip.text = element_text(
        face = "bold",
        size = strip_sz_v,
        colour = if (nv) "grey8" else "grey10",
        hjust = 0,
        lineheight = if (nv) 0.96 else 0.96,
        margin = margin(b = if (nv) 5 else 5, t = if (nv) 3 else 2)
      ),
      panel.spacing = pan_sp_v,
      axis.text.x = if (!is.null(scenario_x_label_map) && length(scenario_x_label_map) > 0L && isTRUE(nv)) {
        element_text(size = 8.5, colour = "grey25")
      } else {
        element_blank()
      },
      axis.ticks.x = element_line(colour = "grey45", linewidth = 0.35),
      axis.text.y = if (isTRUE(nv) && isTRUE(show_axis_baseline_lbl)) {
        ggtext::element_markdown(size = ax_y_sz, colour = "grey25")
      } else {
        element_text(size = ax_y_sz, colour = "grey25")
      },
      axis.ticks.y = element_line(colour = "grey45", linewidth = 0.35),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position = if (show_shape_legend) "top" else "none",
      legend.justification = "left",
      legend.box = "horizontal",
      legend.margin = margin(b = 1, t = 0),
      plot.margin = pl_marg_v
    )
}

p_leg_src <- if (isTRUE(vertical_forest_layout)) {
  ggplot(plot_df, aes(x = y_id, y = delta)) +
    geom_point(
      aes(colour = plot_colour, shape = plant_shape),
      data = dplyr::slice_head(plot_df, n = 4L),
      size = if (isTRUE(VERTICAL_ONE_ROW_FIGURE)) 3.35 else 3.05,
      stroke = if (isTRUE(VERTICAL_ONE_ROW_FIGURE)) 0.98 else 0.78
    ) +
    scale_x_discrete() +
    scale_y_continuous() +
    scale_colour_identity(guide = "none") +
    scale_shape_manual(
      name = NULL,
      values = c("Without plantation" = 1L, "With plantation" = 2L)
    ) +
    theme_void(base_size = 10) +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_blank(),
      legend.text = element_text(size = 8.5),
      legend.key.width = unit(0.55, "cm"),
      legend.key.height = unit(0.38, "cm"),
      legend.spacing.x = unit(0.35, "cm")
    )
} else {
  ggplot(plot_df, aes(x = delta, y = y_id)) +
    geom_point(
      aes(colour = plot_colour, shape = plant_shape),
      data = dplyr::slice_head(plot_df, n = 4L),
      size = 3.05,
      stroke = 0.78
    ) +
    scale_colour_identity(guide = "none") +
    scale_shape_manual(
      name = NULL,
      values = c("Without plantation" = 1L, "With plantation" = 2L)
    ) +
    theme_void(base_size = 10) +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_blank(),
      legend.text = element_text(size = 8.5),
      legend.key.width = unit(0.55, "cm"),
      legend.key.height = unit(0.38, "cm"),
      legend.spacing.x = unit(0.35, "cm")
    )
}
leg_shape <- get_legend(p_leg_src)

p_bird_w <- inset_icon_on_plot(
  forest_ggplot_single(
    prep_one_outcome(plot_df, LAB_BIRDS),
    FALSE,
    show_axis_baseline_lbl = isTRUE(VERTICAL_ONE_ROW_FIGURE),
    scenario_x_label_map = scenario_x_label_map
  ),
  load_icon_grob(ICON_PATHS$birds),
  w = ICON_WH_BIRD_MEGA,
  h = ICON_WH_BIRD_MEGA
)
p_beetle_w <- inset_icon_on_plot(
  forest_ggplot_single(prep_one_outcome(plot_df, LAB_BEETLES), FALSE, scenario_x_label_map = scenario_x_label_map),
  load_icon_grob(ICON_PATHS$dung_beetles)
)
p_mega_w <- inset_icon_on_plot(
  forest_ggplot_single(prep_one_outcome(plot_df, LAB_MEGA), FALSE, scenario_x_label_map = scenario_x_label_map),
  load_icon_grob(ICON_PATHS$megatrees),
  w = ICON_WH_BIRD_MEGA,
  h = ICON_WH_BIRD_MEGA
)
p_carbon_w <- inset_icon_on_plot(
  forest_ggplot_single(prep_one_outcome(plot_df, LAB_CARBON), FALSE, scenario_x_label_map = scenario_x_label_map),
  carbon_icon_grob()
)

## Layout: 2x2 outcomes, then shared x-axis row, then bottom row (landscape text L | shape+bivariate R)
landscape_detail <- sprintf("%.0f Mha of primary forest", landscpape_in_hectares / 1e6)
production_detail <- if (is.finite(production_mm3_p05)) {
  sprintf(
    "0.5, producing %s Mm\u00b3 over 60 yrs.",
    format(round(production_mm3_p05, 2), trim = TRUE, nsmall = 2)
  )
} else {
  "0.5, producing (volume unavailable) Mm\u00b3 over 60 yrs."
}
production_note <- paste0(
  "Total landscape timber harvest at production target P = ",
  P_FOCUS,
  " (60-yr horizon; from scenario yields)."
)
caption_cell <- ggdraw() +
  draw_label(
    "Starting landscape:",
    x = 0.5,
    y = 0.84,
    hjust = 0.5,
    vjust = 0.5,
    size = 11.8,
    fontface = "bold",
    colour = "grey8"
  ) +
  draw_label(
    landscape_detail,
    x = 0.5,
    y = 0.66,
    hjust = 0.5,
    vjust = 0.5,
    size = 10.4,
    fontface = "plain",
    colour = "grey10"
  ) +
  draw_label(
    "Production target (P):",
    x = 0.5,
    y = 0.48,
    hjust = 0.5,
    vjust = 0.5,
    size = 11.8,
    fontface = "bold",
    colour = "grey8"
  ) +
  draw_label(
    production_detail,
    x = 0.5,
    y = 0.32,
    hjust = 0.5,
    vjust = 0.5,
    size = 10.4,
    fontface = "plain",
    colour = "grey10"
  ) +
  draw_label(
    stringr::str_wrap(production_note, width = 42),
    x = 0.5,
    y = 0.1,
    hjust = 0.5,
    vjust = 0.5,
    size = 7.6,
    fontface = "plain",
    colour = "grey38",
    lineheight = 1.08
  )

legend_footer <- bivariate_legend_with_cell_arrows(plot_df, cols_tbl, scenario_axis_levels)
bottom_right <- legend_footer

## Shape legend (plantation) full-width strip at very top of figure
leg_row <- ggdraw() +
  draw_grob(
    leg_shape,
    x = 0.5,
    y = 0.5,
    width = 0.98,
    height = 0.95,
    hjust = 0.5,
    vjust = 0.5
  )

p_outcomes_grid <- plot_grid(
  p_bird_w,
  p_beetle_w,
  p_mega_w,
  p_carbon_w,
  ncol = if (isTRUE(outcomes_one_row)) 4L else 2L,
  nrow = if (isTRUE(outcomes_one_row)) 1L else 2L,
  align = "hv",
  axis = "tblr"
)

xlab_row_production <- ggdraw() +
  draw_label(
    "Production target (P = 0.5)",
    x = 0.5,
    y = 0.5,
    hjust = 0.5,
    vjust = 0.5,
    size = 10.2,
    colour = "grey10",
    fontface = "plain"
  )

p_outcomes_grid <- if (isTRUE(VERTICAL_ONE_ROW_FIGURE)) {
  plot_grid(
    p_outcomes_grid,
    xlab_row_production,
    ncol = 1L,
    rel_heights = c(1, 0.038)
  )
} else {
  p_outcomes_grid
}

ylab_stripe <- ggdraw() +
  draw_label(
    "Change vs old-growth baseline landscape",
    x = 0.5,
    y = 0.5,
    angle = 90,
    hjust = 0.5,
    vjust = 0.5,
    size = 10.2,
    colour = "grey10",
    fontface = "plain"
  )

xlab_row <- ggdraw() +
  draw_label(
    "Change vs old-growth baseline landscape",
    x = 0.5,
    y = 0.5,
    hjust = 0.5,
    vjust = 0.5,
    size = 10.2,
    colour = "grey10",
    fontface = "plain"
  )

p_outcomes <- if (isTRUE(vertical_forest_layout)) {
  plot_grid(
    ylab_stripe,
    p_outcomes_grid,
    ncol = 2L,
    rel_widths = c(if (isTRUE(VERTICAL_ONE_ROW_FIGURE)) 0.038 else 0.045, 1),
    align = "hv",
    axis = "tblr"
  )
} else {
  p_outcomes_grid
}

p_bottom <- plot_grid(
  caption_cell,
  bottom_right,
  ncol = 2L,
  nrow = 1L,
  align = "hv",
  axis = "tblr"
)

p_inner <- if (isTRUE(vertical_forest_layout)) {
  plot_grid(
    p_outcomes,
    p_bottom,
    ncol = 1L,
    rel_heights = c(1, if (isTRUE(VERTICAL_ONE_ROW_FIGURE)) 0.5 else 0.52),
    align = "h",
    axis = "lr"
  )
} else if (isTRUE(compact_layout)) {
  plot_grid(
    p_outcomes,
    xlab_row,
    p_bottom,
    ncol = 1L,
    rel_heights = c(1, 0.042, 0.58),
    align = "h",
    axis = "lr"
  )
} else {
  plot_grid(
    p_outcomes,
    xlab_row,
    p_bottom,
    ncol = 1L,
    rel_heights = c(1, 0.048, 0.52),
    align = "h",
    axis = "lr"
  )
}

p_combined <- plot_grid(
  leg_row,
  p_inner,
  ncol = 1L,
  rel_heights = c(0.055, 1),
  align = "h",
  axis = "lr"
)

dir.create(PATHS$export_dir, recursive = TRUE, showWarnings = FALSE)
out_path <- file.path(PATHS$export_dir, OUTPUT_NAME)

save_plot(
  filename = out_path,
  plot = p_combined,
  base_width = FIG_SAVE_WIDTH_IN,
  base_height = FIG_SAVE_HEIGHT_IN,
  device = "pdf"
)

message("Saved: ", normalizePath(out_path, winslash = "/", mustWork = FALSE))
