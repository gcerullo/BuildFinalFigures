# Auto-extracted for Rmd sourcing — builds plot_df for P=0.5 vs ref forest figure (do not edit by hand; regenerate from forest script if needed)
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

## Bivariate tile stretch in x (â‰ˆ1 = square cells in x vs y data units)
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

## Bird / megatree thumbnails: ~1.5Ã— default npc size when VERTICAL_ONE_ROW_FIGURE only
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
    " â€” caption volume left blank."
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

## Delta stock: no P = 0 in RDS â€” reference = median at minimum P (same window / slope).
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
  stop("No rows at P = 0.5 after filtering - check AllPrimary / all_primary and tolerances.")
}
