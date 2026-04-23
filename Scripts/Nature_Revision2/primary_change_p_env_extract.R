# ----------------------------------------------------------------------------
# Extract tables for "Changing P in Primary Landscapes" (Rmd / manuscript).
# Consumed by Scripts/Nature_Revision2/extracting_ms_results/extracting_ms_results*.Rmd
# Mirrors data definitions in 02_build_modified_final_figures.R:
#   environmental_by_starting — AllPrimary column, loser birds/beetles,
#   megatrees 50 m with 95% intervals, cumulative C stock diff vs baseline
#   (60 yr window, slope 1, 80% model intervals, values in billion MgC).
# Main output: `sections` — compact tables per landscape (P≈0.1 block, P=1 block,
#   Δ range, median timber); composition + min/max scenario on one line each.
# No CSV/RDS outputs; returns data.frames only.
# ----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(data.table)
})

resolve_path <- function(primary) {
  if (file.exists(primary)) return(primary)
  stop("Missing input: ", primary, " (run from BuildFinalFigures root).", call. = FALSE)
}

## Short labels only (no `fct_relevel` — avoids warnings when a single scenario is loaded).
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
        TRUE ~ as.character(scenarioName)
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

add_plantation_type <- function(df) {
  df %>%
    mutate(
      shape_class = dplyr::case_when(
        is.na(propPlant) ~ "Point",
        propPlant > 0 ~ "Cross",
        TRUE ~ "Point"
      )
    )
}

prep_join_plantation <- function(df, propOGcomp) {
  pc <- propOGcomp %>%
    mutate(index = as.character(index), production_target = as.numeric(production_target)) %>%
    group_by(index, production_target) %>%
    summarise(propPlant = dplyr::first(propPlant), .groups = "drop")
  df %>%
    mutate(index = as.character(index), production_target = as.numeric(production_target)) %>%
    left_join(pc, by = c("index", "production_target")) %>%
    rename_scenario_name() %>%
    add_plantation_type()
}

## Median national timber yield (Mm3) at production targets near `p_tgt`
## (same spirit as forest caption: median unique yields / 1e6).
yield_mm3_median_at_p <- function(comp_full, p_tgt, scenario_csv = "all_primary_CY_D.csv", tol = 0.03) {
  d <- comp_full %>%
    as.data.frame() %>%
    filter(as.character(scenarioName) == as.character(scenario_csv)) %>%
    mutate(.cd = abs(as.numeric(production_target) - as.numeric(p_tgt)))
  if (!nrow(d)) {
    return(NA_real_)
  }
  if (any(d$.cd <= tol, na.rm = TRUE)) {
    d <- d %>% filter(.cd <= tol)
  } else {
    d <- d %>% filter(.cd == min(.cd, na.rm = TRUE))
  }
  ys <- unique(as.numeric(d$production_yield))
  ys <- ys[is.finite(ys)]
  if (!length(ys)) {
    return(NA_real_)
  }
  stats::median(ys, na.rm = TRUE) / 1e6
}

fmt <- function(x, d = 3) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.finite(x), format(round(x, d), trim = TRUE, scientific = FALSE), "—")
}

## One cell: `mean` or `mean [lo, hi]` when both bounds exist (else mean only).
fmt_mean_lo_hi_line <- function(y, ylo, yhi, d = 3) {
  y <- suppressWarnings(as.numeric(y))
  lo <- suppressWarnings(as.numeric(ylo))
  hi <- suppressWarnings(as.numeric(yhi))
  if (!isTRUE(is.finite(y))) {
    return("—")
  }
  if (isTRUE(is.finite(lo)) && isTRUE(is.finite(hi))) {
    paste0(fmt(y, d), " [", fmt(lo, d), ", ", fmt(hi, d), "]")
  } else {
    fmt(y, d)
  }
}

## Max − min in one band: mean range, with (lo / hi) on the interval pair when available.
fmt_range_band_line <- function(mean_r, lo_r, hi_r, has_ci) {
  if (!isTRUE(is.finite(mean_r))) {
    return("—")
  }
  if (isTRUE(has_ci) && isTRUE(is.finite(lo_r)) && isTRUE(is.finite(hi_r))) {
    paste0(fmt(mean_r), " (", fmt(lo_r), " / ", fmt(hi_r), ")")
  } else {
    fmt(mean_r)
  }
}

## Succinct landscape composition at one (index, production_target) from prop_OGcomp.
lookup_comp_brief <- function(propOGcomp, index, p_grid) {
  if (!isTRUE(is.finite(p_grid)) || identical(index, NA_character_) || identical(index, NA)) {
    return("—")
  }
  pc <- propOGcomp %>%
    as.data.frame() %>%
    dplyr::mutate(
      index = as.character(index),
      production_target = as.numeric(production_target)
    )
  hit <- which(pc$index == as.character(index) & abs(pc$production_target - as.numeric(p_grid)) < 1e-5)
  if (!length(hit)) {
    return("—")
  }
  r <- pc[hit[1L], , drop = FALSE]
  og <- round(100 * suppressWarnings(as.numeric(r$propOG)), 0)
  pl <- round(100 * suppressWarnings(as.numeric(r$propPlant)), 0)
  e1 <- round(100 * suppressWarnings(as.numeric(r$prop1L)), 0)
  e2 <- round(100 * suppressWarnings(as.numeric(r$prop2L)), 0)
  if (!all(is.finite(c(og, pl, e1, e2)))) {
    return("—")
  }
  paste0("OG ", og, "% · pl ", pl, "% · 1L ", e1, "% · 2L ", e2, "%")
}

## One row per index in a band around `p_center`, keeping the grid point closest to the centre.
filter_p_band_one_row_per_index <- function(df, p_center, tol = 0.03) {
  df %>%
    dplyr::mutate(pt = as.numeric(production_target)) %>%
    dplyr::filter(is.finite(pt), abs(pt - p_center) <= tol, is.finite(y)) %>%
    dplyr::group_by(index) %>%
    dplyr::slice_min(order_by = abs(pt - p_center), n = 1L, with_ties = FALSE) %>%
    dplyr::ungroup()
}

## Global max − min among scenarios in the band; argmax/argmin by mean `y`.
## Interval row uses (ylo[max] − ylo[min]) and (yhi[max] − yhi[min]) on those same indices.
minmax_range_in_band <- function(df, p_center, tol = 0.03) {
  d <- filter_p_band_one_row_per_index(df, p_center, tol)
  if (nrow(d) < 2L) {
    return(list(
      mean_range = NA_real_, lo_range = NA_real_, hi_range = NA_real_,
      has_ci = FALSE, idx_max = NA_character_, idx_min = NA_character_,
      ymax = NA_real_, ymin = NA_real_,
      ylo_max = NA_real_, yhi_max = NA_real_, ylo_min = NA_real_, yhi_min = NA_real_,
      pt_max = NA_real_, pt_min = NA_real_
    ))
  }
  imax <- which.max(d$y)
  imin <- which.min(d$y)
  ymax <- as.numeric(d$y[imax])
  ymin <- as.numeric(d$y[imin])
  mean_r <- ymax - ymin
  ylom <- suppressWarnings(as.numeric(d$ylo[imax]))
  yhim <- suppressWarnings(as.numeric(d$yhi[imax]))
  ylomn <- suppressWarnings(as.numeric(d$ylo[imin]))
  yhimn <- suppressWarnings(as.numeric(d$yhi[imin]))
  has_ci <- all(is.finite(c(ylom, yhim, ylomn, yhimn)))
  if (has_ci) {
    lo_r <- ylom - ylomn
    hi_r <- yhim - yhimn
  } else {
    lo_r <- NA_real_
    hi_r <- NA_real_
  }
  list(
    mean_range = mean_r,
    lo_range = lo_r,
    hi_range = hi_r,
    has_ci = has_ci,
    idx_max = as.character(d$index[imax]),
    idx_min = as.character(d$index[imin]),
    ymax = ymax,
    ymin = ymin,
    ylo_max = ylom,
    yhi_max = yhim,
    ylo_min = ylomn,
    yhi_min = yhimn,
    pt_max = as.numeric(d$production_target[imax]),
    pt_min = as.numeric(d$production_target[imin])
  )
}

scenario_name_short <- function(scenario_csv) {
  dplyr::case_when(
    scenario_csv == "all_primary_CY_D.csv" ~ "AllPrimary",
    scenario_csv == "mostly_1L_CY_D.csv" ~ "Mostly1L",
    scenario_csv == "mostly_2L_CY_D.csv" ~ "Mostly2L",
    TRUE ~ NA_character_
  )
}

#' Build summary tables for environmental outcomes (P ≈ 0.1 vs P = 1) × starting landscape.
#'
#' @return A list with `sections` (each: `title`, `subtitle`, `wide`, `detail`),
#'   `minmax_wide` / `minmax_detail` (AllPrimary block only), `minmax_detail_all`,
#'   and `notes`.
build_primary_p_change_env_tables <- function() {
  root <- getwd()
  source(file.path(root, "Inputs", "FixedScenarioParams.R"), local = TRUE)

  PATHS <- list(
    birds = "Data/NR2/OG_baseline_birds.rds",
    birds_iucn = "Data/NR2/OG_baseline_birdsIUCN.rds",
    dung_beetles = "Data/NR2/MasterDBPerformance_fastPilot.rds",
    carbon_stock_diff = "Data/NR2/stock_diff_vs_baseline__windowed__all_traj.rds",
    megatrees = "Data/full_nature_scenario_megatree_performance_all_thresholds.rds",
    all_scenarios = "Inputs/MasterAllScenarios.rds"
  )

  scenarios <- readRDS(resolve_path(PATHS$all_scenarios))
  comp_full <- rbindlist(scenarios, use.names = TRUE)
  rm(scenarios)

  p_lo <- 0.1
  p_hi <- 1
  p_tol <- 0.03

  habInStart <- all_start_landscape %>%
    dplyr::select(scenarioStart) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      originalOG = c(1, 0.2, 0.2, 0.8, 0.2, 0.2),
      original1L = c(0, 0.8, 0, 0, 0.6, 0),
      original2L = c(0, 0, 0.8, 0, 0, 0.6)
    )

  oc_levels <- c(
    "Old-growth birds", "Old-growth beetles", "Megatrees (% landscape)",
    "Cumulative C (billion MgC)"
  )

  prep_outcome_df <- function(df) {
    df %>%
      dplyr::ungroup() %>%
      dplyr::mutate(index = as.character(index)) %>%
      dplyr::filter(!(grepl("\\.csv$", index, ignore.case = TRUE) & !grepl(" ", index))) %>%
      dplyr::mutate(
        y = as.numeric(y),
        ylo = as.numeric(ylo),
        yhi = as.numeric(yhi)
      ) %>%
      dplyr::select(index, production_target, y, ylo, yhi)
  }

  minmax_one <- function(df, outcome_label, propOGcomp, p_lo, p_hi, tol) {
    d <- prep_outcome_df(df)
    r0 <- minmax_range_in_band(d, p_lo, tol)
    r1 <- minmax_range_in_band(d, p_hi, tol)
    dm <- r1$mean_range - r0$mean_range
    dlo <- if (isTRUE(r0$has_ci) && isTRUE(r1$has_ci)) r1$lo_range - r0$lo_range else NA_real_
    dhi <- if (isTRUE(r0$has_ci) && isTRUE(r1$has_ci)) r1$hi_range - r0$hi_range else NA_real_
    has_d <- isTRUE(r0$has_ci) && isTRUE(r1$has_ci)
    delta_range_line <- if (!isTRUE(is.finite(dm))) {
      "—"
    } else if (has_d && isTRUE(is.finite(dlo)) && isTRUE(is.finite(dhi))) {
      paste0(fmt(dm), " (", fmt(dlo), " / ", fmt(dhi), ")")
    } else {
      fmt(dm)
    }
    range_line_p01 <- fmt_range_band_line(r0$mean_range, r0$lo_range, r0$hi_range, r0$has_ci)
    range_line_p1 <- fmt_range_band_line(r1$mean_range, r1$lo_range, r1$hi_range, r1$has_ci)
    tibble::tibble(
      outcome = outcome_label,
      comp_min_p01 = lookup_comp_brief(propOGcomp, r0$idx_min, r0$pt_min),
      min_scenario_line_p01 = fmt_mean_lo_hi_line(r0$ymin, r0$ylo_min, r0$yhi_min),
      comp_max_p01 = lookup_comp_brief(propOGcomp, r0$idx_max, r0$pt_max),
      max_scenario_line_p01 = fmt_mean_lo_hi_line(r0$ymax, r0$ylo_max, r0$yhi_max),
      range_line_p01 = range_line_p01,
      comp_min_p1 = lookup_comp_brief(propOGcomp, r1$idx_min, r1$pt_min),
      min_scenario_line_p1 = fmt_mean_lo_hi_line(r1$ymin, r1$ylo_min, r1$yhi_min),
      comp_max_p1 = lookup_comp_brief(propOGcomp, r1$idx_max, r1$pt_max),
      max_scenario_line_p1 = fmt_mean_lo_hi_line(r1$ymax, r1$ylo_max, r1$yhi_max),
      range_line_p1 = range_line_p1,
      delta_range_line = delta_range_line,
      idx_min_p01 = r0$idx_min,
      idx_max_p01 = r0$idx_max,
      idx_min_p1 = r1$idx_min,
      idx_max_p1 = r1$idx_max
    )
  }

  wide_from_mm <- function(mm, yield_mm3_p01, yield_mm3_p1) {
    nbsp <- "\U00A0"
    ocn <- c(
      "Old-growth birds", "Old-growth beetles", "Megatrees (% landscape)",
      "Cumulative C (billion MgC)"
    )
    pick <- function(outc, col) {
      v <- mm[[col]][mm$outcome == outc]
      if (!length(v)) {
        return("—")
      }
      as.character(v[[1L]])
    }
    hdr_row <- function(label) {
      tibble::tibble(Metric = label, !!!stats::setNames(rep(list(nbsp), length(ocn)), ocn))
    }
    ## Visual break before rows that compare across production targets (escape=FALSE on kable).
    divider_row <- function() {
      tibble::tibble(
        Metric = "<span style=\"display:block;margin-top:0.55rem;padding-top:0.5rem;border-top:1px solid #bbb;font-size:0.82rem;font-weight:600;color:#444;\">Between production targets</span>",
        !!!stats::setNames(rep(list(nbsp), length(ocn)), ocn)
      )
    }
    data_row <- function(metric, colkey) {
      tibble::tibble(
        Metric = metric,
        !!!stats::setNames(lapply(ocn, function(nm) pick(nm, colkey)), ocn)
      )
    }
    ytxt <- if (isTRUE(is.finite(yield_mm3_p01)) && isTRUE(is.finite(yield_mm3_p1))) {
      paste0(
        format(round(yield_mm3_p01, 2), trim = TRUE), " @ P≈0.1; ",
        format(round(yield_mm3_p1, 1), trim = TRUE), " @ P=1"
      )
    } else {
      "—"
    }
    timber_row <- tibble::tibble(
      Metric = "Median timber (Mm³)",
      `Old-growth birds` = ytxt,
      `Old-growth beetles` = ytxt,
      `Megatrees (% landscape)` = ytxt,
      `Cumulative C (billion MgC)` = ytxt
    )
    dplyr::bind_rows(
      hdr_row("P ≈ 0.1"),
      data_row("Composition of min", "comp_min_p01"),
      data_row("Min scenario", "min_scenario_line_p01"),
      data_row("Composition of max", "comp_max_p01"),
      data_row("Max scenario", "max_scenario_line_p01"),
      data_row("Range (max − min) @ P ≈ 0.1", "range_line_p01"),
      hdr_row("P = 1.0"),
      data_row("Composition of min", "comp_min_p1"),
      data_row("Min scenario", "min_scenario_line_p1"),
      data_row("Composition of max", "comp_max_p1"),
      data_row("Max scenario", "max_scenario_line_p1"),
      data_row("Range (max − min) @ P = 1", "range_line_p1"),
      divider_row(),
      data_row("Δ (max − min), P=1 − P≈0.1", "delta_range_line"),
      timber_row
    )
  }

  detail_from_mm <- function(mm, landscape_label) {
    mm %>%
      dplyr::mutate(Landscape = landscape_label) %>%
      dplyr::transmute(
        Landscape,
        Outcome = outcome,
        `Min idx @ P≈0.1` = idx_min_p01,
        `Max idx @ P≈0.1` = idx_max_p01,
        `Min idx @ P=1` = idx_min_p1,
        `Max idx @ P=1` = idx_max_p1
      )
  }

  build_one_landscape <- function(scenario_csv, section_title, section_subtitle) {
    sn <- scenario_name_short(scenario_csv)
    if (is.na(sn)) {
      stop("Unknown scenario_csv: ", scenario_csv, call. = FALSE)
    }

    yield_mm3_p01 <- yield_mm3_median_at_p(comp_full, p_lo, scenario_csv = scenario_csv)
    yield_mm3_p1 <- yield_mm3_median_at_p(comp_full, p_hi, scenario_csv = scenario_csv)

    scenario_composition <- comp_full[as.character(comp_full$scenarioName) == scenario_csv, ]
    if (!nrow(scenario_composition)) {
      n <- length(oc_levels)
      empty_mm <- tibble::tibble(
        outcome = factor(oc_levels, levels = oc_levels),
        comp_min_p01 = rep("—", n),
        min_scenario_line_p01 = rep("—", n),
        comp_max_p01 = rep("—", n),
        max_scenario_line_p01 = rep("—", n),
        range_line_p01 = rep("—", n),
        comp_min_p1 = rep("—", n),
        min_scenario_line_p1 = rep("—", n),
        comp_max_p1 = rep("—", n),
        max_scenario_line_p1 = rep("—", n),
        range_line_p1 = rep("—", n),
        delta_range_line = rep("—", n),
        idx_min_p01 = rep(NA_character_, n),
        idx_max_p01 = rep(NA_character_, n),
        idx_min_p1 = rep(NA_character_, n),
        idx_max_p1 = rep(NA_character_, n)
      )
      return(list(
        title = section_title,
        subtitle = section_subtitle,
        wide = wide_from_mm(empty_mm, NA_real_, NA_real_),
        detail = tibble::tibble(
          Landscape = section_subtitle,
          Outcome = oc_levels,
          `Min idx @ P≈0.1` = rep("—", n),
          `Max idx @ P≈0.1` = rep("—", n),
          `Min idx @ P=1` = rep("—", n),
          `Max idx @ P=1` = rep("—", n)
        )
      ))
    }

    propOGcomp <- prop_OG_fun(scenario_composition, habInStart)

    birds <- dplyr::bind_rows(
      readRDS(resolve_path(PATHS$birds)) %>%
        dplyr::distinct() %>%
        dplyr::rename(bird_grp = spp_category) %>%
        dplyr::filter(as.character(scenarioName) == scenario_csv),
      readRDS(resolve_path(PATHS$birds_iucn)) %>%
        dplyr::distinct() %>%
        dplyr::rename(bird_grp = threatened) %>%
        dplyr::filter(as.character(scenarioName) == scenario_csv)
    ) %>%
      dplyr::ungroup() %>%
      dplyr::distinct()

    dungBeetles <- readRDS(resolve_path(PATHS$dung_beetles)) %>%
      dplyr::ungroup() %>%
      dplyr::distinct() %>%
      dplyr::filter(as.character(scenarioName) == scenario_csv)

    megatrees <- readRDS(resolve_path(PATHS$megatrees)) %>%
      as.data.frame() %>%
      dplyr::filter(as.character(scenarioName) == scenario_csv, as.character(height_filt) == "50")

    carbon <- readRDS(resolve_path(PATHS$carbon_stock_diff))
    if (data.table::is.data.table(carbon)) {
      carbon <- carbon[
        scenarioName == scenario_csv &
          window_year_end == 60 &
          twice_logged_slope_trajectory %in% c("1", "1.0")
      ]
    } else {
      carbon <- carbon %>%
        as.data.frame() %>%
        dplyr::filter(
          scenarioName == scenario_csv,
          window_year_end == 60,
          as.character(twice_logged_slope_trajectory) %in% c("1", "1.0")
        )
    }
    carbon <- carbon %>%
      dplyr::mutate(
        window_year_end = as.numeric(window_year_end),
        twice_logged_slope_trajectory = as.character(twice_logged_slope_trajectory)
      )

    birds_j <- prep_join_plantation(birds, propOGcomp) %>%
      dplyr::filter(.data$scenarioName == !!sn, bird_grp == "loser") %>%
      dplyr::mutate(y = medianRelativeOccupancy, ylo = NA_real_, yhi = NA_real_)

    beetles_j <- prep_join_plantation(dungBeetles, propOGcomp) %>%
      dplyr::filter(.data$scenarioName == !!sn, spp_category == "loser") %>%
      dplyr::mutate(y = medianRelativeOccupancy, ylo = NA_real_, yhi = NA_real_)

    meg_j <- prep_join_plantation(megatrees, propOGcomp) %>%
      dplyr::filter(.data$scenarioName == !!sn, height_filt == "50") %>%
      dplyr::mutate(y = landscape_prop, ylo = landscape_prop_lwr95, yhi = landscape_prop_upr95)

    carb_j <- prep_join_plantation(carbon, propOGcomp) %>%
      dplyr::filter(.data$scenarioName == !!sn) %>%
      dplyr::mutate(
        y = mean_cum_stock_diff_vs_baseline / 1e9,
        ylo = lwr80_cum_stock_diff_vs_baseline / 1e9,
        yhi = upr80_cum_stock_diff_vs_baseline / 1e9
      )

    mm <- dplyr::bind_rows(
      minmax_one(birds_j, "Old-growth birds", propOGcomp, p_lo, p_hi, p_tol),
      minmax_one(beetles_j, "Old-growth beetles", propOGcomp, p_lo, p_hi, p_tol),
      minmax_one(meg_j, "Megatrees (% landscape)", propOGcomp, p_lo, p_hi, p_tol),
      minmax_one(carb_j, "Cumulative C (billion MgC)", propOGcomp, p_lo, p_hi, p_tol)
    ) %>%
      dplyr::mutate(outcome = factor(outcome, levels = oc_levels))

    list(
      title = section_title,
      subtitle = section_subtitle,
      wide = wide_from_mm(mm, yield_mm3_p01, yield_mm3_p1),
      detail = detail_from_mm(mm, section_subtitle)
    )
  }

  landscapes <- list(
    list(
      csv = "all_primary_CY_D.csv",
      title = "AllPrimary starting landscape",
      subtitle = "`all_primary_CY_D.csv` — mostly old-growth / primary start"
    ),
    list(
      csv = "mostly_1L_CY_D.csv",
      title = "Mostly once-logged starting landscape",
      subtitle = "`mostly_1L_CY_D.csv` — mostly once-logged start"
    ),
    list(
      csv = "mostly_2L_CY_D.csv",
      title = "Mostly twice-logged starting landscape",
      subtitle = "`mostly_2L_CY_D.csv` — mostly twice-logged start"
    )
  )

  sections <- lapply(landscapes, function(L) {
    build_one_landscape(L$csv, L$title, L$subtitle)
  })

  minmax_detail_all <- dplyr::bind_rows(lapply(sections, `[[`, "detail"))

  notes <- paste(
    "CY_D deforestation-on scenarios only. Birds/beetles: loser group; megatrees: 50 m threshold; carbon: cumulative stock difference vs baseline (60 yr, slope 1), billion MgC.",
    "Within each starting landscape, `production_target` is restricted to ±0.03 of 0.1 and of 1.0, with one grid point per portfolio (closest to band centre).",
    "Min / max scenarios are **global** argmin / argmax by mean outcome in that band. Composition: OG / pl / 1L / 2L as % of national landscape at the snapped grid for those portfolios.",
    "Min / Max scenario rows show the mean and, for megatrees (95%) and carbon (80%), `mean [lo, hi]` on that portfolio; birds/beetles show the mean only.",
    "After each production-target block, **Range (max − min)** is max(mean)−min(mean) in that band; meg/carbon append `(lower / upper)` on that range when model bounds exist. Rows under **Between production targets** compare across *P*≈0.1 vs *P*=1 (Δ in range; median timber).",
    "Median timber uses the same scenario file and ±0.03 bands in `MasterAllScenarios`."
  )

  list(
    sections = sections,
    ## First block = AllPrimary (convenience for quick `L$minmax_wide`).
    minmax_wide = sections[[1]]$wide,
    minmax_detail = sections[[1]]$detail,
    minmax_detail_all = minmax_detail_all,
    notes = notes
  )
}
