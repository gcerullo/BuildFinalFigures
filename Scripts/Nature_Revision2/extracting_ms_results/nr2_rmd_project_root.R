# Resolve BuildFinalFigures root for NR2 R Markdown.
# Works when the Rmd lives under Scripts/Nature_Revision2/ or Scripts/Nature_Revision2/extracting_ms_results/.
# Optional: set environment variable NR2_PROJECT_ROOT to the BuildFinalFigures folder.

nr2_project_root <- function() {
  env <- Sys.getenv("NR2_PROJECT_ROOT", "")
  if (nzchar(env)) {
    r <- normalizePath(env, winslash = "/", mustWork = FALSE)
    if (file.exists(file.path(r, "Inputs", "FixedScenarioParams.R"))) {
      return(r)
    }
  }
  cand <- character()
  in_dir <- tryCatch(knitr::current_input(dir = TRUE), error = function(e) NA_character_)
  if (is.character(in_dir) && nzchar(in_dir) && !is.na(in_dir)) {
    in_dir <- normalizePath(in_dir, winslash = "/", mustWork = FALSE)
    for (k in seq.int(0L, 14L)) {
      p <- if (k == 0L) {
        in_dir
      } else {
        do.call(file.path, c(list(in_dir), as.list(rep("..", k))))
      }
      cand <- c(cand, normalizePath(p, winslash = "/", mustWork = FALSE))
    }
  }
  wd <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  cand <- c(cand, wd)
  for (k in seq_len(12L)) {
    cand <- c(
      cand,
      normalizePath(
        do.call(file.path, c(list(wd), as.list(rep("..", k)))),
        winslash = "/",
        mustWork = FALSE
      )
    )
  }
  cand <- unique(cand[!is.na(cand) & nzchar(cand)])
  for (d in cand) {
    if (file.exists(file.path(d, "Inputs", "FixedScenarioParams.R"))) {
      return(normalizePath(d, winslash = "/"))
    }
  }
  stop(
    "Could not find BuildFinalFigures project root (missing Inputs/FixedScenarioParams.R). ",
    "Knit from the repo or set environment variable NR2_PROJECT_ROOT.",
    call. = FALSE
  )
}
