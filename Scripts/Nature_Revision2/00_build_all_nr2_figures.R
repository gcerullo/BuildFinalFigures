# ----------------------------------------------------------------------------
# Nature Revision 2 — batch figure build
#
# I use this as a one-shot driver: it calls the three NR2 figure builders in order so I do not have to remember the sequence.
# Inputs: project root with Inputs/FixedScenarioParams.R; NR2 RDS inputs expected by the child scripts under Data/NR2 (and related paths they define).
# Outputs: Figures/NR2/PetalPlots, Figures/NR2/GeomPoint_Modified, Figures/NR2/GeomPoint_Replicable (written by the sourced scripts).
# ----------------------------------------------------------------------------

## Batch-build all Nature Revision 2 figure outputs under Figures/NR2/
## -----------------------------------------------------------------
## Petals:        Figures/NR2/PetalPlots          (03_build_petalplot_replicable.R)
## Geom (mod):    Figures/NR2/GeomPoint_Modified  (02_build_modified_final_figures.R)
## Geom (base):   Figures/NR2/GeomPoint_Replicable (02_BuildMultipanelPerformanceFigure_replicable.R)
##
## From the BuildFinalFigures project root:
##   Rscript Scripts/Nature_Revision_2/00_build_all_nr2_figures.R
##
## Child scripts honour RUN_ALL=TRUE via the environment (set below).
## Optional: NR2_PROJECT_ROOT=/path/to/BuildFinalFigures if not inferable from --file=.
## -----------------------------------------------------------------

args <- commandArgs(FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
root <- Sys.getenv("NR2_PROJECT_ROOT", unset = "")
if (!nzchar(root) && length(file_arg)) {
  script_path <- sub("^--file=", "", file_arg[[1]])
  root <- normalizePath(file.path(dirname(normalizePath(script_path)), "..", ".."))
}
if (!nzchar(root)) {
  root <- normalizePath(getwd())
}
if (!file.exists(file.path(root, "Inputs", "FixedScenarioParams.R"))) {
  stop(
    "Could not find project root (Inputs/FixedScenarioParams.R). ",
    "Run from BuildFinalFigures or set NR2_PROJECT_ROOT."
  )
}
setwd(root)
message("NR2 batch: project root = ", getwd())
## Child scripts read ACTIVE_RUN / INCLUDE_LEGEND from the process environment.
## Clear stale values so each script uses its own RUN_ALL defaults (e.g. petal vs geom run keys differ).
Sys.unsetenv(c("PRODUCTION_TARGET", "ACTIVE_RUN", "INCLUDE_LEGEND"))

scripts <- c(
  "Scripts/Nature_Revision_2/03_build_petalplot_replicable.R",
  "Scripts/Nature_Revision_2/02_build_modified_final_figures.R",
  "Scripts/Nature_Revision_2/02_BuildMultipanelPerformanceFigure_replicable.R"
)
for (rel in scripts) {
  full <- normalizePath(rel, winslash = "/", mustWork = TRUE)
  message("\n========== NR2 batch: ", basename(full), " ==========")
  Sys.setenv(RUN_ALL = "true", MODE = "build")
  ## Petal script also reads MODE; geom scripts ignore it.
  status <- system2(Sys.which("Rscript"), args = c("--vanilla", shQuote(full)), stdout = "", stderr = "")
  Sys.unsetenv(c("RUN_ALL", "MODE"))
  if (!is.null(status) && status != 0L) {
    stop("NR2 batch failed on ", rel, " (exit ", status, ")")
  }
}

message("\nNR2 batch complete. Outputs:")
message("  - ", file.path(getwd(), "Figures", "NR2", "PetalPlots"))
message("  - ", file.path(getwd(), "Figures", "NR2", "GeomPoint_Modified"))
message("  - ", file.path(getwd(), "Figures", "NR2", "GeomPoint_Replicable"))
