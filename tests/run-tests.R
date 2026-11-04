# Run from any working directory; verification outputs stay in assistant scratch.
local({
  project <- "G:/Home/Projects/LichenBiomassFullAI"
  module <- file.path(project, "modules", "WB_HartJohnstoneForestClassesAI_Sol6_1")
  old <- .libPaths()
  on.exit(.libPaths(old))
  .libPaths(c(file.path(project, "packages", "4.5"), old))
  library(data.table)
  library(terra)
  library(testthat)
  oldOptions <- options()
  oldThreads <- data.table::getDTthreads()
  oldTerraTemp <- terra::terraOptions(print = FALSE)$tempdir
  on.exit({
    options(oldOptions)
    data.table::setDTthreads(oldThreads)
    terra::terraOptions(tempdir = oldTerraTemp)
  }, add = TRUE)
  source(file.path(module, "R", "classifyHartJohnstoneForest.R"), local = TRUE)
  source(file.path(module, "R", "writeHartJohnstoneForestMap.R"), local = TRUE)
  source(file.path(module, "R", "hartJohnstoneDemoInputs.R"), local = TRUE)
  scratch <- Sys.getenv("HART_TEST_OUTPUT_DIR")
  if (!nzchar(scratch)) stop("Set HART_TEST_OUTPUT_DIR to a disposable test directory.")
  dir.create(scratch, recursive = TRUE, showWarnings = FALSE)
  testthat::test_dir(file.path(module, "tests", "testthat"), env = environment(), stop_on_failure = TRUE)
})
