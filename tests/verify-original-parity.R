# Development verification only; the original is not a runtime dependency.
verifyOriginalParity <- function(originalModule, sol6Module, snapshots, outputDirectory) {
  if (dir.exists(outputDirectory)) stop("Use a new output directory.")
  dir.create(outputDirectory, recursive = TRUE)
  original <- new.env(parent = globalenv())
  revised <- new.env(parent = globalenv())
  sys.source(file.path(originalModule, "R/WB_HartJohnstoneForestClasses.r"), original)
  sys.source(file.path(sol6Module, "R/classifyHartJohnstoneForest.R"), revised)
  rows <- list()
  compare <- function(a, b, variableName = TRUE) {
    x <- as.numeric(terra::values(a)); y <- as.numeric(terra::values(b))
    stopifnot(terra::compareGeom(a, b), identical(is.na(x), is.na(y)),
              identical(x[!is.na(x)], y[!is.na(y)]),
              identical(terra::cats(a), terra::cats(b)),
              identical(terra::coltab(a), terra::coltab(b)),
              identical(names(a), names(b)))
    if (variableName) stopifnot(identical(terra::varnames(a), terra::varnames(b)))
    invisible(TRUE)
  }
  run <- function(label, cohorts, map, parameters = list(), roundtrip = FALSE) {
    baseline <- data.table::copy(cohorts)
    mapValues <- terra::values(map)
    folder <- file.path(outputDirectory, label)
    dir.create(folder)
    csvA <- file.path(folder, "original.csv"); csvB <- file.path(folder, "sol6.csv")
    a <- do.call(original$classifyStand, c(list(
      cohortData = data.table::copy(cohorts), pixelGroupMap = terra::deepcopy(map),
      drainageMap = NULL, classificationTablePath = csvA), parameters))
    copied <- data.table::copy(cohorts)
    b <- do.call(revised$classifyHartJohnstoneForest, c(list(
      cohortData = copied, pixelGroupMap = terra::deepcopy(map),
      classificationTablePath = csvB), parameters))
    compare(a, b)
    stopifnot(isTRUE(all.equal(copied, baseline)), isTRUE(all.equal(cohorts, baseline)),
              identical(terra::values(map), mapValues),
              identical(readLines(csvA), readLines(csvB)))
    if (roundtrip) {
      terra::writeRaster(a, file.path(folder, "original.tif"), datatype = "INT1U")
      terra::writeRaster(b, file.path(folder, "sol6.tif"), datatype = "INT1U")
      ra <- terra::rast(file.path(folder, "original.tif"))
      rb <- terra::rast(file.path(folder, "sol6.tif"))
      # GeoTIFF restores varnames from differing file basenames, not raster data.
      compare(ra, rb, variableName = FALSE)
      x <- as.numeric(terra::values(a)); y <- as.numeric(terra::values(ra))
      stopifnot(terra::compareGeom(a, ra), identical(is.na(x), is.na(y)),
                identical(x[!is.na(x)], y[!is.na(y)]),
                identical(unname(as.matrix(terra::cats(a)[[1L]])),
                          unname(as.matrix(terra::cats(ra)[[1L]]))))
      colorsA <- terra::coltab(a)[[1L]]; colorsR <- terra::coltab(ra)[[1L]]
      stopifnot(identical(unname(as.matrix(colorsA[, -1L])),
                          unname(as.matrix(colorsR[match(1:6, colorsR[[1L]]), -1L]))))
    }
    rows[[length(rows) + 1L]] <<- data.frame(
      case = label, cells = terra::ncell(map), mapped = sum(!is.na(terra::values(a))),
      mismatches = 0L, metadata = "identical", csv = "identical", roundtrip = roundtrip)
    invisible(NULL)
  }
  map <- terra::rast(nrows = 2, ncols = 6, vals = c(0:9, 99, NA))
  cohorts <- data.table::data.table(
    pixelGroup = c(0, 1, 2, 3, 4, 5, 6, 6, 7, 7, 8, 8, 9, 100),
    speciesCode = factor(c("Pinu_ban", "Pinu_con", "Lari_lar", "Pice_mar", "Abie_las",
                           "Unknown", "Pice_mar", "Popu_tre", "Lari_lar", "Popu_tre",
                           "Pice_mar", "Popu_tre", NA, "unused")),
    B = c(100, 100, 100, 100, 100, 100, 2, 1, 3, 1, 0, 0, 100, 100))
  configurations <- list(
    defaults = list(), allPine = list(jackPineSp = "Pinu"),
    empty = list(jackPineSp = character(), larchSp = character(),
                 spruceSp = character(), otherConiferSp = character()),
    overlap = list(jackPineSp = "p", larchSp = "P", spruceSp = "P", otherConiferSp = "P"),
    literal = list(jackPineSp = "Pinu.*", larchSp = "^Lari", spruceSp = "pIcE"))
  for (label in names(configurations)) run(paste0("synthetic-", label), cohorts, map, configurations[[label]])
  for (parameter in c("jackPineSp", "larchSp", "spruceSp", "otherConiferSp")) {
    run(paste0("empty-", parameter), cohorts, map, setNames(list(character()), parameter))
  }
  # Independent cohorts exercise both sides of each original threshold.
  boundary <- data.table::rbindlist(lapply(seq_along(c(2/3 - 1e-8, 2/3, 2/3 + 1e-8, .70, .75 - 1e-8, .75, .75 + 1e-8)), function(i) {
    share <- c(2/3 - 1e-8, 2/3, 2/3 + 1e-8, .70, .75 - 1e-8, .75, .75 + 1e-8)[i]
    data.table::rbindlist(lapply(seq_along(c("Pinu_ban", "Lari_lar", "Pice_mar", "Abie_las", "Popu_tre")), function(j) {
      data.table::data.table(pixelGroup = (i - 1L) * 5L + j,
        speciesCode = c(c("Pinu_ban", "Lari_lar", "Pice_mar", "Abie_las", "Popu_tre")[j],
                        if (j == 5L) "Abie_las" else "Popu_tre"), B = c(share, 1 - share))
    }))
  }))
  boundary[, speciesCode := factor(speciesCode)]
  run("thresholds", boundary, terra::rast(nrows = 5, ncols = 7, vals = 1:35))
  run("all-missing", cohorts, terra::init(map, NA_real_))
  manifest <- utils::read.csv(file.path(snapshots, "artifact_hashes.csv"))
  selected <- grepl("/(cohortData.rds|pixelGroupMap.tif)$", manifest$path)
  manifest <- manifest[selected, ]
  stopifnot(nrow(manifest) == 6L,
            identical(unname(tools::md5sum(file.path(snapshots, manifest$path))), manifest$md5))
  utils::write.csv(manifest, file.path(outputDirectory, "input_hashes.csv"), row.names = FALSE)
  for (year in c(0, 50, 100)) {
    folder <- file.path(snapshots, sprintf("year_%03d", year))
    d <- readRDS(file.path(folder, "cohortData.rds"))
    m <- terra::rast(file.path(folder, "pixelGroupMap.tif"))
    run(sprintf("year-%03d-defaults", year), d, m, roundtrip = TRUE)
    run(sprintf("year-%03d-all-pine", year), d, m, list(jackPineSp = "Pinu"), roundtrip = TRUE)
  }
  result <- do.call(rbind, rows)
  utils::write.csv(result, file.path(outputDirectory, "comparisons.csv"), row.names = FALSE)
  files <- c(list.files(originalModule, recursive = TRUE, full.names = TRUE),
             list.files(sol6Module, recursive = TRUE, full.names = TRUE))
  files <- files[!file.info(files)$isdir]
  utils::write.csv(data.frame(path = files, md5 = unname(tools::md5sum(files))),
                   file.path(outputDirectory, "source_hashes.csv"), row.names = FALSE)
  saveRDS(list(session = sessionInfo(), defaults = formals(revised$classifyHartJohnstoneForest),
               configurations = configurations, drainage = "disabled"),
          file.path(outputDirectory, "provenance.rds"))
  result
}
