#' Explicitly construct demonstration inputs; never called automatically by the module
#' @param cohortData Optional supplied cohorts in LandR format.
#' @param pixelGroupMap Optional supplied single-layer community map.
#' @return List containing supplied objects unchanged and synthetic missing inputs.
#'   With neither supplied, a six-cell EPSG:3005 example covers all six classes.
hartJohnstoneDemoInputs <- function(cohortData = NULL, pixelGroupMap = NULL) {
  if (is.null(pixelGroupMap)) {
    if (is.null(cohortData)) {
      ids <- 1:6
    } else {
      if (!is.data.frame(cohortData) || !"pixelGroup" %in% names(cohortData) ||
          !is.numeric(cohortData$pixelGroup) || any(!is.finite(cohortData$pixelGroup) |
            cohortData$pixelGroup < 0 | cohortData$pixelGroup != trunc(cohortData$pixelGroup))) {
        stop("Cannot build a demo map: supplied cohortData requires valid integer pixelGroup IDs.")
      }
      ids <- sort(unique(cohortData$pixelGroup[cohortData$pixelGroup > 0]))
    }
    n <- max(1L, length(ids))
    pixelGroupMap <- terra::rast(nrows = 1, ncols = n, xmin = 1000000,
                                xmax = 1000000 + 100 * n, ymin = 1000000,
                                ymax = 1000100, crs = "EPSG:3005",
                                vals = if (length(ids)) ids else 0)
    names(pixelGroupMap) <- "pixelGroupMap"
  }
  if (is.null(cohortData)) {
    if (!inherits(pixelGroupMap, "SpatRaster") || terra::nlyr(pixelGroupMap) != 1L ||
        !terra::hasValues(pixelGroupMap)) {
      stop("Cannot build demo cohorts: supply a valued single-layer SpatRaster.")
    }
    ids <- terra::unique(pixelGroupMap)[[1L]]
    ids <- ids[!is.na(ids)]
    if (any(!is.finite(ids) | ids < 0 | ids != trunc(ids))) {
      stop("Cannot build demo cohorts: map IDs must be nonnegative integers or NA.")
    }
    ids <- sort(ids[ids > 0])
    recipes <- data.table::data.table(
      recipe = c(1L, 2L, 2L, 3L, 3L, 4L, 4L, 5L, 6L),
      speciesCode = c("Popu_tre", "Popu_tre", "Pice_mar", "Pice_mar", "Abie_bal",
                      "Pinu_ban", "Pinu_con", "Lari_lar", "Pice_gla"),
      B = c(100, 50, 50, 50, 50, 80, 20, 100, 100)
    )
    groups <- data.table::data.table(pixelGroup = ids, recipe = (seq_along(ids) - 1L) %% 6L + 1L)
    cohortData <- merge(groups, recipes, by = "recipe", allow.cartesian = TRUE)
    cohortData[, recipe := NULL]
    cohortData[, speciesCode := factor(speciesCode)]
    data.table::setcolorder(cohortData, c("pixelGroup", "speciesCode", "B"))
  }
  list(cohortData = cohortData, pixelGroupMap = pixelGroupMap)
}
