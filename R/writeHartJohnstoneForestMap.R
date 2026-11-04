#' Export a classified raster and its matching QGIS palette style
#' @param forestMap Raster returned by classifyHartJohnstoneForest.
#' @param outputDirectory Existing or creatable simulation output directory.
#' @param simulationTime Finite scalar simulation time, used in unique filenames.
#' @param filenamePrefix Safe basename distinguishing this module's exports.
#' @return Invisibly, the GeoTIFF and QML paths. Existing outputs are never replaced.
writeHartJohnstoneForestMap <- function(forestMap, outputDirectory, simulationTime,
                                      filenamePrefix = "WB_HartJohnstoneForestClassesAI_Sol6_1_Map") {
  if (!is.character(filenamePrefix) || length(filenamePrefix) != 1L ||
      is.na(filenamePrefix) || !grepl("^[A-Za-z0-9_-]+$", filenamePrefix)) {
    stop("filenamePrefix must be a nonempty safe basename.")
  }
  if (!is.numeric(simulationTime) || length(simulationTime) != 1L || !is.finite(simulationTime)) {
    stop("simulationTime must be a finite numeric scalar.")
  }
  if (!dir.exists(outputDirectory) && !dir.create(outputDirectory, recursive = TRUE)) {
    stop("Cannot create output directory: ", outputDirectory)
  }
  stamp <- sprintf("%.17g", simulationTime)
  base <- file.path(outputDirectory, paste0(filenamePrefix, "_time_", stamp))
  tif <- paste0(base, ".tif")
  qml <- paste0(base, ".qml")
  if (any(file.exists(c(tif, qml, paste0(tif, ".aux.xml"))))) {
    stop("Output already exists; refusing to overwrite: ", base)
  }
  legend <- terra::cats(forestMap)[[1L]]
  colors <- terra::coltab(forestMap)[[1L]]
  colors <- colors[match(legend[[1L]], colors[[1L]]), ]
  if (nrow(legend) != 6L || anyNA(colors)) stop("Raster lacks the complete six-class palette.")
  hex <- grDevices::rgb(colors[[2L]], colors[[3L]], colors[[4L]], maxColorValue = 255)
  # Legend labels are controlled ASCII strings without XML metacharacters.
  entries <- sprintf('      <paletteEntry value="%s" color="%s" alpha="255" label="%s"/>',
                     legend[[1L]], hex, legend[[2L]])
  xml <- c('<?xml version="1.0" encoding="UTF-8"?>',
           '<!DOCTYPE qgis PUBLIC "http://mrcc.com/qgis.dtd" "SYSTEM">',
           '<qgis version="3.34" styleCategories="Symbology">',
           '  <pipe>',
           '    <rasterrenderer type="paletted" band="1" opacity="1" alphaBand="-1" nodataColor="0,0,0,0">',
           '      <rasterTransparency/>', '      <colorPalette>', entries,
           '      </colorPalette>', '    </rasterrenderer>', '  </pipe>', '</qgis>')
  terra::writeRaster(forestMap, tif, datatype = "INT1U", NAflag = 255,
                     gdal = c("COMPRESS=LZW"), overwrite = FALSE)
  writeLines(xml, qml, useBytes = TRUE)
  invisible(c(tif = tif, qml = qml))
}
