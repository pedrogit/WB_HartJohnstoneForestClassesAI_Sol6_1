#' Classify LandR forest communities using relative cohort biomass
#'
#' Adapts Hart et al. (2019), Table 1 and section 2.4, using an inclusive
#' 66 percent biomass threshold. All Pinus species are pooled; Larix is conifer.
#' Other conifer-dominated groups (including pure fir) use residual class 3.
#' @param cohortData A data.frame/data.table with pixelGroup, speciesCode (LandR
#'   convention), and nonnegative finite B (all cohorts, in consistent units).
#' @param pixelGroupMap Single-layer terra SpatRaster of integer community IDs.
#'   Zero and missing cells become NA; positive IDs require positive biomass.
#' @return A categorical SpatRaster with codes 1:6, six labels and a color table.
#'   Geometry and inputs are unchanged. No outputs are written to disk.
#' @export
classifyHartJohnstoneForest <- function(cohortData, pixelGroupMap) {
  if (!inherits(pixelGroupMap, "SpatRaster") || terra::nlyr(pixelGroupMap) != 1L) {
    stop("pixelGroupMap must be a single-layer terra SpatRaster.")
  }
  required <- c("pixelGroup", "speciesCode", "B")
  if (!is.data.frame(cohortData) || !all(required %in% names(cohortData))) {
    stop("cohortData must contain pixelGroup, speciesCode and B columns.")
  }
  if (!terra::hasValues(pixelGroupMap)) stop("pixelGroupMap has no cell values.")
  ids <- terra::unique(pixelGroupMap)[[1L]]
  ids <- ids[!is.na(ids)]
  if (any(!is.finite(ids) | ids < 0 | ids != trunc(ids))) {
    stop("pixelGroupMap IDs must be nonnegative finite integers or NA.")
  }
  ids <- ids[ids > 0]
  dt <- data.table::as.data.table(cohortData)[, required, with = FALSE]
  if (!is.numeric(dt$pixelGroup) || any(!is.finite(dt$pixelGroup) |
      dt$pixelGroup < 0 | dt$pixelGroup != trunc(dt$pixelGroup))) {
    stop("cohortData pixelGroup IDs must be nonnegative finite integers.")
  }
  if (!is.numeric(dt$B) || any(!is.finite(dt$B) | dt$B < 0)) {
    stop("cohortData B must be finite, nonnegative numeric biomass.")
  }
  dt <- dt[pixelGroup %in% ids]
  missingGroups <- setdiff(ids, dt$pixelGroup)
  if (length(missingGroups)) {
    stop("Positive map IDs without cohorts: ", paste(head(missingGroups, 10L), collapse = ", "))
  }
  if (length(ids)) {
    # Load package data without attaching LandR or its unrelated dependencies.
    taxonomy <- new.env(parent = baseenv())
    utils::data("sppEquivalencies_CA", package = "LandR", envir = taxonomy)
    spp <- as.data.frame(taxonomy$sppEquivalencies_CA)
    if (!all(c("LandR", "Latin_full", "Type") %in% names(spp))) {
      stop("LandR species equivalencies lack the required taxonomy columns.")
    }
    # Varieties may share a LandR code; only conflicting classification matters.
    spp$genus <- sub(" .*", "", spp$Latin_full)
    spp <- unique(spp[, c("LandR", "genus", "Type")])
    used <- unique(as.character(dt$speciesCode[dt$B > 0]))
    ambiguous <- spp$LandR[duplicated(spp$LandR)]
    if (any(used %in% ambiguous)) stop("Ambiguous LandR taxonomy for speciesCode.")
    index <- match(as.character(dt$speciesCode), spp$LandR)
    type <- spp$Type[index]
    unknown <- dt$B > 0 & (is.na(index) | is.na(type) | !type %in% c("Conifer", "Deciduous"))
    if (any(unknown)) {
      stop("Unknown positive-biomass speciesCode: ",
           paste(unique(as.character(dt$speciesCode[unknown])), collapse = ", "))
    }
    genus <- spp$genus[index]
    dt[, `:=`(treeType = type, genus = genus)]
    totals <- dt[, list(
      total = sum(as.numeric(B)),
      deciduous = sum(as.numeric(B[!is.na(treeType) & treeType == "Deciduous"])),
      conifer = sum(as.numeric(B[!is.na(treeType) & treeType == "Conifer"])),
      pine = sum(as.numeric(B[!is.na(genus) & genus == "Pinus"])),
      larch = sum(as.numeric(B[!is.na(genus) & genus == "Larix"])),
      spruce = sum(as.numeric(B[!is.na(genus) & genus == "Picea"]))
    ), by = pixelGroup]
    if (any(!is.finite(totals$total) | totals$total <= 0)) {
      stop("Every positive map ID requires finite, positive total biomass.")
    }
    # A relative tolerance protects exact 66% boundaries, not nearby mixtures.
    threshold <- 0.66 - 8 * .Machine$double.eps
    totals[, class := 2L]
    totals[conifer / total >= threshold, class := 3L]
    totals[deciduous / total >= threshold, class := 1L]
    totals[pine / total >= threshold, class := 4L]
    totals[larch / total >= threshold, class := 5L]
    totals[spruce / total >= threshold, class := 6L]
    lookup <- rbind(c(0, NA_real_), as.matrix(totals[, c("pixelGroup", "class"), with = FALSE]))
    result <- terra::classify(pixelGroupMap, lookup, others = NA)
  } else {
    result <- terra::init(terra::rast(pixelGroupMap), NA_real_)
  }
  names(result) <- "WB_HartJohnstoneForestClassesAI_Sol6_1_Map"
  legend <- data.frame(value = 1:6, label = c(
    "Deciduous", "Conifer-deciduous mix", "Conifer mix",
    "Pine (jack pine + other pines)", "Larch", "Spruce"
  ))
  result <- terra::categories(result, value = legend)
  palette <- grDevices::col2rgb(c("#E5C849", "#B5BD68", "#668C70", "#B5793A", "#D997C5", "#34675C"))
  terra::coltab(result) <- data.frame(value = 1:6, red = palette[1L, ],
                                     green = palette[2L, ], blue = palette[3L, ], alpha = 255L)
  names(result) <- "WB_HartJohnstoneForestClassesAI_Sol6_1_Map"
  result
}
