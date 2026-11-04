# Developed with AI assistance; ecological adaptations are documented in README.md.
defineModule(sim, list(
  name = "WB_HartJohnstoneForestClassesAI_Sol6_1",
  description = "Hart-Johnstone forest classes adapted to LandR cohort biomass, with QGIS-ready succession outputs.",
  keywords = c("forest classification", "biomass", "Hart", "Johnstone"),
  authors = person("Project", "Contributors", role = c("aut", "cre")),
  childModules = character(0),
  version = list(WB_HartJohnstoneForestClassesAI_Sol6_1 = numeric_version("0.1.3")),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.md", "WB_HartJohnstoneForestClassesAI_Sol6_1.Rmd"),
  loadOrder = list(after = "Biomass_core"),
  reqdPkgs = list("SpaDES.core", "data.table", "terra", "LandR"),
  parameters = rbind(
    defineParameter("demoMode", "logical", FALSE, NA, NA,
                    "Explicitly enable synthetic defaults for missing inputs; never replace supplied inputs."),
    defineParameter("demoSuccessionTimestep", "numeric", 1, 0, NA,
                    "Fallback interval in years, used only in demoMode when no Biomass_core timestep is supplied."),
    defineParameter("exportDirectory", "character", "", NA, NA,
                    "Optional classifier-only export directory; empty uses outputPath(sim).")
  ),
  inputObjects = rbind(
    expectsInput("cohortData", "data.table", "LandR cohorts: pixelGroup, speciesCode and B, summed over all ages.", sourceURL = NA_character_),
    expectsInput("pixelGroupMap", "SpatRaster", "LandR community IDs; zero and missing cells become NoData.", sourceURL = NA_character_)
  ),
  outputObjects = rbind(
    createsOutput("WB_HartJohnstoneForestClassesAI_Sol6_1_Map", "SpatRaster",
                  "Six categorical biomass-based forest classes, with labels and palette; exported at initialization and each succession timestep.")
  )
))

.inputObjects <- function(sim) {
  if (!isTRUE(P(sim)$demoMode)) return(invisible(sim))
  hasCohorts <- suppliedElsewhere("cohortData", sim)
  hasMap <- suppliedElsewhere("pixelGroupMap", sim)
  if (!hasCohorts || !hasMap) {
    defaults <- hartJohnstoneDemoInputs(
      cohortData = if (hasCohorts) sim$cohortData else NULL,
      pixelGroupMap = if (hasMap) sim$pixelGroupMap else NULL
    )
    if (!hasCohorts) sim$cohortData <- defaults$cohortData
    if (!hasMap) sim$pixelGroupMap <- defaults$pixelGroupMap
    message("demoMode: created synthetic demonstration inputs only where not supplied elsewhere.")
  }
  invisible(sim)
}

doEvent.WB_HartJohnstoneForestClassesAI_Sol6_1 <- function(sim, eventTime, eventType, debug = FALSE) {
  moduleName <- "WB_HartJohnstoneForestClassesAI_Sol6_1"
  outputName <- paste0(moduleName, "_Map")
  switch(eventType,
    init = {
      interval <- params(sim)$Biomass_core$successionTimestep
      if (is.null(interval) && isTRUE(P(sim)$demoMode)) {
        interval <- P(sim)$demoSuccessionTimestep
        message("demoMode: using demoSuccessionTimestep; outputs are for demonstration, not inference.")
      }
      if (!is.numeric(interval) || length(interval) != 1L || !is.finite(interval) || interval <= 0) {
        stop("A finite positive Biomass_core successionTimestep is required, or enable demoMode with a valid demoSuccessionTimestep.")
      }
      mod$successionTimestep <- interval
      mod$successionIndex <- 0L
      # SpaDES schedules init at priority 1; defer classification until after core growth and summary.
      sim <- scheduleEvent(sim, time(sim), moduleName, "classifyAndSave", eventPriority = 8.5)
    },
    classifyAndSave = {
      sim[[outputName]] <- classifyHartJohnstoneForest(sim$cohortData, sim$pixelGroupMap)
      destination <- P(sim)$exportDirectory
      if (!nzchar(destination)) destination <- outputPath(sim)
      writeHartJohnstoneForestMap(sim[[outputName]], destination, time(sim))
      mod$successionIndex <- mod$successionIndex + 1L
      nextTime <- start(sim) + mod$successionIndex * mod$successionTimestep
      if (nextTime <= end(sim)) {
        sim <- scheduleEvent(sim, nextTime, moduleName, "classifyAndSave", eventPriority = 8.5)
      }
    },
    stop("Unknown WB_HartJohnstoneForestClassesAI_Sol6_1 event: ", eventType)
  )
  invisible(sim)
}
