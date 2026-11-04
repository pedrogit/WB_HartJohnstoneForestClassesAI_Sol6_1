# Developed with AI assistance; ecological adaptations are documented in README.md.
defineModule(sim, list(
  name = "WB_HartJohnstoneForestClassesAI_Sol6_1",
  description = "Hart-Johnstone forest classes adapted to LandR cohort biomass, with QGIS-ready succession outputs.",
  keywords = c("forest classification", "biomass", "Hart", "Johnstone"),
  authors = person("Project", "Contributors", role = c("aut", "cre")),
  childModules = character(0),
  version = list(WB_HartJohnstoneForestClassesAI_Sol6_1 = numeric_version("0.2.0")),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.md", "WB_HartJohnstoneForestClassesAI_Sol6_1.Rmd"),
  loadOrder = list(after = "Biomass_core"),
  reqdPkgs = list("SpaDES.core", "SpaDES.tools", "data.table", "terra"),
  parameters = rbind(
    defineParameter(".plots", "character", "screen", NA, NA, "Used by Plot."),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "First plot time; NA disables plotting."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "Plot interval; NA follows classification."),
    defineParameter("WB_HJForestClassesTimeStep", "numeric", NA_real_, NA, NA,
                    "NA follows core succession (10 years standalone); explicit values must match core."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "First save time; NA disables TIFF/QML saving."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "Save interval; NA follows classification."),
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Whether to cache events or the module."),
    defineParameter("jackPineSp", "character", c("Pinu_ban"), NA, NA, "Literal case-insensitive jack-pine prefixes."),
    defineParameter("larchSp", "character", c("Lari"), NA, NA, "Literal case-insensitive larch prefixes."),
    defineParameter("spruceSp", "character", c("Pice"), NA, NA, "Literal case-insensitive spruce prefixes."),
    defineParameter("otherConiferSp", "character",
                    c("Abie", "Calc", "Cham", "Juni", "Pinu", "Pseu", "Sequ", "Taxu", "Thuj", "Tsug"),
                    NA, NA, "Additional conifer prefixes, applied after jack pine, larch and spruce."),
    defineParameter("writeClassificationCsv", "logical", FALSE, NA, NA,
                    "Whether to write a classification summary CSV in the output path at each classification.")
  ),
  inputObjects = rbind(
    expectsInput("cohortData", "data.table", "LandR cohorts: pixelGroup, speciesCode and B, summed over all ages.", sourceURL = NA_character_),
    expectsInput("pixelGroupMap", "SpatRaster", "LandR community IDs; IDs without cohorts become NoData.", sourceURL = NA_character_)
  ),
  outputObjects = rbind(
    createsOutput("WB_HartJohnstoneForestClassesAI_Sol6_1_Map", "SpatRaster",
                  "Six categorical biomass-based forest classes; standtype layer and optional CSV and TIFF/QML outputs.")
  )
))

.inputObjects <- function(sim) {
  for (input in c("cohortData", "pixelGroupMap")) {
    if (is.null(sim[[input]]) && !suppliedElsewhere(input, sim)) {
      stop(input, " must be supplied; construct demonstration inputs explicitly with hartJohnstoneDemoInputs().")
    }
  }
  invisible(sim)
}

doEvent.WB_HartJohnstoneForestClassesAI_Sol6_1 <- function(sim, eventTime, eventType, debug = FALSE) {
  moduleName <- "WB_HartJohnstoneForestClassesAI_Sol6_1"
  outputName <- paste0(moduleName, "_Map")
  switch(eventType,
    init = {
      if (is.null(sim$cohortData) || is.null(sim$pixelGroupMap))
        stop("cohortData and pixelGroupMap must be supplied before classification initialization.")
      interval <- P(sim)$WB_HJForestClassesTimeStep
      coreInterval <- params(sim)$Biomass_core$successionTimestep
      if (is.na(interval)) interval <- if (is.null(coreInterval)) 10 else coreInterval
      if (!is.numeric(interval) || length(interval) != 1L || !is.finite(interval) || interval <= 0) {
        stop("Classification interval must be finite and positive.")
      }
      if (!is.null(coreInterval) && !isTRUE(all.equal(interval, as.numeric(coreInterval))))
        stop("WB_HJForestClassesTimeStep must match Biomass_core successionTimestep.")
      mod$classificationInterval <- interval
      mod$classificationIndex <- 0L
      # SpaDES schedules init at priority 1; defer classification until after core growth and summary.
      sim <- scheduleEvent(sim, time(sim), moduleName, "reComputeHJForestClassesMap", eventPriority = 8.5)
      if (!is.na(P(sim)$.plotInitialTime))
        sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, moduleName, "plot", eventPriority = 8.75)
      if (!is.na(P(sim)$.saveInitialTime))
        sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, moduleName, "save", eventPriority = 9)
    },
    reComputeHJForestClassesMap = {
      csv <- NULL
      if (isTRUE(P(sim)$writeClassificationCsv)) {
        csv <- file.path(outputPath(sim), paste0(moduleName, "_", sprintf("%03d", time(sim)), ".csv"))
        if (file.exists(csv)) stop("Output already exists; refusing to overwrite: ", csv)
        dir.create(outputPath(sim), recursive = TRUE, showWarnings = FALSE)
      }
      sim[[outputName]] <- classifyHartJohnstoneForest(
        sim$cohortData, sim$pixelGroupMap,
        jackPineSp = P(sim)$jackPineSp, larchSp = P(sim)$larchSp,
        spruceSp = P(sim)$spruceSp, otherConiferSp = P(sim)$otherConiferSp,
        classificationTablePath = csv)
      mod$classificationIndex <- mod$classificationIndex + 1L
      nextTime <- start(sim) + mod$classificationIndex * mod$classificationInterval
      if (nextTime <= end(sim)) {
        sim <- scheduleEvent(sim, nextTime, moduleName, "reComputeHJForestClassesMap", eventPriority = 8.5)
      }
    },
    plot = {
      Plot(sim[[outputName]])
      interval <- if (is.na(P(sim)$.plotInterval)) mod$classificationInterval else P(sim)$.plotInterval
      if (!is.finite(interval) || interval <= 0) stop("Plot interval must be finite and positive.")
      sim <- scheduleEvent(sim, time(sim) + interval, moduleName, "plot", eventPriority = 8.75)
    },
    save = {
      writeHartJohnstoneForestMap(sim[[outputName]], outputPath(sim), time(sim), filenamePrefix = outputName)
      interval <- if (is.na(P(sim)$.saveInterval)) mod$classificationInterval else P(sim)$.saveInterval
      if (!is.finite(interval) || interval <= 0) stop("Save interval must be finite and positive.")
      sim <- scheduleEvent(sim, time(sim) + interval, moduleName, "save", eventPriority = 9)
    },
    stop("Unknown WB_HartJohnstoneForestClassesAI_Sol6_1 event: ", eventType)
  )
  invisible(sim)
}
