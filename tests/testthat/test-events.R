copyModuleFixture <- function(module, destination) {
  target <- file.path(destination, basename(module))
  dir.create(target, recursive = TRUE, showWarnings = FALSE)
  stopifnot(all(file.copy(list.files(module, full.names = TRUE), target, recursive = TRUE)))
}

test_that("actual SpaDES events synchronize to succession and save post-update maps", {
  library(SpaDES.core)
  oldOptions <- options(spades.useRequire=FALSE, Require.install=FALSE, Require.offlineMode=TRUE)
  on.exit(options(oldOptions))
  for (interval in c(1,2)) {
    root <- file.path(scratch,paste0("events-",interval))
    dir.create(file.path(root,"outputs"),recursive=TRUE,showWarnings=FALSE)
    dir.create(file.path(root,"modules"),recursive=TRUE,showWarnings=FALSE)
    copyModuleFixture(module,file.path(root,"modules"))
    file.copy(file.path(module,"tests","fixtures","Biomass_core"),file.path(root,"modules"),recursive=TRUE)
    sim <- simInit(times=list(start=3,end=7,timeunit="year"),
                   modules=c("Biomass_core","WB_HartJohnstoneForestClassesAI_Sol6_1"),
                   params=list(Biomass_core=list(successionTimestep=interval),
                     WB_HartJohnstoneForestClassesAI_Sol6_1=list(.saveInitialTime=3)),
                   objects=list(cohortData=data.table(pixelGroup=1L,speciesCode=factor("Pice_mar"),B=100),
                                pixelGroupMap=rast(nrows=1,ncols=1,vals=1)),
                   paths=list(modulePath=file.path(root,"modules"),inputPath=file.path(root,"inputs"),
                              outputPath=file.path(root,"outputs"),cachePath=file.path(root,"cache")))
    sim <- spades(sim)
    times <- c(3,seq(3+interval,7,by=interval))
    files <- list.files(file.path(root,"outputs"),pattern="\\.tif$",full.names=TRUE)
    expect_length(files,length(times))
    expect_length(list.files(file.path(root,"outputs"),pattern="\\.csv$"),length(times))
    expect_equal(sort(basename(files)),sort(paste0("WB_HartJohnstoneForestClassesAI_Sol6_1_Map_time_",times,".tif")))
    for (i in seq_along(files)) {
      file <- files[[i]]
      expect_equal(as.vector(values(rast(file))),c(6,1,rep(5,length(files)-2L))[[i]])
      expect_true(file.exists(sub("\\.tif$",".qml",file)))
    }
    expect_equal(as.vector(values(sim$WB_HartJohnstoneForestClassesAI_Sol6_1_Map)),5)
  }
})

test_that("first classification follows time-zero growth and summary without weakening validation", {
  library(SpaDES.core)
  oldOptions <- options(spades.useRequire=FALSE, Require.install=FALSE, Require.offlineMode=TRUE)
  on.exit(options(oldOptions))
  root <- file.path(scratch, "initial-zero-biomass")
  dir.create(file.path(root, "modules"), recursive=TRUE, showWarnings=FALSE)
  copyModuleFixture(module, file.path(root, "modules"))
  file.copy(file.path(module, "tests", "fixtures", "Biomass_core"), file.path(root, "modules"), recursive=TRUE)
  sim <- simInit(times=list(start=0, end=2, timeunit="year"),
    modules=c("Biomass_core", "WB_HartJohnstoneForestClassesAI_Sol6_1"),
    params=list(Biomass_core=list(successionTimestep=2, removeInitialZeroBiomass=TRUE),
      WB_HartJohnstoneForestClassesAI_Sol6_1=list(.saveInitialTime=0)),
    objects=list(cohortData=data.table(pixelGroup=1:2, speciesCode=factor(c("Pice_mar","Pinu_con")),
      age=c(10L,0L), B=c(100,0)), pixelGroupMap=rast(nrows=1,ncols=2,vals=1:2)),
    paths=list(modulePath=file.path(root,"modules"), inputPath=file.path(root,"inputs"),
      outputPath=file.path(root,"outputs"), cachePath=file.path(root,"cache")))
  sim <- spades(sim)
  done <- as.data.frame(completed(sim))
  initial <- done[done$eventTime == 0, ]
  classification <- which(initial$moduleName == "WB_HartJohnstoneForestClassesAI_Sol6_1" &
    initial$eventType == "reComputeHJForestClassesMap")
  expect_length(classification, 1)
  expect_equal(initial$eventPriority[classification], 8.5)
  expect_true(which(initial$eventType == "mortalityAndGrowth") < classification)
  expect_true(which(initial$eventType == "summaryBGM") < classification)
  expect_true(sim$fixtureSummaryCompleted)
  expect_false(2L %in% sim$cohortData$pixelGroup)
  expect_equal(as.vector(values(sim$pixelGroupMap)), c(1,0))
  first <- rast(file.path(root,"outputs","WB_HartJohnstoneForestClassesAI_Sol6_1_Map_time_0.tif"))
  expect_equal(as.vector(values(first)), c(6,NA_real_))
  expect_length(list.files(file.path(root,"outputs"), pattern="\\.tif$"), 2)
  expect_length(list.files(file.path(root,"outputs"), pattern="\\.csv$"), 2)
})

test_that("standalone classification defaults to ten years and TIFF saving is optional", {
  library(SpaDES.core)
  oldOptions <- options(spades.useRequire=FALSE, Require.install=FALSE, Require.offlineMode=TRUE)
  on.exit(options(oldOptions))
  root <- file.path(scratch,"standalone-default-events")
  sim <- simInit(times=list(start=3,end=24,timeunit="year"),
    modules="WB_HartJohnstoneForestClassesAI_Sol6_1",
    objects=hartJohnstoneDemoInputs(),
    paths=list(modulePath=dirname(module),inputPath=file.path(root,"inputs"),
      outputPath=file.path(root,"outputs"),cachePath=file.path(root,"cache")))
  p <- params(sim)$WB_HartJohnstoneForestClassesAI_Sol6_1
  expected <- list(.plots="screen", .plotInitialTime=NA, .plotInterval=NA,
    WB_HJForestClassesTimeStep=NA_real_, .saveInitialTime=NA, .saveInterval=NA,
    .useCache=FALSE, jackPineSp="Pinu_ban", larchSp="Lari", spruceSp="Pice",
    otherConiferSp=c("Abie","Calc","Cham","Juni","Pinu","Pseu","Sequ","Taxu","Thuj","Tsug"))
  for (name in names(expected)) expect_equal(p[[name]],expected[[name]])
  expect_false(any(c("demoMode","demoSuccessionTimestep","exportDirectory") %in% names(p)))
  sim <- spades(sim)
  done <- as.data.frame(completed(sim))
  events <- done[done$moduleName == "WB_HartJohnstoneForestClassesAI_Sol6_1" &
    done$eventType == "reComputeHJForestClassesMap", ]
  expect_equal(events$eventTime,c(3,13,23))
  expect_true(all(events$eventPriority == 8.5))
  expect_equal(names(sim$WB_HartJohnstoneForestClassesAI_Sol6_1_Map),"standtype")
  expect_length(list.files(file.path(root,"outputs"),pattern="\\.csv$"),3)
  expect_length(list.files(file.path(root,"outputs"),pattern="\\.(tif|qml)$"),0)
})

test_that("species overrides are forwarded and save timing is independent of classification", {
  library(SpaDES.core)
  oldOptions <- options(spades.useRequire=FALSE, Require.install=FALSE, Require.offlineMode=TRUE)
  on.exit(options(oldOptions))
  root <- file.path(scratch,"species-and-save-events")
  d <- data.table(pixelGroup=1:4,speciesCode=factor(c("Jack","Lar","Spr","Other")),B=100)
  sim <- simInit(times=list(start=3,end=7,timeunit="year"),
    modules="WB_HartJohnstoneForestClassesAI_Sol6_1",
    params=list(WB_HartJohnstoneForestClassesAI_Sol6_1=list(
      WB_HJForestClassesTimeStep=2,jackPineSp="Jack",larchSp="Lar",
      spruceSp="Spr",otherConiferSp="Other",.saveInitialTime=4,.saveInterval=3)),
    objects=list(cohortData=d,pixelGroupMap=rast(nrows=1,ncols=4,vals=1:4)),
    paths=list(modulePath=dirname(module),inputPath=file.path(root,"inputs"),
      outputPath=file.path(root,"outputs"),cachePath=file.path(root,"cache")))
  sim <- spades(sim)
  expect_equal(as.vector(values(sim$WB_HartJohnstoneForestClassesAI_Sol6_1_Map)),c(4,5,6,3))
  expect_length(list.files(file.path(root,"outputs"),pattern="\\.csv$"),3)
  expect_equal(sort(list.files(file.path(root,"outputs"),pattern="\\.tif$")),
    paste0("WB_HartJohnstoneForestClassesAI_Sol6_1_Map_time_",c(4,7),".tif"))
  done <- as.data.frame(completed(sim))
  saves <- done[done$eventType == "save", ]
  expect_equal(saves$eventTime,c(4,7))
  expect_true(all(saves$eventPriority == 9))
})

test_that("explicit classification intervals reject core mismatch and invalid values", {
  library(SpaDES.core)
  oldOptions <- options(spades.useRequire=FALSE, Require.install=FALSE, Require.offlineMode=TRUE)
  on.exit(options(oldOptions))
  makeSim <- function(name, interval) {
    root <- file.path(scratch,name)
    sim <- simInit(times=list(start=3,end=3,timeunit="year"),
      modules="WB_HartJohnstoneForestClassesAI_Sol6_1",
      params=list(WB_HartJohnstoneForestClassesAI_Sol6_1=list(WB_HJForestClassesTimeStep=interval)),
      objects=hartJohnstoneDemoInputs(),
      paths=list(modulePath=dirname(module),inputPath=file.path(root,"inputs"),
        outputPath=file.path(root,"outputs"),cachePath=file.path(root,"cache")))
    params(sim)$Biomass_core <- list(successionTimestep=2)
    sim
  }
  expect_error(spades(makeSim("interval-mismatch",3)),"must match")
  expect_error(spades(makeSim("interval-invalid",0)),"finite and positive")
})
