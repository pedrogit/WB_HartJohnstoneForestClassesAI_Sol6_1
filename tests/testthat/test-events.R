test_that("actual SpaDES events synchronize to succession and save post-update maps", {
  library(SpaDES.core)
  oldOptions <- options(spades.useRequire=FALSE, Require.install=FALSE, Require.offlineMode=TRUE)
  on.exit(options(oldOptions))
  for (interval in c(1,2)) {
    root <- file.path(scratch,paste0("events-",interval))
    dir.create(file.path(root,"outputs"),recursive=TRUE,showWarnings=FALSE)
    dir.create(file.path(root,"modules"),recursive=TRUE,showWarnings=FALSE)
    file.copy(module,file.path(root,"modules"),recursive=TRUE)
    file.copy(file.path(module,"tests","fixtures","Biomass_core"),file.path(root,"modules"),recursive=TRUE)
    sim <- simInit(times=list(start=3,end=7,timeunit="year"),
                   modules=c("Biomass_core","WB_HartJohnstoneForestClassesAI_Sol6_1"),
                   params=list(Biomass_core=list(successionTimestep=interval)),
                   objects=list(cohortData=data.table(pixelGroup=1L,speciesCode="Pice_mar",B=100),
                                pixelGroupMap=rast(nrows=1,ncols=1,vals=1)),
                   paths=list(modulePath=file.path(root,"modules"),inputPath=file.path(root,"inputs"),
                              outputPath=file.path(root,"outputs"),cachePath=file.path(root,"cache")))
    sim <- spades(sim)
    times <- c(3,seq(3+interval,7,by=interval))
    files <- list.files(file.path(root,"outputs"),pattern="\\.tif$",full.names=TRUE)
    expect_length(files,length(times))
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
  file.copy(module, file.path(root, "modules"), recursive=TRUE)
  file.copy(file.path(module, "tests", "fixtures", "Biomass_core"), file.path(root, "modules"), recursive=TRUE)
  sim <- simInit(times=list(start=0, end=2, timeunit="year"),
    modules=c("Biomass_core", "WB_HartJohnstoneForestClassesAI_Sol6_1"),
    params=list(Biomass_core=list(successionTimestep=2, removeInitialZeroBiomass=TRUE),
      WB_HartJohnstoneForestClassesAI_Sol6_1=list(exportDirectory=file.path(root,"exports"))),
    objects=list(cohortData=data.table(pixelGroup=1:2, speciesCode=c("Pice_mar","Pinu_con"),
      age=c(10L,0L), B=c(100,0)), pixelGroupMap=rast(nrows=1,ncols=2,vals=1:2)),
    paths=list(modulePath=file.path(root,"modules"), inputPath=file.path(root,"inputs"),
      outputPath=file.path(root,"outputs"), cachePath=file.path(root,"cache")))
  sim <- spades(sim)
  done <- as.data.frame(completed(sim))
  initial <- done[done$eventTime == 0, ]
  classification <- which(initial$moduleName == "WB_HartJohnstoneForestClassesAI_Sol6_1" &
    initial$eventType == "classifyAndSave")
  expect_length(classification, 1)
  expect_equal(initial$eventPriority[classification], 8.5)
  expect_true(which(initial$eventType == "mortalityAndGrowth") < classification)
  expect_true(which(initial$eventType == "summaryBGM") < classification)
  expect_true(sim$fixtureSummaryCompleted)
  expect_false(2L %in% sim$cohortData$pixelGroup)
  expect_equal(as.vector(values(sim$pixelGroupMap)), c(1,0))
  first <- rast(file.path(root,"exports","WB_HartJohnstoneForestClassesAI_Sol6_1_Map_time_0.tif"))
  expect_equal(as.vector(values(first)), c(6,NA_real_))
  expect_length(list.files(file.path(root,"exports"), pattern="\\.tif$"), 2)
  expect_length(list.files(file.path(root,"outputs"), pattern="\\.tif$"), 0)
})
