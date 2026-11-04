test_that("synthetic defaults cover six classes and preserve partial supplied inputs", {
  defaults <- hartJohnstoneDemoInputs()
  expect_equal(as.vector(values(classifyHartJohnstoneForest(defaults$cohortData, defaults$pixelGroupMap))), 1:6)
  cohorts <- data.table(pixelGroup=c(30L,90L),speciesCode=c("Pice_mar","Lari_lar"),B=100)
  before <- copy(cohorts)
  partial <- hartJohnstoneDemoInputs(cohortData=cohorts)
  expect_identical(cohorts,before)
  expect_identical(partial$cohortData,before)
  expect_equal(as.vector(values(partial$pixelGroupMap)),c(30,90))
  expect_equal(as.vector(values(classifyHartJohnstoneForest(partial$cohortData,partial$pixelGroupMap))),c(6,5))
  map <- rast(nrows=1,ncols=4,vals=c(10,20,0,NA))
  beforeMap <- values(map)
  partial <- hartJohnstoneDemoInputs(pixelGroupMap=map)
  expect_identical(values(partial$pixelGroupMap),beforeMap)
  expect_equal(sort(unique(partial$cohortData$pixelGroup)),c(10,20))
  expect_equal(as.vector(values(classifyHartJohnstoneForest(partial$cohortData,map))),c(1,2,NA,NA))
})

test_that("standalone demo runs, respects supplied inputs and uses fallback only when needed", {
  library(SpaDES.core)
  oldOptions <- options(spades.useRequire=FALSE,Require.install=FALSE,Require.offlineMode=TRUE)
  on.exit(options(oldOptions))
  makeSim <- function(name, objects=list(), demo=TRUE, interval=2) {
    root <- file.path(scratch,name)
    simInit(times=list(start=3,end=7,timeunit="year"),
            modules="WB_HartJohnstoneForestClassesAI_Sol6_1",
            params=list(WB_HartJohnstoneForestClassesAI_Sol6_1=list(demoMode=demo,demoSuccessionTimestep=interval)),
            objects=objects,
            paths=list(modulePath=dirname(module),inputPath=file.path(root,"inputs"),
                       outputPath=file.path(root,"outputs"),cachePath=file.path(root,"cache")))
  }
  sim <- spades(makeSim("demo-empty"))
  expect_equal(as.vector(values(sim$WB_HartJohnstoneForestClassesAI_Sol6_1_Map)),1:6)
  files <- list.files(file.path(scratch,"demo-empty","outputs"),pattern="\\.tif$",full.names=TRUE)
  expect_length(files,3)
  expect_true(all(grepl("_time_(3|5|7)\\.tif$",files)))
  for (file in files) expect_equal(as.vector(values(rast(file))),1:6)
  d <- data.table(pixelGroup=88L,speciesCode="Pice_mar",B=100)
  map <- rast(nrows=1,ncols=1,vals=88)
  for (mode in c("cohorts","map","both")) {
    supplied <- switch(mode,cohorts=list(cohortData=d),map=list(pixelGroupMap=map),
                       both=list(cohortData=d,pixelGroupMap=map))
    sim <- spades(makeSim(paste0("demo-",mode),supplied))
    if(mode!="map") expect_identical(sim$cohortData,d)
    if(mode!="cohorts") expect_identical(values(sim$pixelGroupMap),values(map))
    expect_equal(as.vector(values(sim$WB_HartJohnstoneForestClassesAI_Sol6_1_Map)),if(mode=="map") 1 else 6)
  }
  sim <- makeSim("demo-core-priority",list(cohortData=d,pixelGroupMap=map),interval=0)
  params(sim)$Biomass_core <- list(successionTimestep=4)
  sim <- spades(sim)
  expect_length(list.files(file.path(scratch,"demo-core-priority","outputs"),pattern="\\.tif$"),2)
  expect_error(spades(makeSim("demo-bad-interval",interval=0)),"finite positive")
  expect_error(spades(makeSim("demo-off",list(cohortData=d,pixelGroupMap=map),demo=FALSE)),"finite positive")
  expect_error(spades(makeSim("demo-inconsistent",list(cohortData=d,pixelGroupMap=init(map,99)))),"without cohorts")
})

test_that("time-zero initialization saves a baseline even without succession events", {
  library(SpaDES.core)
  oldOptions <- options(spades.useRequire=FALSE,Require.install=FALSE,Require.offlineMode=TRUE)
  on.exit(options(oldOptions))
  root <- file.path(scratch,"initial-zero")
  map <- rast(nrows=1,ncols=8,vals=c(1:6,0,NA))
  sim <- simInit(times=list(start=0,end=0,timeunit="year"),
                 modules="WB_HartJohnstoneForestClassesAI_Sol6_1",
                 params=list(WB_HartJohnstoneForestClassesAI_Sol6_1=list(demoMode=TRUE)),
                 objects=list(pixelGroupMap=map),
                 paths=list(modulePath=dirname(module),inputPath=file.path(root,"inputs"),
                            outputPath=file.path(root,"outputs"),cachePath=file.path(root,"cache")))
  sim <- spades(sim)
  file <- file.path(root,"outputs","WB_HartJohnstoneForestClassesAI_Sol6_1_Map_time_0.tif")
  expect_true(file.exists(file))
  expect_true(file.exists(sub("\\.tif$",".qml",file)))
  expect_equal(as.vector(values(rast(file))),c(1:6,NA,NA))
  expect_equal(as.vector(values(sim$WB_HartJohnstoneForestClassesAI_Sol6_1_Map)),c(1:6,NA,NA))
  expect_length(list.files(file.path(root,"outputs"),pattern="\\.tif$"),1)
})
