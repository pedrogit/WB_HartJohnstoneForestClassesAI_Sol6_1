test_that("six classes preserve geometry, labels and input objects", {
  map <- rast(nrows = 2, ncols = 4, vals = c(10,20,30,40,50,60,0,NA))
  cohorts <- data.table(pixelGroup = c(10,20,20,30,30,40,40,50,60),
                       speciesCode = c("Popu_tre","Popu_tre","Pice_mar","Pice_mar",
                                       "Abie_bal","Pinu_ban","Pinu_con","Lari_lar","Pice_gla"),
                       B = c(100,50,50,50,50,40,60,100,100))
  before <- copy(cohorts)
  beforeMap <- values(map)
  result <- classifyHartJohnstoneForest(cohorts, map)
  expect_equal(as.vector(values(result)), c(1:6, NA, NA))
  expect_true(compareGeom(map, result))
  expect_identical(cohorts, before)
  expect_identical(values(map), beforeMap)
  expect_equal(levels(result)[[1]]$value, 1:6)
  expect_equal(nrow(coltab(result)[[1]]), 6L)
  expect_identical(names(result), "WB_HartJohnstoneForestClassesAI_Sol6_1_Map")
  expect_equal(as.vector(values(classifyHartJohnstoneForest(as.data.frame(cohorts), map))), c(1:6,NA,NA))
})

test_that("inclusive 66 percent dominance, cohort aggregation and residual conifers", {
  map <- rast(nrows=1,ncols=8,vals=1:8)
  cohorts <- data.table(
    pixelGroup=c(1,1,1,2,2,3,3,4,4,5,5,6,6,7,8,8),
    speciesCode=c("Popu_tre","Popu_tre","Pice_mar","Popu_tre","Pice_mar",
                  "Pinu_ban","Popu_tre","Lari_lar","Popu_tre","Pice_gla","Popu_tre",
                  "Pice_gla","Abie_bal","Abie_bal","Popu_tre","Pice_gla"),
    B=c(33,33,34,65.999,34.001,66,34,66,34,66,34,65.999,34.001,100,34,66))
  expect_equal(as.vector(values(classifyHartJohnstoneForest(cohorts,map))), c(1,2,4,5,6,3,3,6))
})

test_that("nonforest is only map-defined and invalid forest data fail explicitly", {
  empty <- data.table(pixelGroup=integer(),speciesCode=character(),B=numeric())
  map <- rast(nrows=1,ncols=3,vals=c(0,NA,NaN))
  out <- classifyHartJohnstoneForest(empty,map)
  expect_true(all(is.na(values(out))))
  expect_equal(nrow(levels(out)[[1]]),6L)
  forest <- rast(nrows=1,ncols=1,vals=1)
  d <- data.table(pixelGroup=1L,speciesCode="Pice_mar",B=100)
  expect_error(classifyHartJohnstoneForest(empty,forest),"without cohorts")
  expect_error(classifyHartJohnstoneForest(transform(d,B=0),forest),"positive total")
  for (bad in c(-1, NA, Inf)) {
    expect_error(classifyHartJohnstoneForest(transform(d,B=bad),forest),"finite, nonnegative")
  }
  expect_error(classifyHartJohnstoneForest(transform(d,speciesCode="unknown"),forest),"Unknown")
  expect_error(classifyHartJohnstoneForest(d,NULL),"single-layer")
  expect_error(classifyHartJohnstoneForest(d,c(forest,forest)),"single-layer")
  expect_error(classifyHartJohnstoneForest(d,init(forest,-1)),"IDs")
  expect_error(classifyHartJohnstoneForest(d,init(forest,1.5)),"IDs")
  expect_error(classifyHartJohnstoneForest(d[, .(pixelGroup,B)],forest),"columns")
  expect_error(classifyHartJohnstoneForest(transform(d,pixelGroup=NA_real_),forest),"IDs")
  expect_equal(as.vector(values(classifyHartJohnstoneForest(
    rbind(d,data.table(pixelGroup=99L,speciesCode="unused",B=100)),forest))),6)
})

test_that("exports preserve categories and QGIS style and never overwrite", {
  map <- rast(nrows=1,ncols=3,vals=c(1,0,NA))
  d <- data.table(pixelGroup=1L,speciesCode="Pice_mar",B=100)
  out <- classifyHartJohnstoneForest(d,map)
  directory <- file.path(scratch,"roundtrip")
  paths <- writeHartJohnstoneForestMap(out,directory,2.5)
  restored <- rast(paths[["tif"]])
  expect_equal(as.vector(values(restored)),c(6,NA,NA))
  expect_true(compareGeom(restored,out))
  expect_equal(unname(as.matrix(terra::cats(restored)[[1]])),
               unname(as.matrix(terra::cats(out)[[1]])))
  palette <- coltab(restored)[[1]]
  expect_equal(unname(as.matrix(palette[match(1:6,palette$value),2:4])),
               unname(as.matrix(coltab(out)[[1]][,2:4])))
  text <- readLines(paths[["qml"]])
  expect_true(any(grepl('type="paletted"',text,fixed=TRUE)))
  entries <- grep("<paletteEntry ",text,value=TRUE)
  expect_length(entries,6L)
  expect_equal(as.integer(sub('.*value="([^"]+)".*','\\1',entries)),1:6)
  expect_equal(sub('.*label="([^"]+)".*','\\1',entries),levels(out)[[1]]$label)
  expect_equal(toupper(sub('.*color="([^"]+)".*','\\1',entries)),
               toupper(rgb(coltab(out)[[1]]$red,coltab(out)[[1]]$green,coltab(out)[[1]]$blue,maxColorValue=255)))
  expect_true(any(grepl('nodataColor="0,0,0,0"',text,fixed=TRUE)))
  hash <- tools::md5sum(paths)
  expect_error(writeHartJohnstoneForestMap(out,directory,2.5),"refusing to overwrite")
  expect_identical(tools::md5sum(paths),hash)
  expect_error(writeHartJohnstoneForestMap(out,directory,NA_real_),"finite")
  # XML is also parsed when an existing parser is available; none is installed here.
  if (requireNamespace("XML",quietly=TRUE)) {
    doc <- XML::xmlParse(paths[["qml"]], useInternalNodes=TRUE)
    expect_length(XML::getNodeSet(doc,"//paletteEntry"),6L)
  }
})
