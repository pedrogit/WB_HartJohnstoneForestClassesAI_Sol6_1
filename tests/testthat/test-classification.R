test_that("six classes preserve original geometry, categories and standtype names", {
  map <- rast(nrows = 2, ncols = 4, vals = c(10, 20, 30, 40, 50, 60, 0, NA))
  cohorts <- data.table(
    pixelGroup = c(10, 20, 20, 30, 30, 40, 40, 50, 60),
    speciesCode = factor(c("Popu_tre", "Popu_tre", "Pice_mar", "Pice_mar",
                           "Abie_bal", "Pinu_ban", "Pinu_con", "Lari_lar", "Pice_gla")),
    B = c(100, 50, 50, 50, 50, 70, 30, 100, 100)
  )
  result <- classifyHartJohnstoneForest(cohorts, map)
  expect_equal(as.vector(values(result)), c(1:6, NA, NA))
  expect_true(compareGeom(map, result))
  expect_identical(levels(result)[[1]], data.frame(
    ID = 1:6, class = c("deci", "mixed", "conimix", "jackpine", "larch", "spruce")
  ))
  expect_equal(nrow(coltab(result)[[1]]), 6L)
  expect_equal(
    unname(as.matrix(coltab(result)[[1]][, 2:4])),
    unname(t(col2rgb(c("#66c2a5", "#fc8d62", "#8da0cb",
                      "#e78ac3", "#a6d854", "#ffd92f"))))
  )
  expect_identical(names(result), "standtype")
  expect_identical(terra::varnames(result), "standtype")
  expect_equal(as.vector(values(classifyHartJohnstoneForest(
    as.data.frame(cohorts), map
  ))), c(1:6, NA, NA))
})

test_that("default rules aggregate ages without pooling jack pine with other pines", {
  map <- rast(nrows = 1, ncols = 8, vals = 1:8)
  cohorts <- data.table(
    pixelGroup = c(1, 1, 1, 2, 2, 3, 3, 4, 5, 6, 6, 7, 7, 8),
    speciesCode = factor(c(
      "Popu_tre", "Popu_tre", "Pice_mar", "Pinu_ban", "Pinu_con",
      "Lari_lar", "Popu_tre", "Abie_bal", "unknown",
      "Pinu_ban", "Pinu_ban", "Pice_mar", "Pice_gla", NA
    )),
    age = seq_len(14),
    B = c(34, 34, 32, 40, 60, 70, 30, 100, 100, 40, 60, 40, 60, 100)
  )
  expect_equal(as.vector(values(classifyHartJohnstoneForest(cohorts, map))),
               c(1, 3, 3, 3, 1, 4, 6, 1))
})

test_that("two-thirds and 75 percent boundaries use original expressions", {
  map <- rast(nrows = 1, ncols = 3, vals = 1:3)
  epsilon <- 1e-8
  cases <- list(
    list(species = "Pinu_ban", threshold = 2/3, expected = c(2, 4, 4)),
    list(species = "Lari_lar", threshold = 0.75, expected = c(3, 5, 5)),
    list(species = "Pice_mar", threshold = 2/3, expected = c(2, 6, 6)),
    list(species = "Abie_bal", threshold = 2/3, expected = c(2, 3, 3))
  )
  for (case in cases) {
    share <- case$threshold + c(-epsilon, 0, epsilon)
    d <- data.table(
      pixelGroup = rep(1:3, each = 2),
      speciesCode = factor(rep(c(case$species, "Popu_tre"), 3)),
      B = as.vector(rbind(share, 1 - share))
    )
    expect_equal(as.vector(values(classifyHartJohnstoneForest(d, map))),
                 case$expected, info = case$species)
  }
  # Construct the exact strict residual boundary via the original subtraction.
  conifer <- (1 - 2/3) + c(epsilon, 0, -epsilon)
  d <- data.table(
    pixelGroup = rep(1:3, each = 2),
    speciesCode = factor(rep(c("Abie_bal", "Popu_tre"), 3)),
    B = as.vector(rbind(conifer, 1 - conifer))
  )
  expect_equal(as.vector(values(classifyHartJohnstoneForest(d, map))), c(2, 2, 1))
})

test_that("custom prefixes are case-insensitive anchored literal strings", {
  map <- rast(nrows = 1, ncols = 8, vals = 1:8)
  d <- data.table(
    pixelGroup = 1:8,
    speciesCode = factor(c("jP.alpha", "LA+beta", "SP[gamma", "OC?delta",
                           "xJP.alpha", "jPxalpha", "LAAbeta", "Pice_mar")),
    B = 100
  )
  out <- classifyHartJohnstoneForest(
    d, map, jackPineSp = c("JP.", "jp."), larchSp = "la+",
    spruceSp = "sp[", otherConiferSp = "oc?"
  )
  expect_equal(as.vector(values(out)), c(4, 5, 6, 3, 1, 1, 1, 1))
})

test_that("empty species vectors match nothing", {
  map <- rast(nrows = 1, ncols = 4, vals = 1:4)
  d <- data.table(
    pixelGroup = 1:4,
    speciesCode = factor(c("Pinu_ban", "Lari_lar", "Pice_mar", "Abie_bal")),
    B = 100
  )
  expect_equal(as.vector(values(classifyHartJohnstoneForest(
    d, map, jackPineSp = character(), larchSp = character(),
    spruceSp = character(), otherConiferSp = character()
  ))), rep(1, 4))
  expect_equal(as.vector(values(classifyHartJohnstoneForest(
    d, map, jackPineSp = character()
  ))), c(3, 5, 6, 3))
})

test_that("invalid species prefix vectors fail for every group", {
  map <- rast(nrows = 1, ncols = 1, vals = 1)
  d <- data.table(pixelGroup = 1L, speciesCode = factor("Pice_mar"), B = 100)
  invalid <- list(NA_character_, c("Pice", NA_character_), "", " ",
                  " Pice", "Pice ", "\tPice", 1, TRUE, factor("Pice"), list("Pice"))
  for (parameter in c("jackPineSp", "larchSp", "spruceSp", "otherConiferSp")) {
    for (prefixes in invalid) {
      arguments <- c(list(cohortData = d, pixelGroupMap = map),
                     setNames(list(prefixes), parameter))
      expect_error(do.call(classifyHartJohnstoneForest, arguments), parameter)
    }
  }
})

test_that("overlapping species groups use first match without double counting", {
  map <- rast(nrows = 1, ncols = 1, vals = 1)
  d <- data.table(
    pixelGroup = 1L, speciesCode = factor(c("Shared_tree", "Popu_tre")),
    B = c(60, 40)
  )
  expect_equal(as.vector(values(classifyHartJohnstoneForest(
    d, map, jackPineSp = "shared", larchSp = "SHARED",
    spruceSp = "Shared", otherConiferSp = "shared"
  ))), 2)
  d <- d[1]
  d$B <- 100
  for (first in 1:4) {
    groups <- rep(list(character()), 4)
    groups[first:4] <- rep(list("shared"), 5 - first)
    names(groups) <- c("jackPineSp", "larchSp", "spruceSp", "otherConiferSp")
    out <- do.call(classifyHartJohnstoneForest,
                   c(list(cohortData = d, pixelGroupMap = map), groups))
    expect_equal(as.vector(values(out)), c(4, 5, 6, 3)[first])
  }
})

test_that("zero IDs classify with cohorts and missing map groups remain NA", {
  map <- rast(nrows = 1, ncols = 5, vals = c(0, 1, 2, 3, NA))
  d <- data.table(
    pixelGroup = c(0L, 1L, 2L, 99L),
    speciesCode = factor(c("Pinu_ban", "Pice_mar", "unknown", "unused")),
    B = c(100, 0, 100, 100)
  )
  expect_equal(as.vector(values(classifyHartJohnstoneForest(d, map))),
               c(4, 2, 1, NA, NA))
  expect_equal(as.vector(values(classifyHartJohnstoneForest(d[-1], map))),
               c(NA, 2, 1, NA, NA))
})

test_that("empty cohorts and all-missing rasters retain complete metadata", {
  empty <- data.table(pixelGroup = integer(), speciesCode = factor(), B = numeric())
  populated <- data.table(pixelGroup = 1L, speciesCode = factor("Pice_mar"), B = 100)
  maps <- list(
    rast(nrows = 1, ncols = 3, vals = c(0, 1, NA)),
    rast(nrows = 1, ncols = 3, vals = c(NA, NaN, NA))
  )
  for (map in maps) {
    out <- classifyHartJohnstoneForest(empty, map)
    expect_equal(as.vector(values(out)), rep(NA_real_, 3))
    expect_equal(levels(out)[[1]]$ID, 1:6)
    expect_equal(nrow(coltab(out)[[1]]), 6L)
    expect_identical(names(out), "standtype")
    expect_true(compareGeom(map, out))
  }
  expect_equal(as.vector(values(classifyHartJohnstoneForest(populated, maps[[2]]))),
               rep(NA_real_, 3))
})

test_that("structural and finite nonnegative biomass guards remain explicit", {
  forest <- rast(nrows = 1, ncols = 1, vals = 1)
  d <- data.table(pixelGroup = 1L, speciesCode = factor("Pice_mar"), B = 100)
  for (bad in list(-1, NA_real_, NaN, Inf, -Inf, "100", factor("100"))) {
    malformed <- copy(d)
    malformed$B <- bad
    expect_error(classifyHartJohnstoneForest(malformed, forest), "finite, nonnegative")
  }
  expect_error(classifyHartJohnstoneForest(d, NULL), "single-layer")
  expect_error(classifyHartJohnstoneForest(d, c(forest, forest)), "single-layer")
  expect_error(classifyHartJohnstoneForest(d, rast(nrows = 1, ncols = 1)), "cell values")
  for (bad in c(-1, 1.5, Inf)) {
    expect_error(classifyHartJohnstoneForest(d, init(forest, bad)), "IDs")
  }
  expect_error(classifyHartJohnstoneForest(d[, .(pixelGroup, B)], forest), "columns")
  expect_error(classifyHartJohnstoneForest(list(), forest), "columns")
  for (bad in list(NA_real_, -1, 1.5, Inf, "1")) {
    malformed <- copy(d)
    malformed$pixelGroup <- bad
    expect_error(classifyHartJohnstoneForest(malformed, forest), "IDs")
  }
  malformed <- copy(d)
  malformed$speciesCode <- as.character(malformed$speciesCode)
  expect_error(classifyHartJohnstoneForest(malformed, forest), "factor")
})

test_that("classification preserves keyed tables, factors, extra columns and raster inputs", {
  map <- rast(nrows = 1, ncols = 3, vals = c(0, 2, NA))
  names(map) <- "original_groups"
  d <- data.table(
    pixelGroup = c(2L, 0L),
    speciesCode = factor(c("Pice_mar", "Pinu_ban"),
                         levels = c("unused", "Pinu_ban", "Pice_mar")),
    B = c(100, 100), age = c(40L, 80L), speciesNb = c(91L, 92L)
  )
  setkey(d, pixelGroup)
  before <- copy(d)
  mapValues <- values(map)
  mapNames <- names(map)
  mapVariables <- terra::varnames(map)
  mapCategories <- levels(map)
  mapColors <- coltab(map)
  for (input in list(d, as.data.frame(d))) {
    inputBefore <- copy(input)
    out <- classifyHartJohnstoneForest(input, map)
    expect_equal(as.vector(values(out)), c(4, 6, NA))
    expect_identical(input, inputBefore)
  }
  expect_identical(d, before)
  expect_identical(key(d), "pixelGroup")
  expect_identical(values(map), mapValues)
  expect_identical(names(map), mapNames)
  expect_identical(terra::varnames(map), mapVariables)
  expect_identical(levels(map), mapCategories)
  expect_identical(coltab(map), mapColors)
})

test_that("exports preserve categories and QGIS style and never overwrite", {
  map <- rast(nrows=1,ncols=3,vals=c(1,0,NA))
  d <- data.table(pixelGroup=1L,speciesCode=factor("Pice_mar"),B=100)
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
  expect_equal(sub('.*label="([^"]+)".*','\\1',entries),levels(out)[[1]]$class)
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
