context("RSAGA-modules")

library(digest)
library(rgdal)
env <- rsaga.env2()

test_that("Write DEM to disc", {

  data(landslides)
  write.sgrd(data = dem, file = "dem", header = dem$header, env = env, 
             check.module.exists=FALSE)
  
  test <- read.sgrd("dem.sgrd", env=env, check.module.exists=FALSE)
  
  expect_true(digest(test, algo="md5") %in% c("137181ee7294515ceb6ccf04fe975cfd"))
  
})

test_that("Slope", {
  
  rsaga.slope.asp.curv("dem", out.slope = "slope", out.cprof = "cprof", 
                       out.cplan = "cplan", method = "poly2zevenbergen",
                       env = env, check.module.exists=FALSE) 
  
  test <- read.sgrd("slope.sgrd", env=env, check.module.exists=FALSE)
  
  expect_true(digest(test, algo="md5") %in% c("b2852c8fa289636908e1322ee33c3c0b",
                                              "b1b0c8c02db274cad530d2021abd7032"))
})

test_that("Fill Sinks", {
  
  rsaga.fill.sinks("dem", out.dem= "dem_fill", method="planchon.darboux.2001", 
                   env=env, check.module.exists=FALSE)
  
  test <- read.sgrd("dem_fill.sgrd", env=env, check.module.exists=FALSE)
  
  expect_true(digest(test, algo="md5") %in% c("b2852c8fa289636908e1322ee33c3c0b",
                                              "7b67ea5c169b07dd6965c88aa22a48ee"))
})

test_that("Sink Route", {
  
  rsaga.sink.route("dem", out.sink= "sink_route",  env=env, 
                   check.module.exists=FALSE)
  
  test <- read.sgrd("sink_route.sgrd", env=env, check.module.exists=FALSE)
  
  expect_true(digest(test, algo="md5") %in% c("1f538bc0a415dc127c69d882e191cafc"))
})

test_that("Sink Removal", {
  
  rsaga.sink.removal("dem", in.sinkroute = "sink_route",  
                     out.dem= "removed_sinks", env=env, check.module.exists=FALSE)
  
  test <- read.sgrd("removed_sinks.sgrd", env=env, check.module.exists=FALSE)
  
  expect_true(digest(test, algo="md5") %in% c("5feb7dbe2d3e947557d38f194ad37310", 
                                              "b8f14181cdad9bb304c3c0a7e889afe7"))
})

test_that("Close Gaps", {
  
  rsaga.close.gaps("dem", out.dem= "close_gaps", env=env, check.module.exists=FALSE)
  
  test <- read.sgrd("close_gaps.sgrd", env=env, check.module.exists=FALSE)
  
  expect_true(digest(test, algo="md5") %in% c("f110d3106bc8c54554b97b6642a39978", 
                                              "b8f14181cdad9bb304c3c0a7e889afe7", 
                                              "fedc13e7fe041b8a1d7dae5e5fbb729d"
                                              ))
})

test_that("Hillshade", {
  
  rsaga.hillshade("dem", out.grid= "hillshade", exaggeration=10, env=env, 
                  check.module.exists=FALSE)
  
  test <- read.sgrd("hillshade.sgrd", env=env, check.module.exists=FALSE)
  
  expect_true(digest(test, algo="md5") %in% c("ff369e210cc14ac36eeafca86003f5c0"))
})

test_that("PISR2", {
  
  rsaga.pisr2(in.dem = "dem", out.direct.grid = "pisr2", out.diffuse.grid = "pisrdur2_diffuse",
              latitude = 43, unit = "kWh/m2", method = "lumped",
              lmp.transmittance = 60, time.range = c(0,24), time.step = 3,
              start.date = list(day=1,month=10,year=2016), end.date = list(day=6,month=12,year=2016),
              day.step = 10, env = env, show = FALSE, check.module.exists=FALSE)
  
  test <- read.sgrd("pisr2.sgrd", env=env, check.module.exists=FALSE)
  
  expect_true(digest(test, algo="md5") %in% c("ef3290bb060d890a4c07430c7ef9bf24",
                                              "4249dcc756f3bcc80d05e5224959e66d",
                                              "e204a7c8ff803575b0e4c0371600038b"))
})

test_that("Topdown Processing", {
  
  rsaga.topdown.processing(in.dem = "dem", out.carea = "carea", env=env, 
                           check.module.exists=FALSE)
  
  test <- read.sgrd("carea.sgrd", env=env, check.module.exists=FALSE)

  expect_true(digest(test, algo="md5") %in% c("39b364b3605a1422ff6c96a1c5b60319" ,
                                              "6fdcd2d52bc35870600aa1505bb49791"
  ))
})

test_that("Wetness Index", {
  
  rsaga.wetness.index(in.dem="dem",out.wetness.index="swi", env=env, 
                      check.module.exists=FALSE)
  
  test <- read.sgrd("swi.sgrd", env=env, check.module.exists=FALSE)
  
  expect_true(digest(test, algo="md5") %in% c("98fb807d7ec0bf787de6dc4fa88ca35d",
                                              "6d96e79007f8ffd64aeb69111ee052f5"
  ))
})

test_that("Grid Calculus", {
  
  rsaga.grid.calculus(c("dem", "dem"), out.grid = "calculus", formula = "a + b", 
                      env=env, check.module.exists=FALSE)
  
  test <- read.sgrd("calculus.sgrd", env=env, check.module.exists=FALSE)
  
  expect_true(digest(test, algo="md5") %in% c("ca98e329dbd22d6cd310271f55f60740",
                                              "cb20edb9c5a71d88fb1af9c63c4c33d8"
  ))
})

test_that("Contour", {
  
  rsaga.contour("dem", out.shapefile = "countour", zstep=5, env=env, 
                check.module.exists=FALSE)
  
  test <- readOGR(".", "countour")
  
  expect_true(digest(test, algo="md5") %in% c("de4bfbe3b89fca5c831d14fc65498f10"
  ))
})


test_that("Grid to Points Randomly", {
  
  rsaga.grid.to.points.randomly(in.grid = "dem", out.shapefile = "grid_to_points_randomly", 
                                freq = 50, env = env, check.module.exists=FALSE)
  
  test <- readOGR(".", "grid_to_points_randomly")
  
  expect_equal(typeof(test@data$VALUE[1]), "double")
  
  
})






