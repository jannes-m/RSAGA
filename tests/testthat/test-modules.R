context("RSAGA-modules")

library(digest)
library(rgdal)

test_that("Write DEM to disc", {
  
  env <- rsaga.env2()

  data(landslides)
  write.sgrd(data = dem, file = "dem", header = dem$header, env = env, 
             check.module.exists=FALSE)
  
  test <- read.sgrd("dem.sgrd", env=env, check.module.exists=FALSE)
  
  expect_true(digest(test, algo="md5") %in% c("137181ee7294515ceb6ccf04fe975cfd"))
  
})

test_that("Slope", {
  
  env <- rsaga.env2()
  
  rsaga.slope.asp.curv("dem", out.slope = "slope", out.cprof = "cprof", 
                       out.cplan = "cplan", method = "poly2zevenbergen",
                       env = env, check.module.exists=FALSE) 
  
  test <- read.sgrd("slope.sgrd", env=env, check.module.exists=FALSE)
  
  expect_true(digest(test, algo="md5") %in% c("b2852c8fa289636908e1322ee33c3c0b",
                                              "b1b0c8c02db274cad530d2021abd7032"))
})








