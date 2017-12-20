context("RSAGA-modules")

library(digest)
library(rgdal)

test_that("Write DEM to disc", {
  
  env <- rsaga.env2()

  data(landslides)
  
  write.sgrd(data = dem, file = file.path(tempdir(), "dem.sgrd"), header = dem$header, env = env, check.module.exists=FALSE)
  
  test <- read.sgrd(file.path(tempdir(), "dem.sgrd"), env=env, check.module.exists=FALSE)
  
  expect_true(digest(test, algo="md5") %in% c("137181ee7294515ceb6ccf04fe975cfd"))
  
})

test_that("Slope", {
  
  env <- rsaga.env2()
  
  rsaga.slope.asp.curv(file.path(tempdir(), "dem.sgrd"), out.slope = file.path(tempdir(), "slope.sgrd"), method = "poly2zevenbergen",
                       env = env, check.module.exists=FALSE) 
  
  test <- read.sgrd(file.path(tempdir(), "slope.sgrd"), env=env, check.module.exists=FALSE)
  
  expect_true(digest(test, algo="md5") %in% c("b2852c8fa289636908e1322ee33c3c0b",
                                              "b1b0c8c02db274cad530d2021abd7032"))
})







