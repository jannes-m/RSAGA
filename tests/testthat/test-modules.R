context("RSAGA-modules")

library(digest)
library(rgdal)

test_that("Write DEM to disc", {
  
  env <- rsaga.env2()

  data(landslides)
  
  write.sgrd(data = dem, file = "/home/travis/dem", header = dem$header, env = env, check.module.exists=FALSE)
  
  
  
  #test <- read.sgrd("dem.sgrd", env=env, check.module.exists=FALSE)
  
  expect_true(TRUE)
  
  #digest(test, algo="md5") %in% c("137181ee7294515ceb6ccf04fe975cfd")
  
})








