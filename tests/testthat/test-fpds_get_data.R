piidsold <- c("9523ZY21P0045","9531CB24A0009")
piidsnew <- c("9523ZY21P0045","9531CB24A0009","9531CB25P0012")

d <- fpds_get_data(piidsold,verbose=F)

test_that("fpds_get_data() successfully captures data",{
  expect_type(d,"list")
})

test_that("fpds_get_data() returns two data.frames", {
  expect_s3_class(d[[1]],"data.frame")
  expect_s3_class(d[[2]],"data.frame")
})

test_that("fpds_stats() returns output", {
  expect_output(fpds_stats(d))
})

test_that("fpds_get_new() properly captures new data",{
  n <- fpds_get_new(piidsnew,d)
  expect_type(n,"list")
  
})

