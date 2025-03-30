test_that("get_usaspending_outlay works", {
  x <- get_usaspending_outlay("CONT_AWD_05GA0A22P0013_0559_-NONE-_-NONE-")
  y <- get_usaspending_outlay("CONT_AWD_05GA0A22P0013_0559_-NONE-_-NONE-",returnTable = T)
  expect_type(x,"double")
  expect_s3_class(y,"data.frame")
})
