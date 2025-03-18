test_that("doge_get_data() works properly", {
  d <- doge_get_data(verbose=F)
  
  expect_length(d,3)
  expect_s3_class(d$contracts,"data.frame")
  expect_s3_class(d$grants,"data.frame")
  expect_s3_class(d$leases,"data.frame")
  
  expect_output(doge_summarize(d))
})

