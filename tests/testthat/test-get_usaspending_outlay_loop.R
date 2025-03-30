M <- readRDS("data/merge_test_data.RDS")
test_that("get_usaspending_outlay_loop works", {
  x <- get_usaspending_outlay_loop(M)
  expect_s3_class(x,"data.frame")
  expect_equal(names(x),c("unique_id", "gross_outlay_amount"))
})
