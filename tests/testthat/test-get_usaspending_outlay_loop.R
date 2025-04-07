M <- readRDS("data/merge_test_data.RDS")
test_that("get_usaspending_outlay_loop works", {
  x <- get_usaspending_outlay_loop(M,progress_style = 0)
  expect_type(x,"list")
  expect_equal(names(x),c("AWARDS_OUTLAY","IDVS_OUTLAY","ASST_OUTLAY"))
})
