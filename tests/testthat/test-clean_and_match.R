FPDS <- readRDS("data/fpds_test_data.RDS")
DOGE <- readRDS("data/doge_test_data.RDS")

test_that("clean and match works", {
  x <- clean_and_match(FPDS,DOGE,print_status = F)
  expect_s3_class(x,"data.frame")
  expect_message(clean_and_match(FPDS,DOGE,print_status = F,verbose=T))
  expect_output(clean_and_match(FPDS,DOGE,print_status = T))
})
