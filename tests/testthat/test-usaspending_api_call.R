test_that("usaspending_api_call works", {
  x <- usaspending_api_call(unique_id = "CONT_AWD_75D30124F20295_7523_GS00F312CA_4732",type="funding")
  expect_s3_class(x,"data.frame")
})
