a <- data.frame(a=c(1:10),b=c(rep("A",10)),c=as.Date("2000-01-01"),d=as.logical(rep(TRUE)))
b <- data.frame(lapply(a,as.character))
c <- data.frame(x="X",y=NA,z=0)
d <- a
names(d) <- c("a","wut","c","d")


test_that("match_col_types proprly returns matched data", {
  x <- match_col_types(a,b)
  expect_type(x,"list")
  
  a.a <- class(a[[1]])
  a.b <- class(a[[2]])
  a.c <- class(a[[3]])
  a.d <- class(a[[4]])
  b.a <- class(a[[1]])
  b.b <- class(a[[2]])
  b.c <- class(a[[3]])
  b.d <- class(a[[4]])
  
  expect_equal(a.a,b.a)
  expect_equal(a.b,b.b)
  expect_equal(a.c,b.c)
  expect_equal(a.d,b.d)
})

test_that("match_col_types throws correct errors",{
  expect_error(match_col_types(a,c))
  expect_message(match_col_types(a,d))
})
