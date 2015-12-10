### Unit testing ###
require(testthat)
# until this gets on CRAN
# require(dfat)
# we use the github site
require(devtools)
devtools::install_github('garyfeng/dfat')
# or source("R/dfat.R")

############
x<-c("A","B",NA,NA,"C",NA,NA,NA,NA,"D",NA,NA);
###########

test_that("backFillNA works", {
  expect_equal(backFillNA(x), c("A","B","B","B","C","C","C","C","C","D","D","D"))
  expect_equal(backFillNA(c(1,2,3)), c(1,2,3))
  expect_equal(backFillNA(c(NA,2,3)), c(NA,2,3))
  expect_equal(backFillNA(c(NA, NA)), c(NA,NA))
  expect_equal(backFillNA(c("this", "that", NA, NA, "hello")), c("this", "that", "that", "that", "hello"))
})

test_that("backFillNA throws errors", {
  expect_error(backFillNA(list(1,2,3)))
  expect_error(backFillNA(as.matrix(c(1,2,3))))
})