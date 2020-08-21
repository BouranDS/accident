library(testthat)
library(accident)
#
#test_check("accident")
#
testthat::context(desc = "First function")
testthat::test_that(
  desc = "first function",
  code = {
    expect_equal(make_filename(2013), "accident_2013.csv.bz2")
    expect_equal(make_filename(2014), "accident_2014.csv.bz2")
    expect_equal(make_filename(2015), "accident_2015.csv.bz2")
  }
)
