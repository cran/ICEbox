library(testthat)
library(ICEbox)

if (identical(Sys.getenv("NOT_CRAN"), "true")) {
  test_check("ICEbox")
}
