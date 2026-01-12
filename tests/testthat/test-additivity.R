context("additivity")

test_that("backfitter works with lm", {
  set.seed(123)
  n <- 50
  X <- data.frame(x1 = seq(0, 1, length.out = n), x2 = rnorm(n))
  y <- 2 * X$x1 + 3 * X$x2 + rnorm(n, sd = 0.1)
  
  fitMethod <- function(X, y) {
    lm(y ~ ., data = as.data.frame(X))
  }
  predictfcn <- function(object, newdata) {
    predict(object, as.data.frame(newdata))
  }
  
  bf <- backfitter(X, y, predictor = "x1", fitMethod = fitMethod, predictfcn = predictfcn, verbose = FALSE)
  
  expect_s3_class(bf, "backfitter")
  expect_equal(length(bf$g1_of_Xs), n)
  expect_equal(length(bf$g2_of_Xc), n)
  expect_true(bf$delta < 0.1)
})

test_that("additivityLineup works", {
  set.seed(123)
  n <- 30
  X <- data.frame(x1 = seq(0, 1, length.out = n), x2 = rnorm(n))
  y <- X$x1 + X$x2 + rnorm(n, sd = 0.1)
  
  fitMethod <- function(X, y) {
    lm(y ~ ., data = as.data.frame(X))
  }
  predictfcn <- function(object, newdata) {
    predict(object, as.data.frame(newdata))
  }
  
  bf <- backfitter(X, y, predictor = "x1", fitMethod = fitMethod, predictfcn = predictfcn, verbose = FALSE)
  
  mod <- fitMethod(X, y)
  # ice() arguments: object, X, y, predictor
  realICE <- ice(object = mod, X = X, y = y, predictor = "x1", verbose = FALSE)
  
  pdf(NULL)
  on.exit(invisible(dev.off()))
  al <- additivityLineup(bf, fitMethod, realICE, figs = 4)
  
  expect_s3_class(al, "additivityLineup")
  expect_equal(length(al$null_ices), 3)
  expect_true(al$location >= 1 && al$location <= 4)
  expect_equal(length(al$plots), 4)
})

test_that("backfitter handles character predictor", {
  set.seed(123)
  n <- 50
  X <- data.frame(feat1 = seq(0, 1, length.out = n), feat2 = rnorm(n))
  y <- 2 * X$feat1 + 3 * X$feat2 + rnorm(n, sd = 0.1)
  
  fitMethod <- function(X, y) {
    lm(y ~ ., data = as.data.frame(X))
  }
  predictfcn <- function(object, newdata) {
    predict(object, as.data.frame(newdata))
  }
  
  bf <- backfitter(X, y, predictor = "feat1", fitMethod = fitMethod, predictfcn = predictfcn, verbose = FALSE)
  expect_equal(bf$predictor, "feat1")
})

test_that("additivityLineup works with colorvecfcn", {
  set.seed(123)
  n <- 30
  X <- data.frame(x1 = seq(0, 1, length.out = n), x2 = rnorm(n))
  y <- X$x1 + X$x2 + rnorm(n, sd = 0.1)
  
  fitMethod <- function(X, y) lm(y ~ ., data = as.data.frame(X))
  predictfcn <- function(object, newdata) predict(object, as.data.frame(newdata))
  
  bf <- backfitter(X, y, predictor = "x1", fitMethod = fitMethod, predictfcn = predictfcn, verbose = FALSE)
  mod <- fitMethod(X, y)
  realICE <- ice(mod, X, y, predictor = "x1", verbose = FALSE)
  
  color_fcn <- function(ice_obj) {
    rep("red", nrow(ice_obj$ice_curves))
  }
  
  pdf(NULL)
  on.exit(invisible(dev.off()))
  al <- additivityLineup(bf, fitMethod, realICE, figs = 2, colorvecfcn = color_fcn, usecolorvecfcn_inreal = TRUE)
  
  expect_s3_class(al, "additivityLineup")
  expect_equal(length(al$plots), 2)
})
