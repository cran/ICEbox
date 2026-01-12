context("Plot Return Values")

test_that("plot.ice returns plot object", {
  set.seed(123)
  n <- 20
  X <- data.frame(x1 = rnorm(n))
  y <- 2 * X$x1 + rnorm(n)
  mod <- lm(y ~ ., data = cbind(X, y = y))
  ice_obj <- ice(object = mod, X = X, y = y, predictor = "x1", verbose = FALSE)
  
  pdf(NULL)
  ret <- plot(ice_obj, verbose = FALSE)
  invisible(dev.off())
  
  expect_true("plot" %in% names(ret))
  expect_s3_class(ret$plot, "ggplot")
})

test_that("plot.dice returns plot object", {
  set.seed(123)
  n <- 20
  X <- data.frame(x1 = rnorm(n))
  y <- 2 * X$x1 + rnorm(n)
  mod <- lm(y ~ ., data = cbind(X, y = y))
  ice_obj <- ice(object = mod, X = X, y = y, predictor = "x1", verbose = FALSE)
  dice_obj <- dice(ice_obj)
  
  pdf(NULL)
  ret <- plot(dice_obj, verbose = FALSE)
  invisible(dev.off())
  
  expect_true("plot" %in% names(ret))
  expect_s3_class(ret$plot, "ggplot")
})

test_that("clusterICE returns plot object", {
  skip_if_not_installed("randomForest")
  library(randomForest)
  set.seed(123)
  n <- 100
  X <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
  y <- X$x1^2 + X$x1 * X$x2 + rnorm(n)
  mod <- randomForest(x = X, y = y, ntree = 10)
  ice_obj <- ice(object = mod, X = X, y = y, predictor = "x1", verbose = FALSE)
  
  pdf(NULL)
  ret <- clusterICE(ice_obj, nClusters = 2, plot = TRUE)
  invisible(dev.off())
  
  expect_true("cl" %in% names(ret))
  expect_true("plot" %in% names(ret))
  expect_s3_class(ret$cl, "kmeans")
  expect_s3_class(ret$plot, "ggplot")
  
  # When plot = FALSE, plot should be NULL
  ret2 <- clusterICE(ice_obj, nClusters = 2, plot = FALSE)
  expect_null(ret2$plot)
})
