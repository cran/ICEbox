context("clusterICE")

test_that("clusterICE works with ice object", {
  set.seed(123)
  n <- 50
  X <- data.frame(x1 = rnorm(n), x2 = runif(n))
  y <- 2 * X$x1 + 3 * X$x2 + rnorm(n)
  mod <- lm(y ~ ., data = cbind(X, y = y))
  ice_obj <- ice(object = mod, X = X, y = y, predictor = "x1", verbose = FALSE)
  
  pdf(NULL)
  cl <- clusterICE(ice_obj, nClusters = 2, plot = FALSE)
  invisible(dev.off())
  
  expect_s3_class(cl$cl, "kmeans")
  expect_equal(length(cl$cl$cluster), n)
})

test_that("clusterICE works with dice object", {
  set.seed(123)
  n <- 50
  X <- data.frame(x1 = rnorm(n), x2 = runif(n))
  y <- 2 * X$x1 + 3 * X$x2 + rnorm(n)
  mod <- lm(y ~ ., data = cbind(X, y = y))
  ice_obj <- ice(object = mod, X = X, y = y, predictor = "x1", verbose = FALSE)
  dice_obj <- dice(ice_obj)
  
  pdf(NULL)
  cl <- clusterICE(dice_obj, nClusters = 2, plot = FALSE)
  invisible(dev.off())
  
  expect_s3_class(cl$cl, "kmeans")
  expect_equal(length(cl$cl$cluster), n)
})
