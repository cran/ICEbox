context("Advanced Options")

test_that("ice works with indices_to_build", {
  set.seed(123)
  n <- 50
  X <- data.frame(x1 = rnorm(n), x2 = runif(n))
  y <- 2 * X$x1 + 3 * X$x2 + rnorm(n)
  mod <- lm(y ~ ., data = cbind(X, y = y))
  
  indices <- c(1, 5, 10, 20)
  ice_obj <- ice(object = mod, X = X, y = y, predictor = "x1", 
                 indices_to_build = indices, verbose = FALSE)
  
  expect_equal(nrow(ice_obj$ice_curves), length(indices))
})

test_that("ice works with num_grid_pts", {
  set.seed(123)
  n <- 50
  # Ensure x1 has many unique values
  X <- data.frame(x1 = sort(rnorm(n)), x2 = runif(n))
  y <- 2 * X$x1 + 3 * X$x2 + rnorm(n)
  mod <- lm(y ~ ., data = cbind(X, y = y))
  
  n_grid <- 10
  ice_obj <- ice(object = mod, X = X, y = y, predictor = "x1", 
                 num_grid_pts = n_grid, verbose = FALSE)
  
  expect_equal(length(ice_obj$gridpts), n_grid)
})

test_that("ice works with probit option", {
  set.seed(123)
  n <- 100
  X <- data.frame(x1 = rnorm(n), x2 = runif(n))
  prob <- 1 / (1 + exp(-(1 * X$x1 - 1 * X$x2)))
  y <- rbinom(n, 1, prob)
  
  mod <- glm(y ~ ., data = cbind(X, y = y), family = binomial)
  
  # ice expects probabilities when probit=TRUE or logodds=TRUE
  ice_obj <- ice(object = mod, X = X, y = y, predictor = "x1", 
                 predictfcn = function(object, newdata) predict(object, newdata, type = "response"),
                 probit = TRUE, verbose = FALSE)
  
  expect_s3_class(ice_obj, "ice")
  # Values should be on probit scale (approx range -3 to 3 usually, but can be larger)
  # Check if values are not all in [0,1] which would imply probability scale
  expect_true(any(ice_obj$ice_curves < 0) || any(ice_obj$ice_curves > 1))
})

test_that("dice works with custom DerivEstimator", {
  set.seed(123)
  n <- 50
  X <- data.frame(x1 = sort(rnorm(n)), x2 = runif(n))
  y <- X$x1^2 + rnorm(n)
  mod <- lm(y ~ ., data = cbind(X, y = y))
  
  ice_obj <- ice(object = mod, X = X, y = y, predictor = "x1", verbose = FALSE)
  
  # Simple difference estimator
  simple_diff <- function(y, x = ice_obj$gridpts) {
    dy <- diff(y)
    dx <- diff(x)
    d <- dy / dx
    c(d, tail(d, 1)) # Pad to same length
  }
  
  dice_obj <- dice(ice_obj, DerivEstimator = simple_diff)
  expect_s3_class(dice_obj, "dice")
  expect_equal(dim(dice_obj$d_ice_curves), dim(ice_obj$ice_curves))
})

test_that("clusterICE works with centered=TRUE", {
  set.seed(123)
  n <- 50
  X <- data.frame(x1 = rnorm(n), x2 = runif(n))
  y <- 2 * X$x1 + 3 * X$x2 + rnorm(n)
  mod <- lm(y ~ ., data = cbind(X, y = y))
  ice_obj <- ice(object = mod, X = X, y = y, predictor = "x1", verbose = FALSE)
  
  pdf(NULL)
  cl <- clusterICE(ice_obj, nClusters = 2, centered = TRUE, plot = FALSE)
  invisible(dev.off())
  
  expect_s3_class(cl$cl, "kmeans")
})
