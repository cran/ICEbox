context("ice")

test_that("ice works with lm", {
  # Synthetic data
  set.seed(123)
  n <- 50
  X <- data.frame(x1 = rnorm(n), x2 = runif(n))
  y <- 2 * X$x1 + 3 * X$x2 + rnorm(n)
  
  mod <- lm(y ~ ., data = cbind(X, y = y))
  
  # Predictor by name
  # ice() arguments: object, X, y, predictor
  ice_obj <- ice(object = mod, X = X, y = y, predictor = "x1", verbose = FALSE)
  
  expect_s3_class(ice_obj, "ice")
  expect_equal(ice_obj$predictor, "x1")
  expect_equal(nrow(ice_obj$ice_curves), n)
  
  # Predictor by index
  ice_obj2 <- ice(object = mod, X = X, y = y, predictor = 2, verbose = FALSE)
  expect_s3_class(ice_obj2, "ice")
  expect_equal(ice_obj2$predictor, 2)
  
  # Test S3 methods
  expect_output(print(ice_obj))
  expect_output(summary(ice_obj))
  
  # Plotting
  pdf(NULL)
  expect_error(plot(ice_obj), NA)
  invisible(dev.off())
})

test_that("ice works with frac_to_build", {
  set.seed(123)
  n <- 50
  X <- data.frame(x1 = rnorm(n), x2 = runif(n))
  y <- 2 * X$x1 + 3 * X$x2 + rnorm(n)
  mod <- lm(y ~ ., data = cbind(X, y = y))
  
  ice_obj <- ice(object = mod, X = X, y = y, predictor = "x1", frac_to_build = 0.5, verbose = FALSE)
  expect_equal(nrow(ice_obj$Xice), floor(n * 0.5))
})
