context("dice")

test_that("dice works with lm ice object", {
  set.seed(123)
  n <- 50
  X <- data.frame(x1 = rnorm(n), x2 = runif(n))
  y <- 2 * X$x1 + 3 * X$x2 + rnorm(n)
  mod <- lm(y ~ ., data = cbind(X, y = y))
  ice_obj <- ice(object = mod, X = X, y = y, predictor = "x1", verbose = FALSE)
  
  dice_obj <- dice(ice_obj)
  
  expect_s3_class(dice_obj, "dice")
  expect_equal(dim(dice_obj$d_ice_curves), dim(ice_obj$ice_curves))
  
  # Test S3 methods
  expect_output(print(dice_obj))
  expect_output(summary(dice_obj))
  
  # Plotting
  pdf(NULL)
  expect_error(plot(dice_obj), NA)
  invisible(dev.off())
})
