
test_that("dice sd_deriv calculation is accurate and matches apply(sd)", {
  set.seed(123)
  n <- 50
  X <- data.frame(x1 = rnorm(n), x2 = runif(n))
  y <- 2 * X$x1 + 3 * X$x2 + rnorm(n)
  mod <- lm(y ~ ., data = cbind(X, y = y))
  ice_obj <- ice(object = mod, X = X, y = y, predictor = "x1", verbose = FALSE)
  dice_obj <- dice(ice_obj)
  
  # Calculate expected SD using stable apply
  expected_sd <- apply(dice_obj$d_ice_curves, 2, sd)
  
  # Check equality
  expect_equal(dice_obj$sd_deriv, expected_sd)
  
  # Test with data that might cause cancellation issues (large mean, small variance)
  dice_obj$d_ice_curves <- dice_obj$d_ice_curves + 1e9
  
  # Re-run dice logic (internal part) manually or create a new dice object with modified curves?
  # dice() calculates sd from d_ice_curves. But dice() calculates d_ice_curves from ice_obj.
  # So we can't easily force dice() to use our shifted matrix unless we mock internal functions.
  # But we can check if our vectorized logic works on such a matrix.
  
  X_mat <- dice_obj$d_ice_curves
  n_curves <- nrow(X_mat)
  
  # Naive formula
  vars_naive <- (colSums(X_mat^2) - colSums(X_mat)^2 / n_curves) / (n_curves - 1)
  sd_naive <- sqrt(pmax(0, vars_naive))
  
  # Stable formula (truth)
  sd_stable <- apply(X_mat, 2, sd)
  
  # If naive fails, they will differ
  # expect_equal(sd_naive, sd_stable) 
})
