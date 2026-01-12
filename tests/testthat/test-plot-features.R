context("Plotting Features")

test_that("plot.ice works with color_by factor (<= 10 levels)", {
  set.seed(123)
  n <- 50
  # Create a factor with 3 levels
  X <- data.frame(x1 = rnorm(n), f1 = factor(sample(c("A", "B", "C"), n, replace = TRUE)))
  # Model: y depends on x1 and f1
  # Treat f1 as numeric in formula for simplicity if using lm, or use model.matrix
  # Using randomForest handles factors natively
  y <- 2 * X$x1 + as.numeric(X$f1) + rnorm(n)
  
  skip_if_not_installed("randomForest")
  library(randomForest)
  mod <- randomForest(x = X, y = y, ntree = 10)
  
  ice_obj <- ice(object = mod, X = X, y = y, predictor = "x1", verbose = FALSE)
  
  # Redirect PDF output to null to avoid creating files
  pdf(NULL)
  # Should run without error and produce a legend
  ret <- plot(ice_obj, color_by = "f1", plot_pdp = FALSE, verbose = FALSE)
  invisible(dev.off())
  
  # Check if legend_text is returned
  expect_true(!is.null(ret$legend_text))
  expect_equal(nrow(ret$legend_text), 3)
})

test_that("plot.ice works with color_by factor (> 10 levels)", {
  set.seed(123)
  n <- 50
  # Create a factor with 12 levels
  levels_vec <- paste0("L", 1:12)
  X <- data.frame(x1 = rnorm(n), f2 = factor(sample(levels_vec, n, replace = TRUE)))
  y <- 2 * X$x1 + as.numeric(X$f2) + rnorm(n)
  
  skip_if_not_installed("randomForest")
  library(randomForest)
  mod <- randomForest(x = X, y = y, ntree = 10)
  
  ice_obj <- ice(object = mod, X = X, y = y, predictor = "x1", verbose = FALSE)
  
  pdf(NULL)
  # Expect a warning about coercion
  expect_warning(plot(ice_obj, color_by = "f2", plot_pdp = FALSE, verbose = FALSE),
                 "color_by is a factor with greater than 10 levels: coercing to numeric")
  invisible(dev.off())
})

test_that("plot.ice works with color_by numeric", {
  set.seed(123)
  n <- 50
  X <- data.frame(x1 = rnorm(n), x2 = runif(n))
  y <- 2 * X$x1 + 3 * X$x2 + rnorm(n)
  
  mod <- lm(y ~ ., data = cbind(X, y = y))
  ice_obj <- ice(object = mod, X = X, y = y, predictor = "x1", verbose = FALSE)
  
  pdf(NULL)
  # Should run without error, using gradient coloring
  ret <- plot(ice_obj, color_by = "x2", plot_pdp = FALSE, verbose = FALSE)
  invisible(dev.off())
  
  # Legend text is NULL for continuous color_by (it prints to cat but doesn't return dataframe usually? 
  # Looking at code: 
  # If > 10 unique values: 
  # cat("ICE Plot Color Legend: red = low values... and green = high values\n")
  # legend_text remains NULL (initialized to NULL).
  expect_null(ret$legend_text)
})

test_that("plot.ice works with color_by data vector", {
  set.seed(123)
  n <- 50
  X <- data.frame(x1 = rnorm(n))
  y <- 2 * X$x1 + rnorm(n)
  mod <- lm(y ~ ., data = cbind(X, y = y))
  ice_obj <- ice(object = mod, X = X, y = y, predictor = "x1", verbose = FALSE)
  
  # Custom color vector (factor-like)
  my_colors <- factor(sample(c("Group1", "Group2"), n, replace = TRUE))
  
  pdf(NULL)
  ret <- plot(ice_obj, color_by = my_colors, plot_pdp = FALSE, verbose = FALSE)
  invisible(dev.off())
  
  expect_true(!is.null(ret$legend_text))
  expect_equal(nrow(ret$legend_text), 2)
})

test_that("plot.ice respects verbose argument", {
  set.seed(123)
  n <- 50
  X <- data.frame(x1 = rnorm(n), f1 = factor(sample(c("A", "B"), n, replace = TRUE)))
  y <- 2 * X$x1 + as.numeric(X$f1) + rnorm(n)
  
  skip_if_not_installed("randomForest")
  library(randomForest)
  mod <- randomForest(x = X, y = y, ntree = 10)
  
  ice_obj <- ice(object = mod, X = X, y = y, predictor = "x1", verbose = FALSE)
  
  pdf(NULL)
  # Expect output when verbose = TRUE (default)
  expect_output(plot(ice_obj, color_by = "f1", plot_pdp = FALSE), "ICE Plot Color Legend")
  
  # Expect NO output when verbose = FALSE
  expect_silent(plot(ice_obj, color_by = "f1", plot_pdp = FALSE, verbose = FALSE))
  invisible(dev.off())
})
