context("Datasets and Arguments")

test_that("ice works with Boston Housing data (MASS)", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("randomForest")
  library(MASS)
  library(randomForest)
  
  data(Boston)
  X <- Boston
  y <- X$medv
  X$medv <- NULL
  
  set.seed(123)
  # Limit trees for speed
  mod <- randomForest(x = X, y = y, ntree = 10)
  
  # Test with "age" predictor
  ice_obj <- ice(object = mod, X = X, y = y, predictor = "age", 
                 frac_to_build = 0.1, verbose = FALSE)
  
  expect_s3_class(ice_obj, "ice")
  expect_true(nrow(ice_obj$ice_curves) > 0)
  expect_equal(ice_obj$predictor, "age")
})

test_that("ice works with Pima Indians Diabetes (MASS) - Classification", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("randomForest")
  library(MASS)
  library(randomForest)
  
  data(Pima.te)
  y <- Pima.te$type
  X <- Pima.te
  X$type <- NULL
  
  set.seed(123)
  mod <- randomForest(x = X, y = y, ntree = 10)
  
  # Predictor "skin", logodds=TRUE
  # Need to supply predictfcn for randomForest classification to extract prob
  pred_func <- function(object, newdata) {
    predict(object, newdata, type = "prob")[, 2]
  }
  
  ice_obj <- ice(object = mod, X = X, predictor = "skin", logodds = TRUE,
                 predictfcn = pred_func, verbose = FALSE)
  
  expect_s3_class(ice_obj, "ice")
  
  # Check dice on this object
  dice_obj <- dice(ice_obj)
  expect_s3_class(dice_obj, "dice")
  expect_true(dice_obj$logodds)
})

test_that("ice works with WhiteWine data (included in package)", {
  skip_if_not_installed("randomForest")
  library(randomForest)
  
  data(WhiteWine)
  # Use a subset for speed
  WW_subset <- WhiteWine[1:200, ]
  X <- WW_subset
  y <- X$quality
  X$quality <- NULL
  
  set.seed(123)
  mod <- randomForest(x = X, y = y, ntree = 10)
  
  ice_obj <- ice(object = mod, X = X, y = y, predictor = "alcohol", verbose = FALSE)
  
  expect_s3_class(ice_obj, "ice")
  
  # Test color mapping in plot (visual check logic, but ensures code runs)
  pdf(NULL)
  expect_error(plot(ice_obj, x_quantile = TRUE, plot_pdp = TRUE, centered = TRUE), NA)
  invisible(dev.off())
})

test_that("ice fails gracefully with factor predictors", {
  set.seed(123)
  n <- 50
  X <- data.frame(x1 = factor(sample(c("A", "B"), n, replace = TRUE)), x2 = runif(n))
  y <- as.numeric(X$x1) + X$x2 + rnorm(n)
  mod <- lm(y ~ ., data = cbind(X, y = y))
  
  # Expect error because ICE doesn't support factor predictors
  expect_error(ice(object = mod, X = X, y = y, predictor = "x1", verbose = FALSE),
               "ICE does not support factor attributes")
})

test_that("ice works with indices_to_build and frac_to_build conflict", {
  set.seed(123)
  n <- 50
  X <- data.frame(x1 = rnorm(n), x2 = runif(n))
  y <- 2 * X$x1 + 3 * X$x2 + rnorm(n)
  mod <- lm(y ~ ., data = cbind(X, y = y))
  
  expect_error(ice(object = mod, X = X, y = y, predictor = "x1",
                   frac_to_build = 0.5, indices_to_build = 1:10, verbose = FALSE),
               "cannot both be specified simultaneously")
})

test_that("ice works with logodds and probit conflict", {
  set.seed(123)
  n <- 50
  X <- data.frame(x1 = rnorm(n), x2 = runif(n))
  y <- rbinom(n, 1, 0.5)
  mod <- glm(y ~ ., data = cbind(X, y = y), family = binomial)
  
  expect_error(ice(object = mod, X = X, predictor = "x1", 
                   logodds = TRUE, probit = TRUE, verbose = FALSE),
               "You must employ either logodds OR probit but not both")
})
