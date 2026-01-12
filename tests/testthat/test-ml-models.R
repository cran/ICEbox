context("Machine Learning Models")

test_that("ice works with glm (logistic regression)", {
  set.seed(123)
  n <- 100
  X <- data.frame(x1 = rnorm(n), x2 = runif(n))
  # Binary outcome
  prob <- 1 / (1 + exp(-(1 * X$x1 - 1 * X$x2)))
  y <- rbinom(n, 1, prob)
  
  mod <- glm(y ~ ., data = cbind(X, y = y), family = binomial)
  
  # Default predict for glm type="link" (log odds) is standard.
  # ice with logodds=TRUE expects probabilities.
  # If we use logodds=FALSE (default), ice plots the linear predictor (link) directly if predict returns it?
  # No, predict(glm) returns link by default. 
  # Let's test standard behavior: plotting the probability.
  
  ice_obj <- ice(object = mod, X = X, y = y, predictor = "x1", 
                 predictfcn = function(object, newdata) predict(object, newdata, type = "response"),
                 verbose = FALSE)
  
  expect_s3_class(ice_obj, "ice")
  expect_equal(nrow(ice_obj$ice_curves), n)
})

test_that("ice works with randomForest (regression)", {
  skip_if_not_installed("randomForest")
  library(randomForest)
  
  set.seed(123)
  n <- 50
  X <- data.frame(x1 = rnorm(n), x2 = runif(n))
  y <- 2 * X$x1 + 3 * X$x2 + rnorm(n)
  
  mod <- randomForest(x = X, y = y, ntree = 10)
  
  ice_obj <- ice(object = mod, X = X, y = y, predictor = "x1", verbose = FALSE)
  
  expect_s3_class(ice_obj, "ice")
  expect_equal(nrow(ice_obj$ice_curves), n)
})

test_that("ice works with randomForest (classification)", {
  skip_if_not_installed("randomForest")
  library(randomForest)
  
  data(iris)
  # Binary classification for simplicity
  iris_bin <- iris[1:100, ]
  iris_bin$Species <- factor(iris_bin$Species)
  X <- iris_bin[, 1:4]
  y <- iris_bin$Species
  
  mod <- randomForest(x = X, y = y, ntree = 10)
  
  # For classification, we often want probabilities for a specific class.
  # randomForest predict with type="prob" returns a matrix.
  # We need a wrapper to select the column of interest (e.g., column 2 for the second level).
  
  pred_func <- function(object, newdata) {
    predict(object, newdata, type = "prob")[, 2]
  }
  
  # Note: y is not passed because it is a factor
  ice_obj <- ice(object = mod, X = X, predictor = "Sepal.Length",
                 predictfcn = pred_func, logodds = TRUE, verbose = FALSE)
  
  expect_s3_class(ice_obj, "ice")
  expect_equal(nrow(ice_obj$ice_curves), nrow(X))
})

test_that("ice works with rpart (decision tree)", {
  skip_if_not_installed("rpart")
  library(rpart)
  
  set.seed(123)
  n <- 100
  X <- data.frame(x1 = rnorm(n), x2 = runif(n))
  y <- 2 * X$x1 + 3 * X$x2 + rnorm(n)
  
  mod <- rpart(y ~ ., data = cbind(X, y = y))
  
  ice_obj <- ice(object = mod, X = X, y = y, predictor = "x1", verbose = FALSE)
  
  expect_s3_class(ice_obj, "ice")
  expect_equal(nrow(ice_obj$ice_curves), n)
})

test_that("ice works with lda from MASS", {
  skip_if_not_installed("MASS")
  library(MASS)
  
  data(iris)
  X <- iris[, 1:4]
  y <- iris$Species
  
  mod <- lda(x = X, grouping = y)
  
  # predict.lda returns a list with component 'posterior' (matrix)
  pred_func <- function(object, newdata) {
    predict(object, newdata)$posterior[, 2] # Prob of second class
  }
  
  ice_obj <- ice(object = mod, X = X, predictor = "Sepal.Length",
                 predictfcn = pred_func, verbose = FALSE)
  
  expect_s3_class(ice_obj, "ice")
  expect_equal(nrow(ice_obj$ice_curves), nrow(X))
})