test_that("lm_func works", {
  ## We expect the coefficient estimate from the default model to be the same as our user written model
  n = 1000
  x1 = rnorm(n, mean = 3, sd = 1)
  x2 = rnorm(n, mean = 5, sd = 2)
  e = rnorm(n)
  X = data.frame(cbind(x1,x2))
  y = 0.4 + 0.2 * x1 + 0.5 * x2 + e
  full_data = data.frame(cbind(x1,x2,y))
  default_mod = lm(y~., data = full_data)
  tru_coeff = as.vector(summary(default_mod)$coefficients[,1])
  user_mod = lm_func(X,y, na.action = "ignore")
  expect_equal(as.vector(user_mod$betas), tru_coeff)

  ## We expect an error
  n = 1000
  x1 = rep(2,n)
  X = data.frame(cbind(1,x1))
  e = rnorm(n)
  y = 0.5 + 0.3 * x1 + e
  expect_error(lm_func(X, y), "The predictor matrix must be a full rank")

  ## We expect an error
  n = 2
  x1 = rnorm(n, mean = 3, sd = 1)
  x2 = rnorm(n, mean = 3, sd = 1)
  x3 = rnorm(n, mean = 3, sd = 1)
  X = data.frame(cbind(x1,x2,x3))
  e = rnorm(n)
  y = 0.5 + 0.3 * x1 + e
  expect_error(lm_func(X, y), "There are too many parameters in the data frame")

  ## We expect the mean imputation results to be the same as doing the mean imputation outside.
  n = 1000
  x1 = rnorm(n, mean = 3, sd = 1)
  x3 = factor(sample(c("female","male","non-binary","refused to answer"), size = n, replace = TRUE))
  e = rnorm(n)
  y = 0.4 + 0.2 * x1 + 8 * ifelse(x3 == "male",1,0) + 0.5 * ifelse(x3 == "refused to answer",1,0) + 10 * ifelse(x3 == "non-binary",1,0) + e
  set.seed(123)
  ind = sample(x = n , size = 0.1 * n)
  x1[ind] = NA
  set.seed(1234)
  ind = sample(x = n , size = 0.1 * n)
  x3[ind] = NA
  X = data.frame(x1,x3)
  ## Imputing with mean outside the linear regression function
  x1[is.na(x1)]<- mean(x1,na.rm = TRUE)
  my_mode <- function(x) {                                     # Create mode function
    unique_x <- unique(x)
    mode <- unique_x[which.max(tabulate(match(x, unique_x)))]
    mode
  }
  x3[is.na(x3)]<- my_mode(x3)
  full_data = data.frame(x1,x3,y)
  default_mod = lm(y~., data = full_data)
  tru_coeff = as.vector(summary(default_mod)$coefficients[,1])
  user_mod = lm_func(X,y, na.action = "mean_impute")
  expect_equal(as.vector(user_mod$betas), tru_coeff)
})
