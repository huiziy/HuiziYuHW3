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
})
