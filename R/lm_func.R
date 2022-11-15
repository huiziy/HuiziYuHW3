lm_func <- function(X,y,weights, na.action = "ignore") {
  ## We require the input X to be a data frame
  stopifnot("Input predictors are not a data frame." = is.data.frame(X))
  # Missing Data Manipulation
  # There are three missing data treatment:
  # "ignore": any row with missing variables will be removed
  # "mean_impute": impute with the mean of each row; or if categorical data, impute with mode
  # "mice_impute": impute the entire data with the MICE package: which is the Multiple Imputation with Chained Equation
  # We write the missing data treatment function in a separate R function
  treat_na(na.action)
  # matrix of the predictors
  X <- as.matrix(X)
  # vector of ones with same length as rows in the variable data set
  int <- rep(1, length(y))
  # Add intercept column to X
  X <- cbind(int, X)
  # Implement closed-form solution
  betas <- solve(t(X) %*% X) %*% t(X) %*% y
  # Round for easier viewing
  betas <- round(betas, 2)
  # Printing the coefficients
  print(betas)
}
