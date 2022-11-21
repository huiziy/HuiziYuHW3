if (!require("mltools",character.only = T)) {
  install.packages("mltools",quiet=TRUE,repos = "https://cloud.r-project.org")
}
if (!require("data.table",character.only = T)) {
  install.packages("data.table",quiet=TRUE,repos = "https://cloud.r-project.org")
}
if (!require("Hmsic",character.only = T)) {
  install.packages("Hmsic",quiet=TRUE,repos = "https://cloud.r-project.org")
}
library(Hmisc,quietly = TRUE)
library(mltools, quietly = TRUE) ## help with discretization
library(data.table, quietly = TRUE)
#'linear regression function
#'
#'Calculate linear model results including coefficient, standard errors, t and p values, MSE, Rsquared and Adusted Rsquared
#'@param X A data frame (one or multiple columns) of predictors (both continuous and categorical)
#'
#'@param y A vector of outcome values (continuous)
#'
#'@param na.action Possible treatment for NA values, including "ignore", "mean_impute", "mice_impute". Default to "ignore"
#'
#'@return A list of useful attributes calculated from the linear model
#'
#'@examples
#'require("MASS")
#'data(Boston)
#'X = Boston[-ncol(Boston)]
#'y = Boston$medv
#'lm_func(X,y, na.action = "ignore")
#'@export
#'
lm_func <- function(X,y,weights, na.action = "ignore") {
  ## We require the input X to be a data frame
  stopifnot("Input predictors are not a data frame." = is.data.frame(X))
  # Missing Data Manipulation
  # There are three missing data treatment:
  # "ignore": any row with missing variables will be removed
  # "mean_impute": impute with the mean of each row; or if categorical data, impute with mode
  # "mice_impute": impute the entire data with the MICE package: which is the Multiple Imputation with Chained Equation
  # We write the missing data treatment function in a separate R function
  filled_object = treat_na(na.action,X,y)
  predictors = filled_object[[1]]
  y = filled_object[[2]]
  ## If all the variables are numeric, we calculate matrix algebra to find the closed form solution
  if (all.is.numeric(predictors)) {
    # matrix of the predictors
    predictors <- as.matrix(predictors)
    # Add intercept column to X
    X <- cbind(1, predictors)
    if (nrow(X) != length(y)){
      stop("The dimensions of predictors and outcome are different.")

    }
    if (nrow(X) <= ncol(X)){
      stop("There are too many parameters in the data frame")
    }
    if (det(t(X) %*% X) == 0){
      stop("The predictor matrix must be a full rank")
    }
    # Implement closed-form solution
    betas <- solve(t(X) %*% X) %*% t(X) %*% y
    # Printing the coefficients
  } else {
    full_data = data.frame(predictors,y)
    X = model.matrix(y~., data = full_data)
    if (nrow(X) != length(y)){
      stop("The dimensions of predictors and outcome are different.")

    }
    if (nrow(X) <= ncol(X)){
      stop("There are too many parameters in the data frame")
    }
    if (det(t(X) %*% X) == 0){
      stop("The predictor matrix must be a full rank")
    }
    # Implement closed-form solution
    betas <- solve(t(X) %*% X) %*% t(X) %*% y
  }
  ## Calculated related measures
  y_fitted = X %*% betas
  residuals = y - y_fitted
  SSE = sum((residuals)^2)
  SSY = sum((y - mean(y))^2)
  SSR = SSY - SSE
  MSE = SSE / (nrow(X) - ncol(X))
  MSR = SSR / (ncol(X) - 1)
  R_square = SSR / SSY
  R_square_adjusted = 1 - MSE / (SSY / (nrow(X)) - 1)
  ## Hat matrix
  hat_mat = X %*% solve(t(X) %*% X) %*% t(X)
  ## covariance matrix
  cov_betas_mat = MSE * solve(t(X) %*% X)
  var_betas_mat = diag(cov_betas_mat)
  se_beta = sqrt(var_betas_mat)
  t_stat = betas / se_beta
  pt_value = 2*( 1-pt(q = abs(t_stat), df = nrow(X) - ncol(X)) )
  ## Summary Statistics
  summary_result = cbind(betas, se_beta, t_stat, pt_value)
  colnames(summary_result) = c("coefficients","se","t_value", "p_value")
  return(list(fitted_values =y_fitted,
              residuals = residuals,
              betas = betas,
              details = list(beta_se = se_beta,t_stat = t_stat,p_stat = pt_value,hat_mat = hat_mat,SSE = SSE),
              beta_summary = summary_result,
              MSE = MSE,
              R_squared = R_square,
              R_squared_adjusted = R_square_adjusted))
}
