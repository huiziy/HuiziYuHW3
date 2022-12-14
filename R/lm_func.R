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
lm_func <- function(X,y,weights, na.action = c("ignore","mean_impute","mice_impute")) {
  ## We require the input X to be a data frame
  stopifnot("Input predictors are not a data frame." = is.data.frame(X))
  # Missing Data Manipulation
  # There are two missing data treatment:
  # "ignore": any row with missing variables will be removed
  # "mean_impute": impute with the mean of each row; or if categorical data, impute with mode
  # We write the missing data treatment function in a separate R function
  filled_object = treat_na(na.action,X,y)
  ## Extract the complete predictor matrix and the outcome
  predictors = filled_object[[1]]
  y = filled_object[[2]]
  # Preparing the data frame for linear models
  full_data = data.frame(predictors,y)
  X = model.matrix(y~., data = full_data)
  # Error message
  if (nrow(X) <= ncol(X)){
    stop("There are too many parameters in the data frame")
  }
  if (det(t(X) %*% X) == 0){
    stop("The predictor matrix must be a full rank")
  }
  # Implement closed-form solution
  betas <- solve(t(X) %*% X) %*% t(X) %*% y

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
  ## Output to list of objects
  return(list(fitted_values =y_fitted,
              residuals = residuals,
              betas = betas,
              details = list(beta_se = se_beta,t_stat = t_stat,p_stat = pt_value,hat_mat = hat_mat,SSE = SSE),
              beta_summary = summary_result,
              MSE = MSE,
              R_squared = R_square,
              R_squared_adjusted = R_square_adjusted))
}
