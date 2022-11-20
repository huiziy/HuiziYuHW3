### Create Diagnostic plots
#'Residuals vs Fitted Graph
#'
#'Create the residual VS fitted value plot; similar to the diagnostic plots of the default linear models
#'@param lm_object A linear models object obtained from the linear model function "lm_func" in the package
#'@return A residual VS fitted values plot
#'
#'@examples
#'require("MASS")
#'data(Boston)
#'X = Boston[-ncol(Boston)]
#'y = Boston$medv
#'lm_result = lm_func(X,y, na.action = "ignore")
#'res_fitted(lm_object)
#'@export
#'
res_fitted <- function(lm_object) {
  res <- lm_object$residuals
  fitted <- lm_object$fitted_values
  plot(fitted, res, xlab = "Fitted values", ylab = "Residuals", main = "Residuals vs Fitted")
}

#'Scale-Location Graph
#'
#'Create the Scale-Location plot; similar to the diagnostic plots of the default linear models
#'@param lm_object A linear models object obtained from the linear model function "lm_func" in the package
#'@return A Scale-Location plot
#'
#'@examples
#'require("MASS")
#'data(Boston)
#'X = Boston[-ncol(Boston)]
#'y = Boston$medv
#'lm_result = lm_func(X,y, na.action = "ignore")
#'scale_location(lm_object)
#'@export
#'
scale_location <- function(lm_object) {
  res <- lm_object$residuals
  fitted <- lm_object$fitted_values
  hat_mat = lm_object$details$hat_mat
  lev = diag(hat_mat)
  RSE = sqrt(lm_object$details$SSE / (length(fitted) - length(lm_object$betas) - 1))
  stand_res = sqrt(abs(res / (RSE * sqrt(1 - lev))))
  plot(fitted,stand_res, xlab = "Fitted values", ylab = "sqrt of standardized residuals", main = "Scale-Location")
}

#'Residual VS Leverage Graph
#'
#'Create the Residual VS Leverage plot; similar to the diagnostic plots of the default linear models
#'@param lm_object A linear models object obtained from the linear model function "lm_func" in the package
#'@return A Residual VS Leverage plot
#'
#'@examples
#'require("MASS")
#'data(Boston)
#'X = Boston[-ncol(Boston)]
#'y = Boston$medv
#'lm_result = lm_func(X,y, na.action = "ignore")
#'res_lev(lm_object)
#'@export
#'
res_lev <- function(lm_object) {
  res <- lm_object$residuals
  fitted <- lm_object$fitted_values
  hat_mat = lm_object$details$hat_mat
  lev = diag(hat_mat)
  RSE = sqrt(lm_object$details$SSE / (length(fitted) - length(lm_object$betas) - 1))
  stand_res = res / (RSE * sqrt(1 - lev))
  plot(lev, stand_res, xlab = "Leverage", ylab = "Standardized residuals", main = "Residuals vs Leverage")
}

#'QQ Plot of the Standardized Residuals
#'
#'Create the QQ Plot of the Standardized Residuals; similar to the diagnostic plots of the default linear models
#'@param lm_object A linear models object obtained from the linear model function "lm_func" in the package
#'@return A QQ Plot of the Standardized Residuals
#'
#'@examples
#'require("MASS")
#'data(Boston)
#'X = Boston[-ncol(Boston)]
#'y = Boston$medv
#'lm_result = lm_func(X,y, na.action = "ignore")
#'qq_plot(lm_object)
#'@export
#'
qq_plot <- function(lm_object) {
  res <- lm_object$residuals
  fitted <- lm_object$fitted_values
  hat_mat = lm_object$details$hat_mat
  lev = diag(hat_mat)
  RSE = sqrt(lm_object$details$SSE / (length(fitted) - length(lm_object$betas) - 1))
  stand_res = res / (RSE * sqrt(1 - lev))
  qqnorm(stand_res, pch = 1, frame = FALSE)
  qqline(stand_res, col = "steelblue", lwd = 2)
}
