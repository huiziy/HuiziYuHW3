### Create Diagnostic plots

res_fitted <- function(lm_object) {
  res <- lm_object$residuals
  fitted <- lm_object$fitted_values
  plot(fitted, res, xlab = "Fitted values", ylab = "Residuals", main = "Residuals vs Fitted")
}

scale_location <- function(lm_object) {
  res <- lm_object$residuals
  fitted <- lm_object$fitted_values
  hat_mat = lm_object$hat_mat
  lev = diag(hat_mat)
  RSE = sqrt(lm_object$SSE / (length(lm_object) - length(lm_object$betas) - 1))
  stand_res = sqrt(abs(res / (RSE * sqrt(1 - lev))))
  plot(fitted,stand_res, xlab = "Fitted values", ylab = "sqrt of standardized residuals", main = "Scale-Location")
}


res_lev <- function(lm_object) {
  res <- lm_object$residuals
  fitted <- lm_object$fitted_values
  hat_mat = lm_object$hat_mat
  lev = diag(hat_mat)
  RSE = sqrt(lm_object$SSE / (length(lm_object) - length(lm_object$betas) - 1))
  stand_res = res / (RSE * sqrt(1 - lev))
  plot(lev, stand_res, xlab = "Leverage", ylab = "Standardized residuals", main = "Residuals vs Leverage")
}


qq_plot <- function(lm_object) {
  res <- lm_object$residuals
  fitted <- lm_object$fitted_values
  hat_mat = lm_object$hat_mat
  lev = diag(hat_mat)
  RSE = sqrt(lm_object$SSE / (length(lm_object) - length(lm_object$betas) - 1))
  stand_res = res / (RSE * sqrt(1 - lev))
  qqnorm(stand_res, pch = 1, frame = FALSE)
  qqline(stand_res, col = "steelblue", lwd = 2)
}
