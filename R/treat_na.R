require("mice")

treat_na <- function(na.action, data, y) {
  if (na.action == "ignore") {
    full_data = na.omit(cbind(data,y))
    ## Keep only observations that are complete
    imputed_data = full_data[,-ncol(full_data)]
    imputed_y = full_data[,ncol(full_data)]
  } else if (na.action == "mean_impute") {
    ## Note that data should be a data frame instead of a matrix for this to work
    ## Create a custom function for imputing the categorical data with mode.
    my_mode <- function(x) {
      ux <- unique(x)
      tab <- tabulate(match(x, ux))
      mode <- ux[tab == max(tab)]
      ifelse(length(mode) > 1, sample(mode, 1), mode)
    }
    ## Loop through each column and for
    ## (1) numeric data: impute with mean
    ## (2) categorical data: impute with mode
    imputed_data = data
    imputed_y = y
    for (var in 1:ncol(imputed_data)) {
      if (class(imputed_data[,var])=="numeric") {
        print("numeric")
        imputed_data[is.na(imputed_data[,var]),var] <- mean(imputed_data[,var], na.rm = TRUE)
      } else if (class(imputed_data[,var]) %in% c("character", "factor")) {
        print("categorical")
        imputed_data[,var] = as.character(imputed_data[,var])
        imputed_data[is.na(imputed_data[,var]),var] <- my_mode(imputed_data[,var])
        imputed_data[,var] = as.factor(imputed_data[,var])
      }
    }
    ## all numeric response, so we simply impute with the mean
    imputed_y[is.na(imputed_y)] <- mean(imputed_y, na.rm = TRUE)
  } else if (na.action == "mice_impute") {
    full_data = as.matrix(cbind(data,y))
    tempData <- mice(full_data,m=1,maxit=50,meth='pmm',seed=500,printFlag = FALSE)
    imputed_data <- complete(tempData,1)[,-ncol(full_data)]
    imputed_y = complete(tempData,1)[,ncol(full_data)]
  }
  return(list(imputed_data,imputed_y))
}
