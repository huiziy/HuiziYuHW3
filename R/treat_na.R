require("mice")

treat_na <- function(na.action) {
  if (na.action == "ignore") {
    X = na.omit(X)
  } else if (na.action == "mean_impute") {
    ## Note that X should be a data frame instead of a matrix for this to work
    ## Create a custom function for inputing the categorical data with mode.
    my_mode <- function(x) {
      ux <- unique(x)
      tab <- tabulate(match(x, ux))
      mode <- ux[tab == max(tab)]
      ifelse(length(mode) > 1, sample(mode, 1), mode)
    }
    ## Loop through each column and for
    ## (1) numeric data: impute with mean
    ## (2) categorical data: impute with mode
    for (var in 1:ncol(X)) {
      if (class(X[,var])=="numeric") {
        X[is.na(X[,var]),var] <- mean(X[,var], na.rm = TRUE)
      } else if (class(X[,var]) %in% c("character", "factor")) {
        X[is.na(X[,var]),var] <- my_mode(X[,var], na.rm = TRUE)
      }
    }
  } else if (na.action == "mice_impute") {
    tempData <- mice(X,m=1,maxit=50,meth='pmm',seed=500)
    X <- complete(tempData,1)
  }
}
