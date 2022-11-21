#'Missing value imputation
#'
#'Function to treat missing values; also embedded in the lm_func
#'@param na.action Possible treatment for NA values, including "ignore", "mean_impute", "mice_impute". Default to "ignore"
#'
#'@param data A data frame (one or multiple columns) of predictors (continous or categorical) with or without missing values
#'
#'@param y A vector of outcome values (continuous) with or without missing values
#'

#'@return A list of data without missing values that can be used in the linear model; objects include complete predictor matrix and complete outcome variable
#'
#'@examples
#'n = 10000
#'x1 = rnorm(n, mean = 100, sd = 2)
#'e = rnorm(n, mean = 0, sd = 1)
#'y = 5 + 3 * x1 + e
#'## Randomly generate some missing values
#'set.seed(123)
#'ind = sample(x = n , size = 0.1 * n)
#'x1[ind] = NA
#'## Creating a data frame for storing testing data
#'X = data.frame(x1)
#'treat_na(na.action = "mice_impute",X,y)
#'@export
#'
treat_na <- function(na.action, data, y) {
  if (na.action == "ignore") {
    full_data = na.omit(cbind(data,y))
    ## Keep only observations that are complete
    imputed_data = full_data[,-ncol(full_data)]
    imputed_y = full_data[,ncol(full_data)]
  } else if (na.action == "mean_impute") {
    ## Note that data should be a data frame instead of a matrix for this to work
    ## Create a custom function for imputing the categorical data with mode.
    my_mode <- function(x) { # Create mode function
      unique_x <- unique(x)
      mode <- unique_x[which.max(tabulate(match(x, unique_x)))]
      mode
    }
    ## Loop through each column and for
    ## (1) numeric data: impute with mean
    ## (2) categorical data: impute with mode
    imputed_data = data
    imputed_y = y
    for (var in 1:ncol(imputed_data)) {
      if (class(imputed_data[,var])=="numeric") {
        imputed_data[is.na(imputed_data[,var]),var] <- mean(imputed_data[,var], na.rm = TRUE)
      } else if (class(imputed_data[,var]) %in% c("character", "factor")) {
        imputed_data[,var] = as.character(imputed_data[,var])
        imputed_data[is.na(imputed_data[,var]),var] <- my_mode(imputed_data[,var])
        imputed_data[,var] = as.factor(imputed_data[,var])
      }
    }
    ## all numeric response, so we simply impute with the mean
    imputed_y[is.na(imputed_y)] <- mean(imputed_y, na.rm = TRUE)
  }
  return(list(imputed_data,imputed_y))
}
