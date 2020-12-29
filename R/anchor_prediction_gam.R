#' @title anchor_prediction
#'
#' @description Perform a prediction for an Anchor Regression model as described in Rothenhäusler et al.2020
#'
#' @param anchor_model is the Anchor Regression model object
#' @param x is a dataframe containing the matrix x containing the independent variables
#' @param anchor is a dataframe containing the matrix anchor containing the anchor variable
#' @param gamma is the regularization parameter for the Anchor Regression
#' @param target_variable is the target variable name contained in the x dataframe
#' @param bin_factor binary variable that can be transformed to a factor to partial out effects
#'
#' @return A list of predictions.
#' @export
#' @importFrom stats coef lm predict as.formula
#' @examples
#' x <- as.data.frame(matrix(data = rnorm(10000),nrow = 1000,ncol = 10))
#' x$bin <- sample(nrow(x),x = c(1,0),prob = c(0.5,0.5),replace = TRUE)
#' anchor <- as.data.frame(matrix(data = rnorm(2000),nrow = 1000,ncol = 2))
#' colnames(anchor) <- c('X1','X2')
#' gamma <- 2
#' target_variable <- 'V2'
#'
#' anchor_model <- anchor_regression_gam(x, anchor, gamma, target_variable,"bin")
#' anchor_prediction_gam(anchor_model$model, x, anchor, gamma, target_variable,"bin")


anchor_prediction_gam <- function(anchor_model, x, anchor, gamma, target_variable,bin_factor){

  # tranformation
  x <- as.matrix(x)
  anchor <- as.matrix(anchor)
  fit_const <- lm(x ~ 1)
  fit <- lm(x ~ anchor)

  anchor_data <- fit_const$fitted.values + fit$residuals +
    sqrt(gamma) * (fit$fitted.values - fit_const$fitted.values)

  # slice set
  indices <- 1:nrow(anchor_data)
  j <- match(target_variable, colnames(anchor_data))
  x_new <- anchor_data[indices, -c(j)]
  x_new <- as.data.frame(x_new)

  if(is.null(bin_factor) != TRUE){
    x_new[bin_factor] <- as.factor(round(x_new[,bin_factor]))
    }

  # predict with model
  y_pred <- predict(anchor_model, x = x_new)


  return(y_pred)
}
