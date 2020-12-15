#' @title anchor_prediction
#'
#' @description Perform a prediction for an Anchor Regression Model as described in Rothenhäusler et al.2020
#'
#' @param anchor_model is the Anchor Regression model object
#' @param x is a dataframe containing the matrix x containing the independent variables
#' @param anchor is a dataframe containing the matrix anchor containing the anchor variable
#' @param target_variable is the target variable name contained in the x dataframe
#'
#' @return A list of predictions.
#' @export
#' @importFrom stats coef lm
#' @examples
#' x <- as.data.frame(matrix(data = rnorm(100),nrow = 100,ncol = 10))
#' anchor <- as.data.frame(matrix(data = rnorm(200),nrow = 100,ncol = 2))
#' colnames(anchor) <- c('X1','X2')
#' gamma <- 2
#' target_variable <- 'V2'
#' anchor_model <- anchor_regression(x, anchor, gamma, target_variable)
#' anchor_prediction(anchor_model$model, x, anchor, target_variable)


anchor_prediction <- function(anchor_model, x, anchor, target_variable){
  # convert to matrix for lm
  x <- as.matrix(x)
  anchor <- as.matrix(anchor)

  # tranform data
  fit_const <- lm(x ~ 1)
  fit <- lm(x ~ anchor)
  anchor_data <- fit_const$fitted.values + fit$residuals + sqrt(gamma)*(fit$fitted.values-fit_const$fitted.values)
  indices <- 1:nrow(anchor_data)
  j <-  match( target_variable, colnames(anchor_data))
  x <- anchor_data[indices,-c(j)]

  # predictio
  prediction <- predict(anchor_model,type="response", newx = x, s = 'lambda.min')
  return(prediction)
}
