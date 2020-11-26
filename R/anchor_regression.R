#' @title anchor_regression
#'
#' @description Perform an Anchor Regresseion as described in Rothenhäusler et al.2020
#'
#' @param x x is a dataframe containing the matrix x
#' @param anchor anchor is a dataframe containing the matrix anchor
#' @param gamma gamma is the regularization parameter for the Anchor Regression
#' @param target target is the target variable name contained in the x dataframe
#'
#' @return A list with coefficient values and a list with the respective names \code{overview_print}
#' @examples
#' data(example)
#' output_table <- anchor_regression(x, anchor, gamma, target_variable)
#' @export
#' @importFrom glmnet "glmnet"
#' @importFrom glmnet "cv.glmnet"


anchor_regression <- function(x, anchor, gamma, target_variable){
  x <- as.matrix(x)
  anchor <- as.matrix(anchor)
  fit_const <- lm(x ~ 1)
  fit <- lm(x ~ anchor)

  newdata <- fit_const$fitted.values + fit$residuals
  indices <- 1:nrow(newdata)
  j <-  match( 'V2', colnames(newdata))
  fit_glmnet <- cv.glmnet(x = newdata[indices,-c(j)],newdata[indices,j])
  lambda_cv <- fit_glmnet$lambda.1se
  newdata <- fit_const$fitted.values + fit$residuals + sqrt(gamma)*(fit$fitted.values-fit_const$fitted.values)
  fit_glmnet_new <- glmnet(x = newdata[indices,-c(j)],newdata[indices,j],lambda = lambda_cv)

  return_list = list(coeff = c(as.vector(coef(fit_glmnet_new))), names = c(colnames(newdata) ))
  return(return_list)
}
