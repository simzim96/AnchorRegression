#' @title anchor_regression
#'
#' @description Perform an Anchor Regression as described in Rothenhäusler et al.2020
#'
#' @param x is a dataframe containing the matrix x containing the independent variables
#' @param anchor is a dataframe containing the matrix anchor containing the anchor variable
#' @param gamma is the regularization parameter for the Anchor Regression
#' @param target_variable is the target variable name contained in the x dataframe
#' @param lambda indicates the lambda that is used in the Anchor Regression. 'CV' is used if it should be estimated by cross validation on the full subset.
#'
#' @return A list with coefficient values and a list with the respective names \code{overview_print}. Additionally the transformed data as x and y plus the fixed lambda coefficient.
#' @export
#' @importFrom glmnet glmnet cv.glmnet
#' @importFrom stats coef lm
#' @examples
#' x <- as.data.frame(matrix(data = rnorm(1000),nrow = 100,ncol = 10))
#' anchor <- as.data.frame(matrix(data = rnorm(200),nrow = 100,ncol = 2))
#' colnames(anchor) <- c('X1','X2')
#' gamma <- 2
#' target_variable <- 'V2'
#' anchor_regression(x, anchor, gamma, target_variable)








anchor_regression <- function(x, anchor, gamma, target_variable, lambda='CV'){

  # preliminary checks
  if(ncol(x)<3){
    print("unsufficient number of columns")
  }

  # convert to matrix for lm
  x <- as.matrix(x)
  anchor <- as.matrix(anchor)

  # tranform data
  fit_const <- lm(x ~ 1)
  fit <- lm(x ~ anchor)

  # estimate ideal lambda penalization as proposed by CV or skip and use other
  if(lambda=='CV'){
    cv_data <- fit_const$fitted.values + fit$residuals
    indices <- 1:nrow(cv_data)
    j <-  match( target_variable, colnames(cv_data))
    fit_glmnet_lasso <- cv.glmnet(x = cv_data[indices,-c(j)],cv_data[indices,j])
    lambda_cv <- fit_glmnet_lasso$lambda.1se
  }
  else{lambda_cv=lambda}

  # transform data for the Anchor Regression
  anchor_data <- fit_const$fitted.values + fit$residuals + sqrt(gamma)*(fit$fitted.values-fit_const$fitted.values)
  indices <- 1:nrow(anchor_data)
  j <-  match( target_variable, colnames(anchor_data))
  x <- anchor_data[indices,-c(j)]
  y <- anchor_data[indices,j]
  fit_glmnet_anchor <- glmnet(x = x,y = y,lambda = lambda_cv)

  return_list <- list(coeff = c(as.vector(coef(fit_glmnet_anchor))), names = c('Intercept',c(colnames(anchor_data)[!colnames(anchor_data) %in% target_variable] )), x = x, y = y,lambda = lambda_cv, model = fit_glmnet_anchor)
  return(return_list)
}
