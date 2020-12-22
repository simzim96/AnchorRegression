#' @title weighted_anchor_regression
#'
#' @description Perform a prediction for a Weighted Anchor Regression model
#'
#' @param names list of variable names corresponding to the coefficients in coeff
#' @param coeff list of coefficients corresponding to the coefficients in names
#' @param x is a dataframe containing the matrix x containing the independent variables
#' @param anchor is a dataframe containing the matrix anchor containing the anchor variable
#' @param gamma is the regularization parameter for the Anchor Regression
#' @param target_variable is the target variable name contained in the x dataframe
#'
#' @return A list of predictions.
#' @export
#' @importFrom stats coef lm
#' @examples
#'    # number of observed environments
#'    environments <- 10
#'
#'    # populate list with generated data of x and anchor
#'    data_x_list <- c()
#'    data_anchor_list <- c()
#'    for(e in 1:environments){
#'      x <- as.data.frame(matrix(data = rnorm(100),nrow = 100,ncol = 10))
#'      anchor <- as.data.frame(matrix(data = rnorm(200),nrow = 100,ncol = 2))
#'      colnames(anchor) <- c('X1','X2')
#'      data_x_list[[e]] <- x
#'      data_anchor_list[[e]]  <- anchor
#'    }
#'
#'    # estimate model
#'    gamma <- 2
#'    target_variable <- 'V2'
#'    weighted_anchor_model <- weighted_anchor_regression(data_x_list,
#'                                                        data_anchor_list,
#'                                                        gamma,
#'                                                        target_variable,
#'                                                        anchor_model_pre=NULL,
#'                                                        test_split=0.4,
#'                                                        lambda=0)
#'    weighted_anchor_prediction(weighted_anchor_model$names,
#'                               weighted_anchor_model$coeff,
#'                               x,
#'                               anchor,
#'                               gamma,
#'                               target_variable)


weighted_anchor_prediction <- function(names,coeff, x, anchor, gamma, target_variable){
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

  # prediction
  coefficients_df <- data.frame(names = names,coefficients = coeff)
  coefficients_df$names <- NULL
  prediction <- as.matrix(x)%*%as.matrix(colMeans(coefficients_df[,2:ncol(coefficients_df)]))

  return(prediction)
}

