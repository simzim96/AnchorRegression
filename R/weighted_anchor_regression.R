#' @title weighted_anchor_regression
#'
#' @description Estimates weighted Anchor Regression coefficients
#'
#' @param data_x_list list containing coefficient dataframes for different environments
#' @param data_anchor_list list containing anchor dataframes for different environments
#' @param gamma is the regularization parameter for the Anchor Regression
#' @param target_variable is the target variable name contained in the x dataframe
#' @param anchor_model_pre is the pre estimated model for the Anchor Regression. In case of NULL a new model is estimated.
#' @param test_split is desired test/train split for the estimation
#'
#' @return A list estimated coefficients with names, weights and the raw coefficient matrix
#' @export
#' @importFrom stats coef lm


weighted_anchor_regression <- function(data_x_list,data_anchor_list,gamma,target_variable,anchor_model_pre=NULL,test_split=0.4){

  # initialize coefficient output matrix and patient score list
  coefficient_matrix <- NULL
  patient_prediction_scores <- c()

  # loop thorugh all environments
  for(patient in 1:length(data_anchor_list)){

    # create test and train split
    smp_size <- floor(test_split * nrow(data_x_list[[patient]]))


    train_x_ind <- sample(seq_len(nrow(data_x_list[[patient]])), size = smp_size)
    train_anchor_ind <- sample(seq_len(nrow(data_anchor_list[[patient]])), size = smp_size)

    train_x <- data_x_list[[patient]][train_x_ind, ]
    test_x <- data_x_list[[patient]][-train_x_ind, ]

    train_anchor <- data_anchor_list[[patient]][train_anchor_ind, ]
    test_anchor <- data_anchor_list[[patient]][-train_anchor_ind, ]

    # estimate model if desired
    if(is.null(anchor_model_pre) ==TRUE){
      anchor_model <- anchor_regression(train_x,train_anchor,gamma,target_variable)
    }else{
      anchor_model <- anchor_model_pre
    }


    # save coefficients
    coefficient_matrix <- cbind(coefficient_matrix, anchor_model$coeff)

    # predict and calculate the  mse for the predictions
    prediction <- anchor_prediction(anchor_model$model, test_x, test_anchor, gamma, target_variable)
    result <- as.data.frame(test_x[target_variable])
    result$prediction <- prediction
    result$diff2 <- (result[,1] - result$prediction)^2
    mse <- mean(result$diff2)
    patient_prediction_scores <- c(patient_prediction_scores, mse)

  }
  # calculate weights and resulting coefficients
  weights <- patient_prediction_scores/sum(patient_prediction_scores)
  weighted_coefficients <- weights%*%t(coefficient_matrix)

  # return result
  return_list = list(coeff = weighted_coefficients, names = anchor_model$names, raw_coeffs = coefficient_matrix,weights = weights)

  return(return_list)
}
