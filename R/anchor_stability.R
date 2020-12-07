#' @title anchor_stability
#'
#' @description Perform an Anchor Stability Analysis as described in Rothenhäusler et al.2020
#'
#' @param x is a dataframe containing the matrix x containing the independent variables
#' @param anchor is a dataframe containing the matrix anchor containing the anchor variable
#' @param target_variable is the target variable name contained in the x dataframe
#' @param lambda indicates the lambda that is used in the Anchor Regression. 'CV' is used if it should be estimated by cross validation on the full subset.
#' @param alpha significance level for test decision on coefficient significance
#' @return A dataframe containing the stability values for each coefficient
#' @export
#' @importFrom glmnet glmnet cv.glmnet
#' @importFrom stats coef lm
#' @importFrom selectiveInference fixedLassoInf
#' @examples
#' x <- as.data.frame(matrix(data = rnorm(1000),nrow = 100,ncol = 10))
#' anchor <- as.data.frame(matrix(data = rnorm(200),nrow = 100,ncol = 2))
#' colnames(anchor) <- c('X1','X2')
#' gamma <- 2
#' target_variable <- 'V2'
#' anchor_stability(x, anchor, target_variable, lambda, alpha=0.05)


anchor_stability <- function(x, anchor, target_variable, lambda=0, alpha=0.05, p_procedure = "naive"){

  anchor_gamma_0 <- anchor_regression(x, anchor, 0, target_variable)

  anchor_gamma_inf <-  anchor_regression(x, anchor, 1e+29, target_variable)

  if(p_procedure == "naive"){
    stable <- ifelse(anchor_gamma_0$coeff!=0 & anchor_gamma_inf$coeff!=0,"stable", "not-stable")
    result <- data.frame(anchor_gamma_0$names,stable)
    names(result) <- c("coefficient","anchor-stability")
  }
  # Post lasso
  if(p_procedure == "post-lasso"){
    if(sum(coef(anchor_gamma_0$model, s=anchor_gamma_0$lambda/length(anchor_gamma_0$y))[-1])!=0){
      p_0 <- selectiveInference::fixedLassoInf(anchor_gamma_0$x,anchor_gamma_0$y,coef(anchor_gamma_0$model, s=anchor_gamma_0$lambda/length(anchor_gamma_0$y))[-1],anchor_gamma_0$lambda)$pv

    }else{p_0=NULL}

    if(sum(coef(anchor_gamma_inf$model, s=anchor_gamma_inf$lambda/length(anchor_gamma_inf$y))[-1])!=0){
      p_inf <- selectiveInference::fixedLassoInf(anchor_gamma_inf$x,anchor_gamma_inf$y,coef(anchor_gamma_inf$model, s=anchor_gamma_inf$lambda/length(anchor_gamma_inf$y))[-1],anchor_gamma_inf$lambda)$pv

    }else{p_inf=NULL}

    print(p_0==NULL)
    print(p_inf==NULL)

    if(!is.null(p_0) | !is.null(p_inf)){

      result <- data.frame(anchor_gamma_0$names,"Not Stable")
      colnames(result) <- c("coefficient","anchor-stability")
    }else{
      result <- data.frame(anchor_gamma_0$names,p_0,p_inf)

      colnames(result) <- c("coefficient","pv0","pvInf")
      result$anchor-stability <- ifelse(result$pv0<alpha & result$pvInf<alpha,"stable","not_stable")
    }
  }
  return(result)
}



