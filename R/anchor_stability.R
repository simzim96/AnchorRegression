anchor_stability <- function (x, 
                              anchor, 
                              target_variable, 
                              lambda = 0, 
                              alpha = 0.05, 
                              p_procedure = "bootstrap") 
{
  anchor_gamma_0 <- anchor_regression(x, anchor, 0, target_variable)
  anchor_gamma_inf <- anchor_regression(x, anchor, 1e+29, target_variable)
  # naive 
  if (p_procedure == "naive") {
    stable <- ifelse(anchor_gamma_0$coeff != 0 & anchor_gamma_inf$coeff != 
                       0, "stable", "not-stable")
    result <- data.frame(anchor_gamma_0$names, stable)
    names(result) <- c("coefficient", "anchor_stability")
  }
  # post-lasso
  if (p_procedure == "post-lasso") {
    if (sum(coef(anchor_gamma_0$model, s = anchor_gamma_0$lambda/length(anchor_gamma_0$y))[-1]) != 
        0) {
      p_0 <- selectiveInference::fixedLassoInf(anchor_gamma_0$x, 
                                               anchor_gamma_0$y, coef(anchor_gamma_0$model, 
                                                                      s = anchor_gamma_0$lambda/length(anchor_gamma_0$y))[-1], 
                                               anchor_gamma_0$lambda)$pv
    }
    else {
      p_0 = NULL
    }
    if (sum(coef(anchor_gamma_inf$model, s = anchor_gamma_inf$lambda/length(anchor_gamma_inf$y))[-1]) != 
        0) {
      p_inf <- selectiveInference::fixedLassoInf(anchor_gamma_inf$x, 
                                                 anchor_gamma_inf$y, coef(anchor_gamma_inf$model, 
                                                                          s = anchor_gamma_inf$lambda/length(anchor_gamma_inf$y))[-1], 
                                                 anchor_gamma_inf$lambda)$pv
    }
    else {
      p_inf = NULL
    }
    if (!is.null(p_0) | !is.null(p_inf)) {
      result <- data.frame(anchor_gamma_0$names, "Not Stable")
      colnames(result) <- c("coefficient", "anchor-stability")
    }
    else {
      result <- data.frame(anchor_gamma_0$names, p_0, p_inf)
      colnames(result) <- c("coefficient", "pv0", "pvInf")
      result$anchor_stability <- ifelse(result$pv0 < alpha & 
                                          result$pvInf < alpha, "stable", "not_stable")
    }
  }
  # bootstrap
  if(p_procedure == "bootstrap"){
    
    
    nboot = 100
    
    
  
    result_frame <- NULL
    
    for(i in 1:nboot){
      x_b <- sample(x[x$is_infused!=1,])
      anchor_b <- sample(anchor[x$is_infused!=1,])
      anchor_gamma_target <- anchor_regression(x_b, anchor_b, target_gamma, target_variable)
      result_frame <- rbind(result_frame,anchor_gamma_target$coeff)
      
    }
    
    
    # construct conf-int
    significance_result <- c()
    for(i in 1:ncol(result_frame)){
      ct <- anchor_gamma_0$coeff[i]
      
      critN_1 <- quantile(result_frame[,i], alpha/2)
      critN_2 <- quantile(result_frame[,i], 1-alpha/2)
      significance_result[i] <- ( ct < critN_1 | ct > critN_2)
    }
    
    # create frame
    stable <- ifelse(significance_result==TRUE,'stable','not-stable')
    result <- data.frame(anchor_gamma_0$names, stable)
    names(result) <- c("coefficient", "anchor_stability")
    
    
  }
  return(result)
}
