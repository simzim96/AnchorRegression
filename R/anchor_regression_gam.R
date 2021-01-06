#' @title anchor_regression_gam
#'
#' @description Perform an Generalized Additive Anchor Regression
#'
#' @param x is a dataframe containing the matrix x containing the independent variables
#' @param anchor is a dataframe containing the matrix anchor containing the anchor variable
#' @param gamma is the regularization parameter for the Anchor Regression
#' @param target_variable is the target variable name contained in the x dataframe
#' @param bin_factor factor variable that can be transformed to a factor to partial out effects
#' @param force_binary if set to TRUE forces bin_factor to be binary
#'
#' @return A list with coefficient values and a list with the respective names \code{overview_print}. Additionally the transformed data as x and y plus the fixed lambda coefficient.
#' @export
#' @importFrom mgcv gam
#' @importFrom stats coef lm as.formula
#' @examples
#' x <- as.data.frame(matrix(data = rnorm(10000),nrow = 1000,ncol = 10))
#' x$bin <- sample(nrow(x),x = c(1,0),prob = c(0.5,0.5),replace = TRUE)
#' anchor <- as.data.frame(matrix(data = rnorm(2000),nrow = 1000,ncol = 2))
#' colnames(anchor) <- c('X1','X2')
#' gamma <- 2
#' target_variable <- 'V2'
#' anchor_regression_gam(x, anchor, gamma, target_variable,bin_factor =  "bin")


anchor_regression_gam <- function (x, anchor, gamma, target_variable, bin_factor = NULL, force_binary = TRUE) {
  if (ncol(x) < 3) {
    print("unsufficient number of columns")
  }
  # transform data
  x <- as.matrix(x)
  anchor <- as.matrix(anchor)
  fit_const <- lm(x ~ 1)
  fit <- lm(x ~ anchor)

  # slice data for model fitting
  anchor_data <- fit_const$fitted.values + fit$residuals +
    sqrt(gamma) * (fit$fitted.values - fit_const$fitted.values)
  indices <- 1:nrow(anchor_data)
  j <- match(target_variable, colnames(anchor_data))
  x <- anchor_data[indices, -c(j)]
  y <- anchor_data[indices, j]


  # extract binary columns
  x <- as.data.frame(anchor_data)
  uniq <- lapply(x, unique)
  nuniq <- as.data.frame(lengths(uniq))
  nuniq_names <- names(as.data.frame(nuniq))[as.vector(nuniq<4)]


  vars <- colnames(x)[!colnames(x) %in% target_variable]
  vars_non_bin <- vars[!vars %in% nuniq_names]

  # generate formula
  if (is.null(bin_factor) != TRUE) {
          if(is.factor(x[, bin_factor])!=TRUE){
            if(force_binary==TRUE){
              x[bin_factor] <- as.factor(ifelse(round(x[, bin_factor]) >=1,1,0))
            }else{
              x[bin_factor] <- as.factor(round(x[, bin_factor]))
            }
          }

          vars_non_bin_fac <- vars_non_bin[!vars_non_bin %in% bin_factor]
          if (length(nuniq_names) != 0) {
              col <- paste0(", by=", bin_factor, ")+s(")
              form <- paste(target_variable, "~ s(", paste(vars_non_bin_fac,
                  collapse = col), ",by = ",bin_factor,")+", paste(nuniq_names, collapse = " + "))
          }
          else {
              col <- paste0(", by=", bin_factor, ")+s(")
              form <- paste(target_variable, "~ s(", paste(vars_non_bin_fac,
                  collapse = col),",by = ",bin_factor, ")")
          }
      } else {
          if (length(nuniq_names) != 0) {
              form <- paste(target_variable, "~ s(", paste(vars_non_bin,
                  collapse = ") + s("), ")+", paste(nuniq_names,
                  collapse = " + "))
          }
          else {
              form <- paste(target_variable, "~ s(", paste(vars_non_bin,
                  collapse = ") + s("), ")")
          }
      }
  # estimate model
  model <-  gam(as.formula(form),data=x, method = "REML")

  return_list <- list(model = model)
  return(return_list)
}

