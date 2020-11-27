x <- as.data.frame(matrix(data = rnorm(100),nrow = 10,ncol = 10))

anchor <- as.data.frame(matrix(data = rnorm(20),nrow = 10,ncol = 2))
colnames(anchor) <- c('X1','X2')
gamma <- 2

target_variable <- 'V2'

anchor_regression(x, anchor, gamma, target_variable)
