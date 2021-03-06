x <- as.data.frame(matrix(data = rnorm(1000),nrow = 100,ncol = 10))

anchor <- as.data.frame(matrix(data = rnorm(200),nrow = 100,ncol = 2))
colnames(anchor) <- c('X1','X2')
gamma <- 2

target_variable <- 'V2'

anchor_regression(x, anchor, gamma, target_variable)

