generate.data <- function(n, cor){
  epsilon = rnorm(n)
  X = matrix(rnorm(2*n),nrow=n) %*% 
    chol(matrix(c(1, cor, cor,1), 2, 2))
  ## See the question below about X
  beta = c(3, 2, 1)
  Y = cbind(1, X) %*% beta + epsilon
  df = data.frame(Y, X)
  return(df)
}

intervals <- function(n, cor){
  df = generate.data(n, cor)
  mdl = lm(Y~X1+X2, data=df)
  itvals = confint(mdl) ## Get the confidences intervals for the bhats (95%)
  widths = itvals %*% c(-1, 1) ## Find the width of each interval
  avg = mean(widths[-1]) ## get the average width, ignore intercept
  return(avg)
}

n = 250
cors = seq(-1e+1e-3, 1-1e-3, length.out = 50)


intervals <- function(n, cor){
  df = generate.data(n, cor)
  mdl = lm(Y~X1+X2, data=df)
  itvals = confint(mdl) ## Get the confidences intervals for the bhats (95%)
  widths = itvals %*% c(-1, 1) ## Find the width of each interval
  avg = mean(widths[-1]) ## get the average width, ignore intercept
  avg
}
## calculate the average width for each value of cors
## repeat the simulation 25 times
avg.widths = replicate(25, sapply(cors, intervals, n=n)) # better than looping
## plot cors (x-axis) vs. average widths (y-axis)
dat = data.frame(correlation=cors, avg.widths=avg.widths)
ggplot(dat, aes(x=correlation,y=avg.widths)) + geom_point() + geom_path() +
  scale_y_log10() + ylab('average CI width')
  
ggplot(dat, aes(x=correlation,y=avg.widths)) + geom_path(color='#ff4900') + 
  geom_point(color='#0b62a4') +
  scale_y_log10() +theme_minimal(12,'Times') + ylab('average CI width')
  
ggsave(file='../assets/img/ci-widths.jpg',width=8,height=3)
  