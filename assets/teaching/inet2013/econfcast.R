########################
## Code for econometric forecasting slides
########################


## This section generates the autoregressive model and calculates its risk

## Set magic numbers
p.true = 10 # order of true AR
N = 75
n.samp = 100 
max.ar = 17 # largest order to search over
noise.var = 1
b = seq(.9,.1,len=p.true)*rep(c(1,-1),p.true/2) # the AR coefficients, this is stationary


## Function to predict from the ar output
ar.pred <- function(y, bhat){
  p = length(bhat)-1
  n = length(y)
  resid = double(n-p-1)
  for(i in 1:(n-p)) resid[i] = y[i+p] - sum(c(1,y[i:(i+p-1)])*rev(bhat))
  return(mean(resid^2,na.rm=T))
}

## Generate time series
set.seed(12345) # for replicable research
x = replicate(n.samp, arima.sim(list(ar=b),n=N,sd=sqrt(noise.var)) )

## Preallocate space
estim.coef <-vector('list', length=max.ar)
for(j in 1:max.ar) estim.coef[[j]] = matrix(NA, n.samp, j+1)
train.err <- matrix(NA, n.samp, max.ar)
test.err <- matrix(NA, n.samp, max.ar)



## Estimate all the coeffecients for all lags (loops are inefficient but clear)
## Save training error
for (i in 1:n.samp) {
  for (j in 1:max.ar) {
    bhat <- ar.ols(x[,i], aic=FALSE, order.max=j)
    estim.coef[[j]][i,] <- c(bhat$ar, bhat$x.intercept)
    train.err[i,j] <- mean(bhat$resid^2, na.rm=TRUE)
  }
}



## Use bhat of each x's to predict all other x's for each order

for (i in 1:n.samp) {
  y <- x[,-i]
  print(i)
  for (j in 1:max.ar) {
    bhat = estim.coef[[j]][i,]
    pred.ar = apply(y, 2, ar.pred, bhat=bhat)
    test.err[i,j] = mean(pred.ar)
  }
}



## Plotting
train.mean <- colMeans(train.err)
test.mean <- colMeans(test.err)

# Plot on one graph
cut = 15 # in case you don't want to plot all max.ar
blue = '#0B61A4'
orange = '#FF9200'
par(family='serif')
plot(NA,NA,ylim=range(test.err[,1:cut], train.err[,1:cut]),
     ylab="prediction error",xlim=c(1,cut),xlab="complexity (order)",las=1,bty='n',main='')
matlines(t(train.err),col=orange,lwd=.1,lty=1)
matlines(t(test.err),col=blue,lwd=.1,lty=1)
lines(train.mean,col=orange,lwd=3)
lines(test.mean,col=blue,lwd=3)
## Beware magic numbers below
text(3,2,label='High bias\n Low Variance')
text(13,2,label='Low bias\n High variance')
arrows(c(4,12),c(1.85,1.85),c(2,14),len=.1)
abline(v=which.min(test.mean),lty=3,col=blue,lwd=2)
arrows(12, train.mean[12]+.05, 12, test.mean[12]-.05, len=.1,code=3)
text(13,.5*(train.mean[12]+test.mean[12]),'optimism')


## Code for the BVAR and Ridge VAR begins here

## Colors I like
green = '#00AF64'
blue = '#0B61A4'
red = '#FF4900'
orange = '#FF9200'

require(manipulate)

par(mar=c(3,4,.1,.1),bty='n',family='serif')

## BVAR example
n = 100
p = 2

Y = matrix(NA, n, p)
set.seed(12345)
for(i in 1:p) Y[,i] = arima.sim(list(ar=.9),n)

par(mfrow=c(1,1))
X = Y[-n,]
Y.trim = Y[-1,]


ridge <- function(lam){
  XtX = t(X) %*% X
  XtY = t(X) %*% Y.trim
  pred = X %*% solve(XtX + lam*diag(2)) %*% XtY
  matplot(pred,col=c(red,blue),lty=2,ty='l',ylim=range(Y))
  matlines(Y.trim,col=c(red,blue),lty=1)
}

bvar <- function(lam){
  XtX = t(X) %*% X
  XtY = t(X) %*% Y.trim
  pred = X %*% solve(XtX + lam*diag(2)) %*% (XtY- lam*diag(2)) #really??
  matplot(pred,col=c(red,blue),lty=2,ty='l',ylim=range(Y))
  matlines(Y.trim,col=c(red,blue),lty=1)
}

manipulate(ridge(lam), lam=slider(1,1000,step=10))
manipulate(bvar(lam), lam=slider(1,5000,step=50))




## HP filter Example begins here
hpfilter <- function(y, w=1600){
  eye = diag(length(y))
  d = diff(eye,d=2)
  ybar = solve(eye + w*crossprod(d), y)
  resid = (y/ybar - 1)*100
  return(list(ybar=ybar,resid=resid))
}

hp.plot <- function(w, y){
  hp = hpfilter(y, w)
  par(mfrow=c(1,2),family='serif')
  plot(date, y, ty='l',col=blue,las=1,ann=FALSE,lwd=3)
  lines(date,hp$ybar,col=red,lwd=2)
  plot(date, hp$resid,col=red,lwd=3,ty='l',ann=FALSE,
       las=1,ylim=c(-6.5,4))
  abline(h=0,col=blue,lwd=1)
}
d = read.csv('~/Dropbox/INET Course/gdp.csv')
## Code to plot GDP data
par(mfrow=c(1,2))
plot(date,gdpc1,ty='l',col=blue,las=1,ann=FALSE,lwd=3)
plot(date,gdpc1.chg,ty='l',col=blue,las=1,ann=FALSE,lwd=3)
par(mfrow=c(1,1))
plot(date[225:264],gdpc1[225:264],ty='l',col=blue,las=1,ann=FALSE,lwd=3)


## HP filter lambda
manipulate(hp.plot(w=lambda, gdpc1), lambda=picker(.1,10,500,1600,5000,10000))

detach(d)

## Code to do the DSGE examples (note: I have not included the actual RBC code.
## Feel free to email me if you want this as well
## Change the path to load the data
load('~/Dropbox/INET Course/data/RBCsimul.Rdata') # RBC flipping results
load('~/Dropbox/INET Course/data/RBCdetrend1.Rdata') # filtered macro data
load('~/Dropbox/INET Course/data/RBCraw.Rdata') # raw macro data


colvec = c(blue, red, green, orange)
date = seq(1949,by=.25,len=245)
date1 = seq(1948,by=.25,len=249)


par(mfrow=c(1,1), mar=c(3,4,1,1),family='serif',las=1,bty='n')
matplot(date1,rbc.data,col=colvec,lty=1,lwd=3,ty='l',ann=FALSE) # raw data
matplot(date1[-1],t(rbc.detrend1)[-1,], col=colvec,lty=1,lwd=3,ty='l') #1-sided detrend
plot(date1[-1],rep(NA,length(date1[-1])),ylim=range(rbc.detrend1[,-1])) # recession
rect(2007.75,-10,2009.5,10,col='grey80',border=NA)
matlines(date1[-1],t(rbc.detrend1)[-1,], col=colvec,lty=1,lwd=3,ty='l')
mean((one.side[[1]]$Y[,-c(234:243)]-one.side[[1]]$pred[,-c(234:243)])^2)*1000
mean((one.side[[1]]$Y[,234:243]-one.side[[1]]$pred[,234:243])^2)*1000





## Set up Lasso var(4)

Y = t(rbc.detrend1)[-c(1:4),]
X1 = t(rbc.detrend1)[-c(1:3,249),]
X2 = t(rbc.detrend1)[-c(1:2,248:249),]
X3 = t(rbc.detrend1)[-c(1,247:249),]
X4 = t(rbc.detrend1)[-c(246:249),]
X = cbind(X1,X2,X3,X4)

require(glmnet)
elnet = list()
cv.el = list()
best.lam = double(4)
preds = list()
oos.mse = double(4)
for(i in 1:4){
  cv.el[[i]] = cv.glmnet(X[1:205,], Y[1:205,i],alpha=1)
  best.lam[i] = which.min(cv.el[[i]]$cvm)
  elnet[[i]] = glmnet(X[1:205,],Y[1:205,i],alpha=1)
  preds[[i]] = predict(elnet[[i]], X[206:245,], s=cv.el[[i]]$lambda.min)
  oos.mse[i] = mean((preds[[i]]-Y[206:245,i])^2)
}

par(mfrow=c(2,1))
oos = 206:245
ylims = range(Y[oos,1],one.side[[1]]$pred[1,oos],preds[[1]])
plot(date[oos], Y[oos,1], ty='l', lwd=3,ylim=ylims)
lines(date[oos], one.side[[1]]$pred[1,oos], col=orange,lwd=3)
lines(date[oos], preds[[1]], col=green,lwd=3)

ylims = range(Y[oos,2],one.side[[1]]$pred[2,oos],preds[[2]])
plot(date.fcast[oos], Y[oos,2], ty='l', lwd=3,ylim=ylims)
lines(date.fcast[oos], one.side[[1]]$pred[2,oos], col=orange,lwd=3)
lines(date.fcast[oos], preds[[2]], col=green,lwd=3)

ylims = range(Y[oos,3],one.side[[1]]$pred[3,oos],preds[[3]])
plot(date.fcast[oos], Y[oos,3], ty='l', lwd=3,ylim=ylims)
lines(date.fcast[oos], one.side[[1]]$pred[3,oos], col=orange,lwd=3)
lines(date.fcast[oos], preds[[3]], col=green,lwd=3)

ylims = range(Y[oos,4],one.side[[1]]$pred[4,oos],preds[[4]])
plot(date.fcast[oos], Y[oos,4], ty='l', lwd=3,ylim=ylims)
lines(date.fcast[oos], one.side[[1]]$pred[4,oos], col=orange,lwd=3)
lines(date.fcast[oos], preds[[4]], col=green,lwd=3)

## Plot switching performance
train = double(24)
for(i in 1:24) train[i] = one.side[[i]]$train
ord = names(one.side)
baseline = one.side[[1]]$baseline

par(mfcol=c(1,1))
colord = ifelse(train[-1]>train[1],blue,red)
plot(NA,NA,ylim=c(0,1),xlim=c(1,23),xaxt='n')
abline(h=train[1]/baseline,lwd=.5,col=blue)
text(1:23,train[-1]/baseline,label=ord[-1],col=colord)
abline(h=mean(oos.mse)/baseline,col=red)
