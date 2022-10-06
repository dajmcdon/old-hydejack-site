##############
# Change the working directory to your directory tree!
###############
setwd('/Users/darrenho/Dropbox/INET Course/')

#crime = read.table('sexdata.Rdata',sep=',',header=F)

load('./data/sexdata.Rdata')
load('./data/sexmean.Rdata')

sex.data    = cbind(sex.mean,sex.data)
sex.data.na = apply(sex.data,2,is.na)
apply(sex.data.na,2,sum)
sex.data.new = sex.data[,apply(sex.data.na,2,sum)<20]
sex.data.new.complete = complete.cases(sex.data.new)


total.data = sex.data.new[sex.data.new.complete,]
ids = total.data$id
Y   = total.data$feeling_happy

rem = c(which(names(total.data) == 'id'),
#        which(names(total.data) == 'sex.mean'),
        which(names(total.data) == 'feeling_happy'),
        which(names(total.data) == 'gender'),
        which(names(total.data) == 'sex.more.than.once'),
        which(names(total.data) == 'sex_yesterday'),
        which(names(total.data) == 'anx_happy_rev'),
        which(names(total.data) == 'anx_steady_rev'),                
        which(names(total.data) == 'sex_morn'))

X   = total.data[,-rem]



n = length(Y)
p = ncol(X)

#########################
#beta_ols   = summary(lm(Y~.,data=X)) ##### NOT WELL DEFINED
#########################


#########################
# Ridge
#########################
library(glmnet)
ridge.out = cv.glmnet(x=scale(X),y=Y,alpha=0)#,nfolds=10,lambda=seq(.000001,.02,length=100))
which.min(ridge.out$cvm)

pdf('./Slides/Figures/ridgeCV.pdf')
plot(ridge.out$lambda[50:100],ridge.out$cvm[50:100],xlab='lambda',ylab='CV error',type='l')
print(ridge.out$lambda[which.min(ridge.out$cvm)])
abline(v=ridge.out$lambda[which.min(ridge.out$cvm)])
dev.off()

beta_ridge = ridge.out$glmnet.fit$beta[,which.min(ridge.out$cvm)]
pdf('./Slides/Figures/beta_ridge.pdf')
par(mar=c(5.1 ,6.1 ,4.1 ,2.1))
barplot(beta_ridge,horiz=T,cex.names=.4,las=1)#names.arg=c(''))
#axis(2,padj=0,at=1:ncol(X),labels=names(X),lwd.ticks=-1)
dev.off()

pdf('./Slides/Figures/beta_ridgeNoLabels.pdf')
par(mar=c(5.1 ,6.1 ,4.1 ,2.1))
barplot(beta_ridge,horiz=T,names.arg=c(''))
#axis(2,padj=0,at=1:ncol(X),labels=names(X),lwd.ticks=-1)
dev.off()

pdf('./Slides/Figures/beta_ridgePath.pdf')
lambdaRidgeGrid = ridge.out$lambda[41:100]
betaRidgeAll    = ridge.out$glmnet.fit$beta[,41:100]
plot(c(),xlim=range(lambdaRidgeGrid),ylim=range(betaRidgeAll),xlab='lambda',ylab='Coefficients')
colors = rainbow(nrow(betaRidgeAll),start=0,end=1)
for(i in 1:nrow(betaRidgeAll)){
	lines(lambdaRidgeGrid,betaRidgeAll[i,],type='l',col=colors[i])
}
abline(v=ridge.out$lambda[which.min(ridge.out$cvm)])
dev.off()

#########################
# Lasso
#########################

lasso.out = cv.glmnet(x=as.matrix(X),y=Y,alpha=1,lambda=seq(.01,.1,length=100),nfolds=20)
which.min(lasso.out$cvm)
lasso.out$lambda[which.min(lasso.out$cvm)]
pdf('./Slides/Figures/lassoCV.pdf')
plot(lasso.out$lambda,lasso.out$cvm,xlab='lambda',ylab='CV error',type='l')
print(lasso.out$lambda[which.min(lasso.out$cvm)])
abline(v=lasso.out$lambda[which.min(lasso.out$cvm)])
dev.off()

plotRange = c(-.15,.35)
beta_lasso = lasso.out$glmnet.fit$beta[,which.min(lasso.out$cvm)]
pdf('./Slides/Figures/beta_lasso.pdf')
par(mar=c(5.1 ,6.1 ,4.1 ,2.1))
barplot(beta_lasso,horiz=T,cex.names=.4,las=1,xlim=plotRange)#names.arg=c(''))
#axis(2,padj=0,at=1:ncol(X),labels=names(X),lwd.ticks=-1)
dev.off()

pdf('./Slides/Figures/beta_lassoNoLabels.pdf')
par(mar=c(5.1 ,6.1 ,4.1 ,2.1))
barplot(beta_lasso,horiz=T,names.arg=c(''),xlim=plotRange)
#axis(2,padj=0,at=1:ncol(X),labels=names(X),lwd.ticks=-1)
dev.off()

pdf('./Slides/Figures/beta_ridgeNoLabelsCompare.pdf')
par(mar=c(5.1 ,6.1 ,4.1 ,2.1))
barplot(beta_ridge,horiz=T,names.arg=c(''),xlim=plotRange)
#axis(2,padj=0,at=1:ncol(X),labels=names(X),lwd.ticks=-1)
dev.off()

pdf('./Slides/Figures/beta_lassoPath.pdf')
lambdalassoGrid = lasso.out$lambda[5:length(lasso.out$lambda)]
betalassoAll    = lasso.out$glmnet.fit$beta[,5:length(lasso.out$lambda)]
plot(c(),xlim=range(lambdalassoGrid),ylim=range(betalassoAll),xlab='lambda',ylab='Coefficients')
colors = rainbow(nrow(betalassoAll),start=0,end=1)
for(i in 1:nrow(betalassoAll)){
	lines(lambdalassoGrid,betalassoAll[i,],type='l',col=colors[i])
}
abline(v=lasso.out$lambda[which.min(lasso.out$cvm)])
dev.off()

beta_lasso = lasso.out$glmnet.fit$beta[,which.min(lasso.out$cvm)]
out.lm = lm(Y~.,data=X[,which(abs(beta_lasso) > 0)])
summary(out.lm)