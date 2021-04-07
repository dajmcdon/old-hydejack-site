##############
# Change the working directory to your directory tree!
###############
setwd('/Users/darrenho/Dropbox/INET Course/')



## Random forests for recession prediction
states = read.csv('./data/statelevel.csv')
national = read.csv('./data/national.csv')
n = nrow(states)
states = states[-c((n-5):n),-1]
attach(national)
term.spread = GS10 - TB3MS
growth.rate <- function(tseries, horizon=1){
  n = length(tseries)
  Y = tseries[(horizon+1):n]/tseries[1:(n-horizon)]
  return(Y-1)
}

growth.vars = apply(cbind(SP500,INDPRO,PAYEMS),2,growth.rate,horizon=3)
X.fin = cbind(growth.vars[,1], FEDFUNDS[-c(1:3)],term.spread[-c(1:3)])
X.act = cbind(growth.vars[,2:3])
X.nat = cbind(X.fin[-1,],X.act[-dim(X.act)[1],])
names(X.nat) = c('sp500','fed.funds','term.spread','ind.prod','pay.emp')
dates = seq(1960+4/12,by=1/12,length=614)
Y = as.factor(USREC[-c(1:4)])
Y.baseline = RECPROUSM156N[-c(1:4)]
detach(national)
X.state = states[-c(1:3,nrow(states)),]
X.nat = data.frame(X.nat)

X = cbind(X.state,X.nat)
names.X = names(X)
names.X[52:56] = c('sp500','fed.funds','term.spread','ind.prod','pay.emp')
colnames(X) = names.X

lag.dat <- function(X,Y,fcast.horizon=1){
  n = length(Y)
  X = X[1:(n-fcast.horizon),]
  Y = Y[(1+fcast.horizon):n]
  return(list(X=as.matrix(X),Y=Y))
}

now = list(X=as.matrix(X),Y=Y)
one = lag.dat(X,Y,1)
two = lag.dat(X,Y,2)
three = lag.dat(X,Y,3)


train = 1:477 #predict most recent recession
test  = 478:length(Y)

miss.class =function(pred.class,true.class,return=FALSE){
	confusion.mat = table(pred.class,true.class)
	if(return){
		return(1-sum(diag(confusion.mat))/sum(confusion.mat))	
	}
	else{
		print('miss-class')
		print(1-sum(diag(confusion.mat))/sum(confusion.mat))
		print('confusion mat')
		print(confusion.mat)
	}
}

out.glm  = glm(Y~.,data=X,subset=train,family=binomial)#glm fails to converge

#require(MASS)
##LDA
#out.lda  = lda(Y~.,data=X,subset=train)
#pred.lda = predict(out.lda,X[test,])
#miss.class(pred.lda$class,Y[test])
##QDA
#out.qda  = qda(Y~.,data=X,subset=train)
#pred.qda = predict(out.qda,X[test,])
#miss.class(pred.qda$class,Y[test])


##############
# TREE THINGS
##############
require(tree)
pdf('./Slides/Figures/recessionTrees0a.pdf')
out.tree.plt = tree(Y~.,data=X[,c(20,52)])#I picked these to make an interesting small tree
tmp.tree     = prune.tree(out.tree.plt,best=3)
plot(tmp.tree)
text(tmp.tree)
dev.off()

pdf('./Slides/Figures/recessionTrees0b.pdf')
require(graphics)
color = rep('blue',nrow(X))
color[X$Maine < 0.113708 & X$sp500 < -0.0313227] = 'orange'
plot(X$Maine,X$sp500,xlab='Maine',ylab='S & P 500',cex=.4,col=color)
abline(h = -0.0313227) #S & P 500
segments(x0 = 0.113708, y0 = min(X$sp500-.5), y1 = -0.0313227) #MAINE
text(x = c(-2,-2,1.5),y = c(-0.2,0.2,-0.2), labels=c('R1','R2','R3'))
dev.off()

out.tree.orig   = tree(Y~.,data=X,subset=train,split='dev')
out.tree.cv     = cv.tree(out.tree.orig,K=3)
best.k          = out.tree.cv$k[which.min(out.tree.cv$dev)]
out.tree        = prune.tree(out.tree.orig,k=best.k)
class.tree      = predict(out.tree,X[test,],type='class')
class.tree.orig = predict(out.tree.orig,X[test,],type='class')
miss.class(class.tree,Y[test])
miss.class(class.tree.orig,Y[test])

pdf('./Slides/Figures/recessionTrees1unpruned.pdf')
plot(out.tree.orig)
text(out.tree.orig)
dev.off()

pdf('./Slides/Figures/recessionTrees1pruned.pdf')
plot(out.tree)
text(out.tree)
dev.off()

pdf('./Slides/Figures/recessionTrees2postProb.pdf')
out.tree.plot = out.tree
pure.tree       = predict(out.tree.plot,X[test,],type='vector')
color = rep('red',length(test))
color[class.tree == Y[test]] = 'blue'
plot(dates[test],pure.tree[,2],col=color,cex=1.2,pch=19,xlab='Date', ylab='Probability')#,main='Posterior Prob')
dev.off()
pdf('./Slides/Figures/recessionTrees2class.pdf')
color = rep('red',length(test))
color[class.tree.orig == Y[test]] = 'blue'
plot(dates[test],Y[test],col=color,cex=1.2,pch=19,xlab='Date',ylab='Classification',yaxt='n')
ticks = c(1,2)
labels = c('Growth','Recession')
axis(2,at=ticks, labels=labels,las=1,cex.axis=.75)
dev.off()

#RF
require(randomForest)
out.rf   = randomForest(Y~.,data=X,subset=train)
class.rf = predict(out.rf,X[test,])
miss.class(class.rf,Y[test])
#bagging
out.bag   = randomForest(Y~.,data=X,subset=train,mtry=ncol(X))
class.bag = predict(out.bag,X[test,])
miss.class(class.bag,Y[test])
pdf('./Slides/Figures/	')
varImpPlot(out.bag,type=2,main='')
dev.off()

pdf('./Slides/Figures/recessionTrees2classBag.pdf')
color = rep('red',length(test))
color[class.bag == Y[test]] = 'blue'
plot(dates[test],Y[test],col=color,cex=1.1,pch=19,xlab='Date',ylab='Classification',yaxt='n')
#points(dates[test],Y[test],pch=19,cex=.3)
ticks = c(1,2)
labels = c('Growth','Recession')
axis(2,at=ticks, labels=labels,las=1,cex.axis=.75)
dev.off()

##############
# Alternate Classifiers
##############

#Knn
require(class)
knn.cv.results = rep(0,10)
sweep = 1
for(k in 1:10){
	knn.cv.out            = knn.cv(X[train,],Y[train],k=k)
	knn.cv.results[sweep] = mean(knn.cv.out == Y[train])
	sweep = sweep + 1
}
print(which.max(knn.cv.results))
class.knn   = knn(X[train,],X[test,],Y[train],k=which.max(knn.cv.results))
miss.class(class.knn,Y[test])
#svm
require(e1071)
ranges=list('gamma'=seq(0,.005,length=10)[-1],'cost'=seq(.45,1,length=10)[-1])
tune.svm  = tune(svm,train.x=X[train,],train.y=Y[train],ranges=ranges)
out.svm   = svm(Y~.,data=X,subset=train,
gamma=tune.svm$best.parameters$gamma,
cost=tune.svm$best.parameters$cost)
class.svm = predict(out.svm,X[test,])
miss.class(class.svm,Y[test])
print(lapply(ranges,range))
print(tune.svm$best.parameters)
#SVM polynomial
ranges=list('gamma'=seq(0,.5,length=10)[-1],'cost'=seq(.45,1,length=10)[-1],
'coef0' = seq(0,.5,length=10)[-1])
tune.svm  = tune(svm,train.x=X[train,],train.y=Y[train],ranges=ranges)
out.svm   = svm(Y~.,data=X,subset=train,type='C',kernel='polynomial',
gamma=tune.svm$best.parameters$gamma,
cost=tune.svm$best.parameters$cost,
coef0=tune.svm$best.parameters$coef0)
class.svm = predict(out.svm,X[test,])
table(class.svm,Y[test])
print(lapply(ranges,range))
print(tune.svm$best.parameters)

pdf('recessionResultsNoLag.pdf')
par(mfrow=c(4,1))
color = rep('red',length(test))
color[pred.lda$class == Y[test]] = 'blue'
plot(dates[test],Y[test],col=color,main='LDA')
color = rep('red',length(test))
color[class.svm == Y[test]] = 'blue'
plot(dates[test],Y[test],col=color,main='SVM')
color = rep('red',length(test))
color[class.rf == Y[test]] = 'blue'
plot(dates[test],Y[test],col=color,main='RF')
color = rep('red',length(test))
color[class.bag == Y[test]] = 'blue'
plot(dates[test],Y[test],col=color,main='Bagging')
dev.off()



#LDA
require(diffusionMap)
distMat  = dist(X,method='man')
diffMap  = diffuse(distMat,eps.val=50)
Y_01         = rep(0,length(Y))
Y_01[Y == 1] = 1
require(rgl)
plot3d(diffMap$X[,1],diffMap$X[,2],diffMap$X[,3],col=Y_01[train]+1)
diffMap  = diffuse(distMat,eps.val=200)
outPCA   = prcomp(X)
out.lda.pca  = lda(Y~.,data=data.frame(outPCA$x),subset=train)
pred.lda.pca = predict(out.lda.pca,data.frame(outPCA$x[test,]))
table(pred.lda.pca$class,Y[test])


require(randomForest)
out.rf   = randomForest(Y~.,data=X,subset=train)
class.rf = predict(out.rf,X[test,])
miss.class(class.rf,Y[test])

distMat  = dist(X)
diffMap  = diffuse(distMat,eps.val=50)
epsilonGrid = seq(5,100,by=10)
nCompsGrid = seq(5,50,by=5)
results = matrix(0,nrow=length(nCompsGrid),ncol=length(epsilonGrid))

i = 1
for(epsilon in epsilonGrid){
	print(epsilon)
	j = 1			
	diffMap  = diffuse(distMat,eps.val=epsilon)	
	for(nComps in nCompsGrid){
		out.rf.diff   = randomForest(Y~.,data=diffMap$X[,1:nComps],subset=train)
		class.rf.diff = predict(out.rf.diff,diffMap$X[test,1:nComps])
		results[i,j]  = miss.class(class.rf.diff,Y[test],return=TRUE)
		j = j + 1
	}
	i = i + 1
}

epsilonGrid = seq(75,100,by=10)
nCompsGrid = seq(5,50,by=5)
results = matrix(0,nrow=length(epsilonGrid),ncol=length(nCompsGrid))

i = 1
for(epsilon in epsilonGrid){
	print(epsilon)
	j = 1			
	diffMap  = diffuse(distMat,eps.val=epsilon)	
	for(nComps in nCompsGrid){
		out.lda.diff   = lda(Y~.,data=data.frame(diffMap$X[,1:nComps]),subset=train)
		class.lda.diff = predict(out.lda.diff,data.frame(diffMap$X[test,1:nComps]))$class
		results[i,j]   = miss.class(class.lda.diff,Y[test],return=TRUE)
		j = j + 1
	}
	i = i + 1
}

