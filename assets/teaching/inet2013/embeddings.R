genesT = read.table("../data/nci.data")
genes = t(genesT)
n = nrow(genes)
p = ncol(genes)

Y = 
c('CNS','CNS','CNS','RENAL','BREAST','CNS','CNS','BREAST','NSCLC','NSCLC','RENAL','RENAL',
'RENAL','RENAL','RENAL','RENAL','RENAL','BREAST','NSCLC','RENAL','UNKNOWN','OVARIAN',
'MELANOMA','PROSTATE','OVARIAN','OVARIAN','OVARIAN','OVARIAN','OVARIAN','PROSTATE','NSCLC',        
'NSCLC','NSCLC','LEUKEMIA','K562B-repro','K562A-repro','LEUKEMIA','LEUKEMIA','LEUKEMIA',
'LEUKEMIA','LEUKEMIA','COLON','COLON','COLON','COLON','COLON','COLON','COLON','MCF7A-repro',
'BREAST','MCF7D-repro','BREAST','NSCLC','NSCLC','NSCLC','MELANOMA','BREAST','BREAST',
'MELANOMA','MELANOMA','MELANOMA','MELANOMA','MELANOMA','MELANOMA')

color = rep(0,length(Y))
tmp   = rainbow(length(levels(as.factor(Y))))
sweep = 1
for(lev in levels(as.factor(Y))){
	color[Y == lev] = tmp[sweep]
	sweep = sweep + 1
}

out = prcomp(genes,scale=T,center=T)
loadings = out$rot
scores   = out$x

pdf('../Slides/Figures/pcaGenes.pdf')
plot(scores[,1],scores[,2],xlab='PC1',ylab='PC2',type='n')
text(scores[,1],scores[,2],Y,cex=.7,col=color)
dev.off()

library(rgl)
scoresMat = cbind(scores[,1],scores[,2],scores[,3])
plot3d(scoresMat,col='white')
text3d(scoresMat,text=Y,cex=.7,col=color)


library(scatterplot3d)
library(mvtnorm)

n = 100
sigma = matrix(c(1,.5,.5,.5),nrow=2,byrow=T)
X = rmvnorm(n,c(0,0),sigma)
x = X[,1]
x_order = order(x)
x = sort(x)
y = X[x_order,2]
#t = X[,3]
nCols   = 10
rainVec = rainbow(nCols)
color   = 
c(
rep(rainVec[1],n/nCols),rep(rainVec[2],n/nCols),rep(rainVec[3],n/nCols),
rep(rainVec[4],n/nCols),rep(rainVec[5],n/nCols),rep(rainVec[6],n/nCols),
rep(rainVec[7],n/nCols),rep(rainVec[8],n/nCols),rep(rainVec[9],n/nCols),
rep(rainVec[10],n/nCols))
pdf('../Slides/Figures/pcaGood1.pdf')
plot(x,y,col=color,xlab='X1',ylab='X2',xlim=c(-3,3),ylim=c(-3,3),asp=1,pch=19)
dev.off()

X = cbind(x,y)
pca.out = svd(scale(X))
pca.scores = pca.out$u %*% diag(pca.out$d)
#plot(pca.scores,col=color,xlab='X1',ylab='X2',,xlim=c(-3,3),ylim=c(-3,3),asp=1,pch=19)
pca.scores.red  = cbind(sort(pca.scores[,1]),rep(0,n))
pca.scores.can  = pca.out$v %*% t(pca.scores.red)
mean.vec        = apply(X,2,mean)
sd.vec          = apply(X,2,sd)
pca.scores.plot = t( (pca.scores.can+mean.vec)*sd.vec )
pdf('../Slides/Figures/pcaGood2.pdf')
plot(pca.scores.plot,col=color,xlab='X1',ylab='X2',,xlim=c(-3,3),ylim=c(-3,3),asp=1,pch=19)
dev.off()

## To Check
#plot(x,y,col=color,xlab='X1',ylab='X2',xlim=c(-3,3),ylim=c(-3,3),asp=1)
#points(pca.scores.plot,col=color)

n = 100
t = seq(0,4*pi,length=n)
X = cbind(3*t/2*sin(t),t/2*cos(t))

nCols   = 10
rainVec = rainbow(nCols)
color   = 
c(
rep(rainVec[1],n/nCols),rep(rainVec[2],n/nCols),rep(rainVec[3],n/nCols),
rep(rainVec[4],n/nCols),rep(rainVec[5],n/nCols),rep(rainVec[6],n/nCols),
rep(rainVec[7],n/nCols),rep(rainVec[8],n/nCols),rep(rainVec[9],n/nCols),
rep(rainVec[10],n/nCols))
lims = c(-4.7*pi,3.5*pi)
pdf('../Slides/Figures/pcaBad1.pdf')
plot(X,col=color,xlab='X1',ylab='X2',xlim=lims,ylim=lims,asp=1,pch=19)
dev.off()

pca.out = svd(scale(X))
pca.scores = pca.out$u %*% diag(pca.out$d)
pca.scores.red  = cbind(pca.scores[,1],rep(0,n))
pca.scores.can  = pca.out$v %*% t(pca.scores.red)
mean.vec        = apply(X,2,mean)
sd.vec          = apply(X,2,sd)
pca.scores.plot = t( (pca.scores.can)*sd.vec )
pdf('../Slides/Figures/pcaBad2.pdf')
plot(pca.scores.plot,col=color,xlab='X1',ylab='X2',xlim=lims,ylim=lims,asp=1,pch=19)
#scatterplot3d(x,y,t)
dev.off()

#### To Check
plot(X,col=color,xlab='X1',ylab='X2',xlim=lims,ylim=lims,asp=1,pch=19)
points(pca.scores.plot,col=color)

###########
# Nonlinear embeddings
###########
nonLinear = function(gamma){
	Wgamma = exp(-Delta/gamma)

	P = apply(Wgamma,1,sum)
	Pinv = P^(-1)
#	R = diag(rep(1,n)) - diag(Pinv)%*% Wgamma
	R = diag(Pinv)%*% Wgamma #this works the same, with reversed evals
	eig = eigen(R)
	evecs = eig$vectors
	return(evecs)
}

Delta = as.matrix(dist(genes,diag=TRUE,upper=TRUE))

pdf('../lectures/figures/spiralManifoldExampleDist.pdf')
image(Delta,col=topo.colors(10))
dev.off()

pdf('../lectures/figures/spiralManifoldExampleDist.pdf')
gamma = 45
Wgamma = exp(-Delta/gamma)
image(Wgamma,col=topo.colors(10))
dev.off()

pdf('../lectures/figures/spiralManifoldExampleWgamma.pdf')
par(mfrow=c(3,3))
gammaGrid = c(25,30,35,40,45,50,55,60,65)
for(gamma in gammaGrid){
	evecs = nonLinear(gamma)
	plot(evecs[,2],evecs[,3],xlab='eigenvector 2',ylab='eigenvector 3',type='n')
	text(evecs[,2],evecs[,3],Y,cex=.7,col=color)
}

library(rgl)
evecs    = nonLinear(gamma=45)
evecsMat = cbind(evecs[,2],evecs[,3],evecs[,4])
plot3d(evecsMat,col='white',xlab='Eigenvector 1',ylab='Eigenvector 2',zlab='Eigenvector 3')
text3d(evecsMat,text=Y,cex=.7,col=color)

plot3d(evecsMat,type='s',size=.65,col = t)

