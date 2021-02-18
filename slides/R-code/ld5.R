# library(Hmisc)
# library(BayesFactor)
# library(MCMCpack)

#function cleaning the data by
#1. taking out 2 participants who essentially gave up
#2. Excluding trials with too fast and too slow responses
#3. Excluding trials with wrong responses
#4. Excluding the first 20 trials of the first block
#5. Excluding the first trial in every block
clean=function()
{
indat=read.table(url('https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/lexDec-dist5/ld5.all'))
colnames(indat)=c('sub','block','trial','stim','resp','rt','error')

bad1=indat$sub%in%c(34,43)
bad2=indat$rt<250 | indat$rt>2000
bad3=indat$err==1
bad4=indat$block==0 & indat$trial<20
bad5=indat$trial==0

bad=bad1 | bad2 | bad3 |bad4 |bad5
dat=indat[!bad,]
return(dat)
}

#This function makes Figure 3A in the manuscript
plotMean=function(dat,mylab='')
{
reduced=as.data.frame.table(tapply(dat$rt,list(dat$sub,dat$stim),mean))
colnames(reduced)=c("sub","stim","rt")
reduced$sub=as.factor(reduced$sub)
reduced$stim=as.factor(reduced$stim)
summary(aov(rt~stim+Error(sub/stim),data=reduced))
MSE=628
I=length(levels(reduced$sub))
se=sqrt(MSE/I)
ci=se*2
mrt=tapply(dat$rt,dat$stim,mean)
errbar(1:6,mrt,mrt+ci,mrt-ci
       ,axes=F,ylab="Response Time (ms)",xlab='Digit'
       ,cap=.03, ylim = c(floor(min(mrt-ci)-20), ceiling(max(mrt+ci) + 20)))
lines(1:3,mrt[1:3])
points(1:3,mrt[1:3],pch=21,bg='cornflowerblue',cex=1.2)
lines(4:6,mrt[4:6])
points(4:6,mrt[4:6],pch=21,bg='cornflowerblue',cex=1.2)
axis(2, at = seq(570, 660, 30))
axis(1,at=1:3,lab=c(2:4))
axis(1,at=4:6,lab=c(6:8))
mtext(side=3,adj=0,cex=1.0,mylab)
}

#This function makes figure 3B in the manuscript
plotIndv=function(combo, mylab='', xlabel = c(expression(paste(Delta[1]))
                                           ,expression(paste(Delta[2]))
                                           ,expression(paste(Delta[3]))
                                           ,expression(paste(Delta[4]))), ...)
{
#mcol=rainbow(I,start=0,end=.8)
mcol=rgb(.1,.1,.1,.3)
I=dim(combo)[1]
o=order(combo[,1])
matplot(t(combo[o,]),typ='l',pch=20,col=mcol,lty=1,axes=F,ylab="Response Time (ms)",xlab="",...)
matpoints(t(combo[o,]),pch=20,col=mcol,cex=.9)
axis(2)
axis(1,at=1:4,labels=c("3-2","4-3","7-6","8-7"))
axis(1, at=1:4
     , labels = xlabel
     , line=1.5, tick=F)
abline(h=0)
mtext(side=3,adj=0,cex=1.0,mylab)
}


###########Bayesian model estimation, comparison, and figure#############

#Function returning model estimates for the unconstrained model and Bayes factors for all models
doBayes=function(dat, keep = 1000:10000)
{
M = max(keep)
sub=as.integer(as.factor(dat$sub))
I=max(sub)
# library(BayesFactor)
eta=matrix(nrow=I,ncol=2,1:(2*I))
alpha=matrix(nrow=I,ncol=2,(2*I+1):(4*I))
beta=matrix(nrow=I,ncol=2,(4*I+1):(6*I))
mu.alpha=(6*I+1):(6*I+2)
mu.beta=(6*I+3):(6*I+4)

#Design matrix for unconstrained model
status=dat$stim%/%3+1
faster=dat$stim%in%c(0,5)
slower=dat$stim%in%c(2,3)
N=length(dat$sub)
X=matrix(nrow=N,ncol=6*I+4,0)  #without grand mean
for (n in 1:N)
{
	X[n,eta[sub[n],status[n]]]=1
	if (faster[n]) {
		X[n,alpha[sub[n],status[n]]]=-1
		X[n,mu.alpha[status[n]]]=-1}
	if (slower[n]) {
		X[n,beta[sub[n],status[n]]]=1
		X[n,mu.beta[status[n]]]=1}
}

#unconstrained model estimates and BF
y=dat$rt
gMap=rep(0:2,c(2*I,4*I,4))
samples=nWayAOV(y,X,gMap,rscale=c(1,.1,.16),posterior=T,iterations=M)
out1=nWayAOV(y,X,gMap,rscale=c(1,.1,.16),posterior=F)

#Design matrix for propositional representation
X1=matrix(nrow=N,ncol=2*I,0)  #without grand mean
for (n in 1:N)
{
  X1[n,eta[sub[n],status[n]]]=1
}

#BF for propositional representation model
gMap=rep(0,2*I)
out2=nWayAOV(y,X1,gMap,rscale=c(1))

#Are the effects greater than 0? Analog representation
#Estimation of posterior probability of all effects being in the expected direction
a1=samples[keep, alpha[,1]+1]+samples[keep, mu.alpha[1]+1]>0
a2=samples[keep, alpha[,2]+1]+samples[keep, mu.alpha[2]+1]>0
b1=samples[keep, beta[,1]+1]+samples[keep, mu.beta[1]+1]>0
b2=samples[keep, beta[,2]+1]+samples[keep, mu.beta[2]+1]>0

a1.pass=apply(a1,1,mean)==1
a2.pass=apply(a2,1,mean)==1
b1.pass=apply(b1,1,mean)==1
b2.pass=apply(b2,1,mean)==1

postProb=mean(a1.pass & a2.pass & b1.pass & b2.pass)
pm=apply(samples,2,mean)

#Estimation of prior probability of all effects being in the expected direction (analog representation)
#prior on alpha
Mprior=10 * M
mu.alpha1=rcauchy(Mprior,0,.16)
mu.alpha2=rcauchy(Mprior,0,.16)
mu.beta1=rcauchy(Mprior,0,.16)
mu.beta2=rcauchy(Mprior,0,.16)
g=rinvgamma(Mprior,.5,.5*.1^2)
pass=1:Mprior
for (m in 1:Mprior)
{
	a1=mean(rnorm(I,mu.alpha1[m],sqrt(g[m]))>0)==1
	a2=mean(rnorm(I,mu.alpha2[m],sqrt(g[m]))>0)==1
	b1=mean(rnorm(I,mu.beta1[m],sqrt(g[m]))>0)==1
	b2=mean(rnorm(I,mu.beta2[m],sqrt(g[m]))>0)==1
	pass[m]=(a1&a2&b1&b2)
}
priorProb=mean(pass)

BF=c(postProb/priorProb,exp(out1$bf-out2$bf))

#Preparing samples for return
a1samp=colMeans(samples[keep, alpha[,1]+1]+samples[keep, mu.alpha[1]+1])
a2samp=colMeans(samples[keep, alpha[,2]+1]+samples[keep, mu.alpha[2]+1])
b1samp=colMeans(samples[keep, beta[,1]+1]+samples[keep, mu.beta[1]+1])
b2samp=colMeans(samples[keep, beta[,2]+1]+samples[keep, mu.beta[2]+1])

samps <- cbind(a1samp, b1samp, b2samp, a2samp)

return(list(BF=BF,samples=samps))
}

#dev.off()
