##BIOL709B
#Class 7 notes
#Nate Swenson - swenson@umd.edu
#19-oct-2017

#install.packages(c("picante","lefse"))
library(vegan)
library(picante)
library(lefse)

data(lefse.sample)

lefse.sample

#rarefaction
min(rowSums(lefse.sample))
lefse.sample[1,lefse.sample[1,]>0]

rep.int(colnames(lefse.sample[1,lefse.sample[1,]>0]),times=c(lefse.sample[1,lefse.sample[1,]>0]))

min.abund = min(rowSums(lefse.sample))
com.1 = lefse.sample[1,lefse.sample[1,]>0]
vec.com.1 = rep.int(colnames(com.1),times=c(com.1))
first.sample = sample(vec.com.1, min.abund,replace=F)
length(unique(first.sample))

jack.funk = function(x){
	a.sample = sample(x, min.abund,replace=F)
	sr.sample = length(unique(a.sample))
	return(sr.sample)
}

hist(replicate(10000,jack.funk(vec.com.1)))



jack.sr = function(one.com){
	
	one.com = one.com[one.com>0]
	
	vec.com = rep.int(names(one.com),times=c(one.com))
	
	jack.sub.funk = function(vec.com){
		a.sample = sample(vec.com, min.abund,replace=F)
		sr.sample = length(unique(a.sample))
		return(sr.sample)
	}

	sr.dist = replicate(100,jack.funk(vec.com))
	return(sr.dist)
}


output.srs = apply(lefse.sample,1,jack.sr)
boxplot(output.srs[,1:5])

apply(output.srs,2,quantile)

apply(output.srs,2,function(x) quantile(x,c(0.025,0.5,0.975)))

quants = apply(output.srs,2,function(x) quantile(x,c(0.025,0.5,0.975)))

plot(1:ncol(quants),quants[2,],ylim=c(0,max(quants+1)))

for(i in 1:ncol(quants)){

	lines(rep(i,2),c(quants[1,i], quants[3,i]))

}


text(1,7,"A")
text(2,7,"A")
text(3,7,"B")
text(4,7,"C")
text(5,7,"C")


########################################################
########################################################
#bootstrapping
########################################################
########################################################

alpha.diversities = c(10,20,32,31,22,38,10,28,2,44,55,61,31,13,12,15,18,20,22)
mean.alpha = mean(alpha.diversities)
sd.alpha = sd(alpha.diversities)

boot.s = sample(alpha.diversities, 10, replace=T)
mean(boot.s)
sd(boot.s)

boot.1000 = replicate(1000,mean(sample(alpha.diversities, 10, replace=T)))
hist(boot.1000)
abline(v=mean.alpha,col="red")



########################################################
########################################################
#jackknifing
########################################################
########################################################

pa.samp = decostand(lefse.sample, method="pa")

#number of species * number of samples of meta community minus number of samples -1 times species richness without com 1
pseudo.val.1 = (ncol(pa.samp)*nrow(pa.samp)) - ((nrow(pa.samp)-1)*(sum(colSums(pa.samp[-1,])>0)))

pseudos = matrix(NA,nrow=1,ncol=nrow(pa.samp))
for(i in 1:nrow(pa.samp)){

	pseudos[,i] = (ncol(pa.samp)*nrow(pa.samp)) - ((nrow(pa.samp)-1)*(sum(colSums(pa.samp[-i,])>0)))

}

mean(pseudos)

########################################################
########################################################
#monte carlo null modeling
########################################################
########################################################


vegdist(pa.samp,method="jaccard")

obs.jac = as.matrix(vegdist(pa.samp,method="jaccard"))

randomizeMatrix(pa.samp,null.model="independentswap")
as.matrix(randomizeMatrix(pa.samp,null.model="independentswap"))

replicate(5,as.matrix(vegdist(randomizeMatrix(pa.samp,null.model="independentswap"),method="jaccard")))

nulls = replicate(1000,as.matrix(vegdist(randomizeMatrix(pa.samp,null.model="independentswap"),method="jaccard")))

hist(nulls[2,1,])
abline(v=obs.jac[2,1],col="red")

rank(c(obs.jac[2,1], nulls[2,1,]))[1]




