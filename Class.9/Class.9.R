##BIOL709B
#Class 9 notes
#Nate Swenson - swenson@umd.edu
#26-oct-2017

##calculate critical values for a t-distribution with df = 5
# upper tail
qt(0.95, 5)

#lower tail
qt(0.05, 5)

#calculate the p-value of a t given df = 5, one sided (upper tail)
1- pt(1.99, 5)

#critical value for a given alpha.level and sample size given mean and sd of a null. 
Tcrit = function(mu0, f0, n, alpha.level){

	mu0 + sqrt(f0^2/n) * qnorm(1-alpha.level)
	
}

#calculate critical values
Tcrit(12, 3, 25, 0.05)
Tcrit(12, 3, 25, 0.02)

#write a power function
Pow = function(mu0,mu1,f0,f1,n,alpha.level){

	1-pnorm((Tcrit(mu0,f0,n,alpha.level)-mu1)/sqrt(f1^2/n))


}

#calculate power for two means, their sds with a sample size of 25 and an alpha of 0.05
Pow(12,12.98691,3,3,25,0.05)
Pow(12,12.98691,1,1,25,0.05)

#plot a power curve across means
curve(Pow(12,x,1,1,25,0.05),12,15)

#plot a power curve across sds
curve(Pow(12,13,x,x,25,0.05),0,5)

#plot a power curve across sample sizes
curve(Pow(12,13,1,1,x,0.05),3,100,ylim=c(0,1))
par(new=T)
curve(Pow(12,12.8,1,1,x,0.05),3,100,ylim=c(0,1),col="gray")
par(new=T)
curve(Pow(12,12.6,1,1,x,0.05),3,100,ylim=c(0,1),col="blue")
par(new=T)
curve(Pow(12,12.4,1,1,x,0.05),3,100,ylim=c(0,1),col="red")
par(new=T)
curve(Pow(12,12.2,1,1,x,0.05),3,100,ylim=c(0,1),col="orange")
par(new=T)
curve(Pow(12,12.1,1,1,x,0.05),3,100,ylim=c(0,1),col="violet")

abline(v=10)

#critical value for F stat by samples per treatment (n) and number of treatments
Fcrit = function(n, n.treatments){

	qf(0.95, n.treatments-1, n.treatments*(n-1))
}


#calculate critical value
Fcrit(5,4)

#calculate power of a F function
Fpow = function(n, n.treatments){

	1-pf(Fcrit(n,n.treatments), n.treatments-1, n.treatments*(n-1),n)
}

#calculate power using n and number of treatments
Fpow(5,4)


#install.packages("pwr")
library(pwr)

#use pwr package to calculate power of one way anova
pwr.anova.test(4,5,1,0.05)

#plot power curves
curve(pwr.anova.test(4,5,x,0.05)$power,0,2)

curve(pwr.anova.test(4,x,1,0.05)$power,3,10)


library(lattice)

#make a fake matrix to see how levelplot() works
mat = matrix(rbind(rep(1,3),rep(2,3)),nrow=2)
levelplot(mat)
levelplot(t(mat))
levelplot(t(mat),col.regions=heat.colors(50))

#empty output matrix 0-5 effect size and 3:10 sample size
output.mat = matrix(NA, nrow=6,ncol=8)


for(i in 0:5){
	for(j in 3:10){
	
		output.mat[i+1,j-2] = pwr.anova.test(4,j,i,0.05)$power

	}
}


#plot output
levelplot(t(output.mat),col.regions=heat.colors(20))




#vary effect sizes from 0 to 5 by 0.2 increments
es = seq(0,5,.2)

#make output matrix
output.mat2 = matrix(NA, nrow=length(es),ncol=8)

for(i in 1:length(es)){
	for(j in 3:10){

		output.mat[i,j-2] = pwr.anova.test(4,j,es[i],0.05)$power

	}
}

#make levelplot of output
levelplot(t(output.mat),col.regions=heat.colors(20))

#make the plot pretty
rownames(output.mat) = es
colnames(output.mat)=c(3:10)
levelplot(t(output.mat),col.regions=heat.colors(20),xlab="sample size",ylab="effect size")



##############################
##############################
############################## NOW make your own level plot for a t-test power output
##############################

#use pwr2 package to calculate power of two way anova
#install.packages("pwr2")
library(pwr2)
pwr.2way(a=3, b=3, alpha=0.05, size.A=4, size.B=5, f.A=0.8, f.B=0.4)


##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
#make a community data matrix
matt = matrix(rbind(c(0,1,0,1,0), c(1,0,1,0,1), c(0,1,0,1,0)), nrow=3)
rownames(matt) = c("com1","com2","com3")
colnames(matt) = paste("sp.",1:5,sep="")

#use picante functions
library(picante)

#calculate beta diversity using jaccard
vegdist(matt,method="jaccard")

#pick out beta of first two communities
obs = vegdist(matt,method="jaccard")[1]

#generate a randomized community data matrix using independent swap null and calculate jaccard of com1 vs com2
vegdist(randomizeMatrix(matt, null.model="independentswap"),method="jaccard")[1]

#replicate null model 999 times
null.values = replicate(999,vegdist(randomizeMatrix(matt, null.model="independentswap"),method="jaccard")[1])

#calculate quantile using default and "last" methods for ties
rank(c(obs, null.values))[1]
rank(c(obs, null.values),ties.method="last")[1]

#how many unique null values are there? What does it mean?
unique(null.values)



#############################################################################

matt = matrix(rbind(rep(c(0,1),50), rep(c(1,0),50),rep(c(0,1),50)), nrow=3)
rownames(matt) = c("com1","com2","com3")
colnames(matt) = paste("sp.",1:100,sep="")

vegdist(matt,method="jaccard")
obs = vegdist(matt,method="jaccard")[1]

null.values = replicate(999,vegdist(randomizeMatrix(matt, null.model="independentswap"),method="jaccard")[1])

rank(c(obs, null.values))[1]
rank(c(obs, null.values),ties.method="last")[1]
unique(null.values)

null.values = replicate(19999,vegdist(randomizeMatrix(matt, null.model="independentswap"),method="jaccard")[1])

unique(null.values)



#############################################################################

matt = matrix(rbind(c(rep(c(0,1),25),rep(0,50)), c(rep(0,50),rep(c(1,0),25)),rep(c(0,1),50)), nrow=3)
rownames(matt) = c("com1","com2","com3")
colnames(matt) = paste("sp.",1:100,sep="")

vegdist(matt,method="jaccard")
obs = vegdist(matt,method="jaccard")[1]

null.values = replicate(999,vegdist(randomizeMatrix(matt, null.model="independentswap"),method="jaccard")[1])

rank(c(obs, null.values))[1]
rank(c(obs, null.values),ties.method="last")[1]
unique(null.values)



#############################################################################

matt = matrix(rbind(c(rep(c(0,1),25),rep(0,50)), c(rep(0,50),rep(c(1,0),25)),rep(c(0,1),50)), nrow=3)
rownames(matt) = c("com1","com2","com3")
colnames(matt) = paste("sp.",1:100,sep="")

vegdist(matt,method="jaccard")
obs = vegdist(matt,method="jaccard")[2]

null.values = replicate(999,vegdist(randomizeMatrix(matt, null.model="independentswap"),method="jaccard")[2])

rank(c(obs, null.values))[1]
rank(c(obs, null.values),ties.method="last")[1]
unique(null.values)
