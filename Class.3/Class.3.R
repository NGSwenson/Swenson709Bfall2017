##BIOL709B
#Class 3 notes
#Nate Swenson - swenson@umd.edu
#14-sept-2017

#install.packages(c("vegan","plyr","picante","untb","reshape2"))
library(vegan)
library(plyr)
library(picante)
library(reshape2)

cdm = read.csv("example.com.data2.csv",row.names=1)
cdm = as.matrix(cdm)

#calculate total number of individuals per community (assumes values in cells are counts of individuals). If relative abundance data, then sums to 1.
abund.per.com = rowSums(cdm)

#total abundance of speciesd in the meta-community
abund.per.sp = colSums(cdm)

#convert cdm into relative abundance data
rel.abund.cdm = cdm/rowSums(cdm)

#convert cdm into a presence-absence matrix
pa.cdm = ifelse(cdm > 0,1,0)

#calculate species richness per community
sr.per.com = rowSums(pa.cdm)

#calculate occupancy rates of species
sp.oc.rate = colSums(pa.cdm)

#prepare graphics device to make a 10 panel plot (2 rows by 5 columns)
par(mfrow=c(2,5))

#for the 10 rows (communities) in the example cdm, plot a histogram of the values (ie abundance)
for(i in 1:10){
	hist(cdm[i,])
}

#plot histograms again, but only include species that are present (ie that have cell values > 0)
for(i in 1:10){
	hist(cdm[i,cdm[i,] >0])
}


##Decostand transformations
#convert to presence-absence
decostand(cdm, method = "pa")

#convert to relative abundance by dividing by total of rows
decostand(cdm, method = "total")

#log transform
decostand(cdm, method = "log",logbase=2)

##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
set.seed(52)

#replicate text 300 times
rep("sp.",300)

#now replicate again but give them a species number from 1 to 10
paste(rep("sp.",300),sample(1:10,300,replace=T),sep="")

#now do all of it but adding a 3rd column that is a 1 for present
tmp.sp = as.data.frame(cbind(paste(rep("sp.",300),sample(1:10,300,replace=T),sep=""),1))

#similar approach for community assignment as well
rep("com.",300)
rep(1:10,each=30)

#community data as it might be in your notebook
tmp.com = as.data.frame(paste( rep("com.",300),rep(1:10,each=30) , sep=""))

com.dat = cbind(tmp.com, tmp.sp)
colnames(com.dat) = c("community","spp","present")

#"recast" your data to look like a community data matrix (almost)
new.cdm = recast(com.dat,com.dat[,1]~com.dat[,2])

#add row names that are the community names
rownames(new.cdm)=new.cdm[,1]

#remove first column because it is the community names (ie not the right format)
new.cdm = new.cdm[,-1]

#return it to the original style
new.cdm2 = cbind(rownames(new.cdm),new.cdm)

#melt a cdm to what you originally had
melt(new.cdm2)


##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
#alpha diversity metrics
diversity(cdm, index="shannon")
diversity(cdm, index="simpson")
fisher.alpha(cdm)

#richness
specnumber(cdm)
rowSums(pa.cdm)

#species accumulation curves for the meta-community
sp.a.c. =specaccum(cdm, method="random")
plot(sp.a.c.)

#look at vegdist() for beta diversity calculations
?vegdist

#jaccard dist object output
jaccard.output = vegdist(cdm, method="jaccard")
jaccard.output 

#jaccard matrix output
jaccard.output = vegdist(cdm, method="jaccard", diag=TRUE, upper = TRUE)
jaccard.output 

#bray curtis (abundance weighted metric()
bray.output = vegdist(cdm, method="bray")

#make a fake factor category stating the forest type of each community
my.groups = factor(c(rep("rain.forest",5),rep("dry.forest",5)))

#calcluate the beta dispersion for whole cdm
my.mod = betadisper(bray.output, my.groups)
my.mod

plot(my.mod)

anova(my.mod)

permutest(my.mod, pairwise = TRUE, permutations = 999)

my.mod.HSD = TukeyHSD(my.mod)

plot(my.mod.HSD)

plot(hclust(bray.output),col=my.groups)


set.seed(10)
enviro.dat = cbind(abs(rnorm(10)), sort(abs(rnorm(10,2))) ,as.data.frame(my.groups))
colnames(enviro.dat) = c("soil.h2o.content", "light.levels", "forest.type")

adonis2(cdm ~ soil.h2o.content * forest.type, data = enviro.dat)


var.part.out=varpart(cdm,~ soil.h2o.content, ~light.levels+forest.type, data=enviro.dat)

plot(var.part.out)



######
#Meta-Com Models
#Neutral From Steven's book
library(untb)
?untb

#here start is starting ecosystem (we used 25 individuals per species (1 through 20)
#D is deaths per timestep
#prob of new individual not coming from existing indiv
#keep means keep each time step of simulation
sim = untb(start = rep(1:20,25), prob=0, gens=2500, D=9, keep=T)

dim(sim)

times = c(1,50,2500)
sppcolors = sapply(times,function(i) grey((sim[i,]-1)/20))

layout(matrix(1:3,nr=1))

par(mar=c(1,1,3,1))

for(j in 1:3){

plot(c(1,20), c(1,25), type="n",axes=F)
points(rep(1:20,25), rep(1:25,each=20), pch=19,cex=2,col=sppcolors[,j])
title(paste("Time = ", times[j], sep=""))
}


#wrangle output into a species table through time
sp.tab = species.table(sim)


##now can you compile the population sizes through time and plot them for each species?
##can you calculate species richness through time?
##can you calculate beta diversity between times? Plot it as time lag on x axis and beta on y


