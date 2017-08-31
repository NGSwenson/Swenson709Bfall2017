##BIOL709B
#Class 1 notes
#Nate Swenson - swenson@umd.edu
#31-aug-2017


##working.directories
################################
#find out what your working directory is
getwd()

#set your working directory or use drop down
setwd("/Users/ngswenson/GitHub")

#see change
getwd()

#list all files in working directory
list.files(getwd())

list.files()

################################
## some very simple calculations
################################
#addition
15+2

#subtraction
15-2

#multiplication
15*2

#division
15/2


################################
## HELP ME!!!!
################################
??PCA

?princomp


################################
## Create an object
################################
#make an object called tot.rain and assign it a value of 50.2
tot.rain = 50.2

#add a value to tot.rain
tot.rain+22.1
tot.rain.new = tot.rain+22.1
tot.rain.new

#case matters
#Tot.rain+22.1

#make a new rainfall object
tot.rain.2 = 22.1

#different way of assigning <- instead of =
tot.rain.2 <- 22.1
tot.rain.2

#== is not assignment. More like asking is it equal to
tot.rain.2 == 22.1
tot.rain.2 == 22.2

#ask if it is NOT (!) equal to
tot.rain.2 != 22.2

#add objects
tot.rain + tot.rain.2 

#concatenate components of objects
all.rain.data = c(tot.rain, tot.rain.2)

#lets generate 40 random numbers from a normal distribution with an expected mean of 0 and sd of 1
random.data.1 = rnorm(40, mean = 0, sd = 1)

random.data.1

# a second random dataset
random.data.2 = rnorm(40, mean = 0, sd = 1)

#how many values in this vector object
length(random.data.1)

#list all objects currently in workspace
ls()

################################
## Basic plotting 
################################
#plot a histogram of the data
hist(random.data.1)

#plot a histogram of the data
hist(random.data.2)

#a bivariate plot with data.1 on x and data.2 on y
plot(random.data.1, random.data.2)

#a bivariate plot, but this time using a model call where data.1 is regressed (~) onto data.2
plot(random.data.1 ~ random.data.2)

#color outlines of the symbols red
plot(random.data.1 ~ random.data.2, col="red")

#change pch to change symbol
plot(random.data.1 ~ random.data.2, col="red", pch =17)

#change pch here to fill in symbol with a background of blue
plot(random.data.1 ~ random.data.2, col="red", pch =21, bg="blue")

#original with red outline
plot(random.data.1 ~ random.data.2, col="red")

#plot data with blue background and x and y labels (xlab and ylab)
plot(random.data.1 ~ random.data.2, col="red", pch =21, bg="blue", xlab="rainfall", ylab="temperature")


################################
## Reading in txt or csv files
################################
setwd("/Users/ngswenson/GitHub/Swenson709Bfall2017/Homework.1")
#read in example csv file using read.table command
my.prod.dat = read.table("my.prod.data.csv",sep=",",header=T)

#read in example csv file using read.csv command
my.prod.dat.alt = read.csv("my.prod.data.csv")

#read in example csv txt using read.table command
my.prod.dat.txt =read.table("my.prod.data.txt",sep="\t",header=T)


################################
## Writing out files as txt or csv
################################
#write out tables using write.table command, but using different delimitors (sep's)
write.table(my.prod.dat,"my.prod.new.data.csv",sep=",",row.names=F,col.names=T,quote=F)
write.table(my.prod.dat,"my.prod.new.data.txt",sep="\t",row.names=F,col.names=T,quote=F)

################################
## LOOK AT THE DATA!!!!
################################
#look at first 6 values of object
head(my.prod.dat)

#how many rows does it have?
nrow(my.prod.dat)

#how many columns does it have?
ncol(my.prod.dat)

#what are the dimensions of the object (nrows, ncols, depth?)
dim(my.prod.dat)

#look at just one column. here column 2
my.prod.dat[,2]

#look at just one row. here row 3
my.prod.dat[3,]

#call out only rows where the continent is "Asia"
my.prod.dat[my.prod.dat$continent=="Asia",]

#call out only rows where the continent is "Europe"
my.prod.dat[my.prod.dat$continent=="Europe",]

#call out only rows where the continent is "Asia" using subset
subset(my.prod.dat, my.prod.dat$continent=="Asia")

#call out only rows where the continent is "forest" using subset
subset(my.prod.dat, my.prod.dat$site == "forest")

#look at the structure of an object
str(my.prod.dat)

#calculate the sum of the values in column 2
sum(my.prod.dat[,2])

#calculate the mean of the values in column 2
mean(my.prod.dat[,2])

#calculate the sd and var of the values in column 2
sd(my.prod.dat[,2])
var(my.prod.dat[,2])

#plot a boxplot of data logging column 2
boxplot(log(my.prod.dat[,2])~my.prod.dat[,1])

#plot a biplot of data logging column 2
plot(my.prod.dat[,3]~log(my.prod.dat[,2]))

#plot a biplot of data logging column 2 while coloring points based on column 4
plot(my.prod.dat[,3]~log(my.prod.dat[,2]),pch=21,bg=my.prod.dat[,4])

#tell R you are going to make a image with 3 plots where there is 1 row and 3 columns
par(mfrow = c(1,3))

#replot all 3 types of plots
boxplot(log(my.prod.dat[,2])~my.prod.dat[,1])
plot(my.prod.dat[,3]~log(my.prod.dat[,2]))
plot(my.prod.dat[,3]~log(my.prod.dat[,2]),pch=21,bg=my.prod.dat[,4])

################################
## Make some data
################################
#This is how I madee the original fake dataset. I made a data.frame
my.next.prod.dat = data.frame(site = c("swamp","swamp","forest","forest","desert","desert","grassland","grassland"), sp.rich = c(10,5,1000,500,2,5,10,25), productivity = c(0.25,.11,1.99,1.5, 0.11,.05, 1.25,1.00), continent=c("Asia","Europe","Asia","Europe","Asia","Europe","Asia","Europe"))

#Make a vector with a series of 7 numbers from 1 to 7
series.mat = c(1:7)

#make an empty vector of NA's with 6 of them using the repete (rep()) function
empty.vec = rep(NA,6)

#make a matrix with 12 values in 3 rows from 1 to 12
matrix(1:12,nrow=3)

#transpose a matrix
transposed.mat = t(matrix(1:12,nrow=3))

#calculate the sum of each row
rowSums(transposed.mat)

#calculate the sum of each column
colSums(transposed.mat) 


################################
## Basic analyses
################################
#perform a simple linear regression model regressing productivity onto species richness
lm(my.prod.dat$productivity ~ log(my.prod.dat$sp.rich))

#get summary info from this model by wrapping the original code in summary()
summary(lm(my.prod.dat$productivity ~ log(my.prod.dat$sp.rich)))

#assign output
mod.output = summary(lm(my.prod.dat$productivity ~ log(my.prod.dat$sp.rich)))

#run some anovas
summary(aov(my.prod.dat$productivity ~ my.prod.dat$continent))
summary(aov(my.prod.dat$productivity ~ my.prod.dat$site))

#do a posthoc pairwise comparison analysis using TukeyHSD
TukeyHSD(aov(my.prod.dat$productivity ~ my.prod.dat$site))

#again, how to subset data, but choosing appropriate rows in only a single column (productivity)
my.prod.dat
my.prod.dat[my.prod.dat$continent=="Asia","productivity"]
my.prod.dat[my.prod.dat$continent=="Europe","productivity"]

#run a t.test on productivity in Asia vs Europe
t.test(my.prod.dat[my.prod.dat$continent=="Asia","productivity"],my.prod.dat[my.prod.dat$continent=="Europe","productivity"])


################################
## Write some loops
################################

#look at the empty vector we made
empty.vec

for(i in 1:3){ 
	empty.vec[i] = i+1
}

#what changed and why
empty.vec

################################
## Write a function
################################
#let's make a simple function. Here the function will take an input, add a value of 10 to whatever is given and then return the new value
my.func = function(x){
	my.output = x+10
	return(my.output)
}

#for a single input
my.func(12)

#for a vector of values
my.func(c(1:12))






