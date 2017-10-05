##BIOL709B
#Class 5 notes
#Nate Swenson - swenson@umd.edu
#28-sept-2017


###DEALING WITH NUMBVERS
#convert a value to an integer
as.integer(10.8)

#round values to different levels
round(10.8, digits=0)
round(10.81,digits=1)
round(10.86,digits=1)

#a fake vector of values
a = c(1.1, 1.5, 2.2, 2.8)

#compare rounding to ceiling and floor outputs
round(a)
ceiling(a)
floor(a)

#use as.numeric
as.numeric(10.81)

#generating a sequence of numbers 
seq(from =1, to =19, by=1)
seq(from =1, to =19, by=2)

#repeating numbers x times
rep(1:3,times=1)
rep(1:3,times=2)

#comparing values
a = 9
b = 8
c = 8

a == b
b == c
b != c
a > b
a < b

#comparing vectors
tmp = c(1,3,4,6)
tmp2 = c(1,3,10,18)
tmp == tmp2
tmp != tmp2

#get tmp values that aren't in tmp2
tmp[tmp != tmp2]
which(tmp2 != tmp)
tmp[which(tmp2 != tmp)]

identical(tmp, tmp2)
tmp3 = c(1,3,4,6)
identical(tmp,tmp3)

#compare the similarity of numerical vectors based on a threshold
a = c(1.001, 5.0000001)
b = c(1.001, 5.0000005)
c = c(1.005, 5.0000005)

all.equal(a,b, tolerance = 0)
all.equal(a,c, tolerance = 0)
all.equal(a,b, tolerance = 0.001)
all.equal(a,c, tolerance = 0.001)
all.equal(a,b,tolerance=0.000001)
all.equal(a,c,tolerance=0.000001)


###DEALING WITH TEXT
#how many elements
length(c("I", "REally", "love","this","class"))

#number of characters per element
nchar(c("I", "REally", "love","this","class"))

#pasting function
paste("I", "REally", "love","this","class", sep=".")
paste("I", "REally", "love","this","class", sep="RALPH")

#change cases
xx= c("I", "REally", "love","this","class")
tolower(xx)
toupper(xx)


yy = c("I really love this class")

#capitalize words function
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
      sep="", collapse=" ")
}

simpleCap(yy)


#change a specific character or run of characters
chartr("R","r",xx)
chartr("RE","re",xx)

#return only certain characters based on their locations
substr(xx,2,3)
substr(xx,1,2)

#fake words
yy = paste("I", "REally", "love","this","class", sep="-")

#string together words with "-"
strsplit(yy,split="-")[[1]]

#unlist the output
unlist(strsplit(yy,split="-"))
length(strsplit(yy,split="-")[[1]])

#make two vectors of word elements
com1 = c("spp.1","spp.2","spp.3")
com2 = c("spp.1","spp.2","spp3", "spp.55")

#intersect and union of words
intersect(com1,com2)
union(com1,com2)

#find the unique elements
setdiff(com1,com2)
setdiff(com2,com1)

#finding T/F to whether an element is in an object
is.element("spp.55",com2)
is.element("spp.55",com1)
"spp.55" %in% com2

#sorting character elements
sort(com1)
sort(com2)

##Dealing with dates
date()

#converting to date format
as.Date("2017-09-28")
as.Date("09/28/2017", format = "%m/%d/%Y")

#use this page for dealing with dates more easily
install.packages("lubridate")
library("lubridate")

#make up a data and ask for various information, year, month, week of the year, day of the month, day in the year, day in the week
y = as.Date("2017-09-28")
year(y)
month(y)
week(y)
mday(y)
yday(y)
wday(y)

#provide label output
month(y,label=T)
wday(y,label=T)

#make a second fake day and compare it to first
z = as.Date("2017-10-5")
z-y
as.numeric(z-y)


##Dealing with data.frames
tmp = data.frame(rnorm(10), rep(c("sp1","sp2","sp3","sp4","sp5"),times=2))
colnames(tmp) = c("trait","species")
spl.tmp = split(tmp,tmp$species)
spl.tmp
spl.tmp[1]
spl.tmp["sp3"]
unlist(spl.tmp)

#summarize data in df (or matrix) using tapply and an index
tapply(tmp$trait,tmp$species,FUN=mean)
tapply(tmp$trait,tmp$species,FUN=max)

#summarize data in list using lapply 
lapply(spl.tmp,FUN = function(x){mean(x$trait)})
lapply(spl.tmp,FUN = function(x){max(x$trait)})

##Dealing with matrices
mat = matrix(1:9,ncol=3)
mat[2,2] = NA

#are there na's?
is.na(mat)
!is.na(mat)

#what rows or columns have non-NA values throughout?
complete.cases(mat)
mat[complete.cases(mat),]
mat[,complete.cases(mat)]




