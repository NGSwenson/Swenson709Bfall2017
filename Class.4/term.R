#My terminal attempt

tmp = runif(50)

for(i in 1:10){

samp = sample(tmp, 10, replace=T)

write.table(as.matrix(samp), paste(i, ".output.txt", sep=""), sep="\t",row.names=F,col.names=F,quote=F)


}

