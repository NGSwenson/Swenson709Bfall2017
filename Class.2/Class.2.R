##BIOL709B
#Class 2 notes
#Nate Swenson - swenson@umd.edu
#7-sept-2017
## Draws heavily from Steven's 2010, Primer of Ecology in R

install.packages(c("primer", "deSolve"))
library(primer)
library(deSolve)

################################
#### Density independent population growth
################################
# begin with creating a simple vector of population sizes ordered temporally
N = c(1,4,16,64,256)

# now a vector of years that can be associated with the population sizes
year = 2011:2015

# plot pop size against year without and with log transformation
plot(year, N)
plot(year, log(N))

# calculate lambda using vector math - next year N divided by this year N
lambda = N[2:5]/N[1:4]

###################################
## Discrete population growth model

# Project pop at time t (here time 2) given time t-1 (here time 1)
Nt = (lambda[1]^1)*N[1]


# now project model forward 100 time steps
N100 = (lambda[1]^100) * N[1]

# build your own function for building projections through time, input original pop size (No), lambda value, and number of time steps
# name/define function as discrete.mod
discrete.mod =  function(No, my.lambda, time){
	# the discrete model
		Nt = (my.lambda^time) * No 
	
	#return output
		return(Nt)
}


# run your function with starting population size of 1, lambda of 1.5 and projecting to pop size at time = 10 
discrete.mod(1,1.5,10)


# run your function with starting population size of 1, lambda of 1.5 and 10 time steps
discrete.mod(1,1.5,1:10)


# plot the 10 time steps on x axis versus the output of your discrete model with No=1, lambda = 1.5
plot(1:10, discrete.mod(1,1.5,1:10))



#####################################
## Continuous population growth model
# start with defining some parameters
# time length to consider
time = 2

# instantaneous growth rate
little.r = 0.5

# starting pop size
No = 1

#continuous model for a given time window
Nt = No*(exp(little.r*time))

#output
Nt 

#now redifine time as a vector of 1 time steps and run continuous model
time = 1:10
Nt = No*(exp(little.r*time))
Nt 

#plot growth over 10 time steps against a vector of 1 to 10
plot(1:10, Nt)

# write your own function for continuous pop growth (dens independent)
# name function as continuous.mod with starting pop size, rate of increase and time steps as input
continuous.mod =  function(No, my.r, time){

	# the continuous model
	Nt = No*(exp(my.r*time))
	
	#return output
	return(Nt)
}

# run your new continuous model function with starting pop of 2, r = 0.3, and ten time steps
continuous.mod(2,.3,1:10)

#####################################
### Randomly varying little r
## now let's have some fun by building a model where little r stochastically varies through time

## define a new function called random.continuous mod
## function uses initial pop size (No), an average/expected little r, an expected level of variation in little r., number of time steps to project
random.continuous.mod = function(No, average.little.r, sd.in.r, time.steps){

	#make an empty matrix, with the number of colums 1 and the number of rows equal to the number of time steps
	pops = matrix(NA, nrow = time.steps,ncol=1)
	
	# calculate first time step using the expected little r
	pops[1] = No*(exp(average.little.r*1))

	# write a for loop. Starting with 1 and going to time steps minus 1
	for(i in 1:(time.steps-1)){
	
		# obtain a random little r value from a random normal distribution with the expected little r as mean and the given st dev in little r
		random.little.r = rnorm(1, mean = average.little.r, sd = sd.in.r)
		
		# now put that random little r into the continuous model using the previous year pop size as well to project the next year.
		# first iteration it is pop of year 1 (i) and a random little r to project pop at time 2 (i+1)
		pops[i+1] = pops[i]*(exp(random.little.r*1))

	}

#return output of population sizes
return(pops)
}

# plot the results of log population sizes with No =2, exp little.r =0.5, sd little r =1, and 500 time steps against a vector of 1 to 500
plot(1:500,log(random.continuous.mod(2, .5, 1, 500)))

# prepare graphics device for a 3 panel plot with 1 row and 3 columns
par(mfrow = c(1,3))

# in left panel plot the results of log population sizes with No =2, exp little.r =0.5, sd little r =1, and 500 time steps against a vector of 1 to 500
plot(1:500,log(random.continuous.mod(2, .5, 1, 500)))

# in middle panel plot the results of log population sizes with No =2, exp little.r =0.5, sd little r =2, and 500 time steps against a vector of 1 to 500
plot(1:500,log(random.continuous.mod(2, .5, 2, 500)))

# in middle panel plot the results of log population sizes with No =2, exp little.r =0.5, sd little r =8, and 500 time steps against a vector of 1 to 500
plot(1:500,log(random.continuous.mod(2, .5, 8, 500)))

################################################################################################
################################################################################################
################################################################################################
################################################################################################
################################
#### Density independent demography
################################

M = matrix(1:4, nrow=2, byrow=T)
N = matrix(c(10,20,30,40),nrow=2)
M
N

1*10+2*20
1*30+2*40
3*10+4*20
3*30+4*40

M %*% N

N.seeds = 100
N.saplings = 500
N.adults = 50

stage.matrix.N0 = matrix(c(N.seeds,N.saplings,N.adults), byrow=T)
stage.matrix.N0
rownames(stage.matrix.N0) = c("seeds","saplings","adults")
stage.matrix.N0

projection.matrix = matrix(c(0,0.5,20,0.3,0,0,0,0.5,0.9), nrow = 3, byrow=T)
projection.matrix

stage.matrix.N1 = projection.matrix %*% stage.matrix.N0 
stage.matrix.N1

stage.proj.function = function(No.matrix, proj.mat, years){

	pops = matrix(NA, nrow = nrow(No.matrix), ncol = years)
	pops[,1]  = No.matrix
	
	for(i in 2:years){
	
	pops[,i] = proj.mat %*% pops[,i-1]
	
	}
return(pops)
}

output = stage.proj.function(stage.matrix.N0, projection.matrix, 10)

output

matplot(1:10,t(output))
matplot(1:10,log(t(output)))


eigen(projection.matrix)


dom.pos = which.max(eigen(projection.matrix)[["values"]])

##calculate lambda = asymptotic rate of finite increase
lambda.1 = Re(eigen(projection.matrix)[["values"]][dom.pos])

stage.matrix.N1

abline(0,lambda.1)


################################################################################################
################################################################################################
################################################################################################
################################################################################################
################################
#### Density dependent growth
################################


Nt1 = 40
little.r = 0.5
K = 100

Nt2 = Nt1 + (little.r*(1 - (Nt1/K))*Nt1)
Nt2

time = 50

log.growth = function(N1, lil.r, cary.cap, iterations){

	pop = c(NA)

	pop[1] = N1 + (lil.r*(1 - (N1/cary.cap))*N1)

	for(i in 2:iterations){
	
	pop[i] = pop[i-1] + (lil.r*(1 - (pop[i-1]/cary.cap))*pop[i-1])
	}
	
return(pop)	

}

log.growth(2, 0.5, 100, 40)

plot(1:40, log.growth(2, 0.5, 100, 40))

plot(1:40, log.growth(2, 0.5, 100, 40), ylim=c(0,100))
points(1:40, log.growth(2, 1.5, 100, 40), col="red")
points(1:40, log.growth(2, 2.5, 100, 40), col="blue")


plot(1:40, log.growth(2, 0.5, 100, 40), ylim=c(0,200))
points(1:40, log.growth(2, 1.5, 100, 40), col="red")
points(1:40, log.growth(2, 2.5, 100, 40), col="blue")

plot(1:40, log.growth(2, 0.5, 100, 40), ylim=c(0,200),type="l")
lines(1:40, log.growth(2, 1.5, 100, 40), col="red")
lines(1:40, log.growth(2, 2.5, 100, 40), col="blue")

growth.increments = log.growth(2, 0.5, 100, 40)[2:40] - log.growth(2, 0.5, 100, 40)[1:39]

per.capita.growth = growth.increments/log.growth(2, 0.5, 100, 40)[1:39]

plot(log.growth(2, 0.5, 100, 40)[1:39], growth.increments)

plot(log.growth(2, 0.5, 100, 40)[1:39] , per.capita.growth)


summary(lm(per.capita.growth  ~ log.growth(2, 0.5, 100, 40)[1:39]))

#r = intercept
#r*alpha =slope

abs(-5.000e-03/5.000e-01)

#K=1/alpha 
 1/(abs(-5.000e-03/5.000e-01))


growth.v.pop = expression(r * N * (1 - alpha * N))

r = 1; alpha = 0.01; N = 1:120

plot(N, eval(growth.v.pop), type = "l", ylab = "Pop Growth Rate (dn/dt)", xlab = "N")

arrows(20,2,80,2, length=0.1, lwd=3)
arrows(122,-2,109,-2,length = 0.1, lwd=3)
abline(h = 0,col="blue")
abline(v = 1/alpha,col="red")

################################################################################################
################################################################################################
################################################################################################
################################################################################################
################################
#### Lotka Volterra
################################
comp.coeffs = matrix(c(0.001, 0.0005,0.0001,0.002), byrow=T,nrow=2)
comp.coeffs
Nt0_1 = 50
Nt0_2 = 25
r_1 = 0.8
r_2 = 1.0

Nt1_1 = Nt0_1 + r_1 * Nt0_1 * (1 - comp.coeffs[1,1]*Nt0_1 - comp.coeffs[1,2] * Nt0_2)
Nt1_1 

Nt1_2 = Nt0_2 + r_2 * Nt0_2 * (1 - comp.coeffs[2,1]*Nt0_2 - comp.coeffs[2,2] * Nt0_1)
Nt1_2


lv.funk = function(N_1, N_2, coeff.mat, r_1, r_2, time.steps){

	pop.1 = c(NA)
	pop.2 = c(NA)

	pop.1[1] = N_1 + r_1 * N_1 * (1 - coeff.mat[1,1]*N_1 - coeff.mat[1,2] * N_2)
	pop.2[1] = N_2 + r_2 * N_2 * (1 - coeff.mat[2,1]*N_2 - coeff.mat[2,2] * N_1)

	for(i in 2:time.steps){

		pop.1[i] = pop.1[i-1] + r_1 * pop.1[i-1] * (1 - coeff.mat[1,1]*pop.1[i-1] - coeff.mat[1,2] * pop.2[i-1])
		pop.2[i] = pop.2[i-1] + r_1 * pop.2[i-1] * (1 - coeff.mat[2,1]*pop.2[i-1] - coeff.mat[2,2] * pop.1[i-1])

	}
return(cbind(pop.1,pop.2))
}

lv.output = lv.funk(10,12, comp.coeffs, 1.1, 1.0, 50)
lv.output

plot(1:50, lv.output[,1], type = "l",col="blue")
lines(1:50, lv.output[,2], col="red")

plot(0,0,ylim=c(0,1000), xlim =c(0,1000),col="white",xlab="N1",ylab="N2")

for(i in 2:40){ 
	arrows(lv.output[i-1,1], lv.output[i-1,2],lv.output[i,1], lv.output[i,2], length = 0.1)    
}




comp.coeffs2 = matrix(c(0.01, 0.0005,0.01,0.0002), byrow=T,nrow=2)


lv.output = lv.funk(10,12, comp.coeffs2, 1.1, 1.0, 20)
lv.output

plot(1:20, lv.output[,1], type = "l",col="blue", ylim=c(0,150))
lines(1:20, lv.output[,2], col="red")

plot(0,0,ylim=c(0,150), xlim =c(0,150),col="white",xlab="N1",ylab="N2")

for(i in 2:20){ 
	arrows(lv.output[i-1,1], lv.output[i-1,2],lv.output[i,1], lv.output[i,2], length = 0.1)    
}



comp.coeffs3 = matrix(c(0.01, 0.005,0.01,0.002), byrow=T,nrow=2)
lv.output = lv.funk(10,12, comp.coeffs3, 2.1, 2.0, 20)
lv.output

plot(1:20, lv.output[,1], type = "l",col="blue", ylim=c(0,150))
lines(1:20, lv.output[,2], col="red")

plot(0,0,ylim=c(0,150), xlim =c(0,150),col="white",xlab="N1",ylab="N2")

for(i in 2:20){ 
	arrows(lv.output[i-1,1], lv.output[i-1,2],lv.output[i,1], lv.output[i,2], length = 0.1)    
}


lv.output = lv.funk(20,5, comp.coeffs3, 2.1, 2.0, 20)

for(i in 2:20){ 
	arrows(lv.output[i-1,1], lv.output[i-1,2],lv.output[i,1], lv.output[i,2], length = 0.1, col="blue")    
}



lv.output = lv.funk(20,120, comp.coeffs3, 2.1, 2.0, 20)

for(i in 2:20){ 
	arrows(lv.output[i-1,1], lv.output[i-1,2],lv.output[i,1], lv.output[i,2], length = 0.1, col="red")    
}





lv.funk.con = function(t,n, parms){

	with(as.list(parms), {
	
	dn1dt = r1 * n[1] * (1-a11 * n[1] - a12 * n[2])
	dn2dt = r2 * n[2] * (1-a22 * n[2] - a21 * n[1])
	list(c(dn1dt, dn2dt))
	})
}

parms = c(r1=1, r2=0.1, a11=0.2, a21=0.1, a22=0.02, a12 = 0.1)
initialN = c(2,1)
output = ode(y = initialN, times=1:100, func = lv.funk.con, parms = parms)
matplot(output[,1],output[,-1], type="l)






LVCompGames()

