# Problem 1 part d) 

set.seed(10000)

nsims <- 10000 # number of simulations
seq <-1:nsims

smean <- c() # initiliazing list of sample means
mse <- c() # initializing list to calculate mean squared error

for (i in seq){
	p <- 0.89  # Bernoulli parameter 
	n <- 25 # sample size 
	x <- rbinom(n,1,p)
	avg <- mean(x)
	smean[i] <- avg
	se <- (avg-p)^2
	mse[i] <- se
}

hist(smean, main = "Distribution of X-bar", xlab = "Sample Mean")
ex <- mean(smean)
vx <- var(smean)

# Mean squared error 

MSE <- mean(mse)

# ----------------------------------------------------------------------------

# Problem 2 part b) 

set.seed(10000)

nsims <- 10000 # number of simulations
seq <-1:nsims

smean <- c() # initiliazing list of sample means
#mse <- c() # initializing list to calculate mean squared error

for (i in seq){
	lambda <- 4 # Bernoulli parameter 
	n <- 20 # sample size 
	x <- rpois(n,lambda)
	avg <- mean(x)
	smean[i] <- avg
	se <- (avg-lambda)^2
	mse[i] <- se
}

hist(smean, main = "Distribution of X-bar", xlab = "Sample Mean")
ex <- mean(smean)
vx <- var(smean)

# Mean squared error 

MSE <- mean(mse)

#---------------------------------------------------------------------------------
# Problem 4 part c) 

u0 <- 0       # null hypothesis
u1 <- 0.3     # alternative hypothesis
alpha <- 0.05 # significance level
sigma <-4    # standard deviation

d_p <- 0.9  # desired power

z <- qnorm(1-alpha) 

s<- 10000 # maximum sample size considered

seq <- 36:s

val <-c()

for (i in seq){
	x <- (u0-u1)/(sigma/sqrt(i))+z
	power <- 1-pnorm(x)
	val[i] <- power
	if (power > d_p){
		n <- i # sample size for desire dpower 
		break 
	}
}
