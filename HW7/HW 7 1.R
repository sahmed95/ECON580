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