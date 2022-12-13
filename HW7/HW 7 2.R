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