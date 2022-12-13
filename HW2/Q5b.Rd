set.seed(42)
simulations <-  seq(10,10000, 20) # sample sizes

n <-400 

p <- 0.3

E <- c()
V <-c()

m <- 1:length(simulations) 
for (i in m){
	val <- rbinom(simulations[i], n, p)
	e <- mean(val)
	v <- var(val)
	E[i] <- e
	V[i] <- v
}

# Calculating theoretical expectation and variance 

theo_E <-rep(n*p, length(simulations)) # theoretical = np
theo_V <- rep(n*p*(1-p), length(simulations)) #theoretical = np(1-p)



# Plotting the Expectation and Variance 

plot(simulations, E, pch = 20, main ="Binomial, n =400, p = 0.3", xlab="Sample Size", ylab = "E(X)")
points(simulations, theo_E, col="blue", type = "l")


plot(simulations, V, pch = 20, main ="Binomial, n =400, p = 0.3", xlab="Sample Size", ylab = "V(X)")
points(simulations, theo_V, col="blue", type = "l")

# Plotting the difference

# calculating the differences 

diff_E <- abs(theo_E-E) 
diff_V <- abs(theo_V-V)


plot(simulations, diff_E, pch =20, main = "Absolute Error of Expectaion and Variance", xlab = "Number of simulations", ylab ="Absolute error")
points(simulations, diff_V, col="red")




