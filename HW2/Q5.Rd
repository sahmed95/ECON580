set.seed(42)
simulations <-  seq(10,10000, 20) # sample sizes  
lambda <- 2 # parameter 
E <- c()
V <-c()
n <- 1:length(simulations) 
for (i in n){
	val <- rpois(simulations[i],lambda)
	e <- mean(val)
	v <- var(val)
	E[i] <- e
	V[i] <- v
}

theoretical <- rep(2, length(simulations)) # theoretical = 2


# Plotting the Expectation and Variance 


plot(simulations, E, pch = 20, main ="Poisson Distribution with lambda =2", xlab="Sample Size", ylab = "E(X)")
points(simulations, theoretical, col="blue", type = "l")


plot(simulations, V, pch = 20, main ="Poisson Distribution with lambda =2", xlab="Sample Size", ylab = "V(X)")
points(simulations, theoretical, col="blue", type = "l")

# Plotting the difference

# calculating the differences 

diff_E <- abs(theoretical-E) 
diff_V <- abs(theoretical-V)

zero <- rep(2,length(simulations))
plot(simulations, diff_E, pch =20, main = "Absolute Error of Expectaion and Variance", xlab = "Number of simulations", ylab ="Absolute error")
points(simulations, diff_V, col="red")
points(simulations, zero, col="blue", type = "l")



