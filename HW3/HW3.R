# Question 4

set.seed(100)

n <- 1000 # number of simulated values 

delta <- 0.01 

X <- rnorm(n,5, 1) # generating simulated values 

V <- exp(delta*X)  # simulated values of V

E_V<- mean(V)

W <- exp(-delta*X) # simulated values of W 

E_W <- mean(W)

d <- (E_V-E_W)/(2*delta)  # derivative of E(e^tX) evaluated at t = 0

#------------------------------------------------------------------------

# Question 7


k = 3
lambda = 2 

# probability of X being less than or equal to 5 

q <- ppois(k*lambda-1,lambda) 


# probability of X being greater than 5 = probability of X being greater than equal to 6

p <- 1-q


