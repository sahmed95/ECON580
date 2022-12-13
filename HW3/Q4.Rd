set.seed(100)

n <- 1000 # number of simulated values 

delta <- 0.01 

X <- rnorm(n,5, 1) # generating simulated values 

V <- exp(delta*X)  # simulated values of V

E_V<- mean(V)

W <- exp(-delta*X) # simulated values of W 

E_W <- mean(W)

d <- (E_V-E_W)/(2*delta)  # derivative of E(e^tX) evaluated at t = 0



