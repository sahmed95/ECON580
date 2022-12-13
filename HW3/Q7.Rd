
k = 3
lambda = 2 

# probability of X being less than or equal to 5 

q <- ppois(k*lambda-1,lambda) 


# probability of X being greater than 5 = probability of X being greater than equal to 6

p <- 1-q


