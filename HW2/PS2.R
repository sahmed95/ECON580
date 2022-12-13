# Question 1

set.seed(5)
nsims <- 1000  # number of simulations
seq <- 1:nsims
n <-5 # number of tosses 
X5 <-c() # initializing list of total winnings
for (i in seq){
x5 = 0 
fair <- sample(1:2, n, replace=TRUE)  # 5 tosses of the coin
seq1 <- 1:length(fair)
for (j in seq1){
if (fair[j] == 1) x5 <- x5 + (2/3^j)  # 1 = heads and sum the winnings
	}
X5[i] <- x5       
	}

 
plot.ecdf(X5, main = "Empirical CDF of X5")


x_vals<- unique(X5) 

# extracting unique values from X_5; this represents all the possible values X_5 can take 

# there are 32 different event and fair coin means 
# probability of each event is 1/32, that is, the probability that X_5
#takes on one of the values is 1/32. We have extracted the values from 
#X_5 above 

prob <- rep(1/length(x_vals), length(x_vals))

plot(x_vals, prob, xlab = "Values of x", ylab = "P(X_5 =x)", main = "Theoretical PMF of X_5", type = 'h')

m <- 1:length(x_vals)
probs <- c() 

for (i in m){
	bool <- X5 == x_vals[i] # creates 1 if true 0 if false 
	p = mean(bool)  # mean will give the proportion 
	probs[i] = p
}

plot(x_vals, probs, xlab = "Values of x", ylab = "P(X_5 =x)", main = "Empirical PMF of X_5", type = 'h')
points(x_vals, prob, col = "red", type = 'l')

# ------------------------------------------------------------------

# Question 2


set.seed(2)
n <- 500  # sample size 

# Exponential random variable with parameter lambda = 2 
lambda <-2
data <- rexp(n, lambda)

# Theoretical CDF 

probs <- seq(from=0.01,to=0.99,by=0.01)
xvals <- qexp(probs, lambda)

plot.ecdf(data,xlim=c(min(xvals),max(xvals)), main ="CDF for Exponential Distribution with lambda = 2", pch=20)
points(xvals,probs,col="red",type="l")


#----------------------------------------------------------------------

# Question 3

val <- c(0.03,0.11,0.42, 0.44,0.47,0.66,0.75, 0.88, 0.89, 0.90) # values 


# theoretical cdf F(t) =t on the interval [0,1]
x <- seq(0,1,0.01) 
plot.ecdf(val, xlim= c(0,1),main = "Empirical and Theoretical CDF")
points(x, x, type = 'l', col = 'blue')


# For all t in [0,1]

F <- ecdf(val)
diff <- abs(F(x)- x)
K <- max(diff)

# finding the corresponding t :

# finding the index for which the max difference occurs

t_index <- which(diff== K)

t <- x[t_index]

print(K)
print(t)


# Just for this dataset 

F <- ecdf(val)
diff <- abs(F(val)- val)
K <- max(diff)

# finding the corresponding t :

# finding the index for which the max difference occurs

t_index <- which(diff== K)

t <- val[t_index]
print(K)
print(t)

# ----------------------------------------------------------------------

# Q5 a) 

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



# Question 5b) 


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


# ---------------------------------------------------------------------

# Question 6

set.seed(42)
p <- 0.2 # probability of heads for biased coin
procedure <- 1:1000

tot_num_flips = 0

for (i in procedure){
	num_flips <- 0
	repeat{
	x_val <- c()
	x_val[1] <-  rbinom(1, 1, p) # 1st flip
	x_val[2]<- rbinom(1,1,p)	 # 2nd flip 
	num_flips <- num_flips + 2   # doing 2 flips 
	if (x_val[1] != x_val[2]) {  # if the flips are different
		break 
				}		
		}
	tot_num_flips <- tot_num_flips + num_flips	
	}
	



avg_num_flips <- tot_num_flips/length(procedure)
print(avg_num_flips)
