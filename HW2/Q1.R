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

