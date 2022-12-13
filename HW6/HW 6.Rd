

newcomb <- read.table('/Users/student/Desktop/Fall 2020/580/HW 6/newcomb-light-data.csv',header=TRUE,sep=",")
y <- newcomb$time


mu0 <- 30 # mean of prior distribution 
tau20 <- 0.01 # precision (or 1/variance) of prior distribution

nu0 <- 1
phi20 <- 1

par(mfrow=c(1, 2)) 
plot(seq(0,60,by=0.1), (dnorm(seq(0,60,by=0.1),mu0,1/sqrt(tau20))), type="l",xlab=expression(mu), ylab="Density",main=expression(paste("Density for prior on ", mu))) 

plot(seq(0,12,by=0.01), (dgamma(seq(0,12,by=0.01),nu0/2,nu0/(2*phi20))), type="l",xlab=expression(phi^2), ylab="Density",main=expression(paste("Density for prior on ", phi^2)))

mean.y <-mean(y) 
var.y <- var(y) 
n <-length(y)

summary <- matrix(c(mean.y, var.y, n),ncol=3,byrow=TRUE)
colnames(summary) <- c("Mean","Variance","n")
summary <- as.table(summary)

G<- 100
H<- 100

mu.grid <- seq(24.78,24.88, length = G)
phi2.grid <-seq(30,100, length =H)

post.grid <- matrix(nrow=G,ncol =H) #to store posterior 
for(g in 1:G){
	for (h in 1:H){
		post.grid[g,h] <- 
		dnorm(mu.grid[g], mu0, 1/sqrt(tau20))*
		dgamma(phi2.grid[h], nu0/2, nu0/(2*phi20))*
		prod(dnorm(y,mu.grid[g], 1/sqrt(phi2.grid[h])))
	}
}

post.grid <- post.grid/sum(post.grid) # normalizing 

### Part a i) 

# Image of joint posterior  

image(mu.grid, phi2.grid, post.grid, main = expression(paste("Heat map for Posterior on (",mu,",", phi^2,")")), xlab = expression(mu), ylab =expression(phi^2))

par(mfrow=c(1,2))

### Part a ii) 

# Compute marginal posteriors for mu and phi2

post.mu <- rowSums(post.grid)
post.phi2 <- colSums(post.grid)

plot(mu.grid, post.mu, type ="l", main = expression(paste("Marginal posterior for ", mu)), xlab = expression(mu), ylab = "Density")

plot(phi2.grid, post.phi2, type ="l", main = expression(paste("Marginal posterior for ", phi^2)), xlab = expression(phi^2), ylab = "Density")
#---------------------------------------------------------------------------------
### Part b) 

# Gibbs Sampling

# Chain length 

S <- 10000 # generate 10,000 dependent samples 
samples <- matrix(nrow=S, ncol = 2) # to store samples 

# column 1 will contain mu samples and column 2 will contain phi^2 samples

# Starting values 

samples[1,] <- c(mean.y,1/var.y)

set.seed(10)
for (s in 2:S){
	
# Generating new value of mu from f(mu| prev value of phi2, data)
	prev.phi2 <- samples[s-1,2]
	mustar <- (mu0*tau20+n*mean.y*prev.phi2)/(tau20+n*prev.phi2)
	phi2star <- tau20+n*prev.phi2
	mu.new <-rnorm(1, mustar, 1/sqrt(phi2star))
	
# Generating a new value of phi2 from f(phi2| new value of mu, data)
	astar <- (nu0+n)/2
	bstar <- (nu0/phi20 + (n-1)*var.y + n*(mean.y-mu.new)^2)/2
	phi2.new <- rgamma(1,astar, bstar)
	samples[s,] <- c(mu.new, phi2.new)
}

image(mu.grid, phi2.grid, post.grid, main=expression(paste("Heat map for Posterior on (", mu, ",", phi^2, ") and Scatterplot of the draws") ), xlab = expression(mu), ylab = expression(phi^2)) 
points(samples[,1], samples[,2], pch = "+")

### Part b ii) 

quantile(samples[,1], c(0.025, 0.5, 0.975)) # for mu

### Part b iii) 

quantile(samples[,2], c(0.025, 0.5, 0.975)) # for phi^2


### Part b iv) 

quantile(1/sqrt(samples[,2]), c(0.025, 0.5, 0.975))  # for population standard deviation (sigma)


### Part b v) 

# Function: c = distance/time = 7.44373/mu
speed = (7.44373)/(samples[,1]*(10^(-6)))

quantile(speed, c(0.025, 0.5, 0.975)) # for c

### Part b vii) 

hist(y, main = "Histogram of observed data", xlab = "time (ms)") 

sample_mean <- mean.y 

sample_var <- var.y 

