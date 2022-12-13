# Problem 4 part c) 

u0 <- 0       # null hypothesis
u1 <- 0.3     # alternative hypothesis
alpha <- 0.05 # significance level
sigma <-4    # standard deviation

d_p <- 0.9  # desired power

z <- qnorm(1-alpha) 

s<- 10000 # maximum sample size considered

seq <- 36:s

val <-c()

for (i in seq){
	x <- (u0-u1)/(sigma/sqrt(i))+z
	power <- 1-pnorm(x)
	val[i] <- power
	if (power > d_p){
		n <- i # sample size for desire dpower 
		break 
	}
}
