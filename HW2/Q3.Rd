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

