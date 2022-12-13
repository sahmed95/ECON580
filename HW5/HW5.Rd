
# Log of the population density and natural logarithm of the vote share 
                              
# Part a) 

# Reading the data 

election <- read.table('Desktop/Fall 2020/580/HW5/election.csv',header=TRUE, sep=",")

# checking the variables 

names(election)   

# # what is the first population density
   
election$log10popdens[1]    

# what is the first ratio of votes
				
election$logpartyratio[1]  

# changing variable names 
  
pop <-election$log10popdens  #log of population density
vote <-election$logpartyratio #natural log of vote ratio

# Best linear predictor 

fit <- lm(vote ~ pop)

# Plotting the best linear predictor 

plot(pop, vote, xlab = "Log Population density", ylab = "Log Ratio of vote share", main= "Best Linear Predictor of Vote Share from Population Density")
abline(fit,col="red")


# Part b 

# Binning the data 

hx <- hist(pop)
abline(v=hx$breaks,col="red")


# Plotting the bins along with the data 

plot(pop,vote,main="Scatterplot for data")
abline(v=hx$breaks,col="red")



z <- rep(0,length(pop))  # will give the mid-point for each bin

bin.no <- rep(0,length(pop)) # will tell us which bin each datapoint is in


for (i in 1:(length(hx$breaks)-1)){
z <- z + rep(hx$mid[i],length(pop))*((pop> hx$breaks[i]) & (pop < hx$breaks[i+1]))
bin.no <- bin.no + rep(i,length(pop))*((pop > hx$breaks[i]) & (pop < hx$breaks[i+1]))
}

boxplot(vote~z,main="Boxplots for each bin") # all box plots at once 



#Approximating the conditional expectation function f(x) = E(Y|X=x)

mean.vote.given.pop <- rep(NA,length(hx$mid))

for(i in 1:length(hx$mid)){
if(hx$counts[i]>0){
mean.vote.given.pop[i] <-  mean(vote[z==hx$mid[i]])
}}

mean.vote.given.pop

plot(hx$mid,mean.vote.given.pop,type="l",xlab="X",ylab="E(logpartyratio|log10popdens)",main="Conditional Expectation Function",col="blue",xlim=c(min(pop),max(pop)),ylim=c(min(vote),max(vote)))

# Part c)

# Loess smoother for conditional expectation 

loess.fit <- loess(vote ~ pop)

# Scatterplot showing the data together with these three functions 

plot(hx$mid,mean.vote.given.pop,type="l",xlab="Log of Population Density",main="Relating vote share with population density",col="blue",xlim=c(min(pop),max(pop)),ylim=c(min(vote),max(vote)))

# loess smoother
points(sort(pop),predict(loess.fit,data.frame(pop=sort(pop))),col="red",type="l")

# adding in the points
points(pop, vote)

# adding in the best linear predictor
abline(fit,col="green")