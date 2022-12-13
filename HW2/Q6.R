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
