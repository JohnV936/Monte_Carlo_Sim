# Functions to be used later
x.dev.calculator <- function(){ # calculates deviation in x direction according to question
  x <- sample(1:3, 1) # random number between 1 and 3
  if (x == 1){
    dev.x <- runif(1,-0.2,0.4) # uniform distribution
  } else{
    dev.x <- 0 
  }
  dev.x
  }

radius.calculator <- function(x,y){ # calculates the magnitude of the radius given x,y deviations
  radius_squared = x^2 + y^2
  radius = sqrt(radius_squared)
  radius
}

# Calculating end of day deviations that would actually happen
# Excluding inaccurate values that happen after misalignment
end.day.dev.2 <- function(deviations){ 
  if(first.outside(deviations) <= length(deviations)){ # check if mislaignment happens
    x <- deviations[first.outside(deviations)] # if so, store the index (sample number)
  }else{
      x <- deviations[length(deviations)] # else, store final deviation
  }  
  x
} 


num.inaccurate <- function(deviations){ # finds the inaccurate deviations and returns the number of them 
  inaccurate.dev = c()
  for(i in 1:length(deviations)){
    if (deviations[i]>12.5){# checking to see if misalignment has already happened
      break
    }else if(deviations[i]>10 & deviations[i]<12.5){
      inaccurate.dev[i] <- deviations[i] # storing all deviations in the inaccurate zone
    }else{
      inaccurate.dev[i] <- 0  
    }
  }
  length(inaccurate.dev[inaccurate.dev>0])
}

first.inaccurate <- function(deviations){ # finds the index of the first inaccurate measurement
  for (i in 1:length(deviations)){
    if(deviations[i]>10 & deviations[i]<=12.5){
      first.inaccurate <- i
      break
    }else{
      first.inaccurate <- length(deviations)+1
    }
  }
  first.inaccurate
}

# returns a vector with the indexes of inaccurate deviations 
index.inaccurate <- function(deviations){ 
  inaccurate.indexes <- c()
  for(i in 1:length(deviations)){
    if (deviations[i]>12.5){# checking to see if misalignment has already happened
      break
    }else if(deviations[i]>10 & deviations[i]<=12.5){
      inaccurate.indexes[i] <- i  # storing all indexes for deviations 
                                  # in the inaccurate zone
    }else{
      inaccurate.indexes[i] <- 0  # storing a 0 if the measurement is in the accurate zone
    }
  }
  inaccurate.indexes[inaccurate.indexes>0]
}

# detects first deviation to go out of the petri-dish and returns the index
first.outside <- function(deviations){ 
  for (i in 1:length(deviations)){
    if(deviations[i]>12.5){
      first.outside <- i
      break
    }else{
      first.outside <- length(deviations)+1 
      # return impossible index number to indicate not going outside
    }
  }
  first.outside
}

# Calculates profit for a given list of deviations with a length n
profit_day <- function(deviations, n){ 
  if (first.outside(deviations)>n){ 
    # checks if the needle goes outside at all
    p <- (n - num.inaccurate(deviations))*800 - num.inaccurate(deviations)*200 
    # case where all the samples are taken
  }else if(first.inaccurate(deviations) < first.outside(deviations)){
    # checks if inaccurate values occur before misalignment 
    p <- (first.outside(deviations)- 1 - num.inaccurate(deviations))*800 - (num.inaccurate(deviations)*200) - 
      (n - first.outside(deviations)+1)*400 - 75000 
      # calculation takes into account inaccurate measurements and misalignment 
  }else if(first.outside(deviations) < first.inaccurate(deviations)){
    # checks if the first outside is before the first inaccurate
    p <- (first.outside(deviations)-1)*800 -  # inaccurate values not applicable
      ((n - first.outside(deviations)+1)*400) - 75000
  }
  p # returns profit for that day
}

#function to calculate everything
align <- function(n){
  dev.y <- rnorm(n, -0.02, 0.1) # generating vector with n y deviations
  cum.dev.y <- cumsum(dev.y) # storing cumulative y deviations
  dev.x <- replicate(n, x.dev.calculator()) # generate vector with n x deviations
  cum.dev.x <- cumsum(dev.x) # storing cumulative y deviations
  dev.totals_over_day <- radius.calculator(cum.dev.x, cum.dev.y) # records deviation radius
  index <- 1:n
  c(end.day.disp=radius.calculator(sum(dev.x), sum(dev.y)), 
    # finds theoretical end of day displacement, ignores the fact 
    # that the alignment could have gone out and back in again 
    end.day.disp.2 = end.day.dev.2(dev.totals_over_day), 
    # finds end of day deviation, stopping after misalignment 
    first.outside=first.outside(dev.totals_over_day),
    # finds first alignment that is outside the petri dish
    num.inaccurate=num.inaccurate(dev.totals_over_day), 
    # computes number of misaligned deviations and checks that 
    # misalignment has not happened already
    first.inaccurate=first.inaccurate(dev.totals_over_day),
    # saves the index of the first inaccurate reading
    # Question asks about "becoming inaccurate"
    profit=profit_day(dev.totals_over_day, n),
    # calculates profit for the day
    
    index.inaccurate=index.inaccurate(dev.totals_over_day)
    # finds all indexes for inaccurate measurements
    )
  }




# Plotting End of Day Deviations
sim_end_day_dev <- replicate(1000, align(300)["end.day.disp"])
# View(sim_end_day_dev)
plot(x=1:1000, y=sim_end_day_dev, main = "Plot of 1000 End of Day Deviations")
abline(h=12.5, col="red",)
# inaccuracy Limit is 10 mm
abline(h=10, col="blue")

# Plotting End of day deviations,
# stopping after misalignment
sim_end_day_dev_2 <- replicate(1000, align(300)["end.day.disp.2"])
plot(x=1:1000, y=sim_end_day_dev_2, main = "Plot of 1000 End of Day Deviations", 
                                    sub = "Stopping after misalignment")
abline(h=12.5, col="red",)
# inaccuracy Limit is 10 mm
abline(h=10, col="blue")


# Distribution of number of tests before first inaccuracy
sim_first_inaccurate <- replicate(10000, align(300)["first.inaccurate"])
sim_first_inaccurate
hist(sim_first_inaccurate[sim_first_inaccurate<=300],
     main = "Distribution of First Inaccurate Measurements",
     xlab = "Sample number")


# Probability of becoming inaccurate at some point during a day
length(sim_inaccurate[sim_inaccurate<=300])/length(sim_inaccurate) #= 0.849
# note: It is possible that the needle becomes inaccurate at some point 
# and then returns to accurate. It could also become inaccyrate again

# Finding distribution of inaccurate measurement indexes
total.inaccurates <- c()  # vector to store all inaccurate measurement 
# indexes for all repetitions
for(i in 1:1000){
  data <- align(300) # performs simulation for the day (assuming 300 samples)
  inaccurate.indexes <- data[7:length(data)] 
  # stores the indexes of inaccurate measurements for that day
  total.inaccurates <- append(total.inaccurates, inaccurate.indexes)
  # appends the inaccurate indexes to one vector for histogram and calculation
}
#inaccurate.indexes

# Probability of any given measurement to be inaccurate, 
# not including values after misalignment
length(total.inaccurates)/(300*1000) # around 0.12 
# total inaccurate measurements divided by total samples over 100 days

# Probability of becoming misaligned at some point during the day
sim_first_outside <- replicate(1000, align(300)[3])
length(sim_first_outside[sim_first_outside<=300])/length(sim_first_outside) # around 0.4
# Assuming that if the needle becomes misaligned, 
# the machine is shut down for the rest of the day

# Distribution of first measurements outside 
hist(sim_first_outside[sim_first_outside<=300],
     xlab = "First Sample outside Petri Dish", 
     main = "First Sample to go Outside over 1000 days")

# Distribution of ALL sample numbers that were inaccurate
hist(total.inaccurates[total.inaccurates<=300],
     xlab = "Inaccurate Samples",
     main = "Inaccurate Samples over the 1000 Days") # distribution of inaccurate indexes over 1000 work days

# Finding max profit
# maximum is almost certainly between 200 and 300
average.profit.broad = replicate(300, NA)
for(N in 200:300){
  profit.sim <- replicate(1000, align(N)[6])
  average.profit.broad[N] <- mean(profit.sim) # storing average for 1000 days
                                              # with N samples    
}
# View(average.profit)
# hist(average.profit[200:300], breaks = 5) # 

plot(x=200:300, y=average.profit.broad[200:300], 
     main = "Mean Profit ~ Number of Sample",
     xlab = "Number of Samples",
     ylab = "Profit in Euros") # plotting profit against N

max(average.profit.broad[200:300])
# Max profit is between 250 and 270 and around 183,100 euros

# rerunning between 250 to 270
average.profit = replicate(300, NA)
for(N in 250:270){
  profit.sim <- replicate(10000, align(N)[6]) # 10000 simulated days per N
  average.profit[N] <- mean(profit.sim)
}
plot(x=250:270, y=average.profit[250:270],
     main = "Mean Profit ~ Number of Sample",
     xlab = "Number of Samples",
     ylab = "Profit in Euros")

# Max seems to be between 250 and 260
average.profit = replicate(300, NA)
for(N in 250:260){
  profit.sim <- replicate(10000, align(N)[6]) # 10000 simulated days per N
  average.profit[N] <- mean(profit.sim)
}

plot(x=250:260, y=average.profit[250:260],
     main = "Mean Profit ~ Number of Sample",
     xlab = "Number of Samples",
     ylab = "Profit in Euros")
average.profit[255]
max(average.profit[250:260]) # 255 samples with a net profit 181,403..60 euros
