data <- read.csv(file = "main.csv")Q

#saving all kharif data in "kharif variable"
kharif <- subset(data, season == "Kharif")
#saving all kharif data in "rabi variable"
rabi <- subset(data, season == "Rabi")

#getting orignal regression model from collected subsets
kmodel<- lm(v37~index,kharif)
rmodel<- lm(v37~index, rabi)

#printing the b1 and b2 ols of the given data
kmodel
rmodel

plot(kharif$index,kharif$v37, col = "blue", main = "v37 and yield index representaion(Kharif)",
     abline(kmodel), cex = 0.75, pch = 16, xlab = "yield index", ylab = "v37")

plot(rabi$index,rabi$v37, col = "purple", main = "v37 and yield index representation(Rabi)",
     abline(rmodel), cex = 0.75, pch = 16, xlab = "yield index", ylab = "v37")

################################Kharif######################################

#Monte Carlo simulation on given data to get the estimates of b1 and b2
krow <-nrow(kharif)

kb1 = c()
kb2 = c()
for(i in 1:1000){
  # shuffle the dataframe by rows(randomize data)
  shuffled_k = kharif[sample(1:krow), ]     
  #take 80% of the randomised data
  k_80 = head(shuffled_k, floor(0.8*krow))
  k_submodel <- lm(v37~index, k_80)
  kb1[i] = k_submodel$coefficients[1]
  kb2[i] = k_submodel$coefficients[2]
}

#mean of the b1 hats from the subpopulation taken from the data
mean(unlist(kb1))
mean(unlist(kb2))

#histogram depicting the Monte Carlo Simulation values of b1 hat
hist(kb1,main = "Values of b1 hat ols in Monte Carlo Simulations", xlab = "b1 hat")
#histogram depicting the Monte Carlo Simulations values of b2 hat
hist(kb2,main = "Values of b2 hat ols in Monte Carlo Simulations", xlab = "b2 hat")
#standard deviation of b1 hat 
sd(kb1)
#standard deviation of b2 hat
sd(kb2)
#standard error oh b1 hat
sd(kb1)/sqrt(floor(0.8*krow))
#standard error oh b2 hat
sd(kb2)/sqrt(floor(0.8*krow))

#################################Rabi#######################################
rrow <-nrow(rabi)

rb1 = c()
rb2 = c()
for(i in 1:1000){
  # shuffle the dataframe by rows(randomize data)
  shuffled_r = rabi[sample(1:rrow), ]     
  #take 80% of the randomised data
  r_80 = head(shuffled_r, floor(0.8*rrow))
  r_submodel <- lm(v37~index, r_80)
  rb1[i] = r_submodel$coefficients[1]
  rb2[i] = r_submodel$coefficients[2]
}

#mean of the b1 hats from the subpopulation taken from the data
mean(unlist(rb1))
mean(unlist(rb2))

#histogram depicting the Monte Carlo Simulation values of b1 hat
hist(rb1,main = "Values of b1 hat ols in Monte Carlo Simulations", xlab = "b1 hat")
#histogram depicting the Monte Carlo Simulations values of b2 hat
hist(rb2,main = "Values of b2 hat ols in Monte Carlo Simulations", xlab = "b2 hat")
#standard deviation of b1 hat 
sd(rb1)
#standard deviation of b2 hat
sd(rb2)
#standard error oh b1 hat
sd(rb1)/sqrt(floor(0.8*rrow))
#standard error oh b2 hat
sd(rb2)/sqrt(floor(0.8*rrow))

summary(kharif$year)
