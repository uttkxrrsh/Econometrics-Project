data <- read.csv(file = "main.csv")

#saving all kharif data in "kharif variable"
kharif <- subset(data, season == "Kharif")
#saving all kharif data in "rabi variable"
rabi <- subset(data, season == "Rabi")

################################Kharif######################################

model1 <-lm(v37~v38+v36+v17+v3+tap+beds, kharif)

summary(model1)

plot(kharif$index,kharif$v37,col="blue",cex = 0.75, pch = 16, 
     xlab = "yield index", ylab = "v37",main = "v37 and yield index representaion(Kharif)")

residuals <- model1$residuals

kharif2 <- subset(subset(subset(subset(subset(subset(subset(kharif, v37 != "NA"),v38 != "NA"),v36 != "NA"), v17 != "NA"), v3 != "NA"), tap != "NA"), beds != "NA")

kharifm_index <- kharif2['index']
kharifm_v38 <- kharif2['v38']
kharifm_v36 <- kharif2['v36']
kharifm_v17 <- kharif2['v17']
kharifm_v3 <- kharif2['v3']
kharifm_tap <- kharif2['tap']
kharifm_beds <- kharif2['beds']

plot(kharifm_index[,1],residuals,col="blue",cex = 0.75, pch = 16, 
     xlab = "yield index", ylab = "u hat",main = "u hat and yield index representaion(Kharif)")

plot(kharifm_v37[,1],predict(model1),col="blue",cex = 0.75, pch = 16, 
     xlab = "actual v37", ylab = "predicted v37",main = "actual and predicted values of v37 representaion(Kharif)")

sum(residuals)                     #5.371703e-12
hist(residuals[residuals>-15], xlab = "residual", main = "Residual(Kharif)")
sum(temp<-residuals*kharifm_v38)   #-5.774023e-11
hist(temp[temp>-200], xlab = "uihat*v38", main = "Residual(Kharif)")
sum(temp<-residuals*kharifm_v36)   #6.182183e-11
hist(temp[temp>-500], xlab = "uihat*v36", main = "Residual(Kharif)")
sum(temp<-residuals*kharifm_v17)   #1.970905e-10
hist(temp[temp>-200], xlab = "uihat*v17", main = "Residual(Kharif)")
sum(temp<-residuals*kharifm_v3)    #7.771032e-07
hist(temp[temp>-5e+05], xlab = "uihat*v3", main = "Residual(Kharif)")
sum(temp<-residuals*kharifm_beds)  #3.664058e-06
hist(temp[temp>-5e+05], xlab = "uihat*beds", main = "Residual(Kharif)")
sum(temp<-residuals*kharifm_tap)   #4.021579e-10
hist(temp[temp>-200], xlab = "uihat*tap", main = "Residual(Kharif)")

#################################Rabi#######################################


model2 <-lm(v37~v38+v36+v17+v3+tap+beds, rabi)

plot(rabi$index,rabi$v37,col="blue",cex = 0.75, pch = 16, 
     xlab = "yield index", ylab = "v37",main = "v37 and yield index representaion(Rabi)")

residuals <- model2$residuals

rabi2 <- subset(subset(subset(subset(subset(subset(subset(rabi, v37 != "NA"),v38 != "NA"),v36 != "NA"), v17 != "NA"), v3 != "NA"), tap != "NA"), beds != "NA")

rabim_index <- rabi2['index']
rabim_v37 <- rabi2['v37']
rabim_v38 <- rabi2['v38']
rabim_v36 <- rabi2['v36']
rabim_v17 <- rabi2['v17']
rabim_v3 <- rabi2['v3']
rabim_tap <- rabi2['tap']
rabim_beds <- rabi2['beds']

plot(rabim_index[,1],residuals,col="blue",cex = 0.75, pch = 16, 
     xlab = "yield index", ylab = "u hat",main = "u hat and yield index representaion(Rabi)")

plot(rabim_v37[,1],predict(model2),col="blue",cex = 0.75, pch = 16, 
     xlab = "actual v37", ylab = "predicted v37",main = "actual and predicted values of v37 representaion(Rabi)")

sum(residuals)                #1.328324e-11
hist(residuals[residuals>-15], xlab = "residual", main = "Residual(Rabi)")
sum(temp<-residuals*rabim_v38)      #-1.679314e-10
hist(temp[temp>-200], xlab = "uihat*v38", main = "Residual(Rabi)")
sum(temp<-residuals*rabim_v36)      #-5.879297e-12
hist(temp[temp>-500], xlab = "uihat*v36", main = "Residual(Rabi)")
sum(temp<-residuals*rabim_v17)      #1.218844e-10
hist(temp[temp>-200], xlab = "uihat*v17", main = "Residual(Rabi)")
sum(temp<-residuals*rabim_v3)       #-5.439597e-07
hist(temp[temp>-5e+05], xlab = "uihat*v3", main = "Residual(Rabi)")
sum(temp<-residuals*rabim_beds)     #1.973749e-06
hist(temp[temp>-5e+05], xlab = "uihat*beds", main = "Residual(Rabi)")
sum(temp<-residuals*rabim_tap)      #3.674158e-10
hist(temp[temp>-200], xlab = "uihat*tap", main = "Residual(Rabi)")