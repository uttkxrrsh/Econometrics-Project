#Q3.
# part(A)
parta <- lm(v40~gdp+beds+tap,data)
summary(parta)

# part(B)
partbpulses <- lm(v40~gdp+beds+tap+index,pulses)
summary(partbpulses)
partbcash  <- lm(v40~gdp+beds+tap+index,cash)
summary(partbcash)
partbcereal <- lm(v40~gdp+beds+tap+index,cereal)
summary(partbcereal)
partbccereal <- lm(v40~gdp+beds+tap+index,ccereal)
summary(partbccereal)
partbhort <- lm(v40~gdp+beds+tap+index,horticulture)
summary(partbhort)
partbboil <- lm(v40~gdp+beds+tap+index,oilseed)
summary(partbboil)

# part(C)
data$icash <- 0
data$icereal <- 0
data$iccereal <- 0
data$ihort <- 0
data$ioil <- 0
data$ipulses <- 0

data <- transform(
  data, icash= ifelse(cropcategory == "Cash", index, 0))
data <- transform(
  data, icereal= ifelse(cropcategory == "Cereal", index, 0))
data <- transform(
  data, iccereal= ifelse(cropcategory == "Coarse Cereal", index, 0))
data <- transform(
  data, ihort= ifelse(cropcategory == "Horticulture", index, 0))
data <- transform(
  data, ioil= ifelse(cropcategory == "Oilseed", index, 0))
data <- transform(
  data, ipulses= ifelse(cropcategory == "Pulse", index, 0))

partc <- lm(v40~gdp+beds+tap+icereal+iccereal+icash+ioil+ihort+ipulses,data)
summary(partc)

#part(D)
partdpulses <- lm(v40~gdp+beds+tap+yield_index_g,pulses)
summary(partdpulses)
partdcash  <- lm(v40~gdp+beds+tap+yield_index_g,cash)
summary(partdcash)
partdcereal <- lm(v40~gdp+beds+tap+yield_index_g,cereal)
summary(partdcereal)
partdccereal <- lm(v40~gdp+beds+tap+yield_index_g,ccereal)
summary(partdccereal)
partdhort <- lm(v40~gdp+beds+tap+yield_index_g,horticulture)
summary(partdhort)
partdoil <- lm(v40~gdp+beds+tap+yield_index_g,oilseed)
summary(partdoil)

#part(E)
data$igcash <- 0
data$igcereal <- 0
data$igccereal <- 0
data$ighort <- 0
data$igoil <- 0
data$igpulses <- 0

data <- transform(
  data, igcash= ifelse(cropcategory == "Cash", index, 0))
data <- transform(
  data, igcereal= ifelse(cropcategory == "Cereal", index, 0))
data <- transform(
  data, igccereal= ifelse(cropcategory == "Coarse Cereal", index, 0))
data <- transform(
  data, ighort= ifelse(cropcategory == "Horticulture", index, 0))
data <- transform(
  data, igoil= ifelse(cropcategory == "Oilseed", index, 0))
data <- transform(
  data, igpulses= ifelse(cropcategory == "Pulse", index, 0))

parte <- lm(v40~gdp+beds+tap+igcereal+igccereal+igcash+igoil+ighort+igpulses,data)
summary(parte)

#part(F)
#adding constant 1 to every entry of the table used for linear regression
print(0 %in% data$gdp) #no 0 in gdpdata
print(0 %in% data$beds) #no 0 in bedsdata
print(0 %in% data$tap) #0 exists in tap data
print(0 %in% data$index) #0 exists in index data

partfpulses <- lm(v40~log(gdp)+log(beds)+log(tap+1)+log(index+1),pulses)
summary(partfpulses)
partfcash  <- lm(v40~log(gdp)+log(beds)+log(tap+1)+log(index+1),cash)
summary(partfcash)
partfcereal <- lm(v40~log(gdp)+log(beds)+log(tap+1)+log(index+1),cereal)
summary(partfcereal)
partfccereal <- lm(v40~log(gdp)+log(beds)+log(tap+1)+log(index+1),ccereal)
summary(partfccereal)
partfhort <- lm(v40~log(gdp)+log(beds)+log(tap+1)+log(index+1),horticulture)
summary(partfhort)
partfoil <- lm(v40~log(gdp)+log(beds)+log(tap+1)+log(index+1),oilseed)
summary(partfoil)

#part(G)
print(0 %in% data$gdp) #no 0 in gdpdata
print(0 %in% data$beds) #no 0 in bedsdata
print(0 %in% data$tap) #0 exists in tap data
print(0 %in% data$index) #0 exists in index data

partg <- lm(v40~log(gdp)+log(beds)+log(tap+1)
            +log(icereal+1)+log(iccereal+1)+log(icash+1)+log(ioil+1)
            +log(ihort+1)+log(ipulses+1),data)
summary(partg)

