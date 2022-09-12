data <- read.csv(file = "main.csv")

#saving all kharif data in "kharif variable"
kharif <- subset(data, season == "Kharif")
#saving all kharif data in "rabi variable"
rabi <- subset(data, season == "Rabi")

####################################Kharif######################################

kharif$dummy = 0

for(i in 1:nrow(kharif)){
  if(kharif$state[i] == "Madhya Pradesh" || kharif$state[i] == "Chhattisgarh"){
    kharif$dummy[i] = 1
  }
}

model3 <-lm(v37~v38+v36+v17+v3+tap+beds+dummy, kharif)
summary(model3)

kharif$dummy = 0
for(i in 1:nrow(kharif)){
  if(kharif$state[i] =="Himachal Pradesh"||kharif$state[i] =="Punjab"||kharif$state[i] =="Uttarakhand"|| kharif$state[i] =="Uttar Pradesh" || kharif$state[i] =="Haryana"){
    kharif$dummy[i] = 1
  }
}

model3 <-lm(v37~v38+v36+v17+v3+tap+beds+dummy, kharif)
summary(model3)


kharif$dummy = 0
for(i in 1:nrow(kharif)){
  if(kharif$state[i] =="Bihar"||kharif$state[i] =="Odisha"||kharif$state[i] =="Jharkhand"|| kharif$state[i] =="West Bengal" ){
    kharif$dummy[i] = 1
  }
}

model3 <-lm(v37~v38+v36+v17+v3+tap+beds+dummy, kharif)
summary(model3)


kharif$dummy = 0
for(i in 1:nrow(kharif)){
  if(kharif$state[i] =="Andhra Pradesh"||kharif$state[i] =="Telangana"||kharif$state[i] =="Karnataka"|| kharif$state[i] =="Kerala" || kharif$state[i] == "Tamil Nadu"){
    kharif$dummy[i] = 1
  }
}

model3 <-lm(v37~v38+v36+v17+v3+tap+beds+dummy, kharif)
summary(model3)

kharif$dummy = 0
for(i in 1:nrow(kharif)){
  if(kharif$state[i] =="Assam"||kharif$state[i] =="Sikkim"||kharif$state[i] =="Nagaland"|| kharif$state[i] =="Meghalaya" || kharif$state[i] == "Manipur" || kharif$state[i] == "Mizoram" || kharif$state[i] == "Tripura" || kharif$state[i]== "Arunachal Pradesh"){
    kharif$dummy[i] = 1
  }
}

model3 <-lm(v37~v38+v36+v17+v3+tap+beds+dummy, kharif)
summary(model3)

#################################Rabi########################################

