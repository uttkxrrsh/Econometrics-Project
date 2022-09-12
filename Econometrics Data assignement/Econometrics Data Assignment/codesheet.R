data <- read.csv(file = "main.csv")

data$gdp <- ''
data$beds <- ''
data$tap <- ''

gdpdata1 <- read.csv(file = "statewisedata.csv")
gdpdata2 <- read.csv(file = "statewisedata2.csv")
bedsdata <- read.csv(file = "2020beds.csv")
tapwater <- read.csv(file = "tapwater.csv")

for(i in 1:32){
  for(j in 1:70572){
    if(tolower(gdpdata1[i,2]) == tolower(data[j,4])){
      if(data[j,7] == 2007){
        data[j, 70] = gdpdata1[i, 3]
      }
      if(data[j,7] == 2008){
        data[j, 70] = gdpdata1[i, 4]
      }
      if(data[j,7] == 2009){
        data[j, 70] = gdpdata1[i, 5]
      }
      if(data[j,7] == 2010){
        data[j, 70] = gdpdata1[i, 6]
      }
      if(data[j,7] == 2011){
        data[j, 70] = gdpdata1[i, 7]
      }
    }
  }
}

write.csv(data, file = "main.csv")


for(i in 1:33){
  for(j in 1:70572){
    if(tolower(gdpdata1[i,2]) == tolower(data[j,4])){
      if(data[j,7] == 2012){
        data[j, 70] = gdpdata2[i, 3]
      }
      if(data[j,7] == 2013){
        data[j, 70] = gdpdata2[i, 4]
      }
      if(data[j,7] == 2014){
        data[j, 70] = gdpdata2[i, 5]
      }
      if(data[j,7] == 2015){
        data[j, 70] = gdpdata2[i, 6]
      }
      if(data[j,7] == 2016){
        data[j, 70] = gdpdata2[i, 7]
      }
      if(data[j,7] == 2017){
        data[j, 70] = gdpdata2[i, 8]
      }
      if(data[j,7] == 2018){
        data[j, 70] = gdpdata2[i, 9]
      }
      if(data[j,7] == 2019){
        data[j, 70] = gdpdata2[i, 10]
      }
    }
  }
}

for(i in 1:37){
  for(j in 1:70572){
    if(tolower(bedsdata[i,2]) == tolower(data[j,4])){
      data[j,71] = bedsdata[i,3]
    }
  }
}

for(i in 1:724){
  for(j in 1:70572){
    if(tolower(tapwater[i,2]) == tolower(data[j,6])){
      data[j,72] = tapwater[i,3]
    }
  }
}

data$beds <- as.numeric(gsub(",","",data$beds))

print(data)
write.csv(data, file = "main.csv")

print(data)
summary(data)
