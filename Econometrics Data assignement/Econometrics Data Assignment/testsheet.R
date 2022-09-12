data_test <- data
data_test$yield_index_g <- 0
View(data_test)

data_test[1,75] = NA

for(i in 2:70572){
  temp<-data_test[i, 67]
  tempprev<-data_test[i-1, 67]
  if((substr(temp,1,nchar(temp)-4) == substr(tempprev,1,nchar(tempprev)-4))&&(data_test[i-1,71]!=0)&&(!is.na(data_test[i-1,71]))){
    data_test[i,75] = (data_test[i,71] - data_test[i-1,71])/data_test[i-1, 71]
  }
  else{
    data_test[i,75] = NA
  }
}

print((data_test[3,69] - data_test[2,69])/data_test[2, 69])
print(data[1,69])
print(data[2,75])