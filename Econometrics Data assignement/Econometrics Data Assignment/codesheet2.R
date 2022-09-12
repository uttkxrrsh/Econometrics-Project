data <- read.csv(file = "main.csv")

library(ggplot2)

sepsis <- data$v40
lbw <- data$v42
pneumonia <- data$v43
diarrhea <- data$v44
fever <- data$v45
measles <- data$v46

# mean
mean(sepsis, na.rm = TRUE)
mean(lbw, na.rm = TRUE)
mean(pneumonia, na.rm = TRUE)
mean(diarrhea, na.rm = TRUE)
mean(fever, na.rm = TRUE)
mean(measles, na.rm = TRUE)

#median
median(sepsis, na.rm = TRUE)
median(lbw, na.rm = TRUE)
median(pneumonia, na.rm = TRUE)
median(diarrhea, na.rm = TRUE)
median(fever, na.rm = TRUE)
median(measles, na.rm = TRUE)

#standard deviation
sd(sepsis, na.rm = TRUE)
sd(lbw, na.rm = TRUE)
sd(pneumonia, na.rm = TRUE)
sd(diarrhea, na.rm = TRUE)
sd(fever, na.rm = TRUE)
sd(measles, na.rm = TRUE)

Mode <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}

#mode
Mode(sepsis)
Mode(lbw)
Mode(pneumonia)
Mode(diarrhea)
Mode(fever)
Mode(measles)

# Q2 B.
# sepsis graph
y11 <- subset(data, year == 2011)

y12 <- subset(data, year == 2012)

y13 <- subset(data, year == 2013)

y14 <- subset(data, year == 2014)

y15 <- subset(data, year == 2015)

y16 <- subset(data, year == 2016)

y11$y <- "2011"
y12$y <- "2012"
y13$y <- "2013"
y14$y <- "2014"
y15$y <- "2015"
y16$y <- "2016"

#binding grpah for every year
year_plot <- rbind(y11,y12,y13,y14,y15,y16)

# year-wise plot for every variable
ggplot(year_plot, aes(v40, fill = y)) + geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity') + stat_bin(bins = 30)
ggplot(year_plot, aes(v42, fill = y)) + geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity') + stat_bin(bins = 30)
ggplot(year_plot, aes(v43, fill = y)) + geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity') + stat_bin(bins = 30)
ggplot(year_plot, aes(v44, fill = y)) + geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity') + stat_bin(bins = 30)
ggplot(year_plot, aes(v45, fill = y)) + geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity') + stat_bin(bins = 30)
ggplot(year_plot, aes(v46, fill = y)) + geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity') + stat_bin(bins = 30)


sk <- subset(data, season == "Kharif")

sr <- subset(data, season == "Rabi")

ss <- subset(data, season == "Summer")

sw <- subset(data, season == "Whole Year")

#binding grpah for every season
season_plot <- rbind(sr,sk,ss, sw)

#season-wise plot of every variable
ggplot(season_plot, aes(v40, fill = season)) + geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity') + stat_bin(bins = 30)
ggplot(season_plot, aes(v42, fill = season)) + geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity') + stat_bin(bins = 30)
ggplot(season_plot, aes(v43, fill = season)) + geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity') + stat_bin(bins = 30)
ggplot(season_plot, aes(v44, fill = season)) + geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity') + stat_bin(bins = 30)
ggplot(season_plot, aes(v45, fill = season)) + geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity') + stat_bin(bins = 30)
ggplot(season_plot, aes(v46, fill = season)) + geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity') + stat_bin(bins = 30)



#Q2 D.-> 1.
gdp <- data$gdp
beds <- data$beds
tap <- data$tap

#finding correlation between number of infant deaths from sepsis and gdp, beds and tapwater access
cor(gdp,sepsis,use = "complete.obs")
cor(beds,sepsis,use = "complete.obs")
cor(tap,sepsis, use = "complete.obs")

#finding correlation between number of infant deaths from lbw and gdp, beds and tapwater access
cor(gdp,lbw,use = "complete.obs")
cor(beds,lbw,use = "complete.obs")
cor(tap,lbw, use = "complete.obs")

#finding correlation between %age of infant deaths from pneumonia and gdp, beds and tapwater access
cor(gdp,pneumonia,use = "complete.obs")
cor(beds,pneumonia,use = "complete.obs")
cor(tap,pneumonia, use = "complete.obs")

#finding correlation between %age of infant deaths from diarrhea and gdp, beds and tapwater access
cor(gdp,diarrhea,use = "complete.obs")
cor(beds,diarrhea,use = "complete.obs")
cor(tap,diarrhea, use = "complete.obs")

#finding correlation between %age of infant deaths from fever and gdp, beds and tapwater access
cor(gdp,fever,use = "complete.obs")
cor(beds,fever,use = "complete.obs")
cor(tap,fever, use = "complete.obs")

#finding correlation between %age of infant deaths from measles and gdp, beds and tapwater access
cor(gdp,measles,use = "complete.obs")
cor(beds,measles,use = "complete.obs")
cor(tap,measles, use = "complete.obs")

#Q2 D.-> 2.
#subset for each crop category
pulses <- subset(data, cropcategory == "Pulse")
cash <- subset(data, cropcategory == "Cash")
cereal <- subset(data, cropcategory == "Cereal")
ccereal <- subset(data, cropcategory == "Coarse Cereal")
horticulture <- subset(data, cropcategory == "Horticulture")
oilseed <- subset(data, cropcategory == "Oilseed")

#finding correlation between yield indices of Cash Crops and 6 DV's
cor(cash$v40,cash$index,use = "complete.obs")
cor(cash$v42,cash$index,use = "complete.obs")
cor(cash$v43,cash$index,use = "complete.obs")
cor(cash$v44,cash$index,use = "complete.obs")
cor(cash$v45,cash$index,use = "complete.obs")
cor(cash$v46,cash$index,use = "complete.obs")

#finding correlation between yield indices of Cerael Crops and 6 DV's
cor(cereal$v40,cereal$index,use = "complete.obs")
cor(cereal$v42,cereal$index,use = "complete.obs")
cor(cereal$v43,cereal$index,use = "complete.obs")
cor(cereal$v44,cereal$index,use = "complete.obs")
cor(cereal$v45,cereal$index,use = "complete.obs")
cor(cereal$v46,cereal$index,use = "complete.obs")

#finding correlation between yield indices of Cerael Crops and 6 DV's
cor(ccereal$v40,ccereal$index,use = "complete.obs")
cor(ccereal$v42,ccereal$index,use = "complete.obs")
cor(ccereal$v43,ccereal$index,use = "complete.obs")
cor(ccereal$v44,ccereal$index,use = "complete.obs")
cor(ccereal$v45,ccereal$index,use = "complete.obs")
cor(ccereal$v46,ccereal$index,use = "complete.obs")

#finding correlation between yield indices of Pulse Crops and 6 DV's
cor(pulses$v40,pulses$index,use = "complete.obs")
cor(pulses$v42,pulses$index,use = "complete.obs")
cor(pulses$v43,pulses$index,use = "complete.obs")
cor(pulses$v44,pulses$index,use = "complete.obs")
cor(pulses$v45,pulses$index,use = "complete.obs")
cor(pulses$v46,pulses$index,use = "complete.obs")

#finding correlation between yield indices of Horticulture Crops and 6 DV's
cor(horticulture$v40,horticulture$index,use = "complete.obs")
cor(horticulture$v42,horticulture$index,use = "complete.obs")
cor(horticulture$v43,horticulture$index,use = "complete.obs")
cor(horticulture$v44,horticulture$index,use = "complete.obs")
cor(horticulture$v45,horticulture$index,use = "complete.obs")
cor(horticulture$v46,horticulture$index,use = "complete.obs")

#finding correlation between yield indices of Oilseed Crops and 6 DV's
cor(oilseed$v40,oilseed$index,use = "complete.obs")
cor(oilseed$v42,oilseed$index,use = "complete.obs")
cor(oilseed$v43,oilseed$index,use = "complete.obs")
cor(oilseed$v44,oilseed$index,use = "complete.obs")
cor(oilseed$v45,oilseed$index,use = "complete.obs")
cor(oilseed$v46,oilseed$index,use = "complete.obs")

#Q2 D.-> 3.
#find yield index growth rate
#filling NA for all years for which no previous year is avaialable
data$yield_index_g <- NA

for(i in 2:70572){
  temp<-data[i, 65]
  tempprev<-data[i-1, 65]
  if((substr(temp,1,nchar(temp)-4) == substr(tempprev,1,nchar(tempprev)-4))&&(data[i-1,69]!=0)&&(!is.na(data[i-1,69]))){
    data[i,73] = (data[i,69] - data[i-1,69])/data[i-1, 69]
  }
  else{
    data[i,73] = NA
  }
}

#subset for each crop category
pulses <- subset(data, cropcategory == "Pulse")
cash <- subset(data, cropcategory == "Cash")
cereal <- subset(data, cropcategory == "Cereal")
ccereal <- subset(data, cropcategory == "Coarse Cereal")
horticulture <- subset(data, cropcategory == "Horticulture")
oilseed <- subset(data, cropcategory == "Oilseed")

#Correlation between yield index growth rate of Cash Crops and 6 DV's 
cor(cash$v40, cash$yield_index_g, use = "complete.obs")
cor(cash$v42, cash$yield_index_g, use = "complete.obs")
cor(cash$v43, cash$yield_index_g, use = "complete.obs")
cor(cash$v44, cash$yield_index_g, use = "complete.obs")
cor(cash$v45, cash$yield_index_g, use = "complete.obs")
cor(cash$v46, cash$yield_index_g, use = "complete.obs")

#Correlation between yield index growth rate of Cereal Crops and 6 DV's
cor(cereal$v40, cereal$yield_index_g, use = "complete.obs")
cor(cereal$v42, cereal$yield_index_g, use = "complete.obs")
cor(cereal$v43, cereal$yield_index_g, use = "complete.obs")
cor(cereal$v44, cereal$yield_index_g, use = "complete.obs")
cor(cereal$v45, cereal$yield_index_g, use = "complete.obs")
cor(cereal$v46, cereal$yield_index_g, use = "complete.obs")

#Correlation between yield index growth rate of Coarse Cereal Crops and 6 DV's
cor(ccereal$v40, ccereal$yield_index_g, use = "complete.obs")
cor(ccereal$v42, ccereal$yield_index_g, use = "complete.obs")
cor(ccereal$v43, ccereal$yield_index_g, use = "complete.obs")
cor(ccereal$v44, ccereal$yield_index_g, use = "complete.obs")
cor(ccereal$v45, ccereal$yield_index_g, use = "complete.obs")
cor(ccereal$v46, ccereal$yield_index_g, use = "complete.obs")

#Correlation between yield index growth rate of Pulse Crops and 6 DV's
cor(pulses$v40, pulses$yield_index_g, use = "complete.obs")
cor(pulses$v42, pulses$yield_index_g, use = "complete.obs")
cor(pulses$v43, pulses$yield_index_g, use = "complete.obs")
cor(pulses$v44, pulses$yield_index_g, use = "complete.obs")
cor(pulses$v45, pulses$yield_index_g, use = "complete.obs")
cor(pulses$v46, pulses$yield_index_g, use = "complete.obs")

#Correlation between yield index growth rate of Horticuture Crops and 6 DV's
cor(oilseed$v40, oilseed$yield_index_g, use = "complete.obs")
cor(oilseed$v42, oilseed$yield_index_g, use = "complete.obs")
cor(oilseed$v43, oilseed$yield_index_g, use = "complete.obs")
cor(oilseed$v44, oilseed$yield_index_g, use = "complete.obs")
cor(oilseed$v45, oilseed$yield_index_g, use = "complete.obs")
cor(oilseed$v46, oilseed$yield_index_g, use = "complete.obs")
