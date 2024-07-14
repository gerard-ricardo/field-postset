#Correlation between nut, max spectra, irradiance,a and deposited sediment. Needs lots of work

# better removal of outliers/sensor malfunction
# better estimation of modal spectra
# dep sed added
# dli better than irr because of darkness

#Data improt
#setwd("C:/Users/g_ric/OneDrive/1 Work/4 Writing/1 Ricardo et al  - Modelling multiple ELHS sed scenario/field-work")
data1 <- read.table(file="./data/sed stressors corr.txt", header= TRUE,dec=",", na.strings=c("", "na"), fill = TRUE) 
head(data1)

# 2 Labelling and wrangling -----------------------------------------------
data1$date.time <- as.numeric(as.character(data1$date.time))
data1$ntu <- as.numeric(as.character(data1$ntu))
data1$max.spectra <- as.numeric(as.character(data1$max.spectra))  #okay this doesn't work, need to get acutal column value
data1$sum.irr <- as.numeric(as.character(data1$sum.irr))
str(data1)
#plot(data1$date.time, data1$ntu)

library(dplyr)
df = filter(data1, date.time > 43434) #trim to the nearest complete day start for MR
df = filter(df, date.time < 43470) #trimmed to the nearest whote day bottom for MR
df = filter(df, ntu < 2000) #trimmed to the nearest whote day bottom for MR
head(df)
tail(df)

df.scaled = data.frame(ntu = scale(df$ntu))
df.scaled$spectra = scale(df$max.spectra)
df.scaled$irr = scale(df$sum.irr)
head(df.scaled)

# 7 Plotting --------------------------------------------------------------
ggplot()+
  geom_point(df.scaled, mapping = aes(x = ntu, y = irr))
  
