#sensor depth

##1) Import data
#2018 florence depth  20/11/18 10:30 am 4.2 measured depth, LAT 1.766 m
setwd("C:/Users/gricardo/OneDrive - Australian Institute of Marine Science/3 Results/6 Post-settlement/2 2018/2018 field/2 water quality")
data1 <- read.table(file="https://raw.githubusercontent.com/gerard-ricardo/data/master/2018%20imo%20depth", header= TRUE,dec=",", na.strings=c("",".","NA"))
head(data1)
options(scipen = 999)  # turn off scientific notation

#2017 Picnic Bay 20/12/2017
#setwd("C:/Users/gricardo/OneDrive - Australian Institute of Marine Science/3 Results/6 Post-settlement/2 2018/2018 field/2 water quality")
data1 <- read.table(file="https://raw.githubusercontent.com/gerard-ricardo/data/master/2017%20picnic%20bay%20depth", header= TRUE,dec=",", na.strings=c("",".","NA"))
head(data1)
options(scipen = 999)  # turn off scientific notation

###############################################


####2)Organising and wrangling####
str(data1) #check data type is correct
nrow(data1)
data1$depth <- as.numeric(as.character(data1$depth))
data1$obs <- factor(formatC(1:nrow(data1), flag="0", width = 3))# unique tank ID for later on
data1 = data1[complete.cases(data1), ]  #make sure import matches NA type
str(data1)
data1 = filter(data1, depth < 7)  #remove anomalies
data1 = filter(data1, depth > 2)  #remove anomalies


#######3)Data exploration####
##Visualize data - plot data split at every factor
library(ggplot2)
dev.off()
source("https://raw.githubusercontent.com/gerard-ricardo/data/master/theme_sleek1")
p0 = ggplot()+geom_point(data1, mapping = aes(x = obs, y = depth),position = position_jitter(width = .02), alpha = 0.50,size = 3 )+theme_sleek1()
#p0 = p0 + facet_wrap(~time+spec)+scale_x_log10(name ="XXXX")#+geom_smooth(data1, mapping = aes(x = raw.x, y = suc/tot))
#p0= p0+ scale_y_continuous( limits = c(0, 1)) 
p0

quantile(data1$depth)
