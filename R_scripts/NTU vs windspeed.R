#imo sensors 2017-2018 field +windspeed

#icx=seq(0.1,157, 0.1)  #Threshold value = 40 (12 - 112)
#icx = c(40, 12, 112)
#k=2 # 1 hr = 1 interval

# 1 Import data -----------------------------------------------------------
#Site  Florence turned ooff because deep
#setwd("C:/Users/g_ric/OneDrive/1 Work/4 Writing/1 Ricardo et al  - Modelling multiple ELHS sed scenario/field-work")
data1 <- read.table(file="florence ntu 2017.txt", header= TRUE,dec=",")
head(data1)

#packages
library(zoo)
library(caTools)
library(gridExtra)
options(scipen = 999)  # turn off scientific notation
options(digits=13)

#Remove cyclones from all
#exclude <- c(40977:40986,41270:41275, 41279:41289,41633:41640,42071:42077, 42121:42125)
#library(dplyr)
#data1 <- data1 %>% filter(!(date %in% exclude))#head(data1)
#data1[complete.cases(data1),]

# 2 Labelling and wrangling -----------------------------------------------
data1$date.time <- as.numeric(as.character(data1$date.time))
data1$NTU <- as.numeric(as.character(data1$NTU))
str(data1)
data1 = data1[complete.cases(data1), ]
head(data1)
data1 = filter(data1, date.time > 43089) #trim to the nearest complete day start for MR
data1 = filter(data1, date.time < 43145)
tail(data1)
str(data1)
plot(data1$NTU)
data1.neg = data1[which(data1[,2]<0),]  #all neg to 0, 16
library(dplyr)
data1 = data1 %>% anti_join(data1.neg)  #subtracts one datafrma efrom another
range(data1$NTU)

#Conversion factor
#CC = 1.07
#data1$SS = data1$NTU*CC
mean(data1$NTU)
# min(data1$NTU)
# quantile(data1$NTU, probs = c(0, 0.25, 0.5, 0.75,0.81 ,.95), na.rm=T) # quartile
# ecdf1 = stats::ecdf(data1$NTU)
# ecdf1(0.8)*100
quantile(data1$NTU, probs = c(0, 0.25, 0.5, 0.75,0.9 ,.95), na.rm=T) # quartile
ecdf2 = stats::ecdf(data1$NTU)
ecdf2(9.4)*100   #in ntu, out percenntile

#Plot peaks of all sites
library(ggplot2)
library(dplyr)
source("https://raw.githubusercontent.com/gerard-ricardo/data/master/theme_sleek1")  #set theme in code
ggplot(NULL)+ geom_line(data=data1, aes(x = date.time, y = NTU)) + scale_x_continuous(limits=c(43089,43145))

#max(data1$date)
par(mfrow = c(2, 1))
head(data1)

# 7 Plotting --------------------------------------------------------------
p1 =ggplot()
p1 = p1+geom_line(data=data1, aes(x = date.time, y = NTU, color = 'mean'))
p1 = p1+ scale_x_continuous(name ="Date", limits=c(43089,43145), breaks=c(43089, 43099, 43110, 43120, 43130, 43141), labels=c("20/12/2017", "30/12/2017", "10/01/2018",'20/01/2018','30/01/2018','10/02/2018')) 
p1 = p1 + scale_color_manual( name = "NTU", labels = "mean", values = 'grey34')
p1 =  p1 + theme_sleek1()
p1 = p1 + theme(legend.position = c(0.93, 0.85))
p1

##############################################################################
#windspeed
# 1 Import data wind -----------------------------------------------------------
#setwd("C:/Users/gricardo/OneDrive - Australian Institute of Marine Science/R/imo sensors")
windspeed <- read.table(file="windspeed 2017.txt", header= TRUE,dec=",", na.strings=c("")) 
head(windspeed)

# 2 Labelling and wrangling -----------------------------------------------
windspeed$date.time2 <- as.numeric(as.character(windspeed$date.time2))
windspeed$wind.speed <- as.numeric(as.character(windspeed$wind.speed))
windspeed$wind.max <- as.numeric(as.character(windspeed$wind.max))
windspeed = windspeed[complete.cases(windspeed), ]
tail(windspeed)
windspeed = filter(windspeed, date.time2 > 43089) #trim to the nearest complete day start for MR
windspeed = filter(windspeed, date.time2 < 43145)

# 7 Plotting --------------------------------------------------------------
# library(RColorBrewer)
# display.brewer.all() 
# #par.frow(1,1)
# brewer.pal(n = 8, name = "Purples")
p2 = ggplot(NULL)
p2 = p2+ geom_line(data=windspeed, aes(x = date.time2, y = wind.speed, color = 'mean')) 
p2 = p2+ scale_x_continuous(name ="Date", limits=c(43089,43145), breaks=c(43089, 43099, 43110, 43120, 43130, 43141), labels=c("20/12/2017", "30/12/2017", "10/01/2018",'20/01/2018','30/01/2018','10/02/2018')) 
p2 = p2 + geom_line(data=windspeed, aes(x = date.time2, y = wind.max, color = 'max'))+scale_y_continuous(name ="Wind speed (km/h)")
p2 = p2 + scale_color_manual( name = "Wind speed", labels = c("max", 'mean'), values = c('#BCBDDC','#807DBA'))  #from brewerpal output purples
p2 = p2+ theme_sleek1()
p2 = p2 + theme(legend.position = c(0.93, 0.85))
p2

#grid.arrange(p1, p2, nrow = 2)
require(cowplot)
plot_grid(p1, p2, labels = "AUTO", ncol = 1, align = 'v')
# setEPS()
# postscript("whatever.eps")
# png("florence ntu.png", width =600)
# plot_grid(p1, p2, labels = "AUTO", ncol = 1, align = 'v')
# dev.off()

########Picnic###################################################################
data2 <- read.table(file="picnic 2017.txt", header= TRUE,dec=",") 
tail(data2)
colnames(data2)[2] <- c('NTU')
head(data2)

# 2 Labelling and wrangling -----------------------------------------------
data2$date.time <- as.numeric(as.character(data2$date.time))
data2$NTU <- as.numeric(as.character(data2$NTU))
str(data2)
data2 = data2[complete.cases(data2), ]
head(data2)
data2 = filter(data2, date.time > 43089) #trim to the nearest complete day start for MR
data2 = filter(data2, date.time < 43145)
tail(data2)
str(data2)
plot(data2$NTU)
data2.neg = data2[which(data2[,2]<0),]  #all neg to 0, 16
library(dplyr)
data2 = data2 %>% anti_join(data2.neg)  #subtracts one datafrma efrom another
range(data2$NTU)

p3 =ggplot()
p3 = p3+ geom_line(data=data2, aes(x = date.time, y = NTU, color = 'mean'))
p3 = p3+ scale_x_continuous(name ="Date", limits=c(43089,43145), breaks=c(43089, 43099, 43110, 43120, 43130, 43141), labels=c("20/12/2017", "30/12/2017", "10/01/2018",'20/01/2018','30/01/2018','10/02/2018')) 
p3 = p3 + scale_color_manual( name = "NTU", labels = "mean", values = 'grey34')
p3 =  p3 + theme_sleek1()
p3 = p3 + theme(legend.position = c(0.93, 0.85))
p3

####
grid.arrange(p1, p2, nrow = 2)
#plot_grid(p3, p2, labels = "AUTO", ncol = 1, align = 'v')
# pdf("picnic ntu.pdf",width=10)
# plot_grid(p3, p2, labels = "AUTO", ncol = 1, align = 'v')
# # mtext(side=1,"Depth.SSC",line=2.5)
# # mtext(side=2,"Number per 1000 fecund coral polyps",line=2.5)
# dev.off()

######panel##############################
#All 2017 plots
#png("all ntu wind.png", width =1000, height = 750)
#plot_grid(p1,p3, p2, labels = "AUTO", ncol = 1, align = 'v')
grid.arrange(p1, p3, p2, nrow = 3)
#dev.off()

######Windspeed to NTU corr by day####################################
#Florence
min.date = ceiling(min(data1$date.time))
max.date = floor(max(data1$date.time))
data1.trim = filter(data1, date.time >= min.date) #trim to the nearest complete day start 
data1.trim = filter(data1, date.time < max.date) #trimmed to the nearest whote day bottom 
data1.trim$floor =  floor(data1.trim$date.time)  #round down to day interger
head(data1.trim, 40)
data1.d = data1.trim %>%
  group_by(floor) %>%
  summarise(estimate = mean(NTU)) %>%
  data.frame()
#Picnic
min.date = ceiling(min(data2$date.time))
max.date = floor(max(data2$date.time))
data2.trim = filter(data2, date.time >= min.date) #trim to the nearest complete day start 
data2.trim = filter(data2, date.time < max.date) #trimmed to the nearest whote day bottom 
data2.trim$floor =  floor(data2.trim$date.time)  #round down to day interger
head(data2.trim, 40)
data2.d = data2.trim %>%
  group_by(floor) %>%
  summarise(estimate = mean(NTU)) %>%
  data.frame()
#Windspeed
min.date = ceiling(min(windspeed$date.time2))
max.date = floor(max(windspeed$date.time2))
windspeed.trim = filter(windspeed, date.time2 >= min.date) #trim to the nearest complete day start 
windspeed.trim = filter(windspeed, date.time2 < max.date) #trimmed to the nearest whote day bottom 
windspeed.trim$floor =  floor(windspeed.trim$date.time2)  #round down to day interger
head(windspeed.trim, 40)
windspeed.d = windspeed.trim %>%
  group_by(floor) %>%
  summarise(estimate = mean(wind.speed)) %>%
  data.frame()
#Florence
md1 = glm(data1.d$estimate ~ windspeed.d$estimate, family = 'poisson')
summary(md1)
library(MASS)
md2 = glm.nb(data1.d$estimate ~ windspeed.d$estimate) 
md2
df.x <- data.frame(raw.x = seq(min(windspeed.d$estimate), max(windspeed.d$estimate), length = 100)) #setting up  new  data frame (df) defining log.x values to run 
vec.x =df.x[,1]
GLMfun = function(i){
  mm <- model.matrix(~raw.x, df.x)  # build model matrix 
  eta <- mm %*% coef(i)
  prediction  <- as.vector(exp(eta))
  se    <- sqrt(diag(mm %*% vcov(i) %*% t(mm)))
  upper  <- exp(eta + 1.96 *se) 
  lower  <- exp(eta - 1.96 *se)
  df = data.frame(vec.x, prediction, lower,upper)
  return(df)
}
df1=GLMfun(md2)
# plot(windspeed.d$estimate, data1.d$estimate)
# lines(df1$vec.x, df1$prediction)
# lines(df1$vec.x, df1$lower)
# lines(df1$vec.x, df1$upper)
#R squared approximations
1-(md1$deviance / md1$null) # 0.8160
p4 =ggplot()
p4 = p4 + geom_line(data = df1, aes(x = vec.x, y = prediction, color = 'mean'))
p4 = p4 + geom_point(aes(x = windspeed.d$estimate, y = data1.d$estimate, color = 'mean'))
p4 = p4 +  geom_ribbon( data = df1,aes(x = vec.x, ymin=lower, ymax=upper), fill="steelblue", alpha=0.2)
p4 = p4+ annotate(geom="text", x=15, y=70, label="r2 = 0.816",color="grey34", size = 10)
p4 = p4+ scale_x_continuous(name ="Windspeed (km/h)") 
p4 = p4+ scale_y_continuous(name ="NTU") 
p4 = p4 + scale_color_manual( name = "Wind speed", labels = c("mean", "mean", "bdb"), values = c('#BCBDDC','#BCBDDC','#BCBDDC' ))  #from brewerpal output purples
p4 =  p4 + theme_sleek1()
p4 = p4 + theme(legend.position = c(0.7, 0.85))
p4
#Picnic
md3 = glm.nb(data2.d$estimate ~ windspeed.d$estimate) 
summary(md3)
1-(md3$deviance / md3$null) # 0.772
df2=GLMfun(md3)
p5 =ggplot()
p5 = p5 + geom_line(data = df2, aes(x = vec.x, y = prediction, color = 'mean'))
p5 = p5 + geom_point(aes(x = windspeed.d$estimate, y = data2.d$estimate, color = 'mean'))
p5 = p5 +  geom_ribbon( data = df2,aes(x = vec.x, ymin=lower, ymax=upper), fill="steelblue", alpha=0.2)
p5 = p5+ annotate(geom="text", x=15, y=30, label="r2 = 0.772",color="grey34", size = 10)
p5 = p5+ scale_x_continuous(name ="Windspeed (km/h)") 
p5 = p5+ scale_y_continuous(name ="NTU") 
p5 = p5 + scale_color_manual( name = "Wind speed", labels = c("mean", "mean", "bdb"), values = c('#BCBDDC','#BCBDDC','#BCBDDC' ))  #from brewerpal output purples
p5 =  p5 + theme_sleek1()
p5 = p5 + theme(legend.position = c(0.7, 0.85))
p5
# png("wind.NTU.day corr.png", width =1000, height = 750)
# plot_grid(p4,p5, labels = "AUTO", ncol = 1, align = 'v')
# dev.off()


#######################Windspeed to NTU corr by hr####################################
#Florence
head(data1)
data1 = dplyr::arrange(data1, date.time)  #dplyr - use this. Allows multiple sorts i.e site then orient
min.date = ceiling(min(data1$date.time))
max.date = floor(max(data1$date.time))
data1.trim = filter(data1, date.time >= min.date) #trim to the nearest complete day start 
data1.trim = filter(data1, date.time < max.date) #trimmed to the nearest whote day bottom 
data1.trim$floor =  floor(data1.trim$date.time)  #round down to day interger
str(data1.trim)
data1.trim$hr.dec = data1.trim$date.time - data1.trim$floor  #decimal time
data1.trim$hr = floor(data1.trim$hr.dec*24)  #to hr and floored to integer
head(data1.trim, 40)
data1.d = data1.trim %>%
  group_by(hr) %>%
  summarise(estimate = mean(NTU)) %>%
  data.frame()
#####################################################################
# #imo sensors 2018-2019 field
# 
# #icx=seq(0.1,157, 0.1)  #Threshold value = 40 (12 - 112)
# #icx = c(40, 12, 112)
# #k=2 # 1 hr = 1 interval
# 
# #packages
# library(zoo)
# library(caTools)
# library(gridExtra)
# 
# options(scipen = 999)  # turn off scientific notation
# options(digits=5)
# 
# #Site
# setwd("C:/Users/gricardo/OneDrive - Australian Institute of Marine Science/R/imo sensors")
# data1 <- read.table(file="florence 2017.txt", header= TRUE,dec=",") 
# tail(data1)
# 
# data2 <- read.table(file="picnic 2017.txt", header= TRUE,dec=",") 
# tail(data2)
# 
# 
# #Remove cyclones from all
# #exclude <- c(40977:40986,41270:41275, 41279:41289,41633:41640,42071:42077, 42121:42125)
# #library(dplyr)
# #data1 <- data1 %>% filter(!(date %in% exclude))#head(data1)
# #data1[complete.cases(data1),]
# 
# #Data organisation
# data1$date.time <- as.numeric(as.character(data1$date.time))
# data1$NTU <- as.numeric(as.character(data1$NTU))
# data1[complete.cases(data1), ]
# 
# data2$date.time3 <- as.numeric(as.character(data2$date.time3))
# data2$NTU <- as.numeric(as.character(data2$NTU))
# data2[complete.cases(data2), ]
# head(data2)
# plot(data2$NTU)
# 
# #Conversion factor
# #CC = 1.07
# #data1$SS = data1$NTU*CC
# mean(data2$NTU)
# quantile(data1$NTU, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm=T) # quartile
# 
# #Plot peaks of all sites
# library(ggplot2)
# library(dplyr)
# ggplot(NULL)+ geom_line(data=data2, aes(x = date.time, y = NTU)) + scale_x_continuous(limits=c(43089,43145))
# 
# #max(data1$date)
# par(mfrow = c(2, 1))


##############################################################################
#2018 windspeed

setwd("C:/Users/gricardo/OneDrive - Australian Institute of Marine Science/3 Results/6 Post-settlement/1 2018/2018 field/AIMS data")
windspeed <- read.table(file="2018 windspeed.txt", header= TRUE,dec=",", na.strings=c("")) 
tail(windspeed)
windspeed[complete.cases(windspeed), ]

windspeed$date.time2 <- as.numeric(as.character(windspeed$date.time2))
windspeed$wind.speed <- as.numeric(as.character(windspeed$wind.speed))
windspeed$wind.max <- as.numeric(as.character(windspeed$wind.max))
windspeed$mean.kots = windspeed$wind.speed/1.852

head(windspeed)
quantile(windspeed$wind.speed, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm=T) # quartile
quantile(windspeed$mean.kots, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm=T) # quartile
# p1 =ggplot()+ 
#   geom_line(data=data1, aes(x = date.time, y = NTU, color = 'mean')) + 
#   scale_x_continuous(name ="Date", limits=c(43424,43522)  ) + 
#   scale_color_manual( name = "NTU", labels = "mean", values = 'black')
# 
# p1   #breaks=c(43089, 43099, 43110, 43120, 43130, 43141),labels=c("20/12/2017", "30/12/2017", "10/01/2018",'20/01/2018','30/01/2018','10/02/2018')

#note last line is important


p2 = ggplot(NULL)
p2 = p2+ geom_line(data=windspeed, aes(x = date.time2, y = wind.speed, color = 'mean')) 
p2 = p2+ scale_x_continuous(name ="Date", limits=c(43424,43522), breaks=c(43425, 43450, 43475, 43500, 43525), labels=c("21/11/2018", "16/12/2018", "10/01/2019", "4/02/2019", "1/03/2019")) 
p2 = p2 + geom_line(data=windspeed, aes(x = date.time2, y = wind.max, color = 'max'))+scale_y_continuous(name ="Wind speed (km/h)")
p2 = p2 + scale_color_manual( name = "Wind speed", labels = c("max", 'mean'), values = c('red','blue'))
p2 = p2 + theme_sleek1()
p2   #, breaks=c(43425, 43450, 43475, 43500, 43525), labels=c("21/11/2018	,16/12/2018,	10/01/2019,	4/02/2019,	1/03/2019')


grid.arrange(p1, p2, nrow = 2)
require(cowplot)
plot_grid(p1, p2, labels = "AUTO", ncol = 1, align = 'v')


##########################################################################


head(data2)

p1 =ggplot()+ 
  geom_line(data=data2, aes(x = date.time3, y = NTU, color = 'mean')) + 
  scale_x_continuous(name ="Date", limits=c(43089,43145), breaks=c(43089, 43099, 43110, 43120, 43130, 43141), labels=c("20/12/2017", "30/12/2017", "10/01/2018",'20/01/2018','30/01/2018','10/02/2018')) +
  scale_color_manual( name = "NTU", labels = "mean", values = 'black')
p1 = p1 + theme_bw()
p1 = p1 + theme(legend.position = c(0.9, 0.85))
p1

#note last line is important


p2 = ggplot(NULL)
p2 = p2+ geom_line(data=windspeed, aes(x = date.time2, y = wind.speed, color = 'mean')) 
p2 = p2+ scale_x_continuous(name ="Date", limits=c(43089,43145), breaks=c(43089, 43099, 43110, 43120, 43130, 43141), labels=c("20/12/2017", "30/12/2017", "10/01/2018",'20/01/2018','30/01/2018','10/02/2018')) 
p2 = p2 + geom_line(data=windspeed, aes(x = date.time2, y = wind.max, color = 'max'))+scale_y_continuous(name ="Wind speed (km/h)")
p2 = p2 + scale_color_manual(name = "Wind speed", labels = c("max", "mean"), values = c('red','blue'))
p2 = p2 + theme_bw()
p2 = p2 + theme(legend.position = c(0.9, 0.85))
p2

grid.arrange(p1, p2, nrow = 2)
require(cowplot)
plot_grid(p1, p2, labels = "AUTO", ncol = 1, align = 'v')






#########################################

