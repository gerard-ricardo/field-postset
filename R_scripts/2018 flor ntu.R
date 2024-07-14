#2018 ntu flor


#libraries
library(ggplot2)
library(dplyr)
library(plotly)
library(gridExtra)


## Data import
#setwd("C:/Users/gricardo/OneDrive - Australian Institute of Marine Science/3 Results/6 Post-settlement/2 2018/2018 field/2 water quality")
data2 <- read.table("https://raw.githubusercontent.com/gerard-ricardo/data/master/2018%20ntu%20flor", sep="\t", header = TRUE,  comment.char = "",check.names = FALSE, quote="", na.strings=c("NA","NaN", " ") )
head(data2)


# 2 Labelling and wrangling -----------------------------------------------
colnames(data2) <- c("date", "time", "date.time", "ntu")   #columns. Can use index to change indic column
data2$date.time <- as.numeric(as.character(data2$date.time))
data2$date <- as.numeric(as.character(data2$date))
data2$time <- as.numeric(as.character(data2$time))
data2$ntu <- as.numeric(as.character(data2$ntu))
# data2$X455nm <- as.numeric(as.character(data2$X455nm))
# data2$X485nm <- as.numeric(as.character(data2$X485nm))
# data2$X515nm <- as.numeric(as.character(data2$X515nm))
# data2$X555nm <- as.numeric(as.character(data2$X555nm))
# data2$X615nm <- as.numeric(as.character(data2$X615nm))
# data2$X660nm <- as.numeric(as.character(data2$X660nm))
# data2$X695nm <- as.numeric(as.character(data2$X695nm))
#data2$date <- as.factor(data2$date)
str(data2)

data2 = data2[complete.cases(data2), ]
str(data2)
data2 = dplyr::filter(data2, date.time > 43424) 
data2 = filter(data2, date.time < 43489)

data2 = data2[complete.cases(data2), ]
head(data2)
options(scipen = 99)  #display x no of digits


#Data cleaning
data2 <- data2[order(data2$date.time),]
tail(data2)
data2 = dplyr::filter(data2, date.time > 43422) #trim away error at start. First real reading but not deployment
data2 = dplyr::filter(data2, date.time < 43489) #trim away error at end
data2 = dplyr::filter(data2, ntu < 500) #trim away error at end
#data2$date
data2$date <- factor(data2$date)
unique(data2$date)   #66 days


#remove negs
data2.neg = data2[which(data2[,4]<0),]  #all neg to 0, 16
data2 = data2 %>% anti_join(data2.neg)  #subtracts one datafrma efrom another
range(data2$ntu)

#removing 2.5 proceeding value above 3ntu
head(data2)
data2$lag = lag(data2$ntu)
data2 = data2[complete.cases(data2), ]  #remove the na
data2$ntu.dif=data2$ntu - data2$lag   #diff
data2$ntu.dif=sqrt(data2$ntu.dif^2)  #overright removing neg numbers
data2$dif.2.5=data2$ntu.dif/data2$ntu  #overright removing neg numbers
data2$lead = lead(data2$dif.2.5)
# head(data2, 100)
# range(data2$ntu.dif)
# hist(data2$ntu.dif, breaks = 100)
data4 = data2[which(data2$lead>2.5),]   #a diffrence on great than 10 ntu between readings. 10 radings
data5 = data4[which(data4$ntu>3),]   #a diffrence on great than 10 ntu between readings. 10 radings
data6 = data2 %>% anti_join(data5)  #subtracts one datafrma efrom another
hist(data6$ntu)
data2 = data6
quantile(data2$ntu)
ecdf = ecdf(data2$ntu)
ecdf(9)
data2$SS = data2$ntu * 1.1  #cf
quantile(data2$SS, probs = seq(0, 1, 0.20))

data3 = select (data2,c(date.time,ntu,SS))  
#write.csv(data3,'2018_ntu.csv')

p1 = ggplot() + geom_point(data = data2, aes(x = date.time, y = ntu, alpha = 0.2), color = 'steelblue')
p1 = p1 + scale_y_continuous(name ="NTU", limits = c(0, 50))
p1

#p1 <- ggplotly(p1)
#p1  #   theme(legend.position="none")

# #Observe data
# library(tidyr)
# head(data2.long)





########################################################################

#aligning time intevals (bins) by deci
# deci.f = 100
# data2$dec.d = floor((data2$date.time*deci.f))/deci.f
# data2$dec.d <- as.factor(data2$dec.d)
# data2.1 = data2 %>% group_by(dec.d) %>% summarise(ntu = mean(ntu))
# data3.1 <-  inner_join(data1.1, data2.1, by = 'dec.d')   #blue/green (see 2018 ms8) to ntu on same time scale
# data3.1$dec.d <- as.numeric(as.character(data3.1$dec.d))
# ggplot() + geom_point(data = data3.1, aes(x = dec.d, y = ntu, alpha = 0.2), color = 'steelblue')+
#   theme(legend.position="none")
# ggplot() + geom_point(data = data3.1, aes(x = sqrt(estimate.x), y = sqrt(estimate.y), alpha = 0.2), color = 'steelblue')+
# theme(legend.position="none")#+ scale_y_continuous( limits = c(0, 100))
# summary(lm(sqrt(data3.1$estimate.y)~sqrt(data3.1$estimate.x)))
# 
# data2.wide <- data2.long %>%pivot_wider(names_from = class, values_from = irrad, names_prefix = "")  #year goes to columns, their areas go as the values, area is the prefix
# data2 = data2.wide
# options(tibble.print_max = 50, tibble.print_min = 50)
# head(data2, 100)
# data2 = data2[complete.cases(data2), ]
# 
# 
# p2 = ggplot() + geom_point(data = data2, aes(x = date.time, y = ntu, alpha = 0.2), color = 'steelblue')+ theme(legend.position="none")+
#   scale_y_continuous( limits = c(0, 100))
# p2

#######################
#animatiion
# data7 = data2
# str(data7)
# data7$date <- as.numeric(as.character(data7$date))
# #data3.1$dec.d <- as.numeric(as.character(data3.1$dec.d))
# data7$day = data7$date-43423
# data7 = filter(data7, ntu < 50) #trim away all anomolies to avoid bias in line spikes
# data7$day = data7$date.time - 43423
# p3 = ggplot(data = data7, aes(x = day, y = ntu, alpha = 0.2)) + geom_line(color = 'grey70')+ theme(legend.position="none")+
#   scale_y_continuous( limits = c(0, 50))
# p3 = p3+ labs(x=expression(Day),
#               y=expression(NTU))
# #p3= p3+ scale_x_continuous(labels=c("6am", "10am",'2pm','6pm'))
# p3 = p3 + geom_point(size = 3, color = 'steelblue')+transition_reveal(day) 
# a_gif <- animate(p3, width = 240, height = 240)


#######################################
grid.arrange(p1, p2, nrow = 2)


############################
