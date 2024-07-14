#2020 transect depths

##1) Import data
#setwd("C:/Users/gricardo/OneDrive - Australian Institute of Marine Science/4 Writing/1 See the light/see the light")
data1 <- read.table(file="https://raw.githubusercontent.com/gerard-ricardo/data/master/2020%20transect%20depths", header= TRUE,dec=",", na.strings=c("",".","NA"))
head(data1)
options(scipen = 999)  # turn off scientific notation


####2)Organising and wrangling####
str(data1) #check data type is correct
data1[] <- lapply(data1, function(x) as.numeric(as.character(x)))   #convert all to numeric


data2 = dplyr::select(data1,c(tape1.m., transect1.lat, transect2.lat, transect3.lat))  #remove column. Make sure have package on front 
library(tidyr)
data2_long = data2 %>% pivot_longer(-tape1.m.,  names_to = "transect" ,values_to = "depth")  #keep vec.x, add all other columns to factors , add all their values to meas)
data2_long$transect <- as.factor(as.character(data2_long$transect))
data2_long = data2_long[complete.cases(data2_long), ]  #make sure import matches NA type


#######3)Data exploration####
##Visualize data - plot data split at every factor
library(ggplot2)
source("https://raw.githubusercontent.com/gerard-ricardo/data/master/theme_sleek1")
p0 = ggplot()+geom_point(data2_long, mapping = aes(x = tape1.m., y = depth), alpha = 0.50, size = 3 )+theme_sleek1()
p0 = p0 + facet_wrap(~transect, nrow = 3)#+scale_x_log10(name ="XXXX")#+geom_smooth(data1, mapping = aes(x = raw.x, y = suc/tot))
p0 = p0 +  scale_y_reverse()
#p0= p0+ scale_y_continuous( limits = c(0, 1)) 
p0


####spline#####
p0 = ggplot()+geom_point(data2_long, mapping = aes(x = tape1.m., y = depth), alpha = 0.50, size = 3 )+theme_sleek1()
p0 = p0 + facet_wrap(~transect, nrow = 3)#+scale_x_log10(name ="XXXX")#+geom_smooth(data1, mapping = aes(x = raw.x, y = suc/tot))
p0 = p0 +  scale_y_reverse()
p0 = p0 + stat_smooth(data2_long, span =0.1, mapping = aes(x = tape1.m., y = depth))
#p0= p0+ scale_y_continuous( limits = c(0, 1)) 
p0

# str(data2_long)
# md1 <- loess(depth ~ tape1.m.*transect, data2_long)
# 
# data2_long %>% 
#   group_by(transect) %>% 
#   arrange(transect, tape1.m.) %>% 
#   mutate(Loess = predict(loess(depth ~ tape1.m., span = .1, data=.),
#     data.frame(tape1.m. = seq(min(tape1.m.), max(tape1.m.), 1))))
# 
# predict(cars.lo, data.frame(speed = seq(5, 30, 1)), se = TRUE)


pg <- ggplot_build(p0)  #extracts data from ggplot
data.loess = pg$data[[2]]
data.loess2 = data.frame(tape = data.loess$x, depth = -data.loess$y, transect = data.loess$PANEL)
#data.loess2_wide <- data.loess2 %>%pivot_wider(names_from = transect, values_from = depth , names_prefix = "transect")  #year goes to columns, their areas go as the values, area is the prefix
write.csv(data.loess2,'data.loess2.csv')

