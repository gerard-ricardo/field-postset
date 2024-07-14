
#Needs more investigation but mostly looks like noise to me!

####Data import 2018####


setwd("C:/Users/gricardo/OneDrive - Australian Institute of Marine Science/3 Results/10 Seeing the light")
data1<- read.table("2018 ms8 flor.txt", sep="\t", header = TRUE,  comment.char = "",check.names = FALSE, quote="", na.strings=c("NA","NaN", " ") )
data1 = filter(data1, date.time > 43424) 
data1 = filter(data1, date.time < 43489) 

#Data import 2019
# data1<- read.table("https://raw.githubusercontent.com/gerard-ricardo/data/master/2019%20ms8%20fl", header = TRUE,  comment.char = "",check.names = FALSE, quote="", na.strings=c("NA","NaN", " ") , fill = T)  #2019 IMO
# data1 = data1[complete.cases(data1), ]
# range(data1$date.time)
# data1 = filter(data1, date.time > 43661) #trim away error at start. First real reading but not deployment
# data1 = filter(data1, date.time < 43673) #trim away error at end


# labeling ----------------------------------------------------------------

tail(data1)
data1 = data1[complete.cases(data1), ]
str(data1)
range(data1$time)
hist(data1$time)

data1$date.time <- as.numeric(as.character(data1$date.time))
data1$date <- as.numeric(as.character(data1$date))
data1$time <- as.numeric(as.character(data1$time))
data1$X425nm <- as.numeric(as.character(data1$X425nm))
data1$X455nm <- as.numeric(as.character(data1$X455nm))
data1$X485nm <- as.numeric(as.character(data1$X485nm))
data1$X515nm <- as.numeric(as.character(data1$X515nm))
data1$X555nm <- as.numeric(as.character(data1$X555nm))
data1$X615nm <- as.numeric(as.character(data1$X615nm))
data1$X660nm <- as.numeric(as.character(data1$X660nm))
data1$X695nm <- as.numeric(as.character(data1$X695nm))
data1$date <- as.factor(data1$date)
str(data1)
data1 = data1[complete.cases(data1), ] %>% data.frame()
head(data1)
nrow(data1)
options(scipen = 999)  #display x no of digits


#Data wrangling
library(ggplot2)
# p1 = ggplot() + geom_point(data = data1, aes(x = date.time, y = X425nm, color = 'mean'))
# data1 = filter(data1, date.time > 43422) #trim away error at start. First real reading but not deployment
# data1 = filter(data1, date.time < 43489) #trim away error at end
# data1 = filter(data1, date.time > 43661) #trim away error at start. First real reading but not deployment
# data1 = filter(data1, date.time < 43673) #trim away error at end
data1$date <- factor(data1$date)
data1$blue.green = data1$X455nm/data1$X555nm  #add a blue:green row


# cleaning ----------------------------------------------------------------

#Clean anomalies (>500 and neg)
#Observe data
library(tidyr)
data1.long = gather(data1, class, irrad, X425nm:X695nm)
head(data1.long)
# p0 = ggplot()+geom_point(data1.long, mapping = aes(x = date.time, y = irrad))+facet_wrap(~class, nrow = 8)#+scale_x_log10(name ="dep sed")
# p0
library(dplyr)
data1.long = filter(data1.long, irrad < 500) #trim away error
# data1.long.neg = data1.long[which(data1.long$irrad<0),]  #all neg to 0, 16
# data1.long = data1.long %>% anti_join(data1.long.neg)  #subtracts one datafrma efrom another
head(data1.long)
range(data1.long$irrad)
data1.wide <- data1.long %>%pivot_wider(names_from = class, values_from = irrad, names_prefix = "")  #year goes to columns, their areas go as the values, area is the prefix
data1 = data1.wide
options(tibble.print_max = 50, tibble.print_min = 50)
data1 = data1[complete.cases(data1), ]
head(data1, 100)
# p0 = ggplot()+geom_point(data1.long, mapping = aes(x = date.time, y = irrad))+facet_wrap(~class, nrow = 8)#+scale_x_log10(name ="dep sed")
# p0

#######################################

# ##########################
# #get blue:green on timescale
# data1.long.2 = split(data1.long, data1.long$class)
# data1.bg = data1.long.2$blue.green
# deci.f = 100
# data1.bg$dec.d = floor((data1.bg$date.time*deci.f))/deci.f
# data1.bg$dec.d <- as.factor(data1.bg$dec.d)
# data1.1 = data1.bg %>% group_by(dec.d) %>% summarise(estimate = mean(irrad))
# 
# 
# 
# #ratio of blues to green
# data1.bg = data1
# data1.bg = data1.bg %>% data.frame()
# data1.bg$day = data1$date.time-43423
# data1.bg = filter(data1.bg, blue.green < 2) #trim for graph
# data1.bg = filter(data1.bg, time > 0.2) #removing darkness
# data1.bg = filter(data1.bg, time < 0.8) #removing darkness
# str(data1.bg)
# range(data1.bg$blue.green)
# data1.bg$blue.green = data1.bg$X455nm/data1.bg$X555nm
# # plot(data1.bg$date.time, data1.bg$X455nm/data1.bg$X555nm)
# p1 = ggplot(data1.bg, mapping = aes(x = day, y = blue.green, alpha = 0.1, color ='grey70'))+ geom_line(color = 'grey70')+ theme(legend.position="none")+ scale_y_continuous( limits = c(0, 1.5))
# # p1= p1+ scale_colour_gradientn(colours = heat(100))
# p1= p1+ scale_color_gradient(low="green", high="blue" ,trans = "sqrt") +theme_sleek()+ theme(legend.position="none")
# p1 = p1+ labs(x=expression('Day'),y=expression('Blue:Green ratio'))
# # p1
# # p1 = p1 + geom_point(data1.bg, mapping = aes(size = 3, color = blue.green))+transition_reveal(day)
# # c_gif <- animate(p1, width = 240, height = 240)
# #strong blue > 1, med >.75 - 1, low <0.75

##############################################
 

# range(data1$X555nm)
# p1
# p2 = ggplot() + geom_point(data = data1, aes(x = date.time, y = X555nm, color = 'mean'))
# data1 = filter(data1, X555nm < 500) #trim away error 
# p2
# p3 = ggplot() + geom_point(data = data1, aes(x = date.time, y = X455nm, color = 'mean'))
# p3
# data1 = filter(data1, X455nm < 2000) #trim away error 
# ####################################################################
# p4 = ggplot() + geom_point(data = data1, aes(x = date.time, y = X695nm, color = 'mean'))
# p4
# data1 = filter(data1, X695nm < 750) #trim away error 


# library(cowplot)
# plot_grid(p1, p2, labels = "AUTO", nrow = 2, align = 'v')



# irrad to PAR ------------------------------------------------------------

#1a) Convert  irradiance to PAR/nm with a ~60 bin
#  This might be 10x too high if you correct binning to 1 nm. Wojeich says it is calibrated
# PAR = Irradiance_simp*nm/con.fac    # Note: This website uses 11961.72 https://www.berthold-bio.com/service-support/support-portal/knowledge-base/how-do-i-convert-irradiance-into-photon-flux.html
df.simp = data.frame(data1$X425nm, data1$X455nm, data1$X485nm, data1$X515nm,data1$X555nm, data1$X615nm, data1$X660nm, data1$X695nm )
head(df.simp)
con.fac = 11973.78
df.simp2 = (data.frame( data1$X425nm*425, data1$X455nm*455, data1$X485nm*485, data1$X515nm*515,data1$X555nm*555, data1$X615nm*615, data1$X660nm*660, data1$X695nm*695))/con.fac
plot(df.simp2$data1.X555nm...555)
head(df.simp2)
range(df.simp2)
df.simp2 = df.simp2[complete.cases(df.simp2), ]
range(df.simp2)
str(df.simp2)
nrow(df.simp2)
nrow(data1)

#1b) fit cubic splines
#Cubic interpolation #note only starting from 425 not 400 (may be less than reality)
#Single spline
long =   nrow(df.simp2)  #no of rows per channel
nm = c(425, 455, 485, 515, 555, 615, 660, 695)
vecc = as.numeric(df.simp2[1,])
sp.length = 16
names = seq(425, 695, length  = sp.length)
plot(nm, df.simp2[1,])
spl.df = spline(nm, df.simp2[1,], n = sp.length)  
lines(spl.df$x, spl.df$y)

#All splines
#tot.par3 = vector("numeric", long)  #placeholder
tot.par3  = matrix(nrow = long, ncol = sp.length)
for(i in 1:long) { 
  bob.spline <- spline(nm, df.simp2[i,], n = sp.length)   
  tot.par3[i,] <- bob.spline$y
} 
tot.par3   #gives a matrix with each row the spline fits
spline.df = tot.par3 %>% data.frame()
str(spline.df)
plot(spline.df$X7)
colnames(spline.df) <- names
spline.df$date.time = as.numeric(data1$date.time)
spline.df.long = gather(spline.df, class, irrad, "425":"695")
spline.df.long$class = as.numeric(spline.df.long$class)
str(spline.df.long)

##################
#animate spectrum
# str(spline.df.long)
# spline.df.long$dec.d = (floor(spline.df.long$date.time*10))/10
# spline.df.long$dec.d = as.factor(spline.df.long$dec.d)
# spline.df.long$class = as.factor(spline.df.long$class)
# spline.df.gr = spline.df.long %>% group_by(dec.d, class) %>% summarise(estimate = mean(irrad)) %>% data.frame()
# nrow(spline.df.gr)
# spline.df.gr$dec.d <- as.numeric(as.character(spline.df.gr$dec.d))
# spline.df.gr$class <- as.numeric(as.character(spline.df.gr$class))
# str(spline.df.gr)
# library(gganimate)
# library(jpeg)
# img <- readJPEG("spectrum.JPG", native = T)
# g <- rasterGrob(img, interpolate=TRUE)
# g
# p0 = ggplot()+geom_point(spline.df.gr, mapping = aes(x = class, y = estimate))+facet_wrap(~dec.d)#+facet_wrap(~date.time)#+scale_x_log10(name ="dep sed")
# p0 + transition_time(dec.d) #+
  # labs(title = "Day: {frame_time}")#+annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)

######################
#1c) AUC of the splines for PAR
#single
library(MESS)
ff = auc(nm, df.simp2[1,], type = 'spline') 
auc(nm, vecc, type = 'spline')
#all
tot.par = vector("numeric", long)  #placeholder
for(i in 1:long) { 
  bob <- auc(nm, df.simp2[i,], type = 'spline')   
  tot.par[i] <- bob
} #THIS ADDS A CUBLINE SPLINE. Because there are only 8 data points, a cubic (join the dot) spline is best. 
data1$tot.par = tot.par  #add to main df
head(data1)  #all good to here
plot(data1$date.time, data1$tot.par)
quantile(data1$tot.par, probs = c(.01, 0.25, 0.5, 0.75, .99)) # quartile
# data3 = data1[which(data1[,12]<0),]  #all neg to 0, 16
# library(dplyr)
# data1 = data1 %>% anti_join(data3)  #subtracts one datafrma efrom another
p0 = ggplot()+geom_point(data1, mapping = aes(x = time, y = tot.par, alpha = 0.1 ), col = 'steelblue')+facet_wrap(~date)#+scale_x_log10(name ="dep sed")
p0= p0+ scale_y_continuous( limits = c(0, 750))
p0

# write.csv(data1,'2018_par_ms8.csv')
####################################################


p0 = ggplot()+geom_point(data1, mapping = aes(x = date.time, y = blue.green, alpha = 0.1 ), col = 'steelblue')#+facet_wrap(~date)#+scale_x_log10(name ="dep sed")
# p0= p0+ scale_y_continuous( limits = c(0, 750))
p0
#trimming for time-periods. https://www.timeanddate.com/sun/australia/townsville?month=11&year=2018
#sunrise: 5:30 = 0.22, 6:30 = 0.77
# astrnomical twightlight = 4:05 am  (20 nov) , astrominalcal twightlight end  = 7:56pm  (26 feb)
#

#Need better filterinng for all types of twilight. Coarse approach below

data2 = filter(data1, tot.par  < 10)   #trimming for night+twilight based on light
data3 = filter(data2, time  < 0.25)   #trimming at 6:00am for low-light day
data4 = filter(data2, time  > 0.77)    #trimming at 6:00pm
data5 = rbind(data3, data4)
data3 = data5
p0 = ggplot()+geom_point(data3, mapping = aes(x = date.time, y = blue.green, alpha = 0.1 ), col = 'steelblue')#+facet_wrap(~date)#+scale_x_log10(name ="dep sed")
# p0= p0+ scale_y_continuous( limits = c(-1, 10))
p0

#sunset twilight
# plot(data4$time, data4$X615nm)
# points(data3$time, data3$X455nm, col = 'blue')

data6 = filter(data4, time  < 0.84)

p0 = ggplot()+geom_point(data6, mapping = aes(x = time, y = X455nm), alpha = 0.5 , size = 1, position = position_jitter(width = .01),col = 'steelblue')#+scale_x_log10(name ="dep sed")
p0 = p0+geom_point(data6, mapping = aes(x = time, y = X615nm), alpha = 0.5 , size = 1, position = position_jitter(width = .01), col = 'red')
p0 = p0+geom_point(data6, mapping = aes(x = time, y = X515nm), alpha = 0.5 , size = 1, position = position_jitter(width = .01), col = 'green')
# p0 = p0+geom_point(data4, mapping = aes(x = time, y = blue.green , alpha = 0.1 , size = 2), col = 'green')
# p0= p0+ scale_y_continuous( limits = c(-1, 10))
p0 = p0+ labs(x=expression(decimal.time),
              y=expression(PAR/nm))
p0

head(data4)
p0 = ggplot()+geom_point(data4, mapping = aes(x = time, y = blue.green, alpha = 0.1 , size = 2), col = 'steelblue')#+scale_x_log10(name ="dep sed")
p0 = p0+geom_line(data4, mapping = aes(x = time, y = blue.green, alpha = 0.1 , size = 2), col = 'red')
# p0 = p0+geom_point(data4, mapping = aes(x = time, y = blue.green , alpha = 0.1 , size = 2), col = 'green')
# p0= p0+ scale_y_continuous( limits = c(-1, 10))
p0


#animation
# data1$date <- as.numeric(as.character(data1$date))
# # data1$day = data1$date-43422
# data1$day = data1$date-43661
# library(gganimate)
# p0 = ggplot()+geom_point(data1, mapping = aes(x = time, y = tot.par,  alpha  = 0.3))+ scale_y_continuous( limits = c(0, 750))+facet_wrap(~day)#+scale_x_log10(name ="dep sed")
# p0 = p0+ labs(x=expression(Time),
#               y=expression(PAR))
# p0= p0+ scale_x_continuous(labels=c("6am", "10am",'2pm','6pm'))+ theme(legend.position="none")
# p0 = p0 + transition_time(day) +
#   labs(title = "Day: {frame_time}")
# b_gif <- animate(p0, width = 240, height = 240)

###############################
#Panel gif
# library(dplyr)
# library(ggplot2)
# library(magick)
# library(gganimate)
# a_mgif <- image_write(a_gif, 'a.gif')
# b_mgif <- image_write(b_gif, 'b.gif')
# c_mgif <- image_write(c_gif, 'c.gif')
# a_mgif <- image_read('a.gif')
# b_mgif <- image_read('b.gif')
# c_mgif <- image_read('c.gif')
# 
# new_gif <- image_append(c(a_mgif[1], b_mgif[1]))
# for(i in 2:100){
#   combined <- image_append(c(a_mgif[i], b_mgif[i]))
#   new_gif <- c(new_gif, combined)
# }
# 
# new_gif
# image_write(new_gif, 'combined.gif')
# image_write(new_gif, 'combined.mp4')

######################################
#Fit a spline to all the daylight data
p0 = ggplot()+geom_point(data1, mapping = aes(x = time, y = tot.par, alpha = 0.5)) +stat_smooth()
sec.day = 24*60*60
sec.daylight = (max(data1$time) - min(data1$time))*sec.day   #seconds during daylight
ggplot(data1, aes(x = time, y = tot.par)) + stat_smooth(aes(outfit=fit<<-..y..), n = sec.daylight) + geom_point()
yy = seq(1:length(fit))
max(fit)  #fit a spline and find max. 
plot(yy,fit)
sum(fit)/1000000  #same as auc
auc(yy,fit, type = c("linear"))/1000000   #dli of the spline
plot(tot.par ~ time, data1)
md.loess = loess(tot.par ~ time, data1)
plot(md.loess)
predict(md.loess)
md.pred = predict(md.loess, data.frame(time = seq(0.25, 0.75, length = 80)), se = TRUE)
df1.upper = md.pred$fit + md.pred$se.fit*1.96
df1.upper = md.pred$fit - md.pred$se.fit*1.96





# quantile for daylight hrs PAR
vs = filter(data1, time >= 0.25) #trim to the daily hrs
vs1 = filter(vs, time <= 0.75) #trim to the daily hrs
quantile(vs1$tot.par, probs = c(.01, 0.25, 0.5, 0.75, .99)) # quartile
mean(vs1$tot.par)

#Max daily PAR
max.d.par = data1 %>% group_by(date) %>% summarise(estimate = max(tot.par))
max.d.par$estimate
levels(data1$date)
plot(max.d.par$estimate)
quantile(max.d.par$estimate, probs = c(.01, 0.25, 0.5, 0.75, .99)) # quartile



###############################################
#Clean data to days for DLIS
data2 = data.frame(date = data1$date, date.time = data1$date.time, data1$tot.par)
data2 = subset(data2, date.time >=43090)  #clipping off zeros
tail(data2, 50)
str(data2)
#data2 = subset(data1, nm <750)
data2$dec <- data2$date.time - floor(data2$date.time)  #extracting decimals (removing date compnent)
data2$dec.hr = data2$dec*24  #(convert to hr of day)
data2$hr = floor(data2$dec.hr)
data2$hr <- as.integer(as.character(data2$hr))



##########################################################
#Intepolate manually is only option using the two methods below

#Fit spline and interpolate for seconds
library(data.table)
df4 = split(data2,data2$date)
str(df4)
levels(data2$date)
# [1] "43088" "43089" "43090" "43091" "43092" "43093" "43094" "43095" "43096" "43097" "43098" "43099" "43100" "43101" "43102" "43103" "43104"
# [18] "43105" "43106" "43107" "43108" "43109" "43110" "43111" "43112" "43113" "43114" "43115" "43116" "43117" "43118" "43119" "43120" "43121"
# [35] "43122" "43123" "43124" "43125" "43126" "43127" "43128" "43129" "43130" "43131"

# [1] "43423" "43424" "43425" "43426" "43427" "43428" "43429" "43430" "43431" "43432" "43433" "43434" "43435" "43436" "43437" "43438" "43439"
# [18] "43440" "43441" "43442" "43443" "43444" "43445" "43446" "43447" "43448" "43449" "43450" "43451" "43452" "43453" "43454" "43455" "43456"
# [35] "43457" "43458" "43459" "43460" "43461" "43462" "43463" "43464" "43465" "43466" "43467" "43468" "43469" "43470" "43471" "43472" "43473"
# [52] "43474" "43475" "43476" "43477" "43478" "43479" "43480" "43481" "43482" "43483" "43484" "43485" "43486" "43487" "43488"


#single 
sec.day = 24*60*60
sec.daylight = (max(data1$time) - min(data1$time))*sec.day   #seconds during daylight
df4
#spline method
df5 = with(df4, loess.smooth(`43463`$date.time, `43463`$data1.tot.par,evaluation = sec.daylight, family = c("symmetric", "gaussian"),span=0.3))
sum( df5$y)/1000000  #11.87 DLI
plot(df4$`43463`$date.time, df4$`43463`$data1.tot.par)
lines(df5$x, df5$y)
#df5 = with(df4, spline(`43102`$date.time, `43102`$tot.par, n = sec.day, method = "periodic"))
#Blocking to hr method
df1 = df4$`43102` %>%group_by(hr) %>%summarise( par.hr = mean(data1.tot.par)) %>% as.data.frame()  # average per block
#df1.b = df4 %>%group_by(date) %>%group_by(hr) %>%summarise( par.hr = mean(data1.tot.par)) %>% as.data.frame()  # average per block
sum(df1$par.hr*60*60/1000000)






#Fit spline to one day
head(data1)
str(data1)
library(MESS)
ffgg = auc(nm, df.simp2[1,], type = 'spline') 
auc(nm, vecc, type = 'spline')




###########################################

