
#muspec

##1) Import data
setwd("C:/Users/gricardo/OneDrive - Australian Institute of Marine Science/2 Methods/3 Protocols/Light workshop 2020")  
#data2 = 'https://raw.githubusercontent.com/gerard-ricardo/data/master/2020%20muspec%20neal%20test%20short'   #this is a subset of the data
data2 = "uSpec_Oct19_22_LOG_0066.txt"
head(data2)
options(scipen = 999)  # turn off scientific notation
 library(beepr)
library(dplyr)


  tt=scan(data2,what="character")  #as character
  tt=unlist(strsplit(tt,split=","))  #delimit by ','
  xx=unlist(lapply(tt,FUN=nchar))  # find size of all characters
  tt=tt[which(xx>0)]  #use only those > 0 i.e non-empty strings
  
    beep(1)
  #   log.dat.f=unlist(strsplit(c(paste(tt[1:3],collapse=" "),paste(tt[4:5],collapse=" "),
  #                   tt[7],tt[11:12],tt[14]),split=","))  #get meta data
  # names(log.dat.f)=c("source","device","firmware","session_start_date",
  #                    "session_start_time","log_file")  #add names to meta data

  port.dat.f=list()   #create placeholder
  port.indices.f=grep("uSpec-LPT",tt)  #indices for letters 'Port'
  
    for(p in 1:(length(port.indices.f)-1)){
       port.dat.f=c(port.dat.f,list(tt[port.indices.f[p]:((port.indices.f[p+1])-1)]))
  }  #take the data from the 1st Port to the next Port, and repeat 
  
    beep(1)
    uspec.length=table(unlist(lapply(port.dat.f,FUN=length)))  #find length of all. Highest tally should be correct length, other indicate errors
  dominant.length=as.numeric(names(uspec.length)[which.max(uspec.length)])  #find highest in tally
  uspec.dat.f=do.call("rbind",port.dat.f[unlist(lapply(port.dat.f,FUN=function(x){length(x)==dominant.length}))])  #extract all of dominant length and 
  #stack in matrix 
  # if(length(DL3.dat.f)>0){
  # colnames(DL3.dat.f)=c("port","date","time","sensor","device_number","voltage_raw_counts",
  #                       "depth_raw_counts","temp_raw_counts","voltage","depth_M","Temp")}  #add names
  # head(DL3.dat.f)
  
  uspec.dat.f = gsub("nan", "na", uspec.dat.f) #change nan to na
  uspec.dat.f2 = uspec.dat.f[,12:ncol(uspec.dat.f)] %>%  data.frame()  #extract irradiance
  names = round(c(349.87,352.52,355.16,357.80,360.44,363.08,365.72,368.35,370.98,373.60,376.22,378.84,381.46,384.07,386.68,389.29,391.89,394.49,397.09,399.68,402.27,
  404.86,407.44,410.02,412.59,415.16,417.73,420.29,422.85,425.41,427.96,430.51,433.05,435.59,438.13,440.66,443.18,445.71,448.22,450.74,453.25,455.75,
  458.25,460.75,463.24,465.73,468.21,470.69,473.16,475.63,478.09,480.55,483.01,485.45,487.90,490.34,492.77,495.20,497.63,500.04,502.46,504.87,507.27,
  509.67,512.06,514.45,516.83,519.21,521.58,523.95,526.31,528.67,531.02,533.36,535.70,538.04,540.37,542.69,545.01,547.32,549.63,551.93,554.22,556.51,
  558.79,561.07,563.34,565.61,567.87,570.13,572.37,574.62,576.85,579.09,581.31,583.53,585.74,587.95,590.15,592.35,594.54,596.72,598.90,601.07,603.23,
  605.39,607.55,609.69,611.83,613.97,616.10,618.22,620.33,622.44,624.55,626.65,628.74,630.82,632.90,634.97,637.04,639.10,641.15,643.20,645.24,647.27,
  649.30,651.32,653.34,655.35,657.35,659.35,661.34,663.32,665.30,667.27,669.24,671.20,673.15,675.09,677.03,678.97,680.89,682.81,684.73,686.63,688.53,
  690.43,692.32,694.20,696.07,697.94,699.81,701.66,703.51,705.36,707.19,709.02,710.85,712.67,714.48,716.28,718.08,719.87,721.66,723.44,725.21,726.98,
  728.74,730.50,732.24,733.99,735.72,737.45,739.17,740.89,742.60,744.31,746.00,747.70,749.38,751.06,752.73,754.40,756.06,757.71,759.36,761.01,762.64,
  764.27,765.89,767.51,769.12,770.73,772.33,773.92,775.51,777.09,778.66,780.23,781.80,783.35,784.90,786.45,787.99,789.52,791.05,792.57,794.09,795.60,
  797.10,798.60,800.09,801.58,803.06,804.54,806.00,807.47,808.93,810.38,811.83,813.27,814.70,816.13,817.56,818.98,820.39,821.80,823.20,824.60,825.99,
  827.37,828.75,830.13,831.50,832.86,834.22,835.58,836.93,838.27,839.61,840.94,842.27,843.59,844.91,846.22,847.53,848.83,850.13,851.42,852.71,853.99,
  855.27,856.55,857.81,859.08,860.34,861.59,862.84,864.08,865.32,866.56,867.79,869.02,870.24,871.45,872.67,873.87,875.08)) %>%  as.character()
  
  names2 <- paste('x',names, sep='' )  #joining columns. Use sep=''  to remove space
colnames(uspec.dat.f2) <- names2   #columns. Can use index to change indices column
str(uspec.dat.f2)
uspec.dat.f2[] <- lapply(uspec.dat.f2, function(x) as.numeric(as.character(x)))   #convert all to numeric
#uspec.dat.f2 = uspec.dat.f2[complete.cases(uspec.dat.f2), ] #remove rows with na

#now add back on any useful columns from uspec.dat.f

head(uspec.dat.f2)
uspec.dat.f2$Date=as.Date(uspec.dat.f[,3],format="%d/%m/%Y")
uspec.dat.f2$Time=uspec.dat.f[,4]
Start.Date=as.Date("01/01/0000",format="%d/%m/%Y")  #note: days are from start of AD, not julien
#uspec.dat.f2$Start.Date=Start.Date
#uspec.dat.f2$Start.time=log.dat.f["session_start_time"]
uspec.dat.f2$j.date=julian(uspec.dat.f2$Date,origin=Start.Date)
ff=strsplit(uspec.dat.f[,4],split=":")
length.ff=unlist(lapply(ff,FUN=length))
ff[which(length.ff!=3)]=rep(NA,3)
time.matrix=do.call("rbind",ff)
hour=as.numeric(time.matrix[,1])
minute=as.numeric(time.matrix[,2])
second=as.numeric(time.matrix[,3])
uspec.dat.f2$deci.time=((hour*60*60)+
                           (minute*60)+
                            second)/(24*60*60)
uspec.dat.f2$deci.j.date=uspec.dat.f2$j.date + uspec.dat.f2$deci.time
head(uspec.dat.f2)


plot(uspec.dat.f2$deci.j.date, uspec.dat.f2$x350)

p1 = ggplot() + geom_point(data = uspec.dat.f2, aes(x = deci.j.date, y = x350))+theme_sleek()
p1

p0 = ggplot()+geom_point(uspec.dat.f2, mapping = aes(x = deci.j.date, y = x350),position = position_jitter(width = .02), alpha = 0.05,size = 3 )+theme_sleek()
#p0 = p0 + facet_wrap(~time+spec)+scale_x_log10(name ="XXXX")#+geom_smooth(data1, mapping = aes(x = raw.x, y = suc/tot))
#p0= p0+ scale_y_continuous( limits = c(0, 1)) 
p0

