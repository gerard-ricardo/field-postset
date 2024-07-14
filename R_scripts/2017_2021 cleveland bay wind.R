

library(plyr)
library(dataaimsr)
library(tidyverse)
library(ggmap)
my_api_key <- "P1SNKfYxyW9dYjtpp05CP3wFCJMhm8CX3TfkgsB9"

library(ggplot2)
source("https://raw.githubusercontent.com/gerard-ricardo/data/master/theme_sleek2")  #set theme in code
#####windspeed##############
#cleveland 
data1 = aims_filter_values("weather", filter_name = "series")  #filter for weatehr
cleveland_id = dplyr::filter(data1,
                            grepl('Cleveland', series))  #get all clevelandtree  IDs
cleveland.wind.id <- filter(cleveland.tree.id,
                       grepl('Wind', series)) %>% filter(.,
                                                         grepl('Speed', series))  #159 (10 mins)  or 184 (30 mins)
cleveland.wind.id

#10 min periods
cleveland.wind <- aims_data("weather", api_key = my_api_key,
                       filters = list(series_id = 47,
                                      from_date = "2018-11-21",
                                      thru_date = "2019-01-22"))  #last 10 years (yyyy/mm/dd)
head(cleveland.wind)




# 2 Labelling and wrangling -----------------------------------------------
data1 = cleveland.wind
str(data1) #check data type is correct
data1 = dplyr::select(data1, c(time, qc_val))
data1 = data1[complete.cases(data1), ]  #make sure import matches NA type


#seprate date and time
library(lubridate)
datetime_string <- data1$time
data1$datetime <- ymd_hms(datetime_string)
data1$date <- as.Date(data1$datetime)
data1$time2 <- format(data1$datetime, "%H:%M:%S")
head(data1)
str(data1)


# Function to convert a date to Excel's serial number
convertDateToExcelSerial <- function(date) {
  reference_date <- as.Date("1899-12-30")
  as.integer(date - reference_date)
}
data1$date_excel_serial <- sapply(data1$date, convertDateToExcelSerial)
str(data1)
tail(data1)

data1$time3 = sapply(strsplit(data1$time2,":"),
                          function(x) {
                            x <- as.numeric(x)
                            (x[1]+x[2]/60 + x[3]/60)/24
                          })  #split by : and then convert to prop

data2 = data.frame(date = data1$date_excel_serial, time = data1$time3, datetime = data1$date_excel_serial+ data1$time3, wind_speed = data1$qc_val)
head(data2)   
#save(data2, file = file.path("./Rdata", "2018_19_windspeed_cleveland.RData"))
load('./Rdata/2018_19_windspeed_cleveland.RData') #data2



