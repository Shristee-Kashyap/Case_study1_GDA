# Case_study1_GDA
# Codes
# DA case study 1- Cyclistic
#How users with casual membership use the service diffrently?
#importing required files for analysis
getwd()
library(readr)
library(readxl)
Jan22_filtered <- read_excel("Jan22_filtered.xlsx")

Feb22_filtered <- read_excel("Feb22_filtered.xlsx")

March22_filtered <- read_excel("March22_filtered.xlsx")

April22_filtered <- read_excel("April22_filtered.xlsx")

May22_filtered <- read_excel("May22-filtered.xlsx")

Aug21_filtered <- read_excel("Aug21_filtered.xlsx")

Dec21_filtered <- read_excel("Dec21_filtered.xlsx")

July21_filtered <- read_excel("July21_filtered.xlsx")

June21_filtered <- read_excel("June21_filtered.xlsx")

Nov21_filtered <- read_excel("Nov21_filtered.xlsx")

Oct21_filtered <- read_excel("Oct21_filtered.xlsx")

Sept21_filtered <- read_excel("Sept21_filtered.xlsx")



View(Jan22_filtered)
View(Feb22_filtered)
View(March22_filtered)
View(April22_filtered)
View(May22_filtered)
View(June21_filtered)
View(July21_filtered)
View(Aug21_filtered)
View(Sept21_filtered)
View(Oct21_filtered)
View(Nov21_filtered)
View(Dec21_filtered)



#installing required packages

install.packages("tidyverse")
library("tidyverse")
install.packages("lubridate")
library(lubridate)
install.packages("ggplot2")
library(ggplot2)
#bindiing data
whole_data <- rbind( Jan22_filtered , Feb22_filtered , March22_filtered , April22_filtered ,
                    May22_filtered, June21_filtered , Aug21_filtered , Sept21_filtered ,  
                    Oct21_filtered , Nov21_filtered , Dec21_filtered )
# removing na values
whole_data <- na.omit(whole_data)

#checking consistency of column names
colnames(Jan22_filtered)
colnames(Feb22_filtered)
colnames(March22_filtered)
colnames(April22_filtered)
colnames(May22_filtered)
colnames(June21_filtered)
colnames(July21_filtered)
colnames(Aug21_filtered)
colnames(Sept21_filtered)
colnames(Oct21_filtered)
colnames(Nov21_filtered)
colnames(Dec21_filtered)
#checking structure of each file
str(Jan22_filtered)
str(Feb22_filtered)
str(March22_filtered)
str(April22_filtered)
str(May22_filtered)
str(June21_filtered)
str(July21_filtered)
str(Aug21_filtered)
str(Sept21_filtered)
str(Oct21_filtered)
str(Nov21_filtered)
str(Dec21_filtered)
#changing ride_length to numeric
install.packages("dplyr")
library(dplyr)
whole_data <- mutate( whole_data, ride_length=as.numeric(ride_length))
#checking structure, column and summary names of whole_data
str(whole_data)
colnames(whole_data)
summary(whole_data)
install.packages("magrittr")
library(magrittr)
library(ggplot2)
library(dplyr)
whole_data %>% 
  group_by ( member_casual) %>% 
  summarise ( total_no_of_rides = n()) %>% 
  arrange ( member_casual ) %>% 
  ggplot ( aes ( x= member_casual,
              y = total_no_of_rides,
              fill = member_casual )) +
  labs ( fill = "Type of membership") +
  geom_col ( position = "dodge" ) +
  ggtitle ( label = 'Rides per year' , subtitle = 'Casual vs members') +
  theme ( plot.title = element_text ( hjust = 0.5)) +
  theme ( plot.subtitle = element_text ( hjust = 0.5)) +
  xlab ('Membership') + ylab('Numbers of riders') 
  geom_text ( aes ( label = total_no_of_rides ), vjust = 0 )
  
  
  head(whole_data) #to check first 6 lines of whole_data
library(lubridate) #to load dates, weekdays ,month 

  
  

  
  whole_data$date <- as.Date(whole_data$started_at)
  whole_data$month <- format(as.Date(whole_data$date) , " %m " )
  whole_data$day  <- format(as.Date(whole_data$date) ,  " %d " )
  whole_data$year <- format(as.Date(whole_data$date) , " %y " )
  whole_data$day_of_week <- format(as.Date(whole_data$date) , "%A")
  
  whole_data$ride_length <- as.numeric(as.character(whole_data$ride_length))
  
  is.numeric(whole_data$ride_length) #for checking data is numeric


  whole_data$ride_length <- difftime(whole_data$ended_at ,whole_data$started_at)

#removing bad data with less than zero ride_length and station name = HQ QR

  whole_data_v2 <- whole_data[!(whole_data$start_station_name == "HQ QR" | whole_data$ride_length < 0 ) , ]
  mean(whole_data_v2$ride_length) #straight average (total ride length/ rides)
  median(whole_data_v2$ride_length) #midpoint no.
  max(whole_data_v2$ride_length) #longest ride
  min(whole_data_v2$ride_length) #shortest ride
  
  # instead of above 4 lines use summary
  summary(whole_data_v2$ride_length)

#running descriptive analysis
  
  aggregate(whole_data_v2$ride_length ~ whole_data_v2$member_casual , FUN = mean)
  aggregate(whole_data_v2$ride_length ~ whole_data_v2$member_casual , FUN = median)
  aggregate(whole_data_v2$ride_length ~ whole_data_v2$member_casual , FUN = max)
  aggregate(whole_data_v2$ride_length ~ whole_data_v2$member_casual , FUN = min)

#Making week weekdaus in order
  
whole_data_v2$day_of_week <- ordered(whole_data_v2$day_of_week , levels = c("Sun" , "Mon" , "Tues" , "Wed" , "Thurs" , "Fri" , "Sat"))

#plotting avaerage duration of rides in weekdays

whole_data_v2 %>% 
  mutate ( weekday = wday ( started_at , label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(no_of_rides = n() , average_duration = mean(ride_length)) %>% 
arrange(member_casual , weekday)
ggplot( aes ( x = weekday , y = average_duration , fill = member_casual))+
  geom_col(position = "dodge")


#Making months in order

whole_data_v2$month <- ordered ( whole_data_v2$month , levels = c ( "Jan", "Feb" , "March" , "April" ,"May" , "June" , "July" , "Aug" , "Sept" , "Oct" , "Nov" , "Dec" ))

#plotting average ride duration in months

whole_data_v2 %>% 
  mutate(Month = month(started_at , label =TRUE)) %>% 
  group_by(member_casual, Month) %>%
  summarise(no_of_rides = n() , average_duration = mean(ride_length)) %>% 
  arrange(member_casual , Month) %>% 
  ggplot( aes(x = Month , y = average_duration , fill = member_casual))+
  geom_col(position = "dodge")


