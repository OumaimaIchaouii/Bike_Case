
library(dplyr)
library(janitor)
library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)
library(tidyr)

##merging all 12 months datasets
csv_files <- list.files(path = "C:\\Users\\user2022\\Desktop\\Data\\Case Study 1\\Data\\geo", recursive = TRUE, full.names=TRUE)

cyclistic <- do.call(rbind, lapply(csv_files, read.csv))
View(cyclistic_merged)
#the dimension of the data set before cleaning
dim(cyclistic_merged)

# removing null rows or columns
library(janitor)
bike_rides <- remove_empty(cyclistic_merged,which = c("cols"))
bike_rides <- remove_empty(cyclistic_merged,which = c("rows"))
dim(bike_rides) 
str(bike_rides)
#format the column names
colnames(bike_rides)


#extract time and day from the variables
bike_rides$start_hour <- lubridate::hour(bike_rides$started_at)
bike_rides$end_hour <- lubridate::hour(bike_rides$ended_at)
bike_rides$start_day <- lubridate::day(bike_rides$started_at)
bike_rides$end_day <- lubridate::day(bike_rides$ended_at)


library(dplyr) #for the usage of select
select(bike_rides,"started_at","ended_at")
#verify the type of the two variables changed
sapply(bike_rides, typeof)
colnames(bike_rides)
str(bike_rides)
# count the rides/hour 
library(ggplot2)
#library scales for transforming the axis to numerical values
library(scales)
library(dplyr)
bike_rides %>% count(start_hour,sort=T) %>% ggplot()+geom_point(mapping=aes(x=start_hour,y=n)) + scale_y_continuous(labels=comma)+ labs(title = "count of bike rides by hour" ,y="number of rides")
## tranform start date and end date to characters to calculate duration

install.packages("lubridate")
library(lubridate)

bike_rides$ended_at<-cyclistic_merged$ended_at
df <- bike_rides  %>%  mutate(started_at = ymd_hms(bike_rides$started_at))
df <- bike_rides  %>%  mutate(ended_at = ymd_hms(bike_rides$ended_at))

str(df)

## calculate the duration of trips 

df$trip_duration<- difftime(df$ended_at, df$started_at, units = "mins")
View(trip_duration)
select(df,trip_duration)
#filter negative durations
df1 <- df %>% filter(trip_duration > 0)

#remove duplicate records 
df1 %>% distinct()
geo33<-  geo33 %>% distinct() %>% na.omit()
#drop rows with missing values
df1<- na.omit(df)
str(df1)
select(df1,-start_hour,-end_hour)

sorted<- df1 %>% group_by(start_hour) %>% arrange(trip_duration) 
library(ggplot2)
sorted <- count(df1$start_hour,sort=T) %>% ggplot()+geom_point(mapping=aes(x=start_hour,y=trip_duration)) + labs(title = "count of bike rides by hour" ,y="trip_duration")

df2 <- df1 %>% select (-c(start_lat, start_lng, end_lat, end_lng, start_station_id,end_station_id, end_station_name,s_hour,e_hour)) 

#add a column for weekdays the ride started
df2$week_day <- weekdays.Date(as.POSIXlt(df2$started_at))
#extract month


df2$month <- lubridate::month(df2$started_at)
df2$month <- month.abb[df2$month]
#extract start and end hour
df$s_hour <- lubridate::hour(df$started_at)
df$e_hour <- lubridate::hour(df$ended_at)


# summaeize mean trip durations by week days and then plot it
mean_df <- df %>% group_by(week_day) %>% summarise(mean_ride_time=mean(trip_duration)) %>% arrange(-mean_ride_time) 
ggplot(data=mean_df)+geom_line(mapping = aes(x=week_day,y=mean_ride_time))



# summaeize max trip durations by week days
max_df <- df %>% group_by(week_day) %>% summarise(max_ride_time=max(trip_duration)) %>% arrange(-max_ride_time)
# summarize all the data frame
summary(df2)


#summarize mean and max durations per casual or annual rider
d <- df %>% group_by(week_day) %>% summarise(mean_ride_time=mean(trip_duration),max_ride_time=max(trip_duration)) %>% arrange(-mean_ride_time)



#number of casual and annual riders 
number <- df %>% count(member_casual)



#plot the graph of number of riders per type
ggplot(data=df,aes(x=member_casual,fill=member_casual) )+geom_bar()+geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "black") + scale_y_continuous(labels=comma)+labs(title="the number of riders by type")+xlab("type of riders")+ylab("number_per_each_type")


#plot trip durations based on rider types
mean(df2$trip_duration)
ggplot(data=df,aes(x=member_casual,y=trip_duration,fill=member_casual ))+geom_bar(stat = "identity")+ scale_y_continuous(labels=comma)+labs(title="trip duration based on type of riders")+xlab("type of riders")+ylab("trip duration in min")+facet_wrap(~week_day)
max(df$trip_duration)
ggplot(data = df) + geom_bar(mapping = aes(x=trip_duration))+ scale_y_continuous(labels=comma)+labs(title="average trip duration based on type of riders")+xlab("type of riders")+ylab("average duration")

#summarize trip durations by type of riders arranged
summary_of_durations<-df %>% group_by(member_casual) %>% summarise(trip_duration) %>% arrange(-trip_duration)

#summarize bike types of bikes by riders
df %>% ggplot()+ geom_bar(mapping = aes(x = rideable_type,fill=member_casual))+ scale_y_continuous(labels=comma)+labs(title="Types of bikes used by riders")+xlab("type of bikes")+ylab("number of bikes")
df %>% ggplot()+ geom_bar(mapping = aes(x = rideable_type,fill=rideable_type))+ scale_y_continuous(labels=comma)+labs(title="Types of bikes")+xlab("type of bikes")+ylab("number of bikes")






# number of casual/members rides per month
  df %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual, month)  %>% 
  ggplot(aes(x = month, 
             y = number_of_rides, 
             fill = member_casual)) +  scale_y_continuous(labels=comma)+
  labs(fill='Rider Group') +
  geom_col(position = "dodge") +
  ggtitle(label = 'Total Rides per Month', subtitle = 'Casual Riders vs Members') + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  xlab('Month') + ylab('Number of Rides') +
  geom_text(aes(label = number_of_rides, hjust = "left"), position = position_dodge(width = 0.9),size = 3, angle = 90)
  
  
  
  
  
  
  df %>% 
    group_by(member_casual, week_day) %>% 
    summarise(number_of_rides = n()) %>%
    arrange(member_casual, week_day)  %>% 
    ggplot(aes(x = week_day, 
               y = number_of_rides, 
               fill = member_casual)) +
    labs(fill='Rider Group') +
    geom_col(position = "dodge") +
    ggtitle(label = 'Total Rides per Day', subtitle = 'Casual Riders vs Members') + 
    theme(plot.title = element_text(hjust = 0.5)) +   scale_y_continuous(labels=comma)+
    theme(plot.subtitle = element_text(hjust = 0.5)) +
    xlab('Day of Week') + ylab('Number of Rides') +
    geom_text(aes(label = number_of_rides, hjust = "right"), position = position_dodge(width = 0.9),size = 3, angle = 90)
 
  
  
  
  
 #--------average trip duration per type of riders 
  df %>% 
    group_by(member_casual) %>% 
    summarise(average_duration = mean(trip_duration)) %>% 
    arrange(member_casual)  %>% 
    ggplot(aes(x = member_casual, 
               y = average_duration, 
               fill = member_casual)) +
    labs(fill='Rider Group') +
    geom_col(position = "dodge") +
    ggtitle(label = 'Average Trip Duration', subtitle = 'Casual Riders vs Members') + 
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.subtitle = element_text(hjust = 0.5)) +
    xlab('Rider Group') + ylab('Trip Duration in Minutes') +
    geom_text(aes(label = round(average_duration/60, digits = 2)), vjust = 0)
  
  
  
  #----------Average trip duration per weekday by member/casual
  df %>% 
    group_by(member_casual, month) %>% 
    summarise(average_duration = mean(trip_duration)) %>% 
    arrange(member_casual, month)  %>% 
    ggplot(aes(x = month, 
               y = average_duration, 
               fill = member_casual)) +
    labs(fill='Rider Group') +
    geom_col(position = "dodge") +
    ggtitle(label = 'Average Trip Duration by Month', subtitle = 'Casual Riders vs Members') + 
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.subtitle = element_text(hjust = 0.5)) +
    xlab('Month') + ylab('Trip Duration in Minutes') +
    geom_text(aes(label = round(average_duration, digits = 2), hjust = "right"), position = position_dodge(width = 0.9),size = 3, angle = 90)

  
  
  #----------Bike Preference per Rider Group
  df %>% 
    group_by(member_casual, rideable_type) %>% 
    summarise(number_of_rides = n()) %>%
    arrange(member_casual, rideable_type)  %>% 
    ggplot(aes(x = rideable_type, 
               y = number_of_rides, 
               fill = member_casual)) + 
    labs(fill='Rider Group') +
    geom_col(position = "dodge") +
    ggtitle(label = 'Total Rides per Year by Bike Type', subtitle = 'Casual Riders vs Members') +  
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.subtitle = element_text(hjust = 0.5)) +
    xlab('Bike Type') + ylab('Number of Rides') +
    geom_text(aes(label = number_of_rides), position = position_dodge(width = 0.9), vjust = 0)
  
  
  
  #----------number of trips duration per bike type in weekdays
  df %>% 
    group_by(member_casual, rideable_type,week_day) %>% 
    
  summarise(number_of_rides = n()) %>% 
    arrange(member_casual, rideable_type)  %>% 
    ggplot(aes(x = rideable_type, 
               y = number_of_rides, 
               fill = member_casual)) + facet_wrap(~week_day)+
    labs(fill='Rider Group') + scale_y_continuous(labels=comma)+
    geom_col(position = "dodge") +
    ggtitle(label = 'Number of Trips by Bike Type in weekdays', subtitle = 'Casual Riders vs Members') + 
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.subtitle = element_text(hjust = 0.5)) +
    xlab('Bike Type') + ylab('Trip Duration in Minutes') +
    geom_text(aes(label =number_of_rides), position = position_dodge(width = 0.9), vjust = 0)  

  
  #----------Average trip duration per bike type
  df %>% 
    group_by(member_casual, rideable_type,week_day) %>% 
    summarise(average_duration = mean(trip_duration)) %>% 
    arrange(member_casual, rideable_type)  %>% 
    ggplot(aes(x = rideable_type, 
               y = average_duration, 
               fill = member_casual)) + facet_wrap(~week_day)+
    labs(fill='Rider Group') +
    geom_col(position = "dodge") +
    ggtitle(label = 'Average Trip Duration by Bike Type in weekdays', subtitle = 'Casual Riders vs Members') + 
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.subtitle = element_text(hjust = 0.5)) +
    xlab('Bike Type') + ylab('Trip Duration in Minutes') +
    geom_text(aes(label = round(average_duration, digits = 2)), position = position_dodge(width = 0.9), vjust = 0)

