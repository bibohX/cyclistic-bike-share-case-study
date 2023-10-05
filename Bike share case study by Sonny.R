#Data Cleaning and Manipulation


# Initial process of data processing
install.packages(c("tidyverse", "lubridate", "janitor","data.table","readr"))




#loading the library
library(tidyverse)  
library(lubridate)  
library(janitor) 
library(data.table)  
library(readr)

#Loading the datasets.
tripdata_202212 <- read_csv("~/R Programming/trip_data/202212-divvy-tripdata.csv")
tripdata_202211 <- read_csv("~/R Programming/trip_data/202211-divvy-tripdata.csv") 
tripdata_202210 <- read_csv("~/R Programming/trip_data/202210-divvy-tripdata.csv") 
tripdata_202209 <- read_csv("~/R Programming/trip_data/202209-divvy-tripdata.csv") 
tripdata_202208 <- read_csv("~/R Programming/trip_data/202208-divvy-tripdata.csv") 
tripdata_202307 <- read_csv("~/R Programming/trip_data/202307-divvy-tripdata.csv") 
tripdata_202306 <- read_csv("~/R Programming/trip_data/202306-divvy-tripdata.csv") 
tripdata_202305 <- read_csv("~/R Programming/trip_data/202305-divvy-tripdata.csv") 
tripdata_202304 <- read_csv("~/R Programming/trip_data/202304-divvy-tripdata.csv") 
tripdata_202303 <- read_csv("~/R Programming/trip_data/202303-divvy-tripdata.csv") 
tripdata_202302 <- read_csv("~/R Programming/trip_data/202302-divvy-tripdata.csv") 
tripdata_202301 <- read_csv("~/R Programming/trip_data/202301-divvy-tripdata.csv")

#colnames() for column names to check the consistencies of data each dataframe.

colnames(tripdata_202307)
colnames(tripdata_202306) 
colnames(tripdata_202305)  
colnames(tripdata_202304) 
colnames(tripdata_202303)  
colnames(tripdata_202302) 
colnames(tripdata_202301) 
colnames(tripdata_202208) 
colnames(tripdata_202209)  
colnames(tripdata_202210) 
colnames(tripdata_202211)  
colnames(tripdata_202212)



#Inspect the structure of multiple data frames, We use a loop or lapply to apply the str function.
lapply( list(tripdata_202208,tripdata_202209,tripdata_202210,tripdata_202211, tripdata_202212,tripdata_202307,
             tripdata_202306,tripdata_202305, tripdata_202304,tripdata_202303,tripdata_202302,tripdata_202301), str )


#Change the dateTime Format of tripdata_202307
tripdata_202307_processed <- tripdata_202307 %>%   mutate(across(c(started_at, ended_at), lubridate::dmy_hms))

# Changing datatype from chr to dttm for tripdata_202307
tripdata_202307_tripv2 <- tripdata_202307_processed %>% mutate(across(c(started_at, ended_at), ~ as.POSIXct(.x, format =                                                       "%Y-%m-%d %H:%M:%S") %>% ymd_hms()))


#Combine data sets into one data frame.

# Combine data sets into one data frame.
df_processed_v2 <- bind_rows(tripdata_202208,tripdata_202209,tripdata_202210,tripdata_202211,tripdata_202212,tripdata_202307,tripdata_202306,tripdata_202305,tripdata_202304,tripdata_202303,tripdata_202302,tripdata_202301)

#Glimpse function to identify the column for clarity of data.
glimpse(df_processed_v2)


#Renaming the columns to more descriptive names.
df_processed_v2 <- df_processed_v2 %>% rename(ride_type = rideable_type, start_time = started_at,end_time = ended_at,customer_type = member_casual)


#added a new columns day_of_week, month, trip_duration intended for aggregation purposes.

#Day of the week the trip started 
df_processed_v2$day_of_week <- format(as.Date(df_processed_v2$start_time),'%a') 

#Month when the trip started 
df_processed_v2$month <- format(as.Date(df_processed_v2$start_time),'%b_%y') 

#Time
df_processed_v2$time <- format(df_processed_v2$start_time, format = "%H:%M:%S")

#Trip Duration in minutes 
df_processed_v2$trip_duration <- (as.double(difftime(df_processed_v2$end_time, df_processed_v2$start_time)))/60


#Convert the 'time' column to POSIXct format.
df_processed_v2 <- df_processed_v2 %>%    mutate(time = as.POSIXct(time, format = "%H:%M:%S"))


#Count the number of rows in a subset of a data frame.
nrow(subset(df_processed_v2, trip_duration <0))


#Count the total row in data frame.
nrow(df_processed_v2)


#Checking if any Testrides were made.
nrow(subset(df_processed_v2, start_station_name %like% "TEST"))  nrow(subset(df_processed_v2, start_station_name %like% "test"))  nrow(subset(df_processed_v2, start_station_name %like% "Test"))



#Remove all unnecessary data.

#Negative trip durations  
df_processed_v2 <- df_processed_v2[!(df_processed_v2$trip_duration <0),]

#check for NULL values and remove rows based on that.  
df_processed_v2 <- df_processed_v2[!is.na(df_processed_v2$trip_duration), ]  

#remove test rides made  
df_processed_v2 <- df_processed_v2[!((df_processed_v2$start_station_name %like% "Test" | df_processed_v2$start_station_name %like% "TEST")),]


#Check if there is only two customer types
table(df_processed_v2$customer_type)



#Summary of trip duration
summary(df_processed_v2$trip_duration)
      



df_processed_v2 %>% 
  group_by(customer_type) %>% 
  summarise(min_trp_duration = min(trip_duration),max_trp_duration = max(trip_duration), median_trp_duration = median(trip_duration),
  mean_trp_duration = mean(trip_duration))



# Order day_of_week column for easier reference
df_processed_v2$day_of_week <- ordered(df_processed_v2$day_of_week, levels=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))


# Create a vector of ordered months in the desired order 
ordered_months <- c("Aug_22", "Sep_22", "Oct_22", "Nov_22","Dec_22","Jan_23", "Feb_23", "Mar_23","Apr_23", "May_23","Jun_23","Jul_23")  

# Apply the ordered levels to the month column 
df_processed_v2$month <- ordered(df_processed_v2$month, levels = ordered_months)


#Summarize into Table
df_processed_v2 %>% group_by(customer_type, day_of_week) %>% 
  summarise(number_of_rides = n(), average_duration_min = mean(trip_duration)) %>% 
  arrange(customer_type, desc(number_of_rides))


#Visualization 1
df_processed_v2 %>% group_by(customer_type, day_of_week) %>%   
  summarise(number_of_rides = n()) %>% 
  arrange(customer_type, day_of_week) %>%    
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = customer_type)) +    
  labs(title = "Total trips by customer type Vs. Day of the week", 
       fill = "Customer Type") +   
  geom_col(width = 0.5, position = position_dodge(width = 0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) + 
  scale_fill_manual(values = c("skyblue", "yellow"))

#Average number of trip by customer type and month
df_processed_v2 %>% group_by(customer_type, month) %>% 
  summarise(number_of_rides = n(), average_duration_mins = mean(trip_duration))

#Visualization 2
df_processed_v2 %>%   group_by(customer_type, month) %>%   
  summarise(number_of_rides = n()) %>%   arrange(customer_type, month) %>%   
  ggplot(aes(x = month, y = number_of_rides, fill = customer_type)) +   
  geom_col(width = 0.5, position = "dodge") +   
  labs(title = "Total trips by customer type Vs. Month", 
       x = "Moth",
       y = "Number of Rides",
       fill = "Customer Type") +
  theme(axis.text.x = element_text(angle = 30)) +   
  scale_fill_manual(values = c("skyblue", "yellow"))



#Visualisation 3
df_processed_v2 %>% 
  group_by(customer_type, day_of_week) %>% 
  summarise(average_trip_duration = mean(trip_duration)) %>% 
  ggplot(aes(x = day_of_week, y = average_trip_duration, fill = customer_type)) +
  geom_col(width = 0.5, position = position_dodge(width = 0.5)) + 
  labs(title = "Average trip duration by customer type Vs. Day of the week", 
       fill = "Customer Type",
       x = "Trip Duration",
       y = "Day of Week")+ 
  scale_fill_manual(values = c("skyblue", "yellow"))


#Visualization 4**
df_processed_v2 %>% 
  group_by(customer_type, month) %>%
  summarise(average_trip_duration = mean(trip_duration)) %>% 
  ggplot(aes(x = month, y = average_trip_duration, fill = customer_type)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) + 
  labs(title ="Average trip duration by customer type Vs. Month",
       x = "Month",
       y = "Average Trip Duration",
       fill = "Customer Type") + 
  theme(axis.text.x = element_text(angle = 30))+
  scale_fill_manual(values = c("skyblue", "yellow"))


#Visualization 5**
df_processed_v2 %>% 
  group_by(customer_type, time) %>% 
  summarise(number_of_trips = n()) %>% 
  ggplot(aes(x = time, y = number_of_trips, color = customer_type, group = customer_type)) + geom_line() + 
  scale_x_datetime(date_breaks = "1 hour", minor_breaks = NULL, date_labels = "%H:%M", expand = c(0, 0)) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(title = "Demand over 24 hours of a day", 
       x = "Time of the day", 
       y = "Number of Trips",
       fill = "Customer Type") + 
  scale_colour_manual(values = c("skyblue", "yellow"))




#Save the cleaned and processed data frame. To be use in Tableau
write.csv(df_processed_v2, "C:\\Users\\Acer\\Documents\\R Programming\\ProjectDF_PROCESS_V2.csv", row.names = FALSE)
