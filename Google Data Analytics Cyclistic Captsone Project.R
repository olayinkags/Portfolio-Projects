# Google Data Analytics Cyclistic Capstone Project


# Install and Load Packages

#install.packages("tidyverse")
#install.packages("lubridate")
#install.packages("ggplot2")
#install.packages("dplyr")

library(tidyverse)
library(lubridate)
library(ggplot2)
library(readr)
library(dplyr)



# Importing 12 months Cyclistic trip data in the year 2021

jan_df = read_csv("C:\\Users\\Dewoyin\\Documents\\Datasets for data analysis\\Cyclistic dataset\\202101-divvy-tripdata.csv")
feb_df = read_csv("C:\\Users\\Dewoyin\\Documents\\Datasets for data analysis\\Cyclistic dataset\\202102-divvy-tripdata.csv")
mar_df = read_csv("C:\\Users\\Dewoyin\\Documents\\Datasets for data analysis\\Cyclistic dataset\\202103-divvy-tripdata.csv")
apr_df = read_csv("C:\\Users\\Dewoyin\\Documents\\Datasets for data analysis\\Cyclistic dataset\\202104-divvy-tripdata.csv")
may_df = read_csv("C:\\Users\\Dewoyin\\Documents\\Datasets for data analysis\\Cyclistic dataset\\202105-divvy-tripdata.csv")
jun_df = read_csv("C:\\Users\\Dewoyin\\Documents\\Datasets for data analysis\\Cyclistic dataset\\202106-divvy-tripdata.csv")
jul_df = read_csv("C:\\Users\\Dewoyin\\Documents\\Datasets for data analysis\\Cyclistic dataset\\202107-divvy-tripdata.csv")
aug_df = read_csv("C:\\Users\\Dewoyin\\Documents\\Datasets for data analysis\\Cyclistic dataset\\202108-divvy-tripdata.csv")
sep_df = read_csv("C:\\Users\\Dewoyin\\Documents\\Datasets for data analysis\\Cyclistic dataset\\202109-divvy-tripdata.csv")
oct_df = read_csv("C:\\Users\\Dewoyin\\Documents\\Datasets for data analysis\\Cyclistic dataset\\202110-divvy-tripdata.csv")
nov_df = read_csv("C:\\Users\\Dewoyin\\Documents\\Datasets for data analysis\\Cyclistic dataset\\202111-divvy-tripdata.csv")
dec_df = read_csv("C:\\Users\\Dewoyin\\Documents\\Datasets for data analysis\\Cyclistic dataset\\202112-divvy-tripdata.csv")


# WRANGLE DATA AND COMBINE INTO A SINGLE FILE

# Check the consistency of each month's column names

colnames(jan_df)
colnames(feb_df)
colnames(mar_df)
colnames(apr_df)
colnames(may_df)
colnames(jun_df)
colnames(jul_df)
colnames(aug_df)
colnames(sep_df)
colnames(oct_df)
colnames(nov_df)
colnames(dec_df)


# Merge all months data frames into one year(2021) view

cyclistic_2021_df <- rbind (jan_df, feb_df, mar_df, apr_df, may_df, jun_df, jul_df, aug_df, sep_df, oct_df, nov_df, dec_df)


# Remove monthly data frames to clear space in the environment

remove(jan_df, feb_df, mar_df, apr_df, may_df, jun_df, jul_df, aug_df, sep_df, oct_df, nov_df, dec_df)


# Make a copy of the year data frame

cyclistic_df <- cyclistic_2021_df
head(cyclistic_df)


# Inspect the data frames and look for incongruencies

str(cyclistic_df)


# Remove columns not needed: ride_id, start_station_id, end_station_id, start_lat, start_long, end_lat, end_lng

cyclistic_df <- cyclistic_df %>%  
  select(-c(ride_id, start_station_id, end_station_id,start_lat,start_lng,end_lat,end_lng))
  head(cyclistic_df)


# Add new columns for date, hour, day, month, and year for further calculations

cyclistic_df$date <- as.Date(cyclistic_df$started_at)                # default format is yyyy-mm-dd
cyclistic_df$day_of_week <- format(as.Date(cyclistic_df$date), "%A") # column for day of week
cyclistic_df$month <- format(as.Date(cyclistic_df$date), "%m")       # column for month
cyclistic_df$day <- format(as.Date(cyclistic_df$date), "%d")         # column for day
cyclistic_df$year <- format(as.Date(cyclistic_df$date), "%Y")        # column for year

fortime <- as.POSIXlt(cyclistic_df$started_at,format="%d/%m/%Y %H:%M")
cyclistic_df$time <- format(fortime, format = "%H:%M")                 #format time as HH:MM:SS
cyclistic_df$hour <- fortime$hour


# Create a calculated ride length column by subtracting ended_at time from started_at time and converted it to minutes

cyclistic_df$ride_length <- minute(cyclistic_df$ride_length)
cyclistic_df$ride_length <- round(cyclistic_df$ride_length, digits = 1)


# Create a column for the month using the full month name

cyclistic_df <-cyclistic_df %>% mutate(month = 
                                             case_when(month == "01" ~ "January",
                                                       month == "02" ~ "February",
                                                       month == "03" ~ "March",
                                                       month == "04" ~ "April",
                                                       month == "05" ~ "May",
                                                       month == "06" ~ "June",
                                                       month == "07" ~ "July",
                                                       month == "08" ~ "August",
                                                       month == "09" ~ "September",
                                                       month == "10" ~ "October",
                                                       month == "11" ~ "November",
                                                       month == "12" ~ "December")
                                             
) 

# Create column for different time_of_day(Night, Morning, Afternoon, Evening)

cyclistic_df <-cyclistic_df %>% mutate(time_of_day = 
                                             case_when(hour == "0" ~ "Night",
                                                       hour == "1" ~ "Night",
                                                       hour == "2" ~ "Night",
                                                       hour == "3" ~ "Night",
                                                       hour == "4" ~ "Night",
                                                       hour == "5" ~ "Night",
                                                       hour == "6" ~ "Morning",
                                                       hour == "7" ~ "Morning",
                                                       hour == "8" ~ "Morning",
                                                       hour == "9" ~ "Morning",
                                                       hour == "10" ~ "Morning",
                                                       hour == "11" ~ "Morning",
                                                       hour == "12" ~ "Afternoon",
                                                       hour == "13" ~ "Afternoon",
                                                       hour == "14" ~ "Afternoon",
                                                       hour == "15" ~ "Afternoon",
                                                       hour == "16" ~ "Afternoon",
                                                       hour == "17" ~ "Afternoon",
                                                       hour == "18" ~ "Evening",
                                                       hour == "19" ~ "Evening",
                                                       hour == "20" ~ "Evening",
                                                       hour == "21" ~ "Evening",
                                                       hour == "22" ~ "Evening",
                                                       hour == "23" ~ "Evening")
)


# Create column for different season (Spring, Summer, Fall, Winter)

cyclistic_df <-cyclistic_df %>% mutate(season = 
                                             case_when(month == "January" ~ "winter",
                                                       month == "Febuary" ~ "winter",
                                                       month == "March" ~ "spring",
                                                       month == "April" ~ "spring",
                                                       month == "May" ~ "spring",
                                                       month == "June" ~ "summer",
                                                       month == "July" ~ "summer",
                                                       month == "August" ~ "summer",
                                                       month == "September" ~ "fall",
                                                       month == "October" ~ "fall",
                                                       month == "November" ~ "fall",
                                                       month == "December" ~ "winter")
  
)


# CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS

# Inspect the new table that has been created and clean data

cyclistic_df <- na.omit(cyclistic_df)  #remove rows with NA values
cyclistic_df <- distinct(cyclistic_df) #remove duplicate rows
colnames(cyclistic_df)                   #List of column names
nrow(cyclistic_df)                       # Total rides
dim(cyclistic_df)                        #Dimensions of the data frame
str(cyclistic_df)                        #See list of columns and data types (numeric, character, etc)
summary(cyclistic_df)                    #Statistical summary of data. Mainly for numerics


# Convert "ride_length"to numeric so we can run calculations on the data

cyclistic_df$ride_length <- as.numeric(as.character(cyclistic_df$ride_length))
is.numeric(cyclistic_df$ride_length)


  
# Remove bad data where ride_length is 0 or negative <=0

cyclistic_df <- cyclistic_df[!(cyclistic_df$ride_length <=0),]


# Statistical summary of the ride_length column

summary(cyclistic_df$ride_length)


# view the full dataset

View(cyclistic_df)


# Compare members and casual users

aggregate(cyclistic_df$ride_length ~ cyclistic_df$member_casual, FUN = mean)
aggregate(cyclistic_df$ride_length ~ cyclistic_df$member_casual, FUN = median)
aggregate(cyclistic_df$ride_length ~ cyclistic_df$member_casual, FUN = max)
aggregate(cyclistic_df$ride_length ~ cyclistic_df$member_casual, FUN = min)


# Rearrange weekdays from sunday to saturday

cyclistic_df$day_of_week <- ordered(cyclistic_df$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))


# Average ride time by each day for members vs casual users

aggregate(cyclistic_df$ride_length ~ cyclistic_df$member_casual + cyclistic_df$day_of_week, FUN = mean)


# Total rides per year by member / casual riders

rides_per_year <- cyclistic_df %>% 
  group_by(member_casual) %>% 
  summarise(number_of_rides = n()) %>%
  arrange(member_casual)
head(rides_per_year, 10)

# Let's visualize total rides per year by member / casual riders

rides_per_year %>% 
  ggplot(aes(x = member_casual, 
             y = number_of_rides, 
             fill = member_casual)) + 
  geom_bar(stat = "identity", color = "black", alpha = 0.7) +
  labs(title = 'Total Rides per Year', subtitle = 'Casual vs Member Riders', x ='Riders', y = 'Total rides') + 
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  theme(legend.position = "none")+
  geom_text(aes(label = number_of_rides), vjust = -0.25, size = 3) +
  scale_fill_manual(values = c("#d7191c", "#2b83ba"))
  


# Total rides per month by member / casual riders

cyclistic_df$month <- ordered(cyclistic_df$month, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")) #rearrange months from January to December
rides_per_month <- cyclistic_df %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n()) %>%
  arrange(member_casual, month)
head(rides_per_month, 10)

# Let's visualize total rides per month by member / casual riders

rides_per_month %>% 
  ggplot(aes(x = month, 
             y = number_of_rides, 
             group = member_casual)) + 
  geom_line(aes(color = member_casual), size = 1) +
  geom_point(aes(color = member_casual))+
  labs(title = 'Total Rides per Month', subtitle = 'Casual vs Member Riders', x ='Months', y = 'Total rides') + 
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("#d7191c", "#2b83ba"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(legend.position ="bottom")
  


# Total rides per seasons by member / casual riders

rides_per_season <- cyclistic_df %>% 
  group_by(member_casual, season) %>% 
  summarise(number_of_rides = n()) %>%
  arrange(member_casual, season)
head(rides_per_season, 10)


# Let's visualize total rides per season by member / casual riders

rides_per_season %>% 
  ggplot(aes(x = season, 
             y = number_of_rides, 
             fill = member_casual)) + 
  geom_bar(stat = "identity",position = "dodge", color = "black", alpha = 0.7) +
  labs(title = 'Total Rides per Season', subtitle = 'Casual vs Member Riders', x ='Season', y = 'Total rides') + 
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  geom_text(aes(label = number_of_rides), vjust = -0.25, size = 3, position = position_dodge(0.9)) +
  scale_fill_manual(values = c("#d7191c", "#2b83ba"))+
  theme(legend.position = 'bottom')


# Total rides per weekday by member / casual riders

rides_per_weekday <- cyclistic_df %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n()) %>%
  arrange(member_casual, day_of_week)
head(rides_per_weekday, 10)


# Let's visualize total rides per weekday by member / casual riders

rides_per_weekday %>% 
  ggplot(aes(x =day_of_week, 
             y = number_of_rides, 
             fill = member_casual)) + 
  geom_bar(stat = "identity",position = "dodge", color = "black", alpha = 0.7) +
  labs(title = 'Total Rides per weekday', subtitle = 'Casual vs Member Riders', x ='weekday', y = 'Total rides') + 
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  geom_text(aes(label = number_of_rides), vjust = -0.25, size = 3, position = position_dodge(0.9)) +
  scale_fill_manual(values = c("#d7191c", "#2b83ba"))+
  theme(legend.position = 'bottom')
  

# Total rides per time of the day by member / casual riders

rides_per_daytime <- cyclistic_df %>% 
  group_by(member_casual, time_of_day) %>% 
  summarise(number_of_rides = n()) %>%
  arrange(member_casual, time_of_day)
head(rides_per_daytime, 10)


# Let's visualize total rides per time of day by member / casual riders

rides_per_daytime %>% 
  ggplot(aes(x = time_of_day, 
             y = number_of_rides, 
             fill = member_casual)) + 
  geom_bar(stat = "identity",position = "dodge", color = "black", alpha = 0.7) +
  labs(title = 'Total Rides per Time of day', subtitle = 'Casual vs Member Riders', x ='Time of the day', y = 'Total rides') + 
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  geom_text(aes(label = number_of_rides), vjust = -0.25, size = 3, position = position_dodge(0.9)) +
  scale_fill_manual(values = c("#d7191c", "#2b83ba"))+
  theme(legend.position = 'bottom')
  

# Bike preference by riders

users_bike_type <- cyclistic_df %>% 
  group_by(member_casual, rideable_type) %>% 
  summarise(number_of_rides = n()) %>%
  arrange(member_casual, rideable_type)
head(users_bike_type, 10)


# Let's visualize total rides per bike type by member / casual riders

users_bike_type %>% 
  ggplot(aes(x = rideable_type, 
             y = number_of_rides, 
             fill = member_casual)) + 
  geom_bar(stat = "identity",position = "dodge", color = "black", alpha = 0.7) +
  labs(title = 'Total Rides by Bike Type', subtitle = 'Casual vs Member Riders', x ='Bike Types', y = 'Total rides') + 
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  geom_text(aes(label = number_of_rides), vjust = -0.25, size = 3, position = position_dodge(0.9)) +
  scale_fill_manual(values = c("#d7191c", "#2b83ba"))+
  theme(legend.position = 'bottom')



# Average trip duration per bike type by member / casual riders

duration_per_biketype <- cyclistic_df %>% 
  group_by(member_casual, rideable_type) %>% 
  summarise(average_duration = mean(ride_length)) %>%
  arrange(member_casual, rideable_type)
head(duration_per_biketype, 10)


# Let's visualize average trip duration per bike type by member / casual riders

duration_per_biketype %>% 
  ggplot(aes(x = rideable_type, 
             y = average_duration, 
             fill = member_casual)) + 
  geom_bar(stat = "identity",position = "dodge", color = "black", alpha = 0.7) +
  labs(title = 'Average Trip Duration per Bike Type', subtitle = 'Casual vs Member Riders', x ='Bike Types', y = 'Avg. Trip Duration in Minutes') + 
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  geom_text(aes(label = round(average_duration, digits =1)), vjust = -0.25, size = 3, position = position_dodge(0.9)) +
  scale_fill_manual(values = c("#d7191c", "#2b83ba"))+
  theme(legend.position = 'bottom')


# Average trip duration per year by member / casual Riders

duration_per_year <- cyclistic_df %>% 
group_by(member_casual) %>% 
  summarise(average_duration = mean(ride_length)) %>%
  arrange(member_casual)
head(duration_per_year, 10)


# Let's visualize average trip duration per year by member / casual riders

duration_per_year %>% 
  ggplot(aes(x = member_casual, 
             y = average_duration, 
             fill = member_casual)) + 
  geom_bar(stat = "identity", color = "black", alpha = 0.7) +
  labs(title = 'Average trip duration by Riders', subtitle = 'Casual vs Member Riders', x ='Riders', y = 'Avg. Trip Durations in minutes') + 
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  theme(legend.position = "none")+
  geom_text(aes(label = round(average_duration, digits =1)), vjust = -0.25, size = 3) +
  scale_fill_manual(values = c("#d7191c", "#2b83ba"))


# Average trip duration per month by member / casual Riders

duration_per_month <- cyclistic_df %>% 
  group_by(member_casual, month) %>% 
  summarise(average_duration = mean(ride_length)) %>%
  arrange(member_casual, month)
head(duration_per_month, 12)


# Let's visualize average trip duration per month by member / casual riders

duration_per_month %>% 
  ggplot(aes(x = month, 
             y = average_duration, 
             group = member_casual)) + 
  geom_line(aes(color = member_casual), size = 1) +
  geom_point(aes(color = member_casual))+
  labs(title = 'Average Trip Duration per Month', subtitle = 'Casual vs Member Riders', x ='Months', y = 'Avg. Trip Duration in Minutes') + 
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("#d7191c", "#2b83ba"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(legend.position ="bottom")


# Average trip duration per season by member / casual Riders

duration_per_season <- cyclistic_df %>% 
  group_by(member_casual, season) %>% 
  summarise(average_duration = mean(ride_length)) %>%
  arrange(member_casual, season)
head(duration_per_season, 10)


# Let's visualize average trip duration per season by member / casual riders

duration_per_season %>% 
  ggplot(aes(x = season, 
             y = average_duration, 
             fill = member_casual)) + 
  geom_bar(stat = "identity",position = "dodge", color = "black", alpha = 0.7) +
  labs(title = 'Average Trip Duration per season', subtitle = 'Casual vs Member Riders', x ='season', y = 'Avg. Trip Duration in Minutes') + 
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  geom_text(aes(label = round(average_duration, digits =1)), vjust = -0.25, size = 3, position = position_dodge(0.9)) +
  scale_fill_manual(values = c("#d7191c", "#2b83ba"))+
  theme(legend.position = 'bottom')


# Average trip duration per weekday by member / casual Riders

duration_per_weekday <- cyclistic_df %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(average_duration = mean(ride_length)) %>%
  arrange(member_casual, day_of_week)
head(duration_per_weekday, 10)


# Let's visualize average trip duration per weekday by member / casual riders

duration_per_weekday %>% 
  ggplot(aes(x = day_of_week, 
             y = average_duration, 
             fill = member_casual)) + 
  geom_bar(stat = "identity",position = "dodge", color = "black", alpha = 0.7) +
  labs(title = 'Average Trip Duration per weekday', subtitle = 'Casual vs Member Riders', x ='weekday', y = 'Avg. Trip Duration in Minutes') + 
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  geom_text(aes(label = round(average_duration, digits = 1)), vjust = -0.25, size = 3, position = position_dodge(0.9)) +
  scale_fill_manual(values = c("#d7191c", "#2b83ba"))+
  theme(legend.position = 'bottom')


# Average trip duration per daytime by member / casual Riders

duration_per_daytime <- cyclistic_df %>% 
  group_by(member_casual, time_of_day) %>% 
  summarise(average_duration = mean(ride_length)) %>%
  arrange(member_casual, time_of_day)
head(duration_per_daytime, 10)


# Let's visualize average trip duration per daytime by member / casual riders

duration_per_daytime %>% 
  ggplot(aes(x = time_of_day, 
             y = average_duration, 
             fill = member_casual)) + 
  geom_bar(stat = "identity",position = "dodge", color = "black", alpha = 0.7) +
  labs(title = 'Average Trip Duration per daytime', subtitle = 'Casual vs Member Riders', x ='time of the day', y = 'Avg. Trip Duration in Minutes') + 
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  geom_text(aes(label = round(average_duration, digits =1)), vjust = -0.25, size = 3, position = position_dodge(0.9)) +
  scale_fill_manual(values = c("#d7191c", "#2b83ba"))+
  theme(legend.position = 'bottom')


# Top start stations for casual riders

top_start_staions <- cyclistic_2021_df %>% 
  group_by(member_casual, start_station_name) %>% 
  summarise(number_of_rides = n()) %>% 
  filter(start_station_name != "" & member_casual == "casual") %>% 
  arrange(-number_of_rides)
head(top_start_staions, 10)


# Top start stations for member riders

top_start_staions <- cyclistic_2021_df %>% 
  group_by(member_casual, start_station_name) %>% 
  summarise(number_of_rides = n()) %>% 
  filter(start_station_name != "" & member_casual == "member") %>% 
  arrange(-number_of_rides)
head(top_start_staions, 10)


# EXPORT SUMMARY FILE FOR FURTHER ANALYSIS

cyclistic_tableau <- cyclistic_df

write.csv(cyclistic_tableau, file = "C:\\Users\\Dewoyin\\Documents\\Projects\\Google Capstone Project\\cyclistic_tableau.csv")