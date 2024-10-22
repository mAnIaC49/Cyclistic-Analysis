library(dplyr)
library(readr)
library(writexl)
# Accessing the data

oct23 <- read_csv("csv files/202310-divvy-tripdata.csv")
nov23 <- read_csv("csv files/202311-divvy-tripdata.csv")
dec23 <- read_csv("csv files/202312-divvy-tripdata.csv")
jan24 <- read_csv("csv files/202401-divvy-tripdata.csv")
feb24 <- read_csv("csv files/202402-divvy-tripdata.csv")
mar24 <- read_csv("csv files/202403-divvy-tripdata.csv")
apr24 <- read_csv("csv files/202404-divvy-tripdata.csv")
may24 <- read_csv("csv files/202405-divvy-tripdata.csv")
jun24 <- read_csv("csv files/202406-divvy-tripdata.csv")
jul24 <- read_csv("csv files/202407-divvy-tripdata.csv")
aug24 <- read_csv("csv files/202408-divvy-tripdata.csv")
sep24 <- read_csv("csv files/202409-divvy-tripdata.csv")

# PROCESS THE DATA

# Combining the data
combined <- rbind(oct23, nov23, dec23, jan24, feb24, mar24, apr24, may24, jun24,
                  jul24, aug24, sep24)

# clearing up the environment from the data of individual months
remove(oct23, nov23, dec23, jan24, feb24, mar24, apr24, may24, jun24, jul24, 
       aug24, sep24)

# Add new columns
combined$ride_length <- combined$ended_at-combined$started_at # ride length
combined$day_of_week <- weekdays(combined$started_at) # day of the week
combined$hour <- format(combined$started_at, "%H") # hour
combined$day <- format(combined$started_at, "%d") # day
combined$month <- format(combined$started_at, "%B") # month


# Rounding up the ride_length column to the nearest whole number
combined$ride_length <- round(combined$ride_length)

combined2 <- combined #Backup

# Check for the total number of NA values in each column
colSums(is.na(combined))

# Clean the data

combined <- distinct(combined) #Remove duplicates
combined <- subset(combined, !ride_length<=300) #Remove rows with ride_length less than 5 minutes
combined$start_station_name <- trimws(combined$start_station_name) #Trim whitespace
combined$start_station_name <- tolower(combined$start_station_name) #Convert all characters to lowercase
combined$end_station_name <- trimws(combined$end_station_name)
combined$end_station_name <- tolower(combined$end_station_name)

# ANALYZE AND SHARE

# Number of rides and Mean, Median Max and standard deviation of ride_length
length(combined$ride_length)
mean(combined$ride_length)
median(combined$ride_length)
max(combined$ride_length)
sd(combined$ride_length)


# Total number of rides and average ride length of the users by day of the week
combined |>
  group_by(day_of_week) |>
  summarise(number_of_rides=n(),
            mean_ride_length=mean(ride_length),
            median_ride_length=median(ride_length),
            sd_ride_length=sd(ride_length))|>
            arrange(desc(number_of_rides))|>
            ungroup()
  
# Total number of rides and average ride length of users by month of the year
combined |>
  group_by(month) |>
  summarise(number_of_rides=n(),
            mean_ride_length=mean(ride_length),
            median_ride_length=median(ride_length),
            sd_ride_length=sd(ride_length))|>
            arrange(desc(number_of_rides))|>
            ungroup()

# Total number of rides and average ride length of users by day of the month
combined |>
  group_by(day) |>
  summarise(number_of_rides=n(),
            mean_ride_length=mean(ride_length),
            median_ride_length=median(ride_length),
            sd_ride_length=sd(ride_length))|>
            arrange(desc(number_of_rides))|>
            ungroup()|>
            print(n=Inf)

# Total number of rides and average ride length of users by hours of the day
combined |>
  group_by(hour) |>
  summarise(number_of_rides=n(),
            mean_ride_length=mean(ride_length),
            median_ride_length=median(ride_length),
            sd_ride_length=sd(ride_length))|>
            arrange(desc(number_of_rides))|>
            ungroup()|>
            print(n=Inf)



# Create tables comparing casual and member riders for visualization


# Total rides, Mean, Median and Standard Deviation of ride_length for casual and member riders
overall <- combined |>                            
  group_by(member_casual) |>
  summarise(number_of_rides=n(),
            mean_ride_length=mean(ride_length),
            median_ride_length=median(ride_length),
            sd_ride_length=sd(ride_length))|>
  arrange(desc(number_of_rides))|>
  ungroup()

# Comparing casual and member riders by day of the week
day_of_week <- combined |>
  mutate(day_of_week=factor(day_of_week, 
  levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
           "Saturday", "Sunday"))) |> # This step ensures that the days are not arranged alphabetically
  group_by(member_casual, day_of_week) |>
  summarise(number_of_rides=n(), avg_ride_length=mean(ride_length)) |>
  arrange(day_of_week, member_casual)

# Comparing casual and member riders by month of the year
months <- combined |>
  mutate(month=factor(month, 
  levels=c("January", "February", "March", "April", "May", "June", "July", "August",
           "September", "October", "November", "December"))) |>
  group_by(member_casual, month) |>
  summarise(number_of_rides=n(), avg_ride_length=mean(ride_length)) |>
  arrange(month, member_casual)

# Comparing casual and member riders by days of a month
days <- combined |>
  group_by(member_casual, day) |>
  summarise(number_of_rides=n(), avg_ride_length=mean(ride_length)) |>
  arrange(day, member_casual)
  
# Comparing casual and member riders by hours of a day
hours <- combined |>
  group_by(member_casual, hour) |>
  summarise(number_of_rides=n(), avg_ride_length=mean(ride_length)) |>
  arrange(hour, member_casual)

# Create a table of location data

# Calculate the average latitude and longitude for each station (both start and end)
station_coords <- combined |>
  select(start_station_name, start_lat, start_lng) |>
  rename(station_name = start_station_name, lat = start_lat, lng = start_lng) |>
  bind_rows(
    combined |>
      select(end_station_name, end_lat, end_lng) |>
      rename(station_name = end_station_name, lat = end_lat, lng = end_lng)
  ) |>
  group_by(station_name) |>
  summarise(
    avg_lat = mean(lat, na.rm = TRUE),
    avg_lng = mean(lng, na.rm = TRUE)
  ) |>
  filter(!is.na(station_name))

# Count the number of rides starting and ending at each station by rider type (casual/member)
station_rides <- combined |>
  group_by(member_casual, start_station_name) |>
  rename(station_name=start_station_name)|>
  summarise(
    start_rides = n(),
    .groups = 'drop'
  ) |>
  full_join(
    combined |>
      group_by(member_casual, end_station_name) |>
      summarise(
        end_rides = n(),
        .groups = 'drop'
      ),
    by = c("member_casual", "station_name" = "end_station_name")
  )

# Merge the latitude and longitude information with the ride counts
location_table <- station_rides |>
  left_join(station_coords, by = "station_name")

# Combine the tables into a list for writing into excel
data_list <- list("Days of a Week"=day_of_week, "Months"=months,
                  "Days of a Month"=days, "Hours of a Day"=hours, "Location"=location_table)

# Write to Excel
write_xlsx(data_list, "Cyclistic_analysis.xlsx")
