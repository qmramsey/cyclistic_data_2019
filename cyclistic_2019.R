# Import Libraries
library(tidyverse)

# Read CSV files
quarter_1 <- read_csv("Data\\Divvy_Trips_2019_Q1.csv")
glimpse(quarter_1)
quarter_2 <- read_csv("Data\\Divvy_Trips_2019_Q2.csv")
glimpse(quarter_2)
quarter_3 <- read_csv("Data\\Divvy_Trips_2019_Q3.csv")
glimpse(quarter_3)
quarter_4 <- read_csv("Data\\Divvy_Trips_2019_Q4.csv")
glimpse(quarter_4)

# Rename column names from quarter_2.csv
colnames(quarter_2) <- colnames(quarter_1)

# Unite into 2019 full-year spreadsheet
first_half <- union_all(quarter_1, quarter_2)
second_half <- union_all(quarter_3, quarter_4)
full_2019 <- union_all(first_half, second_half)
glimpse(full_2019)

library(scales)

# Bar chart showing gender
ggplot(data = full_2019) +
    geom_bar(mapping = aes(x = gender, fill = usertype)) +
    annotate("text", x = 1, y = 726539 / 2, label = "726,539", size = 5) +
    annotate("text", x = 1, y = 726539 + (131439 / 2), label = "131,439", size = 5) +
    annotate("text", x = 2, y = 2188077 / 2, label = "2,188,077", size = 5) +
    annotate("text", x = 2, y = 2188077 + (212743 / 2), label = "212,743", size = 5) +
    annotate("text", x = 3, y = 22751 * 2, label = "22,751", size = 3) +
    annotate("text", x = 3, y = 22751 + (536455 / 2), label = "536,455", size = 5) +
    scale_y_continuous(labels = scales::comma) +
    xlab("Reported Gender") +
    ylab("Number of Riders") +
    labs(fill = "User Type", size = 12) +
    ggtitle("Number of Riders by Reported Gender and User Type")

#switch to see the usertype
ggplot(data = full_2019) +
    geom_bar(mapping = aes(x = usertype, fill = gender)) +
    annotate("text", x = 1, y = 536455 / 2, label = "536,455", size = 8) +
    annotate("text", x = 1, y = 536455 + (212743 / 2), label = "212,743", size = 7) +
    annotate("text", x = 1, y = 536455 + 212743 + (131439 / 2), label = "131,439", size = 5) +
    annotate("text", x = 2, y = 22751 * 2, label = "22,751", size = 5) +
    annotate("text", x = 2, y = 22751 + (2188077 / 2), label = "2,188,077", size = 8) +
    annotate("text", x = 2, y = 22751 + 2188077 + (726539 / 2), label = "726,539", size = 8) +
    scale_y_continuous(labels = scales::comma) +
    xlab("User Type") +
    ylab("Number of Riders") +
    labs(fill = "Reported Gender") +
    ggtitle("Number of Riders by User Type and Gender")

# Calculate the age of the riders using the service and remove NA
full_2019$age_appx <- 2019 - full_2019$birthyear
filtered_age <- full_2019 %>%
    filter(age_appx < 100 & age_appx > 0 & age_appx != "NA")

# Plot age data
ggplot(data = filtered_age) +
    geom_histogram(mapping = aes(x = age_appx),
    fill = "#582a87", color = 'black', binwidth = 1, center = 0.5) + 
    annotate("text", x = 26.5, y = 210000, label = "Age: 26", size = 5) +
    scale_y_continuous(labels = scales::comma) +
    xlab("Approximate Age (2019 - Birth Year)") +
    ylab("Number of Riders") +
    ggtitle("Histogram of the Approximate Age of the Rider")


# Split Histogram by user type
ggplot(data = filtered_age) +
    geom_histogram(mapping = aes(x = age_appx, fill = usertype),
    color = 'black', binwidth = 1, center = 0.5) + 
    facet_wrap(~usertype) + 
    scale_y_continuous(labels = scales::comma) +
    xlab("Approximate Age (2019 - Birth Year)") +
    ylab("Number of Riders") +
    labs(fill = "User Type") +
    ggtitle("Histogram of the Approximate Age of the Rider",
        subtitle = "Split by User Type")


# Split histogram by gender after dropping "NA" gender
filtered_age <- filtered_age %>% filter(gender != "NA")
ggplot(data = filtered_age) +
    geom_histogram(mapping = aes(x = age_appx, fill = gender),
    color = 'black', binwidth = 1, center = 0.5) +
    facet_wrap(~gender) +
    scale_y_continuous(labels = scales::comma) +
    xlab("Approximate Age (2019 - Birth Year)") +
    ylab("Number of Riders") +
    labs(fill = "Reported Gender") +
    ggtitle("Histogram of the Approximate Age of the Rider",
        subtitle = "Split by Gender")


# Turn Start Times into Dates
full_2019$start_date <- as.Date(full_2019$start_time)

# Group to find the number of trips each day of the year by start time
date_grouped <- full_2019 %>%
    group_by(start_date) %>%
    summarize(daily_rides = n())

# Create a column that shows which day of the week each date is
date_grouped$day_of_week <- weekdays(date_grouped$start_date) %>%
    factor(levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
date_grouped

# separate out the customers for daily rides
customer_rides <- full_2019 %>%
    filter(usertype == "Customer") %>%
    group_by(start_date) %>%
    summarize(daily_rides = n())
customer_rides

# Seperate out the subscribers for daily rides
subscriber_rides <- full_2019 %>%
    filter(usertype == "Subscriber") %>%
    group_by(start_date) %>%
    summarize(daily_rides = n())
subscriber_rides

# Turn into long format to be used for line plot
to_pivot <- data.frame(date = date_grouped$start_date,
                      totals = date_grouped$daily_rides,
                      cust = customer_rides$daily_rides,
                      subs = subscriber_rides$daily_rides)
longer <- to_pivot %>% pivot_longer(cols = c("totals", "cust", "subs"),
                                    names_to = "usertype",
                                    values_to = "daily_rides")

# Plot total daily rides, customer rides, and subsciber rides together.
ggplot(data = longer, aes(x = date, y = daily_rides)) +
    geom_line(aes(color = usertype)) +
    scale_color_manual(
            name = "User Type",
            labels = c("Customer", "Subscriber", "Total"),
            values = c("#F8766D", "#619CFF", "#000000")) +
    xlab("Ride Start Date") +
    scale_x_continuous(
        breaks = as.Date(
                 c("2019-01-01", "2019-02-01", "2019-03-01",
                   "2019-04-01", "2019-05-01", "2019-06-01",
                   "2019-07-01", "2019-08-01", "2019-09-01",
                   "2019-10-01", "2019-11-01", "2019-12-01")),
        labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) + 
    ylab("Number of Recoded Rides") +
    scale_y_continuous(labels = scales::comma) +
    ggtitle("Number of Total Rides per Day")


# Same as last, but wil smoothed lines
ggplot(data = longer, aes(x = date, y = daily_rides)) +
    geom_smooth(aes(color = usertype)) +
    scale_color_manual(
            name = "User Type",
            labels = c("Customer", "Subscriber", "Total"),
            values = c("#F8766D", "#619CFF", "#000000")) +
    xlab("Ride Start Date") +
    scale_x_continuous(
        breaks = as.Date(
                 c("2019-01-01", "2019-02-01", "2019-03-01",
                   "2019-04-01", "2019-05-01", "2019-06-01",
                   "2019-07-01", "2019-08-01", "2019-09-01",
                   "2019-10-01", "2019-11-01", "2019-12-01")),
        labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) + 
    ylab("Number of Recoded Rides") +
    scale_y_continuous(labels = scales::comma) +
    ggtitle("Number of Total Rides per Day (Smoothed)")


# set an "order" for the days of the week other than alphabetical
full_2019$weekday <- weekdays(full_2019$start_date) %>%
    factor(levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Plot number of rides based on the day of the week
ggplot(data = full_2019) +
    geom_bar(mapping = aes(x = weekday),
    fill = c("#333333", "#808080",  "#808080",  "#808080", 
             "#808080",  "#808080", "#333333")) +
    xlab("Day of the Week") +
    ylab("Number of Rides") +
    scale_y_continuous(labels = scales::comma) +
    ggtitle("Number of Rides Each Day of the Week")

# Finding out the minimum and maximum ride dates 
mean(date_grouped$daily_rides)
min(date_grouped$daily_rides)
max(date_grouped$daily_rides)

# What date was the largest bike-share ride day?
biggest <- date_grouped %>%
    filter(daily_rides == max(daily_rides))
biggest$start_date

# What date was the smallest bike-share ride day?
smallest <- date_grouped %>%
    filter(daily_rides == min(daily_rides))
smallest$start_date

# dreate a histogram of the number of daily rides to see if the average is acceptable
ggplot(data = date_grouped) +
    geom_histogram(mapping = aes(x = daily_rides),
    color = "black", fill = "#582a87", binwidth = 800, center=400) +
    xlab("Number of Daily Rides") +
    ylab("Number of Days With Each Ride Count") +
    ggtitle("Histogram of the Number of Days with Each Ride Count") +
    scale_x_continuous(labels = scales::comma)

# Create facet histogram that shows daily number of rides split by day of the week
ggplot(data = date_grouped) +
    geom_histogram(mapping = aes(x = daily_rides, fill = day_of_week), color = "black", bins = 15L) +
    facet_wrap(~day_of_week) +
    xlab("Number of Daily Rides (Thousands)") +
    scale_x_continuous(
        breaks = c(0, 5000, 10000, 15000, 20000),
        labels = c(0, 5, 10, 15, 20)) +
    ylab("Number of Days With Each Ride Count") +
    labs(fill = "Day of the Week") +
    ggtitle("Number of Days with Each Ride Count",
        subtitle = "Facet Histogram Split by Day of the Week")


# Create a season column using the date.
date_grouped$season <- ifelse(date_grouped$start_date < "2019-03-20", "Winter",
    ifelse(date_grouped$start_date < "2019-06-21", "Spring",
    ifelse(date_grouped$start_date < "2019-09-23", "Summer",
    ifelse(date_grouped$start_date < "2019-12-21", "Autumn", "Winter")))) %>%
    factor(levels = c("Winter", "Spring", "Summer", "Autumn"))

# Create a facet histogram for each season of the year
ggplot(data = date_grouped) +
    geom_histogram(mapping = aes(x = daily_rides, fill = season),
        color = "black", binwidth = 800, center = 400) +
    facet_wrap(~season) +
    scale_fill_manual(values = c("#00ccff", "#00b300", "#ffff00", "#ff6600")) +
    xlab("Number of Daily Rides") +
    ylab("Number of Days With Each Ride Count") +
    labs(fill = "Season") + 
    scale_x_continuous(labels = scales::comma)

# Create a column for the months from the date
date_grouped$month <- date_grouped$start_date %>%
    month()

# Plot the data based on month
ggplot(data = date_grouped) +
    geom_histogram(mapping = aes(x = daily_rides, fill = month), color = "black", bins = 12L) +
    facet_wrap(~month) +
    scale_x_continuous(
        breaks = c(0, 5000, 10000, 15000, 20000, 25000),
        labels = c(0, 5, 10, 15, 20, 25)) +
        xlab("Number of Daily Rides (Thousands)") +
        ylab("Number of Days with Each Count") +
        labs(fill = "Month")
        ggtitle("Histogram of Number of Days with Each Daily Ride Count",
                subtitle = "Facet Histogram Split by Month")

# create a table showing the averages for each season
season_average <-date_grouped %>%
    group_by(season) %>%
    summarize(average_daily = mean(daily_rides))
season_average

# Turn the datetime format into just the number of seconds from the time (only hours and minutes)
full_2019$seconds_time <- as.numeric(hm(substr(full_2019$start_time, 12, 16)))

# Create a histogram of the time of day the rides are happening
ggplot(data = full_2019) +
    geom_histogram(mapping = aes(x = seconds_time),
    binwidth = 1800, center = 900, color = "black", fill = "#582a87") +
    annotate("text", x = 30000, y = 170000, label = "8-8:30am", size = 5) +
    annotate("text", x = 62500, y = 270000, label = "5-5:30pm", size = 5) +
    scale_x_continuous(
        breaks = c(0, 3600, 7200, 10800, 14400, 18000,
                21600, 25200, 28800, 32400, 36000, 39600,
                43200, 46800, 50400, 54000, 57600, 61200,
                64800, 68400, 72000, 75600, 79200, 82800, 86400),
        labels = c("12AM", 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, "12PM", 1,
                    2, 3, 4, 5, 6, 7, 8, 9, 10, 11, "12AM")) +
    xlab("Time") +
    ylab("Number of Recoded Rides") +
    scale_y_continuous(labels = scales::comma) +
    ggtitle("The Number of Recorded Rides Every Half Hour", subtitle = "Total Across the Year")

# Split the histogram by usertype
ggplot(data = full_2019) +
    geom_histogram(mapping = aes(x = seconds_time, fill = usertype),
    binwidth = 1800, center = 900, color = "black") +
    facet_wrap(~usertype) + 
    scale_x_continuous(
        breaks = c(0, 14400, 28800, 43200, 57600, 72000, 86400),
        labels = c("12AM", 4, 8, "12PM", 4, 8, "12AM")) +
    xlab("Time") +
    ylab("Number of Recorded Rides") +
    labs(fill = "User Type") +
    scale_y_continuous(labels = scales::comma) +
    ggtitle("The Number of Recorded Rides Every Half Hour",
        subtitle = "Total Across the Year Split by User Type")

glimpse(full_2019)

# bounds of trip durations
max(full_2019$tripduration)
min(full_2019$tripduration)

# filter trip durations
filtered_trip <- full_2019 %>%
    filter(tripduration <= 7200)
glimpse(filtered_trip)

#full histogram of trip durations
ggplot(data = filtered_trip) +
    geom_histogram(mapping = aes(x = tripduration), fill = "#582a87",
    binwidth = 180, center = 90, color = 'black') +
    xlab("Trip Duration (HH:MM)") +
    scale_x_continuous(
        breaks = c(0, 900, 1800, 2700, 3600, 4500, 5400, 6300, 7200),
        labels = c("00:00", "00:15", "00:30", "00:45", "01:00", "01:15", "01:30", "01:45", "02:00")) +
    ylab("Counts for Each Time") +
    scale_y_continuous(labels = scales::comma) +
    ggtitle("Number of Rides for Each Duration")

# histogram split by user type
ggplot(data = filtered_trip) +
    geom_histogram(mapping = aes(x = tripduration, fill = usertype),
        binwidth = 180, center = 90, color = 'black') +
    labs(fill = "User Type") +
    xlab("Trip Duration (HH:MM)") +
    scale_x_continuous(
        breaks = c(0, 1800, 3600, 5400, 7200),
        labels = c("00:00", "00:30", "01:00", "01:30", "02:00")) +
    ylab("Counts for Each Time") +
    facet_wrap(~usertype) +
    scale_y_continuous(labels = scales::comma) +
    ggtitle("Number of Rides for Each Duration",
            subtitle = "Histogram Split by User Type")

# boxplot of the same data
ggplot(data = filtered_trip, aes(x = usertype, y = tripduration, fill = usertype)) + 
    stat_boxplot(geom = "errorbar", width = 0.2) +
    geom_boxplot() +
    ylab("Trip Duration (HH:MM)") +
    scale_y_continuous(
        breaks = c(0, 600, 1200, 1800, 2400, 3000, 3600, 4200, 4800, 5400, 6000, 6600, 7200),
        labels = c("00:00", "00:10", "00:20", "00:30", "00:40", "00:50", "01:00",
                    "01:10", "01:20", "01:30", "01:40", "01:50", "02:00")) +
    xlab("User Type") +
    labs(fill = "User Type") +
    ggtitle("Boxplots of Trip Duration Split by User Type")

# Summary Statistics
summary(filtered_trip$tripduration[filtered_trip$usertype == "Customer"])
summary(filtered_trip$tripduration[filtered_trip$usertype == "Subscriber"])