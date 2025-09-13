# Introduction

Weather data analysis is essential for understanding climate patterns, planning for agriculture, 
managing water resources, and preparing for extreme weather events.By examining daily or weekly
measurements of temperature, humidity, and rainfall across multiple cities,we can identify trends,
extremes, and variations in regional climates.

In this assignment, we analyze weather data for three cities—Nashik, Pune, and Mumbai—over a 20-day period.
The goal is to gain hands-on experience in data import, cleaning, manipulation, analysis, and visualization using R. 
The analysis involves handling missing values, computing weekly averages, comparing temperatures and humidity, 
identifying rainfall extremes, and visualizing patterns.

Through this exercise, we aim to draw meaningful insights about climate differences between cities, helping us 
understand both typical and extreme weather behavior in each location.



# Conclusion

The analysis of weather data across Nashik, Pune, and Mumbai revealed several key insights:
  
Temperature trends: Nashik generally showed higher average temperatures, while Mumbai had more moderate values.

Extreme weather: The dataset allowed identification of the hottest and coldest cities and the days with highest rainfall in each city.

Humidity comparison: Mumbai exhibited higher humidity on average, whereas Nashik had lower humidity, reflecting regional climate differences.

Visualization insights: Line graphs, bar plots, and boxplots effectively illustrated trends and variability, making it easier to interpret patterns over time.

Overall, this assignment provided practical experience in data handling, statistical analysis, and visualization in R, and highlighted the importance of analyzing 
weather data for informed decision-making and planning.

# Task 1: Data Import & Inspection

# Import dataset
weather <- read.csv("weather_data.csv", stringsAsFactors = FALSE)
weather$Date <- as.Date(weather$Date)

# Display first few rows
head(weather)

# Display column names
colnames(weather)

# Check for missing values
sum(is.na(weather))          # Total missing values
colSums(is.na(weather))      # Missing values per column

# Handle missing values: replace NA with mean for numeric columns
weather$Temperature[is.na(weather$Temperature)] <- mean(weather$Temperature, na.rm = TRUE)
weather$Humidity[is.na(weather$Humidity)] <- mean(weather$Humidity, na.rm = TRUE)
weather$Rainfall[is.na(weather$Rainfall)] <- mean(weather$Rainfall, na.rm = TRUE)


# Task 2: Data Cleaning & Manipulation

# Filter dataset for my city and 2 other cities
selected_cities <- c("Mumbai", "Pune", "Nashik")
weather_filtered <- weather %>%
  filter(City %in% selected_cities)

# Create Temperature in Fahrenheit
weather_filtered <- weather_filtered %>%
  mutate(Temp_F = Temperature * 9/5 + 32)

# Calculate weekly averages
weather_filtered <- weather_filtered %>%
  mutate(Week = format(Date, "%Y-%U"))  # Week number of year

weekly_avg <- weather_filtered %>%
  group_by(City, Week) %>%
  summarise(
    Avg_Temperature = mean(Temperature, na.rm = TRUE),
    Avg_Humidity = mean(Humidity, na.rm = TRUE),
    Avg_Rainfall = mean(Rainfall, na.rm = TRUE),
    Avg_Temp_F = mean(Temp_F, na.rm = TRUE)
  ) %>%
  arrange(City, Week)

# View weekly averages
head(weekly_avg)


# Task 3: Data Analysis

# 1. Find hottest and coldest city
city_avg_temp <- weather_filtered %>%
  group_by(City) %>%
  summarise(Avg_Temperature = mean(Temperature, na.rm = TRUE)) %>%
  arrange(desc(Avg_Temperature))

hottest_city <- city_avg_temp$City[1]
coldest_city <- city_avg_temp$City[nrow(city_avg_temp)]

cat("Hottest city:", hottest_city, "\nColdest city:", coldest_city, "\n")

# 2. Day with highest rainfall in each city
max_rainfall_day <- weather_filtered %>%
  group_by(City) %>%
  filter(Rainfall == max(Rainfall, na.rm = TRUE)) %>%
  select(City, Date, Rainfall)
print(max_rainfall_day)

# 3. Compare average humidity
city_avg_humidity <- weather_filtered %>%
  group_by(City) %>%
  summarise(Avg_Humidity = mean(Humidity, na.rm = TRUE)) %>%
  arrange(desc(Avg_Humidity))
print(city_avg_humidity)


# Task 4: Visualization

# 1. Line graph: Temperature trends over time
ggplot(weather_filtered, aes(x = Date, y = Temperature, color = City)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title = "Temperature Trends Over Time", x = "Date", y = "Temperature (°C)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 2. Bar plot: Weekly average rainfall
ggplot(weekly_avg, aes(x = Week, y = Avg_Rainfall, fill = City)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Weekly Average Rainfall by City", x = "Week", y = "Avg Rainfall (mm)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 3. Boxplot: Humidity distribution across cities
ggplot(weather_filtered, aes(x = City, y = Humidity, fill = City)) +
  geom_boxplot() +
  labs(title = "Humidity Distribution Across Cities", x = "City", y = "Humidity (%)") +
  theme_minimal()
