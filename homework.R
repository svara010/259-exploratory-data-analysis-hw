# 259 Homework - exploratory data analysis + integrating skills
# For full credit, answer at least 8/10 questions
# List students working with below:

library(tidyverse)
library(lubridate)
library(DataExplorer)

#> These data are drawn from the fivethirtyeight article:
#> http://fivethirtyeight.com/features/what-12-months-of-record-setting-temperatures-looks-like-across-the-u-s/
#> The directory us-weather-history contains a data file for each of 10 cities, labelled by their station name
#> Each data file contains:
#> `date` | The date of the weather record, formatted YYYY-M-D
#> `actual_mean_temp` | The measured average temperature for that day
#> `actual_min_temp` | The measured minimum temperature for that day
#> `actual_max_temp` | The measured maximum temperature for that day
#> `average_min_temp` | The average minimum temperature on that day since 1880
#> `average_max_temp` | The average maximum temperature on that day since 1880
#> `record_min_temp` | The lowest ever temperature on that day since 1880
#> `record_max_temp` | The highest ever temperature on that day since 1880
#> `record_min_temp_year` | The year that the lowest ever temperature occurred
#> `record_max_temp_year` | The year that the highest ever temperature occurred
#> `actual_precipitation` | The measured amount of rain or snow for that day
#> `average_precipitation` | The average amount of rain or snow on that day since 1880
#> `record_precipitation` | The highest amount of rain or snow on that day since 1880

stations <- c("KCLT", "KCQT", "KHOU", "KIND", "KJAX", "KMDW", "KNYC", "KPHL", "KPHX", "KSEA")
cities <- c("Charlotte", "Los Angeles", "Houston", "Indianapolis", "Jacksonville", 
            "Chicago", "New York City", "Philadelphia", "Phoenix", "Seattle")


# QUESTION 1
#> The data files are in the directory 'us-weather-history'
#> Write a function that takes each station abbreviation and reads
#> the data file and adds the station name in a column
#> Make sure the date column is a date
#> The function should return a tibble
#> Call the function "read_weather" 
#> Check by reading/glimpsing a single station's file


library(tidyverse)
library(lubridate)


read_weather <- function(station) {
  file_path <- paste0("us-weather-history/", station, ".csv")
  weather_data <- read_csv(file_path, show_col_types = FALSE) %>%
    mutate(
      station = station,  
      date = ymd(date)   
    )
  return(weather_data)
}

weather_test <- read_weather("KCLT")

glimpse(weather_test)
head(weather_test)

# QUESTION 2
#> Use map() and your new function to read in all 10 stations
#> Note that because map_dfr() has been superseded, and map() does not automatically bind rows, you will need to do so in the code.
#> Save the resulting dataset to "ds"

library(tidyverse)

stations <- c("KCLT", "KCQT", "KHOU", "KIND", "KJAX", 
              "KMDW", "KNYC", "KPHL", "KPHX", "KSEA")

ds <- map(stations, read_weather) %>% bind_rows()

glimpse(ds)

# QUESTION 3
#> Make a factor called "city" based on the station variable
#> (station should be the level and city should be the label)
#> Use fct_count to check that there are 365 days of data for each city 

station_city_map <- setNames(c("Charlotte", "Los Angeles", "Houston", "Indianapolis", "Jacksonville", 
                               "Chicago", "New York City", "Philadelphia", "Phoenix", "Seattle"), stations)

ds <- ds %>%
  mutate(city = factor(station, levels = stations, labels = station_city_map))

fct_count(ds$city)

# QUESTION 4
#> Since we're scientists, let's convert all the temperatures to C
#> Write a function to convert F to C, and then use mutate across to 
#> convert all of the temperatures, rounded to a tenth of a degree

f_to_c <- function(f) {
  round((f - 32) * 5 / 9, 1)  # Convert and round to one decimal place
}

temp_columns <- c("actual_mean_temp", "actual_min_temp", "actual_max_temp", 
                  "average_min_temp", "average_max_temp", "record_min_temp", 
                  "record_max_temp")
ds <- ds %>%
  mutate(across(all_of(temp_columns), f_to_c))

glimpse(ds)

### CHECK YOUR WORK
#> At this point, your data should look like the "compiled_data.csv" file
#> in data-clean. If it isn't, read in that file to use for the remaining
#> questions so that you have the right data to work with.

compiled_data <- read_csv("data-clean/compiled_data.csv", show_col_types = FALSE)
glimpse(ds)
glimpse(compiled_data)

# QUESTION 5
#> Write a function that counts the number of extreme temperature days,
#> where the actual min or max was equal to the (i.e., set the) record min/max
#> A piped function starting with '.' is a good strategy here.
#> Group the dataset by city to see how many extreme days each city experienced,
#> and sort in descending order to show which city had the most:
#> (Seattle, 20, Charlotte 12, Phoenix 12, etc...)
#> Don't save this summary over the original dataset!

count_extreme_days <- function(df) {
  df %>%
    filter(actual_min_temp == record_min_temp | actual_max_temp == record_max_temp) %>%
    count(city, name = "extreme_days") %>%
    arrange(desc(extreme_days))
}

extreme_days_summary <- count_extreme_days(ds)
print(extreme_days_summary)

# QUESTION 6
#> Pull out the month from the date and make "month" a factor
#> Split the tibble by month into a list of tibbles 

ds <- ds %>%
  mutate(month = factor(month(date, label = TRUE, abbr = TRUE), 
                        levels = month.abb)) #just to make sure the orders are correct

monthly_split <- ds %>%
  group_split(month)

names(monthly_split) <- levels(ds$month)

str(monthly_split, max.level = 1)

# QUESTION 7
#> For each month, determine the correlation between the actual_precipitation
#> and the average_precipitation (across all cities), and between the actual and average mins/maxes
#> Use a for loop, and print the month along with the resulting correlation
#> Look at the documentation for the ?cor function if you've never used it before

for (month_name in names(monthly_split)) {
  df <- monthly_split[[month_name]]
  
  cor_precip <- cor(df$actual_precipitation, df$average_precipitation, use = "complete.obs")
  cor_min_temp <- cor(df$actual_min_temp, df$average_min_temp, use = "complete.obs")
  cor_max_temp <- cor(df$actual_max_temp, df$average_max_temp, use = "complete.obs")
  
  cat("\nMonth:", month_name, "\n")
  cat("  Correlation (Actual vs. Avg Precipitation):", round(cor_precip, 3), "\n")
  cat("  Correlation (Actual vs. Avg Min Temp):", round(cor_min_temp, 3), "\n")
  cat("  Correlation (Actual vs. Avg Max Temp):", round(cor_max_temp, 3), "\n")
}

# QUESTION 8
#> Use the Data Explorer package to plot boxplots of all of the numeric variables in the dataset
#> grouped by city, then do the same thing grouped by month. 
#> Finally, use plot_correlation to investigate correlations between the continuous variables only
#> Check the documentation for plot_correlation for an easy way to do this

#grouped by city
plot_boxplot(ds, by = "city")

#grouped by month
plot_boxplot(ds, by = "month")

plot_correlation(ds, cor_args = list(use = "complete.obs"))

# QUESTION 9
#> Create a scatterplot of actual_mean_temp (y axis) by date (x axis)
#> Use facet_wrap to make a separate plot for each city (3 columns)
#> Make the points different colors according to month

ggplot(ds, aes(x = date, y = actual_mean_temp, color = month)) +
  geom_point(alpha = 0.6) +  
  facet_wrap(~ city, ncol = 3) +
  labs(title = "Daily Mean Temperature Trends by City",
       x = "Date",
       y = "Actual Mean Temperature (°C)",
       color = "Month") +  
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# QUESTION 10
#> Write a function that takes the dataset and the abbreviate month as arguments
#> and creates a scatter and line plot of actual temperature (y axis) by date (x axis)
#> Note, just add geom_line() to your ggplot call to get the lines
#> use the ggtitle() function to add the month as a title
#> The function should save the plot as "eda/month_name.png"
#> The eda folder has an example of what each plot should look like
#> Call the function in a map or loop to generate graphs for each month


