# Load necessary libraries
library(tidyverse)
#Problem 1
# Read the dataset
profs <- read.csv("~/Downloads/profs.csv")

# Part A: Histogram of course evaluation scores
ggplot(profs, aes(x = eval)) +
  geom_histogram(binwidth = 0.2, fill = "steelblue", color = "black") +
  labs(title = "Distribution of Course Evaluation Scores",
       x = "Evaluation Score",
       y = "Frequency")
# Part B: Boxplot of evaluation scores by native English speaker
ggplot(profs, aes(x = native, y = eval, fill = native)) +
  geom_boxplot() +
  labs(title = "Evaluation Scores by Native English Speaker",
       x = "Native English Speaker",
       y = "Evaluation Score")
# Part C: Faceted histogram comparing male and female instructors
ggplot(profs, aes(x = eval)) +
  geom_histogram(binwidth = 0.2, fill = "coral", color = "black") +
  facet_wrap(~gender, nrow = 2) +
  labs(title = "Comparison of Course Evaluation Scores by Gender")
# Part D: Scatterplot of attractiveness vs. evaluation scores
ggplot(profs, aes(x = beauty, y = eval)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Instructor Attractiveness vs. Course Evaluation",
       x = "Attractiveness Score",
       y = "Evaluation Score")
#Problem 2 
# Part A: Line graph of average hourly bike rentals
bikeshare <- read.csv("~/Downloads/bikeshare.csv")%>%
  group_by(hr) %>%
  summarise(avg_rentals = mean(total))

ggplot(bikeshare, aes(x = hr, y = avg_rentals)) +
  geom_line(color = "darkblue") +
  labs(title = "Average Hourly Bike Rentals", x = "Hour", y = "Average Rentals")

# Part B: Faceted line graph for working days
bikeshare <- read.csv("~/Downloads/bikeshare.csv")%>%
  group_by(hr, workingday) %>%
  summarise(avg_rentals = mean(total))

ggplot(bikeshare, aes(x = hr, y = avg_rentals, color = as.factor(workingday))) +
  geom_line() +
  facet_wrap(~workingday) +
  labs(title = "Bike Rentals by Hour (Working vs Non-Working Day)")

# Part C: Faceted bar plot for 9 AM ridership by weather
bikeshare <- read.csv("~/Downloads/bikeshare.csv")%>%
  filter(hr == 9) %>%
  group_by(weathersit, workingday) %>%
  summarise(avg_ridership = mean(total))

ggplot(bikeshare, aes(x = as.factor(weathersit), y = avg_ridership, fill = as.factor(workingday))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~workingday) +
  labs(title = "9 AM Ridership by Weather Condition", x = "Weather Situation", y = "Average Ridership")

#Problem 3
capmetro <- read.csv("~/Downloads/capmetro_UT.csv")

capmetro <- capmetro %>%
  mutate(
    day_of_week = factor(day_of_week, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
    month = factor(month, levels = c("Sep", "Oct", "Nov"))
  )

# Part 1: Faceted line graph of boardings by hour
capmetro_summary <- capmetro %>%
  group_by(hour_of_day, day_of_week, month) %>%
  summarise(avg_boardings = mean(boarding, na.rm = TRUE)) # Ensure NA handling

ggplot(capmetro_summary, aes(x = hour_of_day, y = avg_boardings, color = month)) +
  geom_line() +
  facet_wrap(~day_of_week) +
  labs(
    title = "Average Boardings by Hour and Day of Week",
    x = "Hour of the Day",
    y = "Average Boardings",
    color = "Month"
  ) +
  theme_minimal()

# Part 2: Faceted scatter plot of boardings vs temperature
ggplot(capmetro, aes(x = temperature, y = boarding, color =(weekend))) +
  geom_point(alpha = 0.5) +
  facet_wrap(~hour_of_day) +
  labs(
    title = "Temperature vs Boardings",
    x = "Temperature (F)",
    y = "Boardings",
    color = "Weekend "
  ) +
  theme_minimal()

# Problem 4
billboard <- read.csv("~/Downloads/billboard.csv")

# Part A: Top 10 songs by total weeks
top_songs <- billboard %>%
  group_by(performer, song) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(10)

print(top_songs)

# Part B: Musical diversity over time
billboard_filtered <- billboard %>%
  filter(year != 1958 & year != 2021) %>%
  group_by(year) %>%
  summarise(unique_songs = n_distinct(song))

ggplot(billboard_filtered, aes(x = year, y = unique_songs)) +
  geom_line(color = "purple") +
  labs(title = "Musical Diversity Over Time", x = "Year", y = "Unique Songs")

# Part C: Bar plot of ten-week hits by artist
ten_week_hits <- billboard %>%
  group_by(performer) %>%
  filter(n() >= 10) %>%
  summarise(count = n_distinct(song)) %>%
  arrange(desc(count)) %>%
  filter(count >= 30)

ggplot(ten_week_hits, aes(x = reorder(performer, -count), y = count)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  labs(title = "Artists with 30+ Ten-Week Hits", x = "Artist", y = "Number of Hits")


