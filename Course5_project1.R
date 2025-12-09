# Preparing the code for Rmd
# importing packages
library(tidyverse)

# importing data
df <- read.csv("activity.csv")

# converting date from character to date
df$date <- as.Date(df$date)

# aggregating data to daily frequency
df_daily <- df %>% 
    group_by(date) %>% 
    summarise(total_steps = sum(steps, na.rm = TRUE))

# plotting a histogram
g <- ggplot(df_daily, aes(x = total_steps)) +
    geom_histogram(bins = 30, fill = "skyblue", color = "black") +
    labs(title = "Histogram of daily steps",
         x = "Total Daily Steps",
         y = "Count")
print(g)

# calcuating the mean and median of total daily steps
df_daily_summary <- df_daily %>% 
    summarise(mean_total = mean(total_steps, na.rm = TRUE),
              median_total = median(total_steps, na.rm = TRUE))

print(df_daily_summary)

# average daily activity pattern
df_interval <- df %>% 
    group_by(interval) %>% 
    summarise(avg_steps = mean(steps, na.rm = TRUE)) %>% 
    arrange(interval)

g2 <- ggplot(df_interval, aes(x = interval, y = avg_steps)) +
    geom_line() +
    labs(
        title = "Average Daily Activity Pattern",
        x = "5-minute Interval",
        y = "Average Steps"
    )

print(g2)

interval_max <- df_interval %>% 
    slice_max(avg_steps, n = 1) %>% 
    pull(interval)
print(interval_max)















