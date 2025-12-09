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
    labs(title = "Histogram of Daily Steps",
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

# calculating number of rows with NAs
missing_values <- df %>% 
    filter(is.na(steps)) %>% 
    nrow()

print(missing_values)

# imputing missing values
df_imputed <- df %>% 
    left_join(df_interval, by = "interval") %>% 
    mutate(steps_filled = if_else(is.na(steps), avg_steps, steps))

# histogram, mean and median of imputed steps
df_daily_imputed <- df_imputed %>% 
    group_by(date) %>% 
    summarise(total_steps = sum(steps_filled, na.rm = TRUE))

g3 <- ggplot(df_daily_imputed, aes(x = total_steps)) +
    geom_histogram(bins = 30, fill = "skyblue", color = "black") +
    labs(title = "Histogram of Daily Imputed Steps",
         x = "Total Daily Imputed Steps",
         y = "Count")
print(g3)

df_daily_imputed_summary <- df_daily_imputed %>% 
    summarise(mean_total = mean(total_steps, na.rm = TRUE),
              median_total = median(total_steps, na.rm = TRUE))

print(df_daily_imputed_summary)

# the result of imputation of missing steps shows a histogram that is
# more normally distributed. It also reduces the days with zero steps
# and more accurately shows true zero step days.
# the mean and median also becomes the same as it was previously
# skewered towards zero

# creating day type
df_imputed <- df_imputed %>% 
    mutate(
        day_type = if_else(
            wday(date, label = TRUE) %in% c("Sat", "Sun"),
            "weekend",
            "weekday"),
        day_type = factor(day_type, levels = c("weekend", "weekday")))

# calculating average steps per 5min interval for weekday & weekend
df_imputed_day_type <- df_imputed %>% 
    group_by(interval, day_type) %>% 
    summarise(avg_steps = mean(steps_filled, na.rm = TRUE)) %>% 
    arrange(interval, day_type)

g4 <- ggplot(df_imputed_day_type, aes(x = interval, y = avg_steps)) +
    geom_line() +
    facet_wrap(~ day_type, ncol = 1) +
    labs(
        title = "Average Daily Activity Pattern by Day Type",
        x = "5-minute Interval",
        y = "Average Steps"
    )

print(g4)

df_imputed_day_type %>% 
    group_by(day_type) %>% 
    slice_max(avg_steps, n=1)









