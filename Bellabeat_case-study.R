library(tidyverse)
library(lubridate)
library(janitor)
library(ggplot2)

daily_activity <- read.csv("mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
sleep <- read.csv("mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/minuteSleep_merged.csv")
heartrate <- read.csv("mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/heartrate_seconds_merged.csv")

daily_activity_clean <- daily_activity %>% 
  clean_names() %>% 
  mutate(
    id = as.character(id),
    date = as.Date(activity_date, format = "%m/%d/%Y"),
    active_minutes = very_active_minutes + fairly_active_minutes,
    day_name = weekdays(date),
    day_type = case_when(wday(date) %in% c(1, 7) ~ "weekend",
                         TRUE ~ "weekday")
  ) %>% 
  select(id,
         total_steps,
         active_minutes,
         date,
         day_type,
         day_name,
         calories) %>% 
  distinct() %>% 
  drop_na()
head(daily_activity_clean)

sleep_clean <- sleep %>%
  clean_names() %>%
  mutate(
    id = as.character(id),
    datetime = mdy_hms(date),
    date = as_date(datetime),
    time = format(datetime, "%H:%M:%S")
  ) %>%
  select(
    id, 
    date,
    time,
    log_id
  ) %>%
  drop_na() %>%
  distinct()
head(sleep_clean)

heartrate_clean <- heartrate %>% 
  clean_names() %>% 
  mutate(
    id = as.character(id),
    datetime = mdy_hms(time),
    date = as_date(datetime)
  ) %>% 
  drop_na() %>% 
  select(
    id,
    date,
    value
  )
head(heartrate_clean)

sleep_daily <- sleep_clean %>%
  group_by(id, date) %>%
  summarise(
    total_sleep_minutes = n(),   
    sleep_sessions = n_distinct(log_id), .groups = "drop"
  )

heartrate_daily <- heartrate_clean %>%
  group_by(id, date) %>%
  summarise(
    avg_heartrate = mean(value, na.rm = TRUE),
    max_heartrate = max(value, na.rm = TRUE),
    min_heartrate = min(value, na.rm = TRUE), .groups = "drop"
  )
head(sleep_daily)
head(heartrate_daily)

daily_merged <- daily_activity_clean %>% 
  left_join(sleep_daily, by = c("id", "date")) %>% 
  left_join(heartrate_daily, by = c("id", "date"))

daily_merged$avg_heartrate <- as.numeric(
  ifelse(daily_merged$avg_heartrate == "Not Recorded", NA, daily_merged$avg_heartrate)
)

daily_merged <- daily_merged %>% 
  mutate(
    steps_category = case_when(
      total_steps < 5000 ~ "Sedentary",
      total_steps >= 5000 & total_steps <= 10000 ~ "Moderate",
      total_steps > 10000 ~ "Active"
    ) 
  ) %>% 
  select(id, total_steps, steps_category, active_minutes, date, day_type, day_name, calories, total_sleep_minutes, sleep_sessions, avg_heartrate, max_heartrate, min_heartrate)

head(daily_merged)
n_distinct(daily_merged$id)
nrow(daily_merged)

avg_steps <- daily_merged %>%
  summarise(
    Average_Steps = mean(total_steps, na.rm = TRUE)
  )
avg_steps

user_days <- daily_merged %>%
  group_by(day_name) %>% 
  summarise(total_users = n()) %>% 
  mutate(day_name = factor(
    day_name,
    levels = c("Sunday","Monday","Tuesday","Wednesday","Thursday", "Friday", "Saturday")))

ggplot(user_days, aes(x = day_name, y = total_users)) +
  geom_col(fill = "#FA8072") +
  labs(title = "Daily User Count",
       x = "Day",
       y = "Count") +
  theme_minimal()

ggplot(data = daily_merged, aes(x = total_steps, y = calories)) + 
  geom_point(color = "#FA8072") + 
  geom_smooth(color = "blue") + 
  labs(title = "Total Steps vs. Calories",
       x = "Steps",
       y = "Calories")

ggplot(data = daily_merged, aes(x = active_minutes, y = calories)) + 
  geom_point(color = "#FA8072") + 
  geom_smooth(color = "blue") + 
  labs(title = "Active Minutes vs. Calories",
       x = "Minutes",
       y = "Calories")

ggplot(daily_merged, aes(x=total_sleep_minutes)) + 
  geom_histogram(binwidth = 30, fill = "lightblue", color = "white", na.rm = TRUE) +
  labs(title = "Daily Sleep Distribution", x = "Minutes", y = "Days")

ggplot(data = daily_merged, aes(y = total_sleep_minutes, x = total_steps)) + 
  geom_point(color = "#FA8072") + 
  geom_smooth(method = "lm", se = FALSE, color = "blue", na.rm = TRUE) + 
  labs(title = "Total Steps vs Total Sleep",
       y = "Sleep",
       x = "Steps")

ggplot(daily_merged, aes(x=steps_category, y=total_sleep_minutes, color = steps_category)) + 
  geom_violin(trim=FALSE) + geom_boxplot(width=0.1) + scale_color_manual(values=c("#999999", "#FA8072", "#56B4E9")) +
  labs(title = "Sleep Duration by Steps Category",
       x = "Category",
       y = "Sleep (minute)")

ggplot(data = daily_merged, aes(y = avg_heartrate, x = total_steps)) + 
  geom_point(color = "#FA8072") + 
  geom_smooth(method = "lm", se = FALSE, color = "blue", na.rm = TRUE) + 
  labs(title = "Average Heartrate by Steps",
       x = "Steps",
       y = "Avg_hr")

ggplot(daily_merged, aes(x = calories, y = active_minutes)) +
  geom_point(color = "black", alpha = 0.5) +
  geom_smooth(aes(color = day_type), se = TRUE) +
  labs(title = "Calories vs Active Minutes by day type",
       x = "Calories", y = "Minutes") +
  theme_minimal()


  