library(tidyverse)
library(lubridate)

# Read in output file of osx-cpu-temp script
cpu_temp_raw <- read_csv("cpu_temp_log.output", 
                            col_names = c("ts_coll", "cpu_temp"))

# Convert cpu_temp to numeric, create two timestamp fields rounding down ts_coll to day and minute
# Filter data to remove error records
# Create Group ID field that groups records by active continuous minute intervals
cpu_temp_log <- cpu_temp_raw %>%
  mutate(cpu_temp = as.numeric(str_replace(cpu_temp, "°C", "")),
         min_coll = floor_date(ts_coll, "minute"),
         day_coll = floor_date(ts_coll, "day")) %>%
  filter(cpu_temp != -127) %>%
  mutate(row_num = row_number(),
         min_since = difftime(min_coll, min(min_coll), units = "mins"),
         group_id = as.character(min_since - row_num + 1))

# Create session id as a sequential number based on group id
cpu_temp_log <- left_join(cpu_temp_log, data.frame(group_id = unique(cpu_temp_log$group_id), session = 1:length(unique(cpu_temp_log$group_id))),
                          by = "group_id")

# Generate step chart of cpu temperature over time (specific day)
ggplot(data = filter(cpu_temp_log, day_coll == ymd("2022-12-20")),
              aes(x = ts_coll, y = cpu_temp, group = session)) +
         geom_step() +
  scale_y_continuous(name = "CPU Temperature (°C)") +
  scale_x_datetime(name = "Date Collected") +
  theme_light()

# Calculate average, max and median temperatures by day
# Create Group ID field that groups records by active continuous day intervals
cpu_daily_df <- cpu_temp_log %>%
  group_by(day_coll) %>%
  summarize(avg_temp = mean(cpu_temp),
            median_temp = median(cpu_temp),
            max_temp = max(cpu_temp)) %>%
  mutate(row_num = row_number(),
         days_since = difftime(day_coll, min(day_coll), units = "days"),
         group_id = as.character(days_since - row_num + 1))

# Create session id as a sequential number based on group id
cpu_daily_df <- left_join(cpu_daily_df, data.frame(group_id = unique(cpu_daily_df$group_id), session = 1:length(unique(cpu_daily_df$group_id))),
                          by = "group_id") %>%
  pivot_longer(cols = avg_temp:max_temp,
               names_to = "variable",
               values_to = "value")

# Generate step chart of average, max and median temperature over time
ggplot(data = cpu_daily_df,
       aes(x = day_coll, y = value, color = variable, group = group_id)) +
  geom_step() +
  facet_grid(variable ~ ., labeller = labeller(variable = c(avg_temp = "Average Temp", max_temp = "Max Temp", median_temp = "Median Temp")))  +
  scale_color_discrete(breaks = c("avg_temp", "max_temp", "median_temp"), labels = c("Average Temp", "Max Temp", "Median Temp")) +
  scale_y_continuous(name = "CPU Temperature (°C)") +
  scale_x_datetime(name = "Date Collected") +
  theme_light() +
  theme(legend.position = "bottom", legend.title = element_blank(), strip.text.y = element_text(size = 12, face = "bold"))

