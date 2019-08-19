
# Import data and add columns for treatment info --------------------------

library(tidyverse)
library(stringr)

nitrate <- read_csv("./data_raw/nutrients/ruckle_nitrate.csv")

# subsetting the rows with water changes, and not the starting water values
change <- str_detect(nitrate$sample, "^[A-z]")
change <- nitrate[change, ]

start <-  str_detect(nitrate$sample, "^[0-9]")
start <- nitrate[start, ]

start <- start %>% 
  separate(sample, c("sample", "replicate"), sep = "_") %>% 
  group_by(day, sample) %>% 
  mutate(nitrate = mean(nitrate)) # caluclate average for samples with two measurements 
  
#delete duplicate rows
start <- start[!duplicated(start$nitrate), ]

# splitting samples into temp, salinity, nuts and reps
change <- change %>% 
  separate(sample, into = c("site", "temp", "salinity", "nutrients", "replicate"),
           sep = c(2,5,8,11))

start <- start %>% 
  separate(sample, into = c("salinity", "nutrients"),
           sep = 3)

# dropping the T, S, and _ from temp, salinity and nutrients
change$temp <- str_replace_all(change$temp, "T", "")
change$salinity <- str_replace_all(change$salinity, "S", "")
change$nutrients <- str_replace_all(change$nutrients, "_", "")

start$salinity <- str_replace_all(start$salinity, "S", "")
start <- start %>% 
  select(-replicate)

# join change and start back together
nitrate <- full_join(change, start)

nitrate <- nitrate %>% 
  select(-plate, -date_analyzed, -date) 
  
# remove change and start
rm(change)
rm(start)

# Change in nitrate values ------------------------------------------------

# average nitrate levels for each treatment (mean of the 3 petri dishes)
avg_nitrate <- nitrate %>% 
  group_by(day, temp, salinity, nutrients) %>% 
  summarise(avg_nitrate = mean(nitrate))

# create new dataframe with rows of starting nitrate
new_nitrate <- nitrate %>% 
  filter(is.na(site)) %>% 
  rename(start_nitrate = nitrate) %>% 
  select(salinity, nutrients, day, start_nitrate)

# join two dataframe to add in a column for starting nitrate values for each petri dish
## first change the 'day' column in new_nitrate to match the day in nitrate df. 
# ie. change day 0 start water to day 3, day 3 to day 6, day 6 to day 8

final_nitrate <- left_join(nitrate, new_nitrate, by = c("salinity", "nutrients"))

final_nitrate <- final_nitrate %>% 
  filter((day.x - day.y) <= 3 & (day.x - day.y) > 0) %>% # keep only the rows the have day 0 starting values and day 3 water changes
  filter(!is.na(site)) %>%  # filter out the rows with only starting nitrate
  mutate(change_nitrate = (start_nitrate - nitrate)/(day.x - day.y)) # calculate change in nitrate per day

final_nitrate_sum <- final_nitrate %>% 
  group_by(temp, salinity, nutrients, day.x) %>% 
  summarise(avg_change = mean(change_nitrate),
            sd_change = sd(change_nitrate),
            se_change = sd(change_nitrate)/sqrt(3))

## plotting avg change in nitrate from day 0 to day 3 
first_3_days <- final_nitrate %>% 
  filter(day.x == 3)

next_3_days <- final_nitrate %>% 
  filter(day.x == 6)

last_2_days <- final_nitrate %>% 
  filter(day.x == 8)

## plotting only change in nitrate from day 0 to day 3
ggplot(data = first_3_days, aes(x = temp, y = change_nitrate, color = salinity)) + 
  geom_point(size = 3, alpha = 1/2, position = position_dodge(0.5)) + 
  facet_wrap(~nutrients) +
  ylab("nitrate uptake(uM/day)") + 
  xlab(expression('Temperature ('*~degree*C*')')) + 
  ggtitle("change in nitrate over first 3 days")

## plotting only change in nitrate from day 3 to day 6
ggplot(data = next_3_days, aes(x = temp, y = change_nitrate, color = salinity)) + 
  geom_point(size = 3, alpha = 1/2, position = position_dodge(0.5)) + 
  facet_wrap(~nutrients) +
  ylab("nitrate uptake (uM/day)") + 
  xlab(expression('Temperature ('*~degree*C*')')) + 
  ggtitle("change in nitrate over days 3 to 6")

## plotting only change in nitrate from day 6 to day 8
ggplot(data = last_2_days, aes(x = temp, y = change_nitrate, color = salinity)) + 
  geom_point(size = 3, alpha = 1/2, position = position_dodge(0.5)) + 
  facet_wrap(~nutrients) +
  ylab("nitrate uptake (uM/day)") + 
  xlab(expression('Temperature ('*~degree*C*')')) + 
  ggtitle("change in nitrate over days 6 to 8")

ggplot(data = final_nitrate_sum, aes(x = temp, y = avg_change, color = salinity)) + 
  geom_point(aes(shape = day.x), position = position_dodge(0.5)) + 
  facet_wrap(~nutrients) +
  geom_errorbar(aes(ymin = avg_change - se_change, ymax = avg_change + se_change), 
                width = 0.1, position = position_dodge(0.5)) + 
  ylab("change in nitrate (uM/day)") + 
  xlab(expression('Temperature ('*~degree*C*')'))

ggsave("rp_nitrate.pdf", path = "./figures", plot = last_plot())

## plotting avg change in nitrate from day 3 to day 6 
ggplot(data = final_nitrate_sum, aes(x = temp, y = avg_change, color = salinity)) + 
  geom_point(position = position_dodge(0.5)) + 
  facet_wrap(~nutrients) +
  geom_errorbar(aes(ymin = avg_change - sd_change, ymax = avg_change + sd_change), 
                width = 0.1, position = position_dodge(0.5)) + 
  ylab("nitrate uptake (uM/day)") + 
  xlab(expression('Temperature ('*~degree*C*')'))

# random bits of code -----------------------------------------------------

final_nitrate %>% 
  filter(day.x == 6) %>% 
  filter(nutrients == "EN") %>% 
  filter(salinity == 10) %>% View

last_2_days %>% 
  filter(temp == "26") %>% 
  filter(salinity == "10") %>% View

last_2_days %>% 
  arrange()
  
## plotting only change in nitrate from day 6 to day 8
ggplot(data = last_2_days, aes(x = temp, y = change_nitrate, color = salinity)) + 
  geom_point(size = 3, alpha = 1/2, position = position_dodge(0.5)) + 
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~nutrients) +
  ylab("nitrate uptake (uM/day)") + 
  xlab(expression('Temperature ('*~degree*C*')')) + 
  ggtitle("change in nitrate over days 6 to 8")

final_nitrate %>% 
  group_by(temp, salinity, nutrients, day.x) %>% 
  tally() %>% View

