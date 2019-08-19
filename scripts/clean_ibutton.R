## May 22, 2018 - Going to try and load ibutton data and clean it
 #load necessary packages
library(tidyverse)
library(lubridate)

#remove anything in global environment
rm(list = ls())

# Read in data ------------------------------------------------------------

# create list of all file paths for csv files in the ibutton folder
cell_files <- c(list.files("./data_raw/ibuttons", full.names = TRUE))

# give names to each filepath
names(cell_files) <- cell_files %>% 
  gsub(pattern = "csv$", replacement = "")

all_cells <- map_df(cell_files, read_csv, col_names = TRUE, skip = 14,
                    .id = "file_name")


# Prelim cleaning of data -------------------------------------------------


# trying to separate the file name into three columns that include site name, 
# code for site, replicate)
all_cells <- all_cells %>% 
  rename(temperature = Value, unit = Unit, `date_time` = `Date/Time`)

# separating columns to extract site name from file name
all_cells <- all_cells %>%
  separate(file_name, into = c("extra", "extra2", "extra3", "site"),
           sep = "/", extra = "drop", fill = "warn") %>% 
  select(site, date_time, temperature ) %>% 
  separate(site, into = c("site", "code", "ibutton_no"),
           sep = "_", extra = "drop", fill = "warn") 

unique(all_cells$site)

# change time to 24 hour clock ## not working :(
all_cells$date_time <- as.POSIXct(all_cells$date_time, format = "%d/%m/%y %I:%M:%S %p", tz = "UTC")
str(all_cells$date_time)

#separate date/time column  
all_cells <- all_cells %>%   
  separate(date_time, into = c("date", "time"),
           sep = " ", remove = FALSE)

str(all_cells)

all_cells$date <- as.POSIXct(all_cells$date, format = "%Y-%m-%d")
str(all_cells$date)

## why is date still in the time column?
all_cells$time <- as.POSIXct(all_cells$time, format = "%H:%M:%S", tz = "UTC")
str(all_cells$time)


#separate 'code' into columns for exposed/protected, low/high zone, replicate
# all_cells <- all_cells %>% 
#   separate(code, into =c("location", "tide_height", "replicate"),
#            sep = c(1,2), remove = FALSE, extra = "drop")


# Bamfield 2017 - exposed ------------------------------------------------------


#filter out bamfield only
bamfield <- all_cells %>% 
  filter(site == "Bamfield")

# Bamfield exposed ibuttons were done being put out on May 04, 2017 at 15:33pm
# don't know how to do the time yet, 
# so I am going to filter out all times after May 5, 2017
bamfield_exp <- bamfield %>% 
  filter(location == "E") %>% 
  mutate(date = dmy(date), time = hms(time)) %>% 
  filter(date >= "2017-05-05")
# filter(date >= "2017-05-04" & time >= "3:33:00" & am_pm == "PM") 

test <- bamfield %>% 
  filter(date_time >= "2017-06-08" & date_time <= "2017-06-15") %>% 
  group_by(date_time) %>% 
  summarise(avg_temp = mean(temperature)) 

ggplot(data = test, aes(x = date_time, y = avg_temp )) + geom_point()

# messing around with plotting time series of temp

ggplot(bamfield_exp, aes(x = date, y = Value, colour = tide_height)) + geom_point()

bamfield_exp_may <- bamfield_exp %>%  
  filter(date < "2017-06-01")

ggplot(bamfield_exp_may, aes(x = date, y = Value, colour = tide_height)) + geom_point() + 
  geom_smooth(se = FALSE)

bamfield_exp_august <- bamfield_exp %>%  
  filter(date >= "2017-08-01")

ggplot(bamfield_exp_august, aes(x = date, y = Value, colour = tide_height)) + geom_point() + 
  geom_smooth(se = FALSE)

#use only one set of dates ie. R1 and not R2



 # Not using this site anymore
# Bamfield 2017 - protected ----------------------------------------------------
#filter out bamfield only
bamfield <- all_cells %>% 
  filter(site == "Bamfield")

bamfield <- bamfield %>% 
separate(code, into =c("location", "tide_height", "replicate"),
         sep = c(1,2), remove = FALSE, extra = "drop")

bamfield_prot <- bamfield %>% 
  filter(location == "S") %>% 
  mutate(date = ymd(date)) %>% 
  filter(date >= "2017-05-05") 
# ibuttons were put out on May 4th but I can't figure out how to exclude before a specific time on that date
# filtering from May 24th for some reason???

bamfield_p_june <- bamfield_prot %>% 
  filter(date > "2017-06-01" & date < "2017-07-01")

# How to calculate variability? Here I'm calculating the daily range of temp
bamfield_p_june %>% 
  group_by(date) %>% 
  summarise(min_daily_temp = min(temperature), max_daily_temp = max(temperature)) %>% 
  mutate(range_daily_temp = max_daily_temp - min_daily_temp) %>% 
  ggplot(., aes(x = date, y = range_daily_temp)) + geom_point()

bamfield_range <- bamfield_prot %>% 
  group_by(code, ibutton_no, date) %>% 
  summarise(min_daily_temp = min(temperature), max_daily_temp = max(temperature)) %>% 
  mutate(range_daily_temp = max_daily_temp - min_daily_temp) 

bamfield_range_plot <-   ggplot(bamfield_range, aes(x = date, y = range_daily_temp)) + 
  geom_point(aes(colour = code))
bamfield_range_plot       

# Quadra 2017 --------------------------------------------------------

#filter out quadra only
quadra <- all_cells %>% 
  filter(site == "Quadra") %>%
  filter(date_time >= "2017-05-04") # done being put out on May 3rd at 3:30pm

quadra_summary <- quadra %>% 
  group_by(date) %>% 
  summarise(avg_temp = mean(temperature),
            min_temp = min(temperature), 
            max_temp = max(temperature))

# calculating daily range of temp
quadra_range <- quadra %>% 
  group_by(code, ibutton_no, date) %>% 
  summarise(min_daily_temp = min(temperature), max_daily_temp = max(temperature)) %>% 
  mutate(range_daily_temp = max_daily_temp - min_daily_temp) 

quadra_range_plot <- ggplot(data = quadra_range, aes(x = date, y = range_daily_temp)) + 
  geom_point(aes(colour = code))

#messing around with plotting time series of temp

quadra_may <- quadra_temp %>%  
  filter(date < "2017-06-01")

quadra_june <- quadra_temp %>% 
  filter(date_time )
  
  
ggplot(quadra_may, aes(x = date, y = temperature, colour = code)) + geom_point() + geom_smooth(se = FALSE)

quadra_aug <- quadra %>% 
  filter(date >= "2017-08-01")

ggplot(quadra_aug, aes(x = date, y = temperature, colour = code)) + geom_point() + geom_smooth(se = FALSE)
  
# one day in may at quadra
oneday <- quadra %>% 
  filter(date == "2017-05-30") %>% 
  filter(code == "Q1")

# temp data for last week of Quadra for one ibutton only
oneweek <- quadra %>% 
  filter(date >= "2017-05-25" & date <= "2017-05-31") %>% 
  filter(code == "Q1")

ggplot(oneday, aes(time, temperature,)) + geom_point() + geom_point()


## computing summaries...,

quadra %>% 
  group_by(date) %>% 
  summarise(max(Value)) %>% View

bamfield_exp %>% 
  group_by(date) %>% 
  summarise(max(Value)) %>% View

bamfield_prot %>% 
  group_by(date) %>% 
  summarise(mean(Value)) %>% View



 # Not using this site anymore
# Ruckle 2018 -------------------------------------------------------------

ruckle <- all_cells %>% 
  filter(site == "ruckle")

ruckle_ibuttons <- write_csv(ruckle, "./data/ruckle_temp.csv")

ruckle %>% 
  group_by(code, ibutton_no, date) %>% 
  summarise(min_daily_temp = min(temperature), max_daily_temp = max(temperature)) %>% 
  mutate(range_daily_temp = max_daily_temp - min_daily_temp) %>% View
  ggplot(., aes(x = date, y = range_daily_temp)) + 
    geom_point(aes(colour = code)) + 
    ylim(0, 30) + 
  labs(title = "ruckle park",
                    x = "date",
                    y = "range in daily temperature",
                    color = "ibutton location")

#Why are some ranges so low?
ruckle %>% 
  filter(code == "1") %>% 
  filter(ibutton_no == "r1.") %>% 
  filter(date == "2018-08-05") %>% View
# Because it's the dates that are at 
# the end of the ibutton and have less than 24 measurements?

ruckle %>% 
  filter(code == "1") %>% 
  filter(ibutton_no == "r1.") %>% 
  filter(date == "2018-08-02") %>% View

ruckle %>% 
  mutate(id = row_number()) %>% 
  group_by(code, ibutton_no) %>% 
  count(date) %>% 
  filter(n >= 48) %>% View

ruckle %>% 
  group_by(code, ibutton_no, date) %>% 
  summarise(min_daily_temp = min(temperature), max_daily_temp = max(temperature)) %>% 
  mutate(range_daily_temp = max_daily_temp - min_daily_temp) %>% 
  ggplot(., aes(x = date, y = range_daily_temp)) + 
  geom_point(aes(colour = code)) + 
  ylim(0, 30) + 
  labs(title = "ruckle park",
       x = "date",
       y = "range in daily temperature",
       color = "ibutton location")

ruckle %>% 
  mutate(id = row_number()) %>% 
  group_by(code, ibutton_no) %>% 
  count(date) %>% View
  filter(count(date) >= 48) %>% View
  keep(n >= 48) %>% View

# Nanaimo 2017 -----------------------------------------------------------------

nanaimo <- all_cells %>% 
  filter(site == "NP")

nanaimo_range <- nanaimo %>% 
  group_by(code, ibutton_no, date) %>% 
  summarise(min_daily_temp = min(temperature), max_daily_temp = max(temperature)) %>% 
  mutate(range_daily_temp = max_daily_temp - min_daily_temp)

nanaimo_range_plot <- ggplot(data = nanaimo_range, aes(x = date, y = range_daily_temp)) + 
  geom_point(aes(colour = code))

# Tides -------------------------------------------------------------------

quadra_tides <- read_csv("./data_raw/tides/HeriotBay_May2017.csv")

#delete nonsense first row
quadra_tides <- quadra_tides[-1, ]

# Add in month
quadra_tides <- quadra_tides %>% 
  mutate(month = "05", year = "2017") %>% 
  rename(time = Time, height = Height)

# make a column with both date and time
quadra_tides <- quadra_tides %>% 
  unite(date, c(Day, month, year), sep = "-", remove = TRUE) %>% 
  unite(date_time, date:time, sep = " ", remove = FALSE) 

str(quadra_tides$date)
str(quadra_tides$date_time)
str(quadra_tides$time)

# change format of date_time column
quadra_tides$date_time <- as.POSIXct(quadra_tides$date_time, format = "%d-%m-%Y %H:%M:%S", tz = "UTC")
quadra_tides$date <- as.POSIXct(quadra_tides$date, format = "%d/%m/%Y")
quadra_tides$time <- as.POSIXct(quadra_tides$time, format = "%H:%M:%S", tz = "UTC")

str(quadra_tides$date)
str(quadra_tides$date_time)
str(quadra_tides$time)


# Join Temp and Tide data for Quadra into one df --------------------------
a <- left_join(quadra_temp, quadra_tides)

# plot of tide height for the last week of May 2017 in Heriot Bay
ggplot(quadra_week, aes(x = date_time, y = Height)) + geom_point() + geom_smooth(se = FALSE)

## join two data frames, tide and temp data from last week of May


