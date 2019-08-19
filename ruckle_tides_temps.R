# This script will load tide data for fulford harbour, and model it, 
# with the goal of extracting tide height for my ibuttons

library(tidyverse)
library(scales)
library(TideHarmonics)



# Load and Clean tide data ------------------------------------------------


data <- read_csv("./data_raw/Tides/fulford_tides_2018.csv", col_names = FALSE)

months = list("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct","Nov","Dec")

#create a column for the month by extracting it from the first column 
for (i in (1:nrow(data))){
  if (substr(data$X1[i], 1, 3) %in% months == TRUE) {
    curMonth <- as.character(data$X1[i])
    data$month[i] <- curMonth
  } 
  else {
    data$month[i] <- curMonth
  }}

#get rid of the 'm', 'height', etc. 
data <- data %>% 
  rename(day = X1, time = X2, height = X3) %>% 
  filter(day != "Day")

# get rid of the rows that have the month/year in the day column 
data <-subset(data, !(substr(data$day,1,3) %in% months))

#create a column for year 
data <- data %>% 
  separate(month, into = c("month", "year"), sep = "-") 

# add in a "20" in front of the year 
data$year <- paste("20", data$year, sep = "")

#change month abbreviation to a number
mo2Num <- function(x) match(x, month.abb)
data$month <- sapply(data$month, mo2Num)

data <- data %>% 
  unite("date", c("month", "day", "year"), sep = "-", remove = FALSE) %>% 
  mutate(date = mdy(date)) %>% 
  mutate(height = as.numeric(height))

# # changing formats of dates/times
data$date <- as.Date(data$date, format = "%d/%m/%Y")
data$time <- as.POSIXct(data$time,format = "%I:%M %p")
data$time <- format(data$time,"%I:%M:%S %p")
data$DT<- as.POSIXct(paste(data$date, data$time ), format="%Y-%m-%d %I:%M:%S %p")


# Create Tides! -----------------------------------------------------------

# look into whether I should use hc60 or not
data_fit <- ftide(data$height, data$DT, hc60)
t1 <- as.POSIXct("2018-05-01 00:00", tz = "UTC")
t2 <- as.POSIXct("2018-08-31 00:00", tz = "UTC") 

#plot model from two date endpoints given
plot(data_fit, t1, t2)

# points are time points where we have real measured data
points(x = data$DT, y = data$height, col = "pink")

## Set prediction frequency to i button frequency
data_pred  <- data.frame(height = (predict(data_fit, t1, t2, by = (1/60))), time = (seq(ISOdate(2018,5,1), ISOdate(2018,8,31), by = "1 min")))

data_pred  <- data.frame(height = (predict(data_fit, t1, t2, by = (1/60))), time = (seq(t1, t2, by = "1 min")))

data_pred %>%
  ggplot(aes(x= time, y= height)) +
  geom_line() + 
  labs(x = "date", y= "tide height (m)") +
  scale_x_datetime(date_breaks = "1 month", labels = date_format("%b")) +
  theme_classic()


# Ruckle ibutton data ------------------------------------------------------------

temp_data <- read_csv("./data/ruckle_temp.csv")

str(temp_data$date_time)


## Merge files based on time and date column
merged <- merge(data_pred, temp_data, by.x = "time", by.y ="date_time")

## Plot Merged data

# adjust scale of sec y-axis and one variable
scale <- 5

merged %>%
  select(time, height, temperature) %>% 
  ggplot(aes(x= time, y = height)) +
  geom_line(aes(x= time, y= height), color = "blue") + 
  geom_line(aes(x= time, y= temperature/scale), color = "red") + 
  geom_point(aes(x= time, y= height), color = "blue") + 
  geom_point(aes(x= time, y= temperature/scale), color = "red") + 
  labs(x = "date", y= "tide height (m)") +
  scale_x_datetime(date_breaks = "1 month", labels = date_format("%m")) +
  scale_y_continuous(sec.axis = sec_axis(~.*scale, name = "temperature")) +
  theme_bw()


merged %>% 
  select(-c(date, time.y)) %>%
  filter(time >= "2018-06-09 00:14:00" & time <= "2018-06-16 00:41:00") %>% 
  filter(code == 3) %>% 
  filter(ibutton_no == "r1.") %>% 
  ggplot(aes(x= time, y= height)) +
  geom_line(aes(x= time, y= height),color = "blue") + 
  geom_line(aes(x= time, y= temperature/scale), color = "red") + 
  geom_point(aes(x= time, y= height),color = "blue") + 
  geom_point(aes(x= time, y= temperature/scale), color = "red") + 
  labs(x = "Date", y= "Tide height (m)") +
  scale_x_datetime(date_breaks = "1 hour", labels = date_format("%H:%M")) +
  scale_y_continuous(sec.axis = sec_axis(~.*scale, name = "Temperature")) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

merged %>% 
  select(-c(date, time.y)) %>%
  filter(time >= "2018-06-14 00:00:00" & time <= "2018-06-14 24:00:00") %>% 
  filter(code == 3) %>% 
  filter(ibutton_no == "r1.") %>% 
  ggplot(aes(x= time, y= height)) +
  geom_line(aes(x= time, y= height),color = "blue") + 
  geom_line(aes(x= time, y= temperature/scale), color = "red") + 
  geom_point(aes(x= time, y= height),color = "blue") + 
  geom_point(aes(x= time, y= temperature/scale), color = "red") + 
  labs(x = "Date", y= "Tide height (m)") +
  scale_x_datetime(date_breaks = "1 hour", labels = date_format("%H:%M")) +
  scale_y_continuous(sec.axis = sec_axis(~.*scale, name = "Temperature")) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

## trying to write an algorithm to include only daytime hours when temp goes high
#first, some more data cleaning
merged <- merged %>% 
  separate(date, into = c("date", "drop"), sep = " ", remove = TRUE) %>% 
  select(-drop) %>% 
  rename(date_time = time, time = time.y) %>% 
  
for (i in (1:(nrow(merged)-1))) {
  diff <- merged$temperature[i+1] - merged$temperature[i]
  merged$temp_diff[i+1] <- diff
}


# find out where temperature drops the most, for each ibutton in each day
 temp_drops <- merged %>% 
  mutate(unique_id = row_number()) %>% 
  group_by(code, ibutton_no, date) %>% 
  summarise(max_temp_drop = min(temp_diff)) %>% 
  filter(max_temp_drop <= -5) # filtering out the days that didn't get very hot

View(inner_join(temp_drops, merged))

mered <- merged %>% 
  filter(code == "1" & ibutton_no == "r1.") %>% 
  separate(date, into = c("date", "drop"), sep = " ", remove = TRUE) %>% 
  select(-drop) %>% View
  mutate(unique_id = row_number()) %>% 
  group_by(date) %>% 
  summarise(max_temp = max(temperature)) %>% View

         







