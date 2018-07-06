library(TideHarmonics)
library(ggplot2)
library(dplyr)
library(scales)

## Import tide data from http://www.waterlevels.gc.ca/eng/data/predictions/2017
Data<- read.csv("./data_raw/quadra_tides_2017.csv", header=FALSE, stringsAsFactors=FALSE)


# Tides -------------------------------------------------------------------

#list of months to use to search data fram for cells with a month in the cells
months = list("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct","Nov","Dec")

# going through csv to set each date to the correct month under a column "month" 
for (i in (1:nrow(Data))){
  if (substr(Data$V1[i],1,3) %in% months == TRUE) {
    curMonth <- as.character(Data$V1[i])
    Data$Month[i] <- curMonth
  } 
  else {
    Data$Month[i] <- curMonth
  }}

# get rid of rows with extra nonsense
Data <-subset(Data, Data$V1!= "Day" & Data$V3 != "(m)" & !(substr(Data$V1,1,3) %in% months))

# make a column with year
for (i in (1:nrow(Data))){
  Data$Year[i] <- paste("20",substr(Data$Month[i],5, nchar(Data$Month[i])), sep ="")
}

# change month column to get rid of year
for (i in (1:nrow(Data))){
Data$Month[i] <- substr(Data$Month[i],1,3)
}

# change name of columns
colnames(Data) <- c("Day", "Time","Height", "Month", "Year")

#change month abbreviation to a number
mo2Num <- function(x) match(tolower(x), tolower(month.abb))
Data$Month <- sapply(Data$Month,mo2Num)

#combine day, month and year to a date column 
for (i in (1:nrow(Data))){
  Data$Date[i] <- paste(as.character(Data$Day[i]),as.character(Data$Month[i]),as.character(Data$Year[i]), sep = "/")
}

# changing formats of dates/times
Data$Date <- as.Date(Data$Date, format = "%d/%m/%Y")
Data$Time <- as.POSIXct(Data$Time,format = "%I:%M %p")
Data$Time <- format(Data$Time,"%I:%M:%S %p")
Data$DT<- as.POSIXct(paste(Data$Date, Data$Time ), format="%Y-%m-%d %I:%M:%S %p")

# get rid of one row without tide height
Data<- na.omit(Data)

# change format to tide height 
Data$Height<- as.numeric(Data$Height)


# Creating tides ----------------------------------------------------------

# look into whether aI should use hc60 or not
Data_fit <- ftide(Data$Height, Data$DT, hc60)
t1 <- as.POSIXct("2017-01-01 00:00", tz = "UTC")
t2 <- as.POSIXct("2017-12-31 00:00", tz = "UTC")

#plot model from two date endpoints given
plot(Data_fit, t1, t2)

# points are time points where we have real measured data
points(x= Data$DT, y = Data$Height)

## Set prediction frequency to i button frequency
Data_pred  <- data.frame(Height = (predict(Data_fit, t1, t2, by = (1/60))), Time = (seq(ISOdate(2017,1,1), ISOdate(2017,12,31), by = "1 min")))

Data_pred %>%
  ggplot(aes(x= Time, y= Height)) +
  geom_line() + 
  labs(x = "Date", y= "Tide height (m)") +
  scale_x_datetime(date_breaks = "1 month", labels = date_format("%b")) +
  theme_classic()


# Working with ibutton data -----------------------------------------------

temp_data <- quadra_temp
str(temp_data)

## Reformat i button date
temp_data$date_time <- as.POSIXct(temp_data$date_time, format = "%d/%m/%y %I:%M:%S %p", tz = "UTC")
str(temp_data)

## Merge files based on time and date column
merged <- merge(Data_pred,temp_data, by.x = "Time", by.y ="date_time")

## Plot Merged data

# adjust scale of sec y-axis and one variable
scale <- 5

merged %>%
  ggplot(aes(x= Time, y= Height)) +
  geom_line(aes(x= Time, y= Height),color = "blue") + 
  geom_line(aes(x= Time, y= temperature/scale), color = "red") + 
  geom_point(aes(x= Time, y= Height),color = "blue") + 
  geom_point(aes(x= Time, y= temperature/scale), color = "red") + 
  labs(x = "Date", y= "Tide height (m)") +
  scale_x_datetime(date_breaks = "1 month", labels = date_format("%m")) +
  scale_y_continuous(sec.axis = sec_axis(~.*scale, name = "Temperature")) +
  theme_bw()
  

# merged %>% 
  filter(Time >= "2017-06-01 00:41:00" & Time <= "2017-06-08 00:41:00") %>% View

merged %>% 
  filter(Time <= "2017-05-22 00:41:00")  %>% 
  filter(code == "Q1") %>% 
  ggplot(aes(x= Time, y= Height)) +
  geom_line(aes(x= Time, y= Height),color = "blue") + 
  geom_line(aes(x= Time, y= temperature/scale), color = "red") + 
  geom_point(aes(x= Time, y= Height),color = "blue") + 
  geom_point(aes(x= Time, y= temperature/scale), color = "red") + 
  labs(x = "Date", y= "Tide height (m)") +
  scale_x_datetime(date_breaks = "1 hour", labels = date_format("%H:%M")) +
  scale_y_continuous(sec.axis = sec_axis(~.*scale, name = "Temperature")) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

merged %>% 
  filter(Time <= "2017-05-28 00:41:00")  %>% 
  filter(code == "Q5") %>% 
  ggplot(aes(x= Time, y= Height)) +
  geom_line(aes(x= Time, y= Height),color = "blue") + 
  geom_line(aes(x= Time, y= temperature/scale), color = "red") + 
  geom_point(aes(x= Time, y= Height),color = "blue") + 
  geom_point(aes(x= Time, y= temperature/scale), color = "red") + 
  labs(x = "Date", y= "Tide height (m)") +
  scale_x_datetime(date_breaks = "1 day", labels = date_format("%m-%d")) +
  scale_y_continuous(sec.axis = sec_axis(~.*scale, name = "Temperature")) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))
