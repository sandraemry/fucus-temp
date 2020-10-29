## Loading salinity data from Lighthouse stations

library(tidyverse)


# Nanaimo (from Departure Bay) --------------------------------------------

nanaimo_salinity <- read_csv("./data/raw_data/salinity/Departure_Bay_PBS_-_Daily_Sea_Surface_Temperature_and_Salinity_1914-2020.csv", 
                             skip = 1)

nanaimo_salinity <- nanaimo_salinity %>% 
  rename(date = `DATE (YYYY-MM-DD)`, 
         salinity = `SALINITY (PSU)`, 
         water_temp = `TEMPERATURE ( C )`) %>% 
  filter(date > "2019-01-01" & 
          date < "2020-01-01") %>% 
  select(date, salinity, water_temp) %>% 
  filter(salinity < 50) %>% 
  mutate(week = week(date))

## weekly avg of salinity 
nanaimo_salinity %>% 
  group_by(week) %>% 
  summarise(avg_salinity = mean(salinity)) %>% 
  filter(week < 52) %>% 
  ggplot(data = ., aes(x = week, y = avg_salinity)) + 
  geom_point()

 ## min salinity per week
nanaimo_salinity %>% 
  group_by(week) %>% 
  summarise(min_salinity = min(salinity)) %>% 
  filter(week > 24 & week < 36) %>% 
  ggplot(data = ., aes(x = week, y = min_salinity)) + 
  geom_point()



# Point No Point (from Race Rocks) ----------------------------------------

pnp_salinity <- read_csv("./data/raw_data/salinity/Race_Rocks_-_Daily_Sea_Surface_Temperature_and_Salinity_1921-2020.csv", 
                         skip = 1)

pnp_salinity <- pnp_salinity %>% 
  rename(date = `DATE (YYYY-MM-DD)`, 
         salinity = `SALINITY (PSU)`, 
         water_temp = `TEMPERATURE ( C )`) %>% 
  filter(date > "2019-01-01" & 
           date < "2020-01-01") %>% 
  select(date, salinity, water_temp) %>% 
  filter(salinity < 50) %>% 
  mutate(week = week(date))

## weekly avg of salinity 
pnp_salinity %>% 
  group_by(week) %>% 
  summarise(avg_salinity = mean(salinity)) %>% 
  filter(week > 24 & week < 36) %>% 
  ggplot(data = ., aes(x = week, y = avg_salinity)) + 
  geom_point()

## min salinity per week
pnp_salinity %>% 
  group_by(week) %>% 
  summarise(min_salinity = min(salinity)) %>% 
  filter(week > 24 & week < 36) %>% 
  ggplot(data = ., aes(x = week, y = min_salinity)) + 
  geom_point()


