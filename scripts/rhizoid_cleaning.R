library(tidyverse)

rhiz <- read_csv("./data_raw/growth/tpc_rhizoid_lengths.csv")

str(rhiz)

# Cleaning data ---------------

# BI15_04-10 changed to differnet measurement because it was actually taken on a mag of 10x not 20x
a <-  151.449
rhiz$length[rhiz$length == a ] <- (a * 2)

# NP25_04-1 incorrect measurement, changing to a new measurement taken on imagej
rhiz$length[rhiz$picture == "NP25_04-1"] <- 102.062

# NP30_01-1 incorrect measurement, taken on diff mag
rhiz$length[rhiz$picture == "NP30_01-1"] <- (rhiz$length[rhiz$picture == "NP30_01-1"])/2

# add in zeros for slides that only had ungerminated zygotes ie. no growth
site <- c(rep("BI", 120), rep("NP", 59), rep("RP", 120))
temp <- c(rep(25, 60), rep(30, 60), rep(30, 59), rep(25, 60), rep(30, 60))
length <- (rep(0, 299))
rep <- c(rep(1:6, 20), rep(2:6, 1), rep(1:6, 9), rep(1:6, 20))

## check and make sure the correct number of data points were added
tib <- tibble(site, temp, length, rep)
tib %>% 
  group_by(site, temp) %>% 
  count()

#separate picture out into site, temp, replicate, and picture no
rhiz <- rhiz %>% 
  separate(picture, into = c("site", "temp"), sep = 2) %>% 
  separate(temp, into = c("temp", "rep"), sep = "_") %>% 
  separate(rep, into = c("rep", "picture_no"), sep = "-") %>% 
  select(site, temp, length, rep)

# join in the tibble with zeros to main dataframe
rhiz <- rbind(rhiz, tib)

rhiz <- rhiz %>% 
  na.omit()

# write tidy data out to new csv file 
write_csv(rhiz, "./data/tpc_rhizoid_tidy.csv")

