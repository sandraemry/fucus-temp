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
rhiz <- rhiz %>% 
  separate(picture, into = c("site", "temp", "drop1", "rep", "drop2", "photo_id"), sep = c(2,4,5,7,8)) %>% 
  select(-c("drop1", "drop2"))

## Do I have at least 10 measurements for each replicate?
no_of_lengths <- rhiz %>% 
  group_by(site, temp, rep) %>% 
  tally()

df <- do.call("rbind", replicate(1, no_of_lengths[no_of_lengths$n == 9, ], simplify = FALSE))
df$length <- 0
df <- df %>% select(-n)

df_8 <- do.call("rbind", replicate(2, no_of_lengths[no_of_lengths$n == 8, ], simplify = FALSE))
df_8$length <- 0
df_8 <- df_8 %>% 
  select(-n) %>% 
  arrange(site, temp, rep)
df <- full_join(df, df_8)

df_7 <- do.call("rbind", replicate(3, no_of_lengths[no_of_lengths$n == 7, ], simplify = FALSE))
df_7$length <- 0
df_7 <- df_7 %>% 
  select(-n) %>% 
  arrange(site, temp, rep)
df <- full_join(df, df_7)

df_6 <- do.call("rbind", replicate(4, no_of_lengths[no_of_lengths$n == 6, ], simplify = FALSE))
df_6$length <- 0
df_6 <- df_6 %>% 
  select(-n) %>% 
  arrange(site, temp, rep)
df <- full_join(df, df_6)

df_5 <- do.call("rbind", replicate(5, no_of_lengths[no_of_lengths$n == 5, ], simplify = FALSE))
df_5$length <- 0
df_5 <- df_5 %>% 
  select(-n) %>% 
  arrange(site, temp, rep)
df <- full_join(df, df_5)

df_4 <- do.call("rbind", replicate(6, no_of_lengths[no_of_lengths$n == 4, ], simplify = FALSE))
df_4$length <- 0
df_4 <- df_4 %>% 
  select(-n) %>% 
  arrange(site, temp, rep)
df <- full_join(df, df_4)

df_3 <- do.call("rbind", replicate(7, no_of_lengths[no_of_lengths$n == 3, ], simplify = FALSE))
df_3$length <- 0
df_3 <- df_3 %>% 
  select(-n) %>% 
  arrange(site, temp, rep)
df <- full_join(df, df_3)

df_1 <- do.call("rbind", replicate(9, no_of_lengths[no_of_lengths$n == 1, ], simplify = FALSE))
df_1$length <- 0
df_1 <- df_1 %>% 
  select(-n) %>% 
  arrange(site, temp, rep)
df <- full_join(df, df_1)

rm(df_8, df_7, df_6, df_6, df_6, df_5, df_4, df_3, df_1)

rhiz <- full_join(rhiz, df)
rhiz <- rhiz %>% 
  arrange(site, temp, rep)

rhiz$length[which(is.na(rhiz$length))] <- 0

# write tidy data out to new csv file 
write_csv(rhiz, "./data/tpc_rhizoid_tidy_2018.csv")


