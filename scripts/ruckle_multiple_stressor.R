### load germination data for ct max experiments
library(tidyverse)

ctmax_germ <- read_csv("./data_raw/germination/ctmax_germination.csv")


# Germination for ruckle park  ------------------------------------------------------------

rm(list = ls())


ruckle <- ctmax_germ %>% 
  filter(site == "RP") %>% 
  mutate(salinity = as.factor(salinity)) %>% 
  mutate(temperature = as.factor(temperature)) %>% 
  mutate(percent_live = (((total - dead) / total)*100)) %>% 
  mutate(percent_germ = (germinated / (total))*100) %>% 
  select(-notes) %>% 
  na.omit()

ruckle %>% 
  group_by(temperature, salinity, nutrient) %>% 
  summarise(avg_germ = mean(percent_germ),
            sd_germ = sd(percent_germ))


  
survival <- ruckle %>% 
  group_by(temperature, salinity, nutrient) %>% 
  summarise(avg_live = mean(percent_live), 
            std_error = sd(percent_live)/sqrt(length(percent_live)))
  
## sd(x)/sqrt(length(x))
  
germination <- ruckle %>% 
  mutate(percent_germ = (germinated / (germinated + ungerminated))*100) %>% 
  group_by(temperature, salinity, nutrient) %>% 
  summarise(avg_germ = mean(percent_germ),
            std_error = sd(percent_germ)/sqrt(length(percent_germ)))

# Survival ----------------------------------------------------------------


ggplot(data = ruckle, aes(x = temperature, y = percent_live, color = salinity)) + 
  geom_point()

ggplot(data = ruckle, aes(x = temperature, y = percent_germ, color = salinity)) + 
  geom_point()

ggplot(data = ruckle, aes(x = temperature, y = percent_germ, fill = salinity)) + 
  geom_bar(stat = "identity", position = position_dodge())

mod <- lm(data = ruckle, percent_live ~ temperature * salinity * nutrient)
plot(mod)
summary(mod)
anova(mod)

mod <- lm(data = ruckle, percent_germ ~ temperature * salinity * nutrient)
par(mfrow = c(2,2))
plot(mod)
summary(mod)
anova(mod)


# Rhizoid length ----------------------------------------------------------

growth <- read_csv("./data_raw/growth/ctmax_rhizoid_length.csv")

## separating photo name into the correct variables
growth <- growth %>% 
  select(-c(Notes, legend, individual)) %>% 
  separate(photo, into = c("site", "temp", "salinity", "nutrients", "replicate", "individual"),
           sep = c(2,5,8,11,13)) %>% 
  separate(temp, into = c("temp", "drop"), 
           sep = 2) %>% 
  select(-drop) %>% 
  separate(salinity, into = c("salinity", "drop"), 
           sep = 2) %>% 
  select(-drop) %>% 
  separate(nutrients, into = c("nutrients", "drop"),
           sep = 2) %>% 
  select(-drop) %>% 
  separate(replicate, into = c("replicate", "drop"),
           sep = 1) %>% 
  select(-drop) %>% 
  separate(individual, into = c("individual", "drop"),
            sep = 1) %>% 
  select(-drop)

growth_tally <- growth %>% 
  group_by(site, temp, salinity, nutrients, replicate) %>% 
  tally() 
    
growth_tally <- growth_tally %>% 
   mutate(zeros_to_add = 10 - n)

foo <- as.data.frame(rep(0, growth_tally$zeros_to_add[1]))
length <- foo %>% 
  rename(length = `rep(0, growth_tally$zeros_to_add[1])`)

dummy <- matrix(, nrow = sum(growth_tally$zeros_to_add), ncol = 7)
names(dummy)
