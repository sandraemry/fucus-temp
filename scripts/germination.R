
# Data cleaning  ----------------------------------------------------------

# load packages
library(tidyverse)

# read in data
counts <- read_csv("./data_raw/germination_counts.csv")

# separate slides into site, temp and replicate columns
counts <- counts %>% 
  rename(sample = slide) %>% 
  separate(sample, into = c("site", "temperature"), sep = 2, remove = FALSE) %>% 
  separate(temperature, into = c("temp", "replicate"), sep = "_") %>% 
  mutate(dead = (dead + tufts), 
         total = (dead + germinated + ungerminated)) %>% 
  select(-tufts)

survival <- counts %>% 
  select(site, temp, replicate, dead, total) %>% 
  mutate(percent_live = (((total - dead) / total))*100) %>% 
  group_by(site, temp) %>% 
  summarise(avg_live = mean(percent_live))

germination <- counts %>% 
  select(-notes) %>% 
  mutate(percent_germ = (germinated / total)*100) %>% 
  mutate(percent_ungerm = (ungerminated / total)*100) %>% 
  na.omit() %>% 
  mutate(temp = as.integer(temp))



# Playing around with subsetting data -------------------------------------

sub <- counts %>% 
  filter(total >=50) %>% 
  mutate(other = ungerminated + dead) %>% 
  select(sample, germinated, other) %>% 
  na.omit()
  
#initialize dataframe
dum <- as.data.frame(matrix(0, ncol = 2, nrow = nrow(sub)))
names(dum) <- c("germinated", "other")

for(i in 1:nrow(sub)) {
  test <- (c(rep("germinated", sub[i,2]), rep("ungerminated", sub[i,3]))) # making a list of germinated and ungerminated that exists in each sample
  test2 <- sample(test, size = 50, replace = FALSE, prob = NULL) # sampling from each 50 times
  test3 <- as.data.frame(table(test2)) # create dataframe with above results 
  if(test3[1,1] == "germinated") { # trying to store above results in a dataframe 
    dum[i,1] <- test3[1,2] 
    dum[i,2] <- test3[2,2]
  } else 
    dum[i,2] <- test3[1,2] 
}

dum$sample <- sub$sample
dum$number <- c(rep(50, nrow(dum)))
sub$number <- c(rep(100, nrow(dum)))

test <- full_join(sub, dum)
test[is.na(test)] <- 0

test <- test %>% 
  mutate(percent_germ = germinated / (germinated + other)) %>%
  separate(sample, into = c("site", "temperature"), sep = 2, remove = FALSE) %>% 
  separate(temperature, into = c("temp", "replicate"), sep = "_") 

anova(lm(data = test, percent_germ ~ site * temp * number))

test100 <- test %>% 
  filter(number == 100) 
  
test50 <- test %>% 
  filter(number == 50)

anova(lm(data = test100, percent_germ ~ site * temp))
anova(lm(data = test50, percent_germ ~ site * temp))

dog <- test %>% 
  group_by(sample) %>% 
  summarise(max_germ = max(percent_germ), min_germ = min(percent_germ)) 

t.test(dog$max_germ, dog$min_germ)

# Plots - Survival -------------------------------------------------------------------


# plotting survival with averages as a scatterplot
ggplot(data = survival, aes(x = temp, y = avg_live, color = site)) + geom_point()

# plotting survival without averages as a scatterplot
survival <- counts %>% 
  select(site, temp, replicate, dead, total) %>% 
  mutate(percent_live = (((total - dead) / total))*100) %>% 
  na.omit()

ggplot(data = survival, aes(x = temp, y = percent_live, color = site)) + geom_point() + 
  geom_smooth()

# raw date with boxplots
ggplot(data = survival, aes(x = temp, y = percent_live, color = site)) + 
  geom_boxplot() + 
  geom_jitter(position = position_jitter(width = 0.1, height = 0), alpha = 1/4) + 
  xlab("temperature") + 
  ylab("percentage live")

length(unique(counts$slide)) == length(counts$slide)
sum((survival$site == "BI") & (survival$temp == "30"))
sum(survival$site == "RP")
sum(survival$site == "NP")

counts$slide[duplicated(counts$slide)]

counts$slide == "NP20_05"
counts$slide == "RP10_05"


# Plots - Germination  ------------------------------------------------------------


# plotting percent germination of all zygotes

ggplot(data = germination, aes(x = temp, y = percent_germ, color = site)) + 
  geom_boxplot() + 
  geom_jitter(position = position_jitter(width = 0.1, height = 0), alpha = 1/2) + 
  xlab("temperature") + 
  ylab("percentage germinated") + 
  scale_y_continuous(limits = c(0, 100))

ggplot(data = germination, aes(x = temp, y = percent_germ, color = site)) + 
  geom_point() + 
  xlab("temperature") + 
  ylab("percentage germinated") + 
  geom_smooth(lwd = 1, se = TRUE, method = "loess") + 
  scale_y_continuous(limits = c(0, 100))

# Models ------------------------------------------------------------------

mod1 <- lm(germination$percent_germ ~ germination$temp * germination$site)
par(mfrow = c(2,2))
plot(mod1)

summary(mod1)
anova(mod1)
