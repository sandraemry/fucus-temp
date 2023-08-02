# Data from summer 2018 


# Data cleaning  ----------------------------------------------------------

# load packages
library(tidyverse)

# read in data
counts <- read_csv("./data_raw/germination/TPC_germination.csv")

# separate slides into site, temp and replicate columns
counts <- counts %>% 
  rename(plate = slide) %>% 
  separate(plate, into = c("site", "temperature"), sep = 2, remove = FALSE) %>% 
  separate(temperature, into = c("temp", "replicate"), sep = "_") %>% 
  mutate(dead = (dead + tufts), 
         total = (dead + germinated + ungerminated)) %>% 
  mutate(proportion_alive = (germinated / total)*100) %>% # I'm assuming the ones that are alive but ungerminated will eventually die...
  select(-tufts)

# avg_survival <- counts %>% 
#   group_by(site, temp) %>% 
#   summarise(avg_alive = mean(proportion_alive))

# Playing around with subsetting data -------------------------------------

# I think this is to see whether it matters if I count 50 or 100 
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


# plotting survival for the 3 sites
survival_2018 <- ggplot(counts, aes(x = temp, y = proportion_alive)) + 
  geom_point(aes(colour = site), alpha = 1/2, position = position_jitter(width = 0, height = 0.05)) + 
  # scale_x_continuous(limits = c(2, 32), breaks = c(2, 8, 12, 16, 20, 24, 28, 32)) + 
  scale_color_brewer(palette = "Set1") + 
  labs(x = "temperature", y = "proportion alive") + 
  theme(
    panel.background = element_blank(), 
    strip.background = element_rect(colour="black",
                                    fill = "lightgrey"), 
    panel.border     = element_blank(), 
    panel.grid.minor = element_line(colour="grey95"),
    panel.grid.major = element_blank(), 
    axis.ticks = element_line(colour = "black"),
    axis.text.x = element_text(size = 10,
                               hjust = 1,
                               face = "plain"),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 12),
    legend.key = element_blank(),
    legend.position  = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

ggsave("./figures/survival_2018.pdf", survival_2018)

# Models ------------------------------------------------------------------

mod1 <- lm(germination$percent_germ ~ germination$temp * germination$site)
par(mfrow = c(2,2))
plot(mod1)

summary(mod1)
anova(mod1)
