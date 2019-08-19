### load germination data for ct max experiments
rm(list = ls())
library(tidyverse)

# Load germination data ---------------------------------------------------

ctmax_germ <- read_csv("./data_raw/germination/ctmax_germination.csv")
str(ctmax_germ)

ctmax_germ <- ctmax_germ %>% 
  rename(temp = temperature) %>% 
  rename(nutrients = nutrient) %>% 
  mutate(salinity = as.factor(salinity), temp = as.factor(temp),
         nutrients = as.factor(nutrients)) %>% 
  mutate(percent_live = (((total - dead) / total)*100)) %>% 
  mutate(percent_germ = (germinated / (total))*100) %>% 
  select(-notes) %>% 
  na.omit()

# Germination: Ruckle Park ------------------------------------------------------------

# filter out data for ruckle park only
ruckle_germ <- ctmax_germ %>% 
  filter(site == "RP")

# create summary df for avg germination
ruckle_sum <- ruckle_germ  %>% 
  group_by(temp, salinity, nutrients) %>% 
  summarise(avg_germ = mean(percent_germ),
            sd_germ = sd(percent_germ),
            s_error = sd(percent_germ)/sqrt(6))

# plotting average germination of the first 50 individuals 
ggplot(data = ruckle_sum, aes(x = temp, y = avg_germ, color = salinity)) + 
  geom_point(position = position_dodge(0.5)) + 
  facet_wrap(~nutrients) +
  geom_errorbar(aes(ymin = avg_germ - s_error, ymax = avg_germ + s_error), 
                width = 0.1, position = position_dodge(0.5)) + 
  ylab("percent germination") + 
  xlab(expression('Temperature ('*~degree*C*')'))

ggsave("rp_germ.pdf", path = "./figures", plot = last_plot())

par(mfrow = c(1,1))
plot(ruckle_germ$percent_germ)
plot(log(ruckle_germ$percent_germ))
germ_mod <- lm(data = ruckle_germ, log(percent_germ + 1) ~ temp * salinity * nutrients)
plot(germ_mod)

anova(germ_mod)

# Trying to analyze RP germination with a binomial glm --------------------

#create a matrix with raw data of germinated and ungerminated
ruckle_germ <- ruckle_germ %>% 
  mutate(ungerm = total - germinated)

y <- cbind(ruckle_germ$germinated, ruckle_germ$ungerm)  

model <- glm(y ~ ruckle_germ$temp * ruckle_germ$salinity * ruckle_germ$nutrients, family = binomial)
summary(model)
anova(model)
par(mfrow = c(2,2))
plot(model)

model$deviance / model$df.residual
(model$null.deviance - model$deviance) / model$null.deviance

## try with quasi binomial to correct for overdispersion
model <- glm(y ~ ruckle_germ$temp * ruckle_germ$salinity * ruckle_germ$nutrients, family = quasibinomial)
summary(model)
anova(model)
par(mfrow = c(2,2))
plot(model)

# Germination for Neckpoint Park  -----------------------------------------
rm(list = ls())

ctmax_germ <- read_csv("./data_raw/germination/ctmax_germination.csv")

neckpoint_germ <- ctmax_germ %>% 
  mutate(salinity = as.factor(salinity)) %>% 
  mutate(temperature = as.factor(temperature)) %>% 
  mutate(total = germinated + ungerminated + dead) %>% 
  mutate(percent_live = (((total - dead) / total)*100)) %>% 
  mutate(percent_germ = (germinated / (total))*100) %>% 
  select(-notes, -tuft, -total) %>% 
  filter(site == "NP") %>% 
  na.omit()
  
# create summary df for avg germination
neckpoint_sum <- neckpoint_germ %>% 
  group_by(temperature, salinity, nutrient) %>% 
  summarise(avg_germ = mean(percent_germ),
            sd_germ = sd(percent_germ),
            s_error = sd(percent_germ)/sqrt(6))

# plotting average germination of the first 50 individuals 
ggplot(data = neckpoint_sum, aes(x = temperature, y = avg_germ, color = salinity)) + 
  geom_point(position = position_dodge(0.5)) + 
  facet_wrap(~nutrient) +
  geom_errorbar(aes(ymin = avg_germ - s_error, ymax = avg_germ + s_error), 
                width = 0.1, position = position_dodge(0.5)) + 
  ylab("percent germination") + 
  xlab(expression('Temperature ('*~degree*C*')'))

ggsave("np_germ.pdf", path = "./figures", plot = last_plot())

germ_mod <- lm(data = neckpoint_germ, log(percent_germ + 1) ~ temperature * salinity * nutrient)
plot(germ_mod)
plot(log(neckpoint_germ$percent_germ))

anova(germ_mod)

# Survival - not doing anything with this right now ----------------------------------------------------------------

survival <- ruckle %>% 
  group_by(temp, salinity, nutrients) %>% 
  summarise(avg_live = mean(percent_live), 
            std_error = sd(percent_live)/sqrt(length(percent_live)))

## sd(x)/sqrt(length(x))
ggplot(data = ruckle, aes(x = temp, y = percent_live, color = salinity)) + 
  geom_point()

ggplot(data = ruckle, aes(x = temp, y = percent_germ, color = salinity)) + 
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



# Rhizoid length - load data & separate columns ----------------------------------------------------------
rm(list = ls())

growth <- read_csv("./data_raw/growth/ctmax_rhizoid_length.csv")

## separating photo name into the correct variables & filter to RP only 
growth <- growth %>% 
  filter(!is.na(length)) %>% 
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

str(growth)

# Ruckle Park - growth data -----------------------------------------------

rp_growth <- growth %>% 
  filter(site == "RP") %>% 
  mutate(temp = as.factor(temp), 
         salinity = as.factor(salinity), 
         nutrients = as.factor(nutrients), 
         replicate = as.numeric(replicate))

## adding in zeros for zygotes that had no growth but no entry was made in the dataframe
growth_tally <- rp_growth %>% 
  group_by(site, temp, salinity, nutrients, replicate) %>% 
  tally() %>% 
  mutate(zeros_to_add = 10-n) %>% 
  filter(zeros_to_add > 0)

n.times <- growth_tally$zeros_to_add 
dum_1 <- growth_tally[rep(seq_len(nrow(growth_tally)), n.times),]

dum_2 <- as.data.frame(rep(0, sum(n.times)))

dum_2 <- dum_2 %>% 
  rename(length = `rep(0, sum(n.times))`)

growth_2 <- bind_cols(dum_1, dum_2)

rp_growth <- full_join(rp_growth, growth_2)

rp_growth <- rp_growth %>% 
  select(-c(zeros_to_add, n, individual))

rm(dum_1, dum_2, growth_2, growth_tally, n.times)

## add in zeros for the rest that had no measurements at all for any of the replicates - NOT COMPLETE 
y <- data.frame(matrix(ncol = 5, nrow = 1070))

x <- c("site", "temp", "salinity", "nutrients", "length")

colnames(y) <- x

rp_growth %>% 
  group_by(temp, salinity, nutrients, replicate) %>% 
  tally() %>% View

y$site <- rep("RP", 1070)

y$temp <- c(rep("22", 130), rep("24", 240), rep("26", 350), rep("28", 350))

y$salinity <- c(rep("10", 80), rep("20", 20), rep("30", 30), 
                rep("10", 110), rep("20", 100), rep("30", 30), 
                rep("10", 120), rep("20", 120), rep("30", 110), 
                rep("10", 110), rep("20", 120), rep("30", 120))

y$nutrients <- c(rep("CN", 30), rep("EN", 50), rep("EN", 20), 
                 rep("CN", 10), rep("EN", 20), rep("CN", 50), rep("EN", 60), 
                 rep("CN", 40), rep("EN", 60), rep("CN", 20), rep("EN", 10),
                 rep("CN", 60), rep("EN", 60), rep("CN", 60), rep("EN", 60),
                 rep("CN", 60), rep("EN", 50), rep("CN", 50), rep("EN", 60),
                 rep("CN", 60), rep("EN", 60), rep("CN", 60), rep("EN", 60))

y$length <- rep(0, 1070)

y$replicate <- c(rep(c(2,3,6),10), rep(c(1,3,4,5,6),10), rep(c(2,6), 10), 
                 rep(2, 10), rep(c(3,5), 10), rep(c(1,2,3,5,6), 10), rep(1:6, 10), rep(c(1:4), 10), 
                 rep(1:6, 10), rep(c(3,6), 10), rep(4, 10), rep(1:6, 50), rep(2:6, 10), 
                 rep(c(1,2,3,4,6), 10), rep(1:6, 50))

y <- y %>% 
  mutate(temp = as.factor(temp), 
         salinity = as.factor(salinity), 
         nutrients = as.factor(nutrients), 
         replicate = as.numeric(replicate))

# add 'y' dataframe to the growth dataframe
str(y) 
str(rp_growth)

rp_growth <- bind_rows(rp_growth, y)

rp_growth <- rp_growth %>% 
  arrange(temp, salinity, nutrients, replicate)

write_csv(rp_growth, "./data/rp_growth.csv")
 
# Ruckle Park - calculating avg growth  -------------------------------------------------

growth_sum <- rp_growth %>% 
  group_by(temp, salinity, nutrients) %>% 
  summarise(avg_growth = mean(length), 
            sd_growth = sd(length),
            s_error = sd(length)/sqrt(6))


nut_names <- list(
  'CN'="control nutrient",
  'EN'="enriched nutrient"
)


nut_labeller <- function(variable, value){
  return(nut_names[value])
}
  
# plotting avg growth for temp, sal, nut
ggplot(data = growth_sum, aes(x = temp, y = avg_growth, color = salinity)) + 
  geom_point(position = position_dodge(0.5)) + 
  facet_wrap(~nutrients, labeller = nut_labeller) +
  geom_errorbar(aes(ymin = avg_growth - s_error, ymax = avg_growth + s_error), 
                width = 0.4, position = position_dodge(0.5)) + 
  ylab("rhizoid length (um)") + 
  xlab(expression('Temperature ('*~degree*C*')')) + 
  theme_classic()


ggsave("rp_growth.pdf", path = "./figures", , width = 6, height = 4, plot = last_plot())

...

# growth model
hist(log(rp_growth$length+1))
growth_mod <- lm(data = growth, (log(length + 1) ~ temp * salinity * nutrients))
plot(growth_mod)
anova(growth_mod)
hist((log(growth$length+1)))

## trying a gamma distribution
growth_gamma_mod <- glm(data = growth, length ~ temp * salinity * nutrients, family = poisson)

growth %>% 
  filter(temp == "22", salinity == "10", nutrients == "CN") %>% 
  summarise(min_growth = min(length), max_growth = max(length))


# Neckpoint Park - growth data --------------------------------------------

np_growth <- growth %>% 
  filter(site == "NP") %>% 
  mutate(temp = as.factor(temp), 
         salinity = as.factor(salinity), 
         nutrients = as.factor(nutrients), 
         replicate = as.numeric(replicate))

str(np_growth)

np_growth %>% 
  group_by(temp, salinity, nutrients, replicate) %>%
  tally()

## adding in zeros for zygotes that had no growth but no entry was made in the dataframe
growth_tally <- np_growth %>% 
  group_by(site, temp, salinity, nutrients, replicate) %>% 
  tally() %>% 
  mutate(zeros_to_add = 10-n) %>% 
  filter(zeros_to_add > 0)

n.times <- growth_tally$zeros_to_add 
dum_1 <- growth_tally[rep(seq_len(nrow(growth_tally)), n.times),]

dum_2 <- as.data.frame(rep(0, sum(n.times)))

dum_2 <- dum_2 %>% 
  rename(length = `rep(0, sum(n.times))`)

growth_2 <- bind_cols(dum_1, dum_2)
np_growth <- full_join(np_growth, growth_2)
np_growth <- np_growth %>% 
  select(-c(zeros_to_add, n, individual))

rm(dum_1, dum_2, growth_2, growth_tally, n.times)

## add in zeros for the rest that had no measurements at all for any of the replicates - NOT COMPLETE 
np_growth %>% 
  group_by(site, temp, salinity, nutrients, replicate) %>% 
  tally()
  
y <- data.frame(matrix(ncol = 6, nrow = 620))
x <- c("site", "temp", "salinity", "nutrients", "replicate", "length")
colnames(y) <- x

y$site <- rep("NP", 620)
y$temp <- c(rep(24, 10), rep(26, 290), rep(28, 320))
y$salinity <- c(rep(20, 10), rep("10", 100), rep("20", 100), rep("30", 90), rep("10", 120), rep("20", 100),
                rep("30", 100))
y$nutrients <- c(rep("EN", 10), rep("CN", 50), rep("EN", 50), rep("CN", 50), rep("EN", 50), rep("CN", 40),
                 rep("EN", 50), rep("CN", 60), rep("EN", 60), rep("CN", 40), rep("EN", 60), 
                 rep("CN", 60), rep("EN", 40))
y$length <- rep(0, 620)
y$replicate <- c(rep(1, 10), rep(2:6, 10), rep(c(1,2,3,4,6), 10),
                 rep(c(1,2,4,5,6), 10), rep(1:5, 10), rep(c(2,4,5,6), 10),
                 rep(2:6, 10), rep(1:6, 20), rep(c(2,4,5,6), 10), rep(1:6, 20), rep(c(2,4,5,6), 10))

# add 'y' dataframe to the growth dataframe
y$temp <- as.factor(y$temp)
y$salinity <- as.factor(y$salinity)
y$nutrients <- as.factor(y$nutrients)

str(y)
str(np_growth)

np_growth <- bind_rows(np_growth, y)

rm(y)
rm(x)

np_growth <- np_growth %>% 
  arrange(temp, salinity, nutrients, replicate)

write_csv(np_growth, "./data/np_growth.csv")  

# Neckpoint Park - calculating average growth -----------------------------

growth_sum <- np_growth %>% 
  group_by(temp, salinity, nutrients) %>% 
  summarise(avg_growth = mean(length), 
            sd_growth = sd(length), 
            s_error = sd(length)/sqrt(6))

# plotting avg growth for temp, sal, nut
ggplot(data = growth_sum, aes(x = temp, y = avg_growth, color = salinity)) + 
  geom_point(position = position_dodge(0.5)) + 
  facet_wrap(~nutrients) +
  geom_errorbar(aes(ymin = avg_growth - s_error, ymax = avg_growth + s_error), 
                width = 0.4, position = position_dodge(0.5)) + 
  ylab("rhizoid length (um)") + 
  xlab(expression('Temperature ('*~degree*C*')')) + 
  theme_classic() 

ggsave("np_growth.pdf", path = "./figures", plot = last_plot())

