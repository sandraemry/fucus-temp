library(tidyverse)
library(RColorBrewer)

rhiz <- read_csv("./data/tpc_rhizoid_tidy.csv")

rhiz <- rhiz %>% 
  mutate(site = as.factor(site))

# Plots -------------------------------------------------------------------

## picking a different colour palette
jColors <- brewer.pal(n = 3, "Set1")[seq_len(nlevels(as.factor(rhiz$site)))]
names(jColors) <- levels(as.factor(rhiz$site))

# calculate mean rhizoid length for each replicate
rhiz_sum <- rhiz %>% 
  mutate(temp = as.numeric(temp)) %>% 
  group_by(site, temp, rep) %>% 
  summarise(avg_rhiz = mean(length))

p <-   ggplot(data = rhiz_sum, aes(x = temp, y = avg_rhiz, color = site)) 
p + geom_point() + 
  geom_jitter(position = position_jitter(width = .5), alpha = 1/2) + 
  ylab(expression("rhizoid length "~(mu*m))) + 
  xlab("temperature (°C)") + 
  scale_color_manual(values = jColors, name = "site",
                     breaks = c("BI", "NP", "RP"),
                     labels = c("bamfield", "nanaimo", "salt spring")) + 
  theme_classic() + 
  scale_y_continuous(limits = c(0, 500))

ggsave("./figures/TPC.pdf", width = 6, height = 4)

# Models ------------------------------------------------------------------

# messing around with Alyssa's TPC code'

library(tidyverse)
library(minpack.lm)
library(broom)

rhiz$temp <- as.integer(rhiz$temp)
str(rhiz)

# used to plot the best fit models
# Input variables c, T0 (=Tmin), Tm (=Tmax), and m are estimated from the nls best fit (1b)
# h.temp is a number series that encompases the range of temperature values 
# you want to plot over 
fun.mb=function(c,T0,Tm,h.temp,m){c*(-h.temp+T0+Tm)*(Tm-h.temp)*(h.temp-T0)^(1/m)}


rhiz %>% 
  group_by(site) %>% 
  do(tidy(nlsLM(length ~ c*(-temp+T0+Tm)*(Tm-temp)*(temp-T0)^(0.65), data = rhiz, 
                start=list(c=2, T0 = 0, Tm = 35)))) %>% View # producing same value for each site

# trying to fit models to data from each site separately
np <- rhiz %>% 
  filter(site == "NP")

np_mod <- nlsLM(length ~ c*(-temp+T0+Tm)*(Tm-temp)*(temp-T0)^(0.65), data = rhiz, 
                start=list(c=2, T0 = 0, Tm = 35))

rp <- rhiz %>% 
  filter(site == "RP")

rp_mod <- nlsLM(length ~ c*(-temp+T0+Tm)*(Tm-temp)*(temp-T0)^(0.65), data = rhiz, 
                start=list(c=2, T0 = 0, Tm = 35))

bi <- rhiz %>% 
  filter(site == "BI")

bi_mod <- nlsLM(length ~ c*(-temp+T0+Tm)*(Tm-temp)*(temp-T0)^(0.65), data = rhiz, 
                start=list(c=2, T0 = 0, Tm = 35))

# same parameters for all sites? Is that accurate?

rmb<- nlsLM(length ~ c*(-temp+T0+Tm)*(Tm-temp)*(temp-T0)^(0.65), data = rhiz, 
            start=list(c=2, T0 = 0, Tm = 35))

#The output with the estimates for c, T0 and Tm
rmb

# the modified Briere fit over
tempsim <- seq(0, 35, by=.01)

# plotting the mean rhizoid length for each temperature (without separating for site)
plot(data = rhiz, length ~ temp, xlim = c(0,35), ylim = c(0,500))
ggplot(data = rhiz, aes(x = temp, y = length, color = site)) + 
  geom_point() +
  geom_jitter(position = position_jitter(width = 0.1), alpha = 1/4)

#plotting the modified Briere fit with c=12.083, T0=9.875, Tm=30.753,(from rmb) 
# and across temperature range (tempsim)
lines(tempsim, fun.mb(0.2047, 4.6376, 32.3060, tempsim, 1/0.65),
      lwd = 3, col = "deeppink") 

#from Joey

?do
