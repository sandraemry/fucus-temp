library(tidyverse)
library(minpack.lm)

growth <- read_csv("./data/tpc_rhizoid_tidy.csv")

str(growth)

growth <- growth %>% 
 mutate(site = as.factor(site)) 


# TPC model ---------------------------------------------------------------


# used to plot the best fit models
# Input variables c, T0 (=Tmin), Tm (=Tmax), and m are estimated from the nls best fit (1b)
# h.temp is a number series that encompases the range of temperature values 
# you want to plot over 
fun.mb = function(c, T0, Tm, h.temp, m){c * (-h.temp + T0 + Tm)*(Tm -h.temp)*(h.temp - T0)^(1/m)}

fun.b = function(c, T0, Tm, h.temp, m){c * h.temp * (h.temp - T0)*(Tm - h.temp)^(1/m)}



# Salt Spring TPC ---------------------------------------------------------

ruckle <- growth %>% 
  filter(site == "RP")


rmb<- nlsLM(length ~ c*(-temp + T0 + Tm)*(Tm - temp)*(temp - T0)^(0.65), data = ruckle, 
            start = list(c = 12, T0 = 0, Tm = 35))

#The output with the estimates for c, T0 and Tm
rmb  

# the modified Briere fit over
tempsim <- seq(0, 35, by=.01)

# Figure 1B - plotting the growth by temperature 
plot(data = ruckle, length ~ temp, xlim = c(0,35), ylim = c(0,500))

ggplot(data = ruckle, aes(x = temp, y = length, color = site)) + 
  geom_point() 

#plotting the modified Briere fit with c=0.2161, T0=4.4868, Tm=31.4644,(from rmb) 
# and across temperature range (tempsim)
lines(tempsim, fun.mb(0.06591, 4.34452,47.17774,tempsim,1/0.65),
      lwd=3, col="deeppink") #rmb - model the estimates came from


# Nanaimo -----------------------------------------------------------------

nanaimo <- growth %>% 
  filter(site == "NP")

nmb<- nlsLM(length ~ c*(-temp + T0 + Tm)*(Tm - temp)*(temp - T0)^(0.65), data = nanaimo, 
            start = list(c = 12, T0 = 0, Tm = 35))

#The output with the estimates for c, T0 and Tm
nmb  


# Bamfield ----------------------------------------------------------------

bamfield <- growth %>% 
  filter(site == "BI")
  
bmb<- nlsLM(length ~ c*(-temp + T0 + Tm)*(Tm - temp)*(temp - T0)^(0.65), data = bamfield, 
            start = list(c = 12, T0 = 0, Tm = 35))

#The output with the estimates for c, T0 and Tm
bmb  
