# messing around with Alyssa's TPC code'

library(tidyverse)
rhiz <- read_csv("./data_raw/growth/rhizoid_lengths.csv")

rhiz$temp <- as.integer(rhiz$temp)
str(rhiz)
library(minpack.lm)

#     used to plot the best fit models
#Input variables c, T0 (=Tmin), Tm (=Tmax), and m are estimated from the nls best fit (1b)
# h.temp is a number series that encompases the range of temperature values 
# you want to plot over 
fun.mb=function(c,T0,Tm,h.temp,m){c*(-h.temp+T0+Tm)*(Tm-h.temp)*(h.temp-T0)^(1/m)}
fun.b=function(c,T0,Tm,h.temp,m){c*h.temp*(h.temp-T0)*(Tm-h.temp)^(1/m)}

rmb<- nlsLM(length ~ c*(-temp+T0+Tm)*(Tm-temp)*(temp-T0)^(0.65), data = rhiz, 
          start=list(c=12, T0 = 0, Tm = 35))

#The output with the estimates for c, T0 and Tm
rmb

# the modified Briere fit over
tempsim <- seq(0, 35, by=.01)
# Figure 1B - plotting the mean parasite reproductive output by temperature 
plot(data = rhiz, length ~ temp, xlim = c(0,35), ylim = c(0,500))
ggplot(data = rhiz, aes(x = temp, y = length, color = site)) + 
  geom_point() 

#plotting the modified Briere fit with c=12.083, T0=9.875, Tm=30.753,(from rmb) 
# and across temperature range (tempsim)
lines(tempsim, fun.mb(0.06591, 4.34452,47.17774,tempsim,1/0.65),
      lwd=3, col="deeppink") #rmb - model the estimates came from





