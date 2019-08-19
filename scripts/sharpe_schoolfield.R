

#writing the Sharpe-Schoolfield function
schoolfield_high <- function(lnc, E, Eh, Th, temp, Tc) {
  Tc <- 273.15 + Tc
  k <- 8.62e-5
  boltzmann.term <- lnc + log(exp(E/k*(1/Tc - 1/temp)))
  inactivation.term <- log(1/(1 + exp(Eh/k*(1/Th - 1/temp))))
  return(boltzmann.term + inactivation.term)
}

temp <- seq(0, 60, .1)

plot(schoolfield_high(lnc = -1, E = 0.6, Eh = 2, Th = 320, temp = temp, Tc = 20))
