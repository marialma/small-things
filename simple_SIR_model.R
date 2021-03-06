## Base SIR model for creating a shiny app to illustrate herd immunity 
## future goals: represent using network graphs instead of ODEs
## a lot of this code was shamelessly lifted from Dr. John Marshall's class.

rm(list=ls())
setwd("~/Documents/GitHub/small-things")
library(ggplot2)
library(deSolve)
SIR_ode <- function(time, state, theta) {
  beta <- theta["R0"] / theta["D"]
  rec <- 1 / theta["D"]
  
  ## States:
  S <- state["S"]
  I <- state["I"]
  R <- state["R"]
  N <- S + I + R
  
  ## ODEs:
  dS <- -(beta * S * I/N)
  dI <- (beta * S * I/N) - (rec * I)
  dR <- rec * I
  
  return(list(c(dS, dI, dR)))
}


theta <- c(R0 = 1.4, D = 7, N = 100)

initState <- c(S=49, I=1, R=50)

# 
# R0 = basic reproductive number
## - should I split this into infectivity and average contact rates? 
# D = infectious period (how long someone is infectious)
# N = total population
times <- seq(0, 365, by = 1) #how long to run this
trajModel <- data.frame(ode(y=initState, times=times, func=SIR_ode, 
                            parms=theta, method = "ode45"))

newdat <- reshape2::melt(trajModel,id="time")
ggplot(newdat) +
  geom_line(aes(x=time,y=value,color=variable)) + labels(x="Time (days)", y="Population")
  scale_color_manual(values = c("black", "red", "blue"), labels = c("Susceptible", "Infected", "Resistant/Recovered"))


ggplot(trajModel, aes(x=time))+ geom_line(aes(y=I), color = 'blue') + 
  geom_line(aes(y=R, label = R), color = 'red') +
  geom_line(aes(y=S))

#infectednumbers <- theta["N"] - initState["R"] - tail(trajModel$S, n=1)
#df <- data.frame(vax = c("vax"), total=theta["N"],inf=c(infectednumbers))
#df<- mutate(df, pct = inf/total)
#ggplot(df, aes(x=vax, y=pct)) + geom_col() + ylim(0,1)
# want: slides for people to adjust R0, infectious period, total population. 