library(scatterplot3d)
library(deSolve)

###
### Dynamic Colonization-Extinction Model
###

co <- runif(1)
e <- runif(1)

# ODE
# dPdt <- c(1-P) - e*P

# Equilibrium Occupancy
Pstar <- co/(co+e)

# Plot Eq Occupancy
ce <- expand.grid(seq(.1, 1, .1), seq(.1, 1, .1))

Pstar <- c()
for(i in 1:nrow(ce)){
  Pstar[i] <- ce[i, 1]/(ce[i, 1] + ce[i, 2])
}

plot(Pstar~ce[,1])
plot(Pstar~ce[,2])
scatterplot3d(ce[,1], ce[,2], Pstar)

co.ex <- function(times, state, parms){
  with(as.list(c(state, parms)), {
    P1 <- state[1]
    P2 <- state[2]
    P3 <- state[3]
    
    dP1 <- e21*P2 + e31*P3 - c12*P1
    dP2 <- c12*P1 - e21*P2 - c23*P2 + e32*P3
    dP3 <- c23*P2 - e32*P3 - e31*P3
    
    list(c(dP1, dP2, dP3))
  })
}

co.ex.simple <- function(times, state, parms){
  with(as.list(c(state, parms)), {
    P2 <- state[1]
    P3 <- state[2]
    
    dP2 <- c12*(1-P2-P3) - e21*P2 -c23*P2 + e32*P3
    dP3 <- c23*P2 - e32*P3 - e31*P3
    
    list(c(dP2, dP3))
  })
}

#
# Params
## Colonization
c12 <- runif(1) 
c23 <- runif(1)
## Extinction
e21 <- runif(1)
e31 <- runif(1)
e32 <- runif(1)

par <- list(c12 = c12, c23 = c23, e21 = e21, e31 = e31, e32 = e32)

matplot(ode(y = c(0,0), times = 1:100, func = co.ex.simple, parms = par)[,-1], typ = "l")
matplot(ode(y = c(.2,0,0), times = 1:100, func = co.ex, parms = par)[,-1])

