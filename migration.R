lvmod <- function(times, state, parms){
  with(as.list(c(state, parms)), {
    dB <- state[1:2] * parms$alpha + state[1:2] * parms$m %*% state[1:2] + c(.1, .2)*state[3:4] - c(.05, .1)*state[1:2]
    dB2 <- state[3:4] * parms$alpha + state[3:4] * parms$m %*% state[3:4] + c(.05, .1)*state[1:2] - c(.1, .2)*state[3:4]
    
    list(c(dB, dB2))
  })
}

c(10, 20) %*% matrix(c(.1, -.05, .2, -.2), 2, 2)
c(.1, .2) %*% matrix(c(10, 20, 30, 40), 2, 2)

parms <- list(alpha = c(1.5, 1), m = matrix(c(-1, .2, -.3, -1), 2, 2))

library(deSolve)

matplot(ode(runif(4), lvmod, parms, times = 1:100)[,-1], typ = "l")


c(10,10) %*% matrix(c(0, .1, .2, 0), 2, 2)
m <- matrix(runif(16), 4, 4)
diag(m) <- 0
m
c(10, 10, 10, 10) %*% m
