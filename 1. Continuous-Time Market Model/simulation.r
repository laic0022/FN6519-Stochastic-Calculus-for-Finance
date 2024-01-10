N <- 2000
t <- 0:N
dt <- 1.0 / N
mu <- 0.5
sigma <- 0.2
nsim <- 10
S <- matrix(0, nsim, N + 1)
dB <- matrix(rnorm(nsim * N, mean = 0, sd = sqrt(dt)), nsim, N + 1)
for (i in 1:nsim){
    S[i, 1] <- 1.0
    for (j in 1:N+1){
        S[i,j] <- S[i, j-1]+mu * S[i, j - 1] * dt + sigma * S[i, j - 1]  *dB[i, j]
    }
}

plot(t*dt, rep(0, N+1), xlab = "Time", ylab = "Geometric Brownian motion", lwd=2, ylim =
c(min(S),max(S)), type = "l", col = 0,las=1, cex.axis=1.5,cex.lab=1.5, xaxs='i', yaxs='i')
for (i in 1:nsim){
    lines(t * dt, S[i, ], lwd = 2, type = "l", col = i)
}