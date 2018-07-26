library(R2jags)
load("data.Rdata")

##### State-space models for population counts -----
#ouvrir le Rdata modifié dans partie 1
#à partir de 2006 :
data_ssm1<-data[which(data$year>=2006 & data$count=="AN"),]
y<-( aggregate(data_ssm1$Number, by=list(YEAR=data_ssm1$year), FUN=sum) )[,2]
n.years<-length(y)

# Specify model in BUGS language

sink("ssm.txt")
cat("
model {

# Priors and constraints

N.est[1] ~ dunif(100, 500)        # Prior for initial population size
mean.lambda ~ dunif(0, 2)         # Prior for mean growth rate
sigma.proc ~ dunif(0, 10)       # Prior sd of state process
sigma2.proc <- pow(sigma.proc, 2)
tau.proc <- pow(sigma.proc, -2)
sigma.obs ~ dunif(0, 100)       # Prior sd of observation process
sigma2.obs <- pow(sigma.obs, 2)
tau.obs <- pow(sigma.obs, -2)

# Likelihood

# State process

for (t in 1:(T-1)){
lambda[t] ~ dnorm(mean.lambda, tau.proc)T(-10, 1.15)
N.est[t+1] <- N.est[t] * lambda[t] }

# Observation process

for (t in 1:T){
y[t] ~ dnorm(N.est[t], tau.obs) }

}
    ",fill = TRUE)
sink()


# Bundle data
jags.data <- list(y = y, T = n.years)
# Initial values
inits <- function(){list(sigma.proc = runif(1, 0, 5),
                         mean.lambda = runif(1, 0.5, 1.5),
                         sigma.obs = runif(1, 0, 10),
                         N.est = c(runif(1, 100, 500),rep(NA, (n.years-1))))}
# Parameters monitored
parameters <- c("lambda", "mean.lambda",
                "sigma.obs", "sigma.proc",
                "sigma2.obs", "sigma2.proc",
                "N.est")

# MCMC settings
ni <- 250000
nb <- floor(ni/4)
nt <- max(1, floor((ni - nb) / 1000))
nc <- 3

# Call Jags from R
ssm <- jags(jags.data, inits, parameters, "ssm.txt", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)


#plot R-hat pour convergence
plot(ssm$BUGSoutput$summary[,8])


#graph to summarize results
  fitted <- lower <- upper <- numeric()
  n.years <- length(y)
  for (i in 1:n.years){
    fitted[i] <- mean(ssm$BUGSoutput$sims.list$N.est[,i])
    lower[i] <- quantile(ssm$BUGSoutput$sims.list$N.est[,i], 0.025)
    upper[i] <- quantile(ssm$BUGSoutput$sims.list$N.est[,i], 0.975)
    }

  m1 <- min(c(y, fitted,lower))
  m2 <- max(c(y, fitted, upper))
  par(mar = c(4.5, 4, 1, 1), cex = 1.2)

  plot(0, 0, ylim = c(m1, m2), xlim = c(0.5, n.years),
ylab = "Population size", xlab = "Year", las = 1,
col = "black", type = "l", lwd = 2, frame = FALSE, axes = FALSE)
  axis(2, las = 1)
  axis(1, at = 0:n.years, labels = rep(c(NA,2006:2018)), tcl = -0.25)
  polygon(x = c(1:n.years, n.years:1), y = c(lower, upper[n.years:1]),
          col = "grey90", border = "grey90")
  points(y, type = "l", col = "black", lwd = 2)
  points(fitted, type = "l", col = "blue", lwd = 2)
  legend(x = 1, y = m2, legend = c( "Observed", "Estimated"),
         lty = c(1, 1, 1), lwd = c(2, 2, 2), col = c( "black", "blue"),
         bty = "n", cex = 1)

dev.copy2pdf(file = "Fig_tamarau_2.pdf")
dev.off()



## --------------------------------------------------
##
##  Modèle de densité-dépendance simple (Gompertz)
##
## --------------------------------------------------


# Specify model in BUGS language

sink("ssm_gompertz.txt")
cat("
model {

  ##
  ## Priors and constraints
  ##

  N.est[1] ~ dunif(100, 500)        # Prior for initial population size
  K ~ dunif(300, 1000)              # Prior for carrying capacity
  mean.lambda ~ dunif(0, 2)         # Prior for mean growth rate
  sigma.proc ~ dunif(0, 10)         # Prior sd of state process
  sigma2.proc <- pow(sigma.proc, 2)
  tau.proc <- pow(sigma.proc, -2)
  sigma.obs ~ dunif(0, 100)         # Prior sd of observation process
  sigma2.obs <- pow(sigma.obs, 2)
  tau.obs <- pow(sigma.obs, -2)
  beta ~ dnorm(0, 0.0001)           # Prior for density-dependance


  ##
  ## Likelihood
  ##

  # State process
  for (t in 2:(T)){
    lambda[t] ~ dnorm(mean.lambda, tau.proc)T(-10, 1.15)
    ## Modèle de Gompertz --> densité-dépendance 
    ## tu peux essayer une autre fonction (theta-logistic)
    N.est[t] <- N.est[1]* exp(log(K / N.est[1]) * (1 - exp(-beta * t)))
  }

  # Observation process
  for (t in 1:T){
  y[t] ~ dnorm(N.est[t], tau.obs) }

}
",fill = TRUE)
sink()


# Bundle data
jags.data <- list(y = y, T = n.years)
# Initial values
inits <- function(){list(sigma.proc = runif(1, 0, 5),
                         mean.lambda = runif(1, 0.5, 1.5),
                         sigma.obs = runif(1, 0, 10),
                         N.est = c(runif(1, 100, 500),rep(NA, (n.years-1))),
                         K = runif(1, 500, 1000),
                         beta = rnorm(1, 0, 5)
)}
# Parameters monitored
parameters <- c("lambda", "mean.lambda",
                "sigma.obs", "sigma.proc",
                "sigma2.obs", "sigma2.proc",
                "N.est", "K", "beta")
# MCMC settings
ni <- 250000
nb <- floor(ni/4)
nt <- max(1, floor((ni - nb) / 1000))
nc <- 3

# Call Jags from R
ssm.gompertz <- jags(jags.data, inits, parameters, "ssm_gompertz.txt",
    n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)

#                mu.vect  sd.vect
# beta           0.078    0.035
#
# Effet densité-dépendant négatif

#plot R-hat pour convergence
plot(ssm.gompertz$BUGSoutput$summary[,8])

#graph to summarize results
fitted <- lower <- upper <- numeric()
n.years <- length(y)
for (i in 1:n.years){
  fitted[i] <- mean(ssm.gompertz$BUGSoutput$sims.list$N.est[,i])
  lower[i] <- quantile(ssm.gompertz$BUGSoutput$sims.list$N.est[,i], 0.025)
  upper[i] <- quantile(ssm.gompertz$BUGSoutput$sims.list$N.est[,i], 0.975)
  }

m1 <- min(c(y, fitted,lower))
m2 <- max(c(y, fitted, upper))
out <- cbind.data.frame(obs = y, fitted, lower, upper, yr = 1:n.years)

library(ggplot2)
p <- ggplot(out, aes(x = yr, y = obs)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .3) +
    geom_line(linetype = "dotted") +
    geom_point(size = 4, colour = "black", fill = "grey20") +
    geom_point(aes(y = fitted), size = 2, colour = "blue") +
    geom_line(aes(y = fitted), colour = "blue") +
    xlab("Time (in years)") +
    ylab("Population Size")
p
dev.copy2pdf(file = "Fig_tamarau_3.pdf")
dev.off()

