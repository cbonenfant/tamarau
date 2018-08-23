library(R2jags)

setwd("C:/Users/Manon GHISLAIN/Documents/tamarau/datas pour analyses")
load("data.Rdata")

## State space model lambda~site:year------------

data_ssm<-data[which(data$year>=2006 & data$count=="AN"),] #& data$numsite!="17" & data$numsite!="18"),]
table_data<-( aggregate(data_ssm$Number, by=list(year=data_ssm$year, site=data_ssm$numsite), FUN=sum) )

#table_data$x[table_data$x==0]<-0.0000000001

tab_y <- xtabs(x ~ site + year, table_data)
tab_y <- as.data.frame.matrix(tab_y)

n.years<-ncol(tab_y)
n.sites<-nrow(tab_y)

#tab_y_log<-log(tab_y)

# Specify model in BUGS language

sink("ssm1.txt")
cat("
    model {
    
    # Priors and constraints
    
    for(s in 1:S){
    N.est[s,1] ~ dunif(1, 150)        # Prior for initial population size
    }
    mean.lambda ~ dunif(0, 2)         # Prior for mean growth rate
    sigma.proc ~ dunif(0, 10)       # Prior sd of state process
    sigma2.proc <- pow(sigma.proc, 2)
    tau.proc <- pow(sigma.proc, -2)
    
    for(s in 1:S){                       # Prior sd of observation process for each site
    sigma.obs[s] ~ dunif(0, 10)       
    sigma2.obs[s] <- pow(sigma.obs[s], 2)
    tau.obs[s] <- pow(sigma.obs[s], -2)
    }    

    

    # Likelihood
    
    # State process
    
    for (t in 1:(Y-1)){
      for(s in 1:S){
    lambda[s,t] ~ dnorm(mean.lambda, tau.proc)T(-10, 1.15)
    N.est[s,t+1] <- N.est[s,t] * lambda[s,t] }
    }

    # Observation process
    
    for (t in 1:Y){
      for(s in 1:16){
    y[s,t] ~ dnorm(N.est[s,t], tau.obs[s]) }
    }

    for(s in 17:18){
      for (t in 4:Y){
    y[s,t] ~ dnorm(N.est[s,t], tau.obs[s]) }
    }

    }
    ",fill = TRUE)
sink()


# Bundle data
jags.data <- list(y = tab_y, Y = n.years, S = n.sites)
# Initial values
inits <- function(){list(sigma.proc = runif(1, 0, 5),
                         mean.lambda = runif(1, 0.5, 1.5),
                         sigma.obs = runif(n.sites, 0, 6),
                         N.est = array(c(runif(n.sites, 1, 150), rep(NA, (n.sites*(n.years-1))) ) , dim = c(n.sites,n.years) )
                                     )}

# Parameters monitored
parameters <- c("lambda", "mean.lambda",
                "sigma.obs", "sigma.proc",
                "sigma2.obs", "sigma2.proc", "tau.obs",
                "N.est")

# MCMC settings
ni <- 25000
nb <- floor(ni/4)
nt <- max(1, floor((ni - nb) / 1000))
nc <- 3

# Call Jags from R
ssm1 <- jags(jags.data, inits, parameters, "ssm1.txt", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)


#plot R-hat pour convergence
plot(ssm1$BUGSoutput$summary[,8])


#graph to summarize results
fitted <- lower <- upper <- array( NA, dim=c(n.sites,n.years))

for (y in 1:n.years){
  for (s in 1:n.sites){
  fitted[s,y] <- mean(ssm1$BUGSoutput$sims.list$N.est[,s,y])
  lower[s,y] <- quantile(ssm1$BUGSoutput$sims.list$N.est[,s,y], 0.025)
  upper[s,y] <- quantile(ssm1$BUGSoutput$sims.list$N.est[,s,y], 0.975)
}}

m1 <- min(c(min(tab_y), fitted,lower))
m2 <- max(c(max(tab_y), fitted, upper))
par(mar = c(4.5, 4, 1, 1), cex = 1.2)

plot(0, 0, ylim = c(m1, m2), xlim = c(0.5, n.years),
     ylab = "Population size", xlab = "Year", las = 1,
     col = "black", type = "l", lwd = 2, frame = FALSE, axes = FALSE)
axis(2, las = 1)
axis(1, at = 0:n.years, labels = rep(c(NA,2006:2018)), tcl = -0.25)
polygon(x = c(1:n.years, n.years:1), y = c(lower[18,], upper[18,n.years:1]),
        col = "grey90", border = "grey90")
points(y, type = "l", col = "black", lwd = 2)
points(fitted[18,], type = "l", col = "blue", lwd = 2)
legend(x = 1, y = m2, legend = c( "Observed", "Estimated"),
       lty = c(1, 1, 1), lwd = c(2, 2, 2), col = c( "black", "blue"),
       bty = "n", cex = 1)

dev.copy2pdf(file = "Fig_tamarau_2.pdf")
dev.off()


## State space model lambda~site+year (ne fonctionne pas)------------
# Specify model in BUGS language

sink("ssm2.txt")
cat("
    model {
    
    # Priors and constraints
    
    for(s in 1:S){
    N.est[s,1] ~ dunif(1, 200)        # Prior for initial population size
    }
    mean.lambda ~ dunif(0, 2)         # Prior for mean growth rate
    sigma.proc ~ dunif(0, 10)       # Prior sd of state process
    sigma2.proc <- pow(sigma.proc, 2)
    tau.proc <- pow(sigma.proc, -2)

    mean.epsilon ~ dunif(0, 2)         # Prior for mean growth rate by site
    sigma.epsilon ~ dunif(0, 10)       # Prior sd of state process by site
    sigma2.epsilon <- pow(sigma.epsilon, 2)
    tau.epsilon <- pow(sigma.epsilon, -2)
    
    for(s in 1:S){                       # Prior sd of observation process for each site
    sigma.obs[s] ~ dunif(0, 10)       
    sigma2.obs[s] <- pow(sigma.obs[s], 2)
    tau.obs[s] <- pow(sigma.obs[s], -2)
    }    
    
    
    
    # Likelihood
    
    # State process
    
    for (t in 1:(Y-1)){
    for(s in 1:S){
    lambda[t] ~ dnorm(mean.lambda, tau.proc)T(-10, 1.15)
    epsilon[s] ~ dnorm(mean.epsilon, tau.epsilon)T(-10, 1.15)
    N.est[s,t+1] <- N.est[s,t] * (epsilon[s] + lambda[t] )}
    }
    
    # Observation process
    
    for (t in 1:Y){
    for(s in 1:16){
    y[s,t] ~ dnorm(N.est[s,t], tau.obs[s]) }
    }
    
    for(s in 17:18){
    for (t in 4:Y){
    y[s,t] ~ dnorm(N.est[s,t], tau.obs[s]) }
    }
    
    }
    ",fill = TRUE)
sink()


# Bundle data
jags.data <- list(y = tab_y, Y = n.years, S = n.sites)
# Initial values
inits <- function(){list(sigma.proc = runif(1, 0, 5),
                         sigma.epsilon = runif(1, 0, 5),
                         mean.lambda = runif(1, 0.5, 1.5),
                         mean.epsilon = runif(1, 0.5, 1.5),
                         sigma.obs = runif(n.sites, 0, 6),
                         N.est = array(c(runif(n.sites, 1, 150), rep(NA, (n.sites*(n.years-1))) ) , dim = c(n.sites,n.years) )
)}

# Parameters monitored
parameters <- c("lambda", "mean.lambda", "mean.epsilon", "epsilon",
                "sigma.obs", "sigma.proc", "sigma.epsilon", "sigma2.epsilon",
                "sigma2.obs", "sigma2.proc", "tau.obs",
                "N.est")

# MCMC settings
ni <- 25000
nb <- floor(ni/4)
nt <- max(1, floor((ni - nb) / 1000))
nc <- 3

# Call Jags from R
ssm2 <- jags(jags.data, inits, parameters, "ssm2.txt", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)



## State space model lambda~random(year)------------
# Specify model in BUGS language
tot_y<-colSums(tab_y)

sink("ssm3.txt")
cat("
    model {
    
    # Priors and constraints
    
    N.est[1] ~ dunif(1, 200)        # Prior for initial population size
    
    for(s in 1:S){
    N.est.site[s,1]~ dunif(1, 50) 
    }
    

    mean.lambda ~ dunif(0, 2)         # Prior for mean growth rate
    sigma.proc ~ dunif(0, 10)       # Prior sd of state process
    sigma2.proc <- pow(sigma.proc, 2)
    tau.proc <- pow(sigma.proc, -2)

    sigma.temps ~ dunif(0,100) 
    tau.temps <- pow(sigma.temps, -2)
    random.temps[1] ~  dunif(0,100) 

    for(s in 1:S){                       # Prior sd of observation process for each site
    sigma.obs[s] ~ dunif(0, 10)       
    sigma2.obs[s] <- pow(sigma.obs[s], 2)
    tau.obs[s] <- pow(sigma.obs[s], -2)
    }    
    
    # Likelihood
    
    # State process

    for (t in 1:(Y-1)){
    random.temps[t+1] ~ dnorm(0, tau.temps)
    lambda[t] ~ dnorm(mean.lambda, tau.proc)T(-10, 1.15)
    N.est[t+1] <- N.est[t]* lambda[t] + random.temps[t+1] 
    N.est[t+1]<-colSums(N.est.site[,t+1])
    }
    
    
    # Observation process

    for (t in 1:Y){
    for(s in 1:16){
    y[s,t] ~ dnorm(N.est.site[s,t], tau.obs[s]) }
    }

    for(s in 17:18){
    for (t in 4:Y){
    y[s,t] ~ dnorm(N.est.site[s,t], tau.obs[s]) }
    }
    
    }
    ",fill = TRUE)
sink()


# Bundle data
jags.data <- list(y = tab_y, Y = n.years, S = n.sites)
# Initial values
inits <- function(){list(sigma.proc = runif(1, 0, 5),
                         mean.lambda = runif(1, 0.5, 1.5),
                         sigma.temps = runif(1, 0, 100),
                         sigma.obs = runif(n.sites, 0, 6),
                         N.est.site = array(c(runif(n.sites, 1, 150), rep(NA, (n.sites*(n.years-1))) ) , dim = c(n.sites,n.years) )
        )}

# Parameters monitored
parameters <- c("random.temps",
                "sigma.obs", "sigma.temps",
                "sigma2.obs", "tau.temps", "tau.obs",
                "N.est", "lambda", "mean.lambda",
                 "sigma.proc", "sigma2.proc"
                )

# MCMC settings
ni <- 250000
nb <- floor(ni/4)
nt <- max(1, floor((ni - nb) / 1000))
nc <- 3

# Call Jags from R
ssm3 <- jags(jags.data, inits, parameters, "ssm3.txt", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)


#plot R-hat pour convergence
plot(ssm3$BUGSoutput$summary[,8])


#graph to summarize results
fitted <- lower <- upper <- numeric()
for (i in 1:n.years){
  fitted[i] <- mean(ssm3$BUGSoutput$sims.list$N.est[,i]) + mean(ssm3$BUGSoutput$sims.list$random.temps[,i])
  lower[i] <- quantile(ssm3$BUGSoutput$sims.list$N.est[,i], 0.025)
  upper[i] <- quantile(ssm3$BUGSoutput$sims.list$N.est[,i], 0.975)
}

m1 <- min(c(fitted,lower))
m2 <- max(c(fitted, upper))
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

