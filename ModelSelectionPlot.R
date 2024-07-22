# DIC comparison for model selection, using "observed" data from a negative binomial distribution

rm(list=ls()) # Clear Environment

N <- 30
mu <- 7
k=1
variance <- mu+(mu^2)/k
Count <- rnbinom(n=N, mu=mu, size=k) # Counts drawn from negative binomial

# Load necessary library
library(rjags)
library(R2jags)

sink("PoissonFit.txt")
cat("
model {

# Prior
 lambda.est ~ dunif(0, 100)

# Likelihood
    for(i in 1:N) {
      Count[i] ~ dpois(lambda.est)
      Count.new[i] ~ dpois(lambda.est) # Replicate draws from fitted dist
      } #y
}
    ",fill=TRUE)
sink()

# Bundle data
jags.data <- list("N", "Count")

# Initial values
jags.inits <- function(){ list(lambda.est=runif(n=1, min=0, max=100))}

model.file <- 'PoissonFit.txt'

# Parameters monitored
jags.params <- c("lambda.est", "Count.new")

# Call JAGS from R
jagsfit <- jags(data=jags.data, jags.params, inits=jags.inits,
                n.chains = 3, n.thin=1, n.iter = 10000,
                model.file)
print(jagsfit)
Posterior.pois <- jagsfit$BUGSoutput$sims.list$Count.new[,]
#plot(jagsfit)

Poisson.DIC <- jagsfit$BUGSoutput$DIC

sink("NBFit.txt")
cat("
model {

# Prior
 p.est ~ dunif(0, 1)  # JAGS uses p (probability of success) for negative binomial
 k.est ~ dunif(0, 1000)
 mu.est <- k.est*(1-p.est)/p.est

# Likelihood
    for(i in 1:N) {
      Count[i] ~ dnbinom(p.est, k.est)
      Count.new[i] ~ dnbinom(p.est, k.est) # Replicate draws from fitted dist
      } #y
}
    ",fill=TRUE)
sink()

# Bundle data
jags.data <- list("N", "Count")

# Initial values
jags.inits <- function(){ list(p.est=runif(n=1, min=1E-6, max=1),
                               k.est=runif(n=1, min=0, max=1000))}

model.file <- 'NBFit.txt'

# Parameters monitored
jags.params <- c("mu.est", "k.est", "Count.new")

# Call JAGS from R
jagsfit <- jags(data=jags.data, jags.params, inits=jags.inits,
                n.chains = 3, n.thin=1, n.iter = 10000,
                model.file)
print(jagsfit)
#plot(jagsfit)
Posterior.NB <- jagsfit$BUGSoutput$sims.list$Count.new[,]
NB.DIC <- jagsfit$BUGSoutput$DIC
Poisson.DIC - NB.DIC
par(mfrow=c(3,1)) # Multi-frame plot, 3 rows, 1 col
Freq <- table(Count)  # Distribution of simulated counts
#barplot(Freq, main="", xlab="Count", ylab="Frequency")
upper <- max(Count, Posterior.pois, Posterior.NB)
hist(Count, xlim=c(0, upper), main="")
hist(Posterior.pois, xlim=c(0, upper), main="")
hist(Posterior.NB, xlim=c(0, upper), main="")

