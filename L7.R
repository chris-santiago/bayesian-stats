library("car")  # load the 'car' package
data("Anscombe")  # load the data set
?Anscombe  # read a description of the data
head(Anscombe)  # look at the first few lines of the data
pairs(Anscombe)  # scatter plots for each pair of variables

mod.lm.1 = lm(education ~ ., data=Anscombe)
summary(mod.lm.1)


library("rjags")

str.mod.jags.1 = " model {
    for (i in 1:length(education)) {
        education[i] ~ dnorm(mu[i], prec)
        mu[i] = b0 + b[1]*income[i] + b[2]*young[i] + b[3]*urban[i]
    }
    
    b0 ~ dnorm(0.0, 1.0/1.0e6)
    for (i in 1:3) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
    	## Initial guess of variance based on overall
    	## variance of education variable. Uses low prior
    	## effective sample size. Technically, this is not
    	## a true 'prior', but it is not very informative.
    sig2 = 1.0 / prec
    sig = sqrt(sig2)
} "

data.jags = as.list(Anscombe)

params.jags.1 = c("b", "sig")

inits.jags.1 = function() {
  inits = list(
    "b"=rnorm(3,0.0,100.0),
    "prec"=rgamma(1,1.0,1.0)
    )
}

mod.jags.1 = jags.model(
  textConnection(str.mod.jags.1),
  data=data.jags,
  inits=inits.jags.1,
  n.chains=3
  )

update(mod.jags.1, 1000) # burn-in

sim.mod.1 = coda.samples(
  model=mod.jags.1,
  variable.names=params.jags.1,
  n.iter=5000
  )

csim.mod.1 = do.call(rbind, sim.mod.1) # combine multiple chains
plot(sim.mod.1)
gelman.diag(sim.mod.1)
autocorr.diag(sim.mod.1)
effectiveSize(sim.mod.1)
summary(sim.mod.1)

dic.samples(mod.jags.1, 1e5)

##########

str.mod.jags.2 = " model {
    for (i in 1:length(education)) {
        education[i] ~ dnorm(mu[i], prec)
        mu[i] = b0 + b[1]*income[i] + b[2]*young[i]
    }
    
    b0 ~ dnorm(0.0, 1.0/1.0e6)
    for (i in 1:2) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
    	## Initial guess of variance based on overall
    	## variance of education variable. Uses low prior
    	## effective sample size. Technically, this is not
    	## a true 'prior', but it is not very informative.
    sig2 = 1.0 / prec
    sig = sqrt(sig2)
} "

data.jags = as.list(Anscombe)

params.jags.2 = c("b", "sig")

inits.jags.2 = function() {
  inits = list(
    "b"=rnorm(2,0.0,100.0),
    "prec"=rgamma(1,1.0,1.0)
  )
}

mod.jags.2 = jags.model(
  textConnection(str.mod.jags.2),
  data=data.jags,
  inits=inits.jags.2,
  n.chains=3
)

update(mod.jags.2, 1000) # burn-in

sim.mod.2 = coda.samples(
  model=mod.jags.2,
  variable.names=params.jags.2,
  n.iter=5000
)
plot(sim.mod.2)
dic.samples(mod.jags.2, 1e5)

##############

str.mod.jags.3 = " model {
    for (i in 1:length(education)) {
        education[i] ~ dnorm(mu[i], prec)
        mu[i] = b0 + b[1]*income[i] + b[2]*young[i] + b[3]*income[i]*young[i]
    }
    
    b0 ~ dnorm(0.0, 1.0/1.0e6)
    for (i in 1:3) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
    	## Initial guess of variance based on overall
    	## variance of education variable. Uses low prior
    	## effective sample size. Technically, this is not
    	## a true 'prior', but it is not very informative.
    sig2 = 1.0 / prec
    sig = sqrt(sig2)
} "

data.jags = as.list(Anscombe)

params.jags.3 = c("b", "sig")

inits.jags.3 = function() {
  inits = list(
    "b"=rnorm(3,0.0,100.0),
    "prec"=rgamma(1,1.0,1.0)
  )
}

mod.jags.3 = jags.model(
  textConnection(str.mod.jags.3),
  data=data.jags,
  inits=inits.jags.3,
  n.chains=3
)

update(mod.jags.3, 1000) # burn-in

sim.mod.3 = coda.samples(
  model=mod.jags.3,
  variable.names=params.jags.3,
  n.iter=5000
)
plot(sim.mod.3)
dic.samples(mod.jags.3, 1e5)
 
#########
mean(sim.mod.1[, 1])
