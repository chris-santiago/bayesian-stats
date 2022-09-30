library("MASS")
data("OME")
?OME # background on the data
head(OME)

any(is.na(OME)) # check for missing values
dat = subset(OME, OME != "N/A") # manually remove OME missing values identified with "N/A"
dat$OME = factor(dat$OME)
str(dat)

plot(dat$Age, dat$Correct / dat$Trials )
plot(dat$OME, dat$Correct / dat$Trials )
plot(dat$Loud, dat$Correct / dat$Trials )
plot(dat$Noise, dat$Correct / dat$Trials )

######## GLM ########

mod_glm = glm(Correct/Trials ~ Age + OME + Loud + Noise, data=dat, weights=Trials, family="binomial")
summary(mod_glm)

plot(residuals(mod_glm, type="deviance"))
plot(fitted(mod_glm), dat$Correct/dat$Trials)

X = model.matrix(mod_glm)[,-1] # -1 removes the column of 1s for the intercept
head(X)

####### JAGS ######

library("rjags")

mod_string = " model {
	for (i in 1:length(y)) {
		y[i] ~ dbin(phi[i], n[i])
		logit(phi[i]) = b[1] + b[2]*Age[i] + b[3]*OMElow[i] + b[4]*Loud[i] + b[5]*Noiseincoherent[i]
	}
	
	b[1] ~ dnorm(0.0, 1.0/5.0^2)
	for (j in 2:5) {
		b[j] ~ dnorm(0.0, 1.0/4.0^2)
	}
	
} "

data_jags = as.list(as.data.frame(X))
data_jags$y = dat$Correct # this will not work if there are missing values in dat (because they would be ignored by model.matrix). Always make sure that the data are accurately pre-processed for JAGS.
data_jags$n = dat$Trials
str(data_jags) # make sure that all variables have the same number of observations (712).
params = c("b")

run_sim = function(model.str, data, params, n_chains=3, burn_in=1e3, n_iter=5e3){
  
  model.obj = jags.model(
    file=textConnection(model.str),
    data=data,
    n.chains=n_chains
  )
  update(model.obj, burn_in)
  
  model.sim = coda.samples(
    model=model.obj,
    variable.names=params,
    n.iter=n_iter
  )
  
  model.csim = as.mcmc(do.call(rbind, model.sim))
  model.results = list(
    obj=model.obj,
    sim=model.sim,
    csim=model.csim,
    coefs=colMeans(model.csim),
    params=list(
      params=params,
      chains=n_chains,
      burn=burn_in,
      n_iter=n_iter
    )
  )
  return(model.results)
}

mod1 = run_sim(mod_string, data_jags, params)
plot(mod1$sim)
raftery.diag(mod1$sim)
mean(mod1$csim[, 2] < 0)  # prob B2 is negative
mod1$coefs

inv_logit = function(X, b){
  1 / (1 + exp(- X %*% b))
}


# Question 7
# Using the posterior mean estimates of the model coefficients, 
# create a point estimate of the probability of correct responses for a child 
# of age 60 months, with high OME, using a coherent stimulus of 50 decibels. 
x = c(1, 60, 0, 50, 0)
inv_logit(x, mod1$coefs)

phat = inv_logit(model.matrix(mod_glm), mod1$coefs)

(tab0.7 = table(phat > 0.7, (dat$Correct / dat$Trials) > 0.7))
sum(diag(tab0.7)) / sum(tab0.7)
