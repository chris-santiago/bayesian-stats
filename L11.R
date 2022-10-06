library("rjags")
source("funcs.R")


dat = read.csv(file="pctgrowth.csv", header=TRUE)
head(dat)
boxplot(y ~ grp, data=dat)


mod_string = " model {
  for (i in 1:length(y)) {
    y[i] ~ dnorm(theta[grp[i]], sig)
  }
  
  for (j in 1:max(grp)) {
    theta[j] ~ dnorm(mu, tau)
  }
  
  mu ~ dnorm(0, 1e6)
  prec.tau ~ dgamma(1/2.0, 1*3.0/2.0)
  prec.sig ~ dgamma(2/2.0, 2*1.0/2.0)
  tau = sqrt( 1.0 / prec.tau )
  sig = sqrt( 1.0 / prec.sig )
} "

set.seed(102)
data_jags = as.list(dat)
str(data_jags)
params = c("theta", "tau", "mu", "sig")

mod1 = run_sim(
  model.str=mod_string,
  data=data_jags,
  params=params
)
dic1 = dic.samples(mod1$obj, n.iter=1e3)
plot(as.mcmc(mod1$csim))
round(mod1$coefs, 4)


means_anova = tapply(dat$y, INDEX=dat$grp, FUN=mean)
means_anova

plot(means_anova)
points(mod1$coefs, col="red")




library("MASS")
data("OME")

dat = subset(OME, OME != "N/A")
dat$OME = factor(dat$OME) # relabel OME
dat$ID = as.numeric(factor(dat$ID)) # relabel ID so there are no gaps in numbers (they now go from 1 to 63)

## Original reference model and covariate matrix
mod_glm = glm(Correct/Trials ~ Age + OME + Loud + Noise, data=dat, weights=Trials, family="binomial")
X = model.matrix(mod_glm)[,-1]

## Original model (that needs to be extended)
mod_string = " model {
	for (i in 1:length(y)) {
		y[i] ~ dbin(phi[i], n[i])
		logit(phi[i]) = a[ID[i]] + b[1]*Age[i] + b[2]*OMElow[i] + b[3]*Loud[i] + b[4]*Noiseincoherent[i]
	}
	
	for (h in 1:max(ID)) {
	  a[h] ~ dnorm(mu, tau)
	}
	
	mu ~ dnorm(0, 10)
	prec ~ dgamma(1/2., 1/2.)
  tau = sqrt( 1.0 / prec )
	
	for (j in 1:4) {
		b[j] ~ dnorm(0.0, 1.0/4.0^2)
	}
	
} "

data_jags = as.list(as.data.frame(X))
data_jags$y = dat$Correct
data_jags$n = dat$Trials
data_jags$ID = dat$ID
params = c("a", "b", "mu", "tau")

mod2 = run_sim(
  model.str=mod_string,
  data=data_jags,
  params=params
)

plot(mod2$sim)
plot(as.mcmc(mod2$csim))
dic.samples(mod2$obj, n.iter=1e3)
