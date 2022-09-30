x = c(1., .8, 1.2)
B = c(1.5, -.3, 1.0)
log_y = x %*% B
y = exp(log_y)


library("rjags")
library("COUNT")
source("funcs.R")

data("badhealth")

mod_string = " model {
    for (i in 1:length(numvisit)) {
        numvisit[i] ~ dpois(lam[i])
        log(lam[i]) = B[1] + B[2]*badh[i] + B[3]*age[i] + B[4]*age[i]*badh[i]
    }
    
    B[1] ~ dnorm(0.0, 1.0/1e6)
    for (j in 2:4) {
      B[j] ~dnorm(0.0, 1.0/1e4)
    }
} "

set.seed(102)
data_jags = as.list(badhealth)
str(data_jags)
params = c("B")

mod1 = run_sim(
  model.str=mod_string,
  data=data_jags,
  params=params
)
dic1 = dic.samples(mod1$obj, n.iter=1e3)


mod_string = " model {
    for (i in 1:length(numvisit)) {
        numvisit[i] ~ dpois(lam[i])
        log(lam[i]) = B[1] + B[2]*badh[i] + B[3]*age[i]
    }
    
    B[1] ~ dnorm(0.0, 1.0/1e6)
    for (j in 2:3) {
      B[j] ~dnorm(0.0, 1.0/1e4)
    }
} "

mod2 = run_sim(
  model.str=mod_string,
  data=data_jags,
  params=params
)
dic2 = dic.samples(mod2$obj, n.iter=1e3)



dat = read.csv(file="callers.csv", header=TRUE)
call_rate = dat$calls/dat$days_active
plot(dat$isgroup2, call_rate)



mod_string = " model {
    for (i in 1:length(calls)) {
        calls[i] ~ dpois(days_active[i] * lam[i])
        log(lam[i]) = B[1] + B[2]*age[i] + B[3]*isgroup2[i]
    }
    
    for (j in 1:3) {
      B[j] ~dnorm(0.0, 1.0/1e4)
    }
} "

data_jags = as.list(dat)
str(data_jags)
params = c("B")

mod3 = run_sim(
  model.str=mod_string,
  data=data_jags,
  params=params
)

mean(mod3$csim[, 3] > 0)
