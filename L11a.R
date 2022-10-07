library(tidyverse)

data = read_table(file="cookies.txt")
head(data)
table(data$location)

ggplot(data, aes(x=chips)) +
  geom_histogram(bins = 11)

ggplot(data, aes(x=location, y=chips, group=location)) +
  geom_boxplot()


library(rjags)
source("funcs.R")

mod_string = " model {
  for (i in 1:length(chips)) {
    chips[i] ~ dpois(lam[location[i]])
  }
  
  for (j in 1:max(location)) {
    lam[j] ~ dgamma(alpha, beta)
  }
  
  alpha = mu^2 / sig^2
  beta = mu / sig^2
  
  mu ~ dgamma(2.0, 1.0/5.0)
  sig ~ dexp(1.0)
  
} "

set.seed(113)
data_jags = as.list(data)
params = c("lam", "mu", "sig")

mod.1 = run_sim(
  model.str = mod_string,
  data = data_jags,
  params = params
)

#adjust plot margins
par(mar = c(1, 1, 1, 1))
plot(mod.1$sim)

gelman.diag(mod.1$sim)
autocorr.diag(mod.1$sim)
autocorr.plot(mod.1$sim)
effectiveSize(mod.1$sim)

dic = dic.samples(mod.1$obj, n.iter=1e3)
dic

#observation level residuals
(params.post = colMeans(mod.1$csim))
yhat = rep(params.post[1:5], each=30)
resid = data$chips - yhat
gg_resid(resid)

summary(mod.1$sim)
