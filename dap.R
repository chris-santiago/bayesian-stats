library(tidyverse)
library(rjags)

source("funcs.R")

data = datasets::chickwts %>%
  mutate(
    feed_name = feed,
    feed = as.numeric(feed)
  )
head(data)
table(data$feed_name)

ggplot(data, aes(x=feed_name, y=weight, group=feed_name)) +
  geom_boxplot()

########### Base Model ##########
mod.base = lm(weight ~ feed_name + 0, data=data)
summary(mod.base)

########## Pooled Model ###########
mod_string = " model {
  for (i in 1:length(weight)) {
    weight[i] ~ dnorm(mu, sigma)
  }
  
  mu ~ dnorm(0, 1/1e6)
  prec ~ dexp(1)
  sigma = sqrt(1/prec)
} "

set.seed(43)
data_jags = as.list(data)
params = c("mu", "sigma")

mod.pooled = run_sim(
  model.str = mod_string,
  data = data_jags,
  params = params
)

(dic.pooled = dic.samples(mod.pooled$obj, n.iter=1e3))
round(mod.pooled$coefs, 4)
plot(mod.pooled$csim)

res = data$weight - mod.pooled$coefs[1]
gg_resid(res)
plot(data$feed_name, res)

data %>%
  mutate(resid=res) %>% 
  ggplot(aes(x=feed_name, y=resid, group=feed_name)) +
  geom_boxplot() + 
  ggtitle("Residuals") +
  xlab("Feed") +
  ylab("Residual")

ggplot(data, aes(x=feed_name, y=weight, group=feed_name)) +
  geom_boxplot()

######## Hierarchical Model ###########
mod_string = " model {
  for (i in 1:length(weight)) {
    weight[i] ~ dnorm(theta[feed[i]], sigma)
  }
  
  for (j in 1:max(feed)) {
    theta[j] ~ dnorm(mu, tau)
  }
  
  mu ~ dnorm(0, 1/1e6)
  prec.1 ~ dexp(1)
  prec.2 ~ dexp(1)
  tau = sqrt(1/prec.1)
  sigma = sqrt(1/prec.2)
  
} "

set.seed(43)
data_jags = as.list(data)
params = c("theta", "mu", "sigma")

mod.hier = run_sim(
  model.str = mod_string,
  data = data_jags,
  params = params
)
(dic.hier = dic.samples(mod.hier$obj, n.iter=1e3))
round(mod.hier$coefs, 4)
plot(mod.hier$csim, ask=FALSE)

res = data$weight - mod.hier$coefs[3:8][data$feed]
plot(res)
plot(data$feed_name, res)

summary(mod.hier$sim)


######## Heterogeneous Model ###########
mod_string = " model {
  for (i in 1:length(weight)) {
    weight[i] ~ dnorm(theta[feed[i]], sigma)
  }
  
  for (j in 1:max(feed)) {
    theta[j] ~ dnorm(mu[j], tau[j])
    mu[j] ~ dnorm(0, 1.0/1.0e6)
    prec[j] ~ dgamma(5/2.0, 5*1.0/2.0)
    tau[j] = sqrt(1/prec[j])
  }
  
  prec.2 ~ dexp(1)
  sigma = sqrt(1/prec.2)
  
} "

set.seed(43)
data_jags = as.list(data)
params = c("theta", "sigma", "mu", "tau")

mod.het = run_sim(
  model.str = mod_string,
  data = data_jags,
  params = params
)
(dic.het = dic.samples(mod.het$obj, n.iter=1e3))
round(mod.het$coefs, 4)
plot(mod.het$csim, ask=FALSE)

res = data$weight - mod.het$coefs[14:19][data$feed]
plot(res)
plot(data$feed_name, res)

summary(mod.het$sim)

N = 5e3
mu.post = sample(mod.pooled$csim[, 1], N, replace = TRUE)
z = rnorm(N, mean=mu.post, sd=rep(mod.pooled$coefs["sigma"], N))
