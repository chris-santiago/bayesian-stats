library(rstan)

options(mc.cores=parallel::detectCores())

y = rnorm(1e3, 1.5, .2)
fit = stan("stan_demo.stan", data=list(y=y, N=length(y)))
print(fit)

mu = extract(fit, "mu")[[1]]
qplot(mu)

library(shinystan)
fit.shiny = as.shinystan(fit)
launch_shinystan(fit.shiny)
