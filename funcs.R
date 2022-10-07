library("rjags")

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


inv_logit = function(X, b){
  1 / (1 + exp(- X %*% b))
}


gg_resid = function(residuals) {
  as_tibble(residuals) %>%
    rename(resid=value) %>%
    mutate(obs=seq_along(resid)) %>%
    ggplot(aes(x=obs, y=resid)) +
    geom_point() + 
    geom_smooth()
}