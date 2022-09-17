propose = function(x){
  b = x == 1
  as.integer(!b)
}

gen_proposals = function(n, init_x = 1) {
  x = numeric(n)
  x[1] = init_x
  for (i in seq(x)[2:length(x)]) {
    x[i] = propose(x[i-1])
  }
  return(x)
}

z = gen_proposals(1e5)

get_likelihood = function(x, theta){
  dbinom(x, 5, .5) * (1-theta) + dbinom(x, 5, .7) * theta
}

f = function(theta){
  if (theta == 1) {
    return(.6)
  } else {
    return(.4)
  }
}

