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
