SEED = 32
set.seed(SEED)


m = 1e6
a = 2.
b = 1. / 3.

theta = rgamma(m, a, b)
hist(theta, freq=FALSE)
curve(dgamma(x, a, b), col="steelblue", add=TRUE)


theta.mean = mean(theta)
theta.var = var(theta)
theta.sd = sd(theta)

cat("\n", "Estimated mean:", theta.mean, "\n", "Estimated variance:", theta.var, "\n")
cat("\n", "True mean:", a/b, "\n", "True variance:", a/b^2, "\n")

mean(theta < 5.)
pgamma(5., a, b)

quantile(theta, probs=.9)
qgamma(.9, a, b)


get_std_error = function(x){
  1.96 * sd(x) / sqrt(length(x))
}

get_std_error(theta)
get_std_error(theta < 5)


a = 2
b = 2
s = 10
phi = rbeta(m, a, b)
hist(phi, freq = FALSE)
curve(dbeta(x, a, b), col="steelblue", add=TRUE)

y = rbinom(m, s, prob=phi)
hist(y)
mean(y)
plot(prop.table(table(y)), ylab="P(y)", main="Marginal distribution of y")
