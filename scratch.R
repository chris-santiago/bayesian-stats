plot_dist = function(dist_func, dist_params, min=0, max=1) {
  theta = seq(from=min, to=max, 0.01)
  plot(theta, dist_func(theta, dist_params[1], dist_params[2]), type="l")
}

N = 1000

#### Restaurant B

a = 3
b = 200
m = 500
w = 0.1

n = 27
y_bar = 609.7
s2 = 401.8
a_ = a + n / 2
b_ = b + ((n - 1)/2) * s2 + ((w*n)/(2*(w+n))) * (y_bar - m) ** 2
m_ = (n * y_bar + w * m) / (w+n)

cat("Params:", c(a_, b_, m_), "\n")

z = rgamma(N, a_, b_)
sig2_b = 1 / z
mu_b = rnorm(N, y_bar, sqrt(s2/n))

cat("95% CI:", quantile(mu_b, probs=c(.025, .975)), "\n")

#### Restaurant A
a = 3
b = 200
m = 500
w = 0.1

n = 30
y_bar = 622.8
s2 = 403.1
a_ = a + n / 2
b_ = b + ((n - 1)/2) * s2 + ((w*n)/(2*(w+n))) * (y_bar - m) ** 2
m_ = (n * y_bar + w * m) / (w+n)

cat("Params:", c(a_, b_, m_), "\n")

z = rgamma(N, a_, b_)
sig2_a = 1 / z
mu_a = rnorm(N, y_bar, sqrt(s2/n))

cat("95% CI:", quantile(mu_a, probs=c(.025, .975)), "\n")

cat("Post prob mu_a > mu_b:", mean(mu_a > mu_b))

