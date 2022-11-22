library(tidyverse)

# Generate n observations from a mixture of two Poisson distributions
n = 200
w = c(0.7, 0.2, 0.1)
mu = c(1, 2, 6)
k = sample(1:length(w), n, replace=T, prob=w)
x = rpois(n, mu[k])

tibble(x) %>%
  group_by(x) %>%
  count() %>%
  ggplot(aes(x=x, y=n)) +
  geom_bar(stat="identity") +
  ggtitle("Integer Frequency") +
  xlab("Integer") +
  ylab("Count")
