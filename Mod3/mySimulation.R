# simulation, 
# binary distribution, e.g. x represents 'gender', but y is normal disttributed
set.seed(10)
x = rbinom(100, 1, 0.5)
e = rnorm(100, 0, 2)
y = 0.5 + 2 + x + e
summary(y)
plot(x,y)

# poisson distribution
beta_0 = 0.5
beta_1 = 0.3

set.seed(1)
x = rnorm(100)
log_mu = beta_0 + beta_1 * x
y = rpois(100, exp(log_mu))
summary(y)
plot(x,y)
