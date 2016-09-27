x = runif(100)
y = 1 + 1.5*x + rnorm(100, 0, sqrt(0.1^2))
plot(x, y)

data = list("x" = x, "y" = y)
target = function(data, params){
    x = data$x
    y = data$y
    beta0 = params[1]
    beta1 = params[2]
    sig2 = params[3]

    # Likelihood
    out = sum(dnorm(y, beta0 + beta1*x, sqrt(sig2), log = TRUE))

    # Priors
    out = out + dnorm(beta0, 0, 100, log = TRUE)
    out = out + dnorm(beta1, 0, 100, log = TRUE)
    out = out + dgamma(sig2, 1, 0.1, log = TRUE)

    return (out)
    }

### Update separately
out = mcmc_sampler(data = data, target = target, nparam = 3,
    nmcmc = 10000, nburn = 5000, nthin = 1, window = 200,
    bounds = list("lower" = c(-Inf, -Inf, 0), "upper" = c(Inf, Inf, Inf)),
    display = 100, groups = 0)

colMeans(out$accept)


### Update in groups
out = mcmc_sampler(data = data, target = target, nparam = 3,
    nmcmc = 10000, nburn = 5000, nthin = 1, window = 200,
    bounds = list("lower" = c(-Inf, -Inf, 0), "upper" = c(Inf, Inf, Inf)),
    display = 100, groups = list(c(1,2), 3))

colMeans(out$accept)


### Update jointly
out = mcmc_sampler(data = data, target = target, nparam = 3,
    nmcmc = 10000, nburn = 5000, nthin = 1, window = 200,
    bounds = list("lower" = c(-Inf, -Inf, 0), "upper" = c(Inf, Inf, Inf)),
    display = 100)

mean(out$accept)


hpds = apply(out$params, 2, hpd_mult)
plot_hpd(density(out$params[,1]), hpds[,1], "dodgerblue")
plot_hpd(density(out$params[,2]), hpds[,2], "dodgerblue")
plot_hpd(density(out$params[,3]), hpds[,3], "dodgerblue")
