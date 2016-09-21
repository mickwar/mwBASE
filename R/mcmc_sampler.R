#' MCMC sampler
#'
#' @description
#' Sample from a target distribution using Metropolis updates
#'
#' @param data
#'
#'

mcmc_sampler = function(data, target, nmcmc = 1000, nburn = 1000, window = 100, nthin = 1){

    for (i in 2:(nburn + nmcmc)){
        if (floor(i/window) == i/window)
            cat("\r   ", i, "/", nburn+nmcmc)
        params[i,] = params[i-1,]
        cand = mvrnorm(1, params[i-1,], cand.sig)
        if (all(cand > lower) && all(cand < upper)){
            cand.post = calc.post(output, cand)
            if (log(runif(1)) <= cand.post - post){
                post = cand.post
                params[i,] = cand
                accept[i] = 1
                }
            }
        if ((floor(i/window) == i/window) && (i <= nburn))
            cand.sig = autotune(mean(accept[(i-window+1):i]), target = 0.234, k = window/50) *
                (cand.sig + window * var(params[(i-window+1):i,]) / i)
        if (i == (nburn + nmcmc))
            cat("\n")
        }

    }
