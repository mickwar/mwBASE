#' MCMC sampler
#'
#' @description
#' Sample from a target distribution using Metropolis updates
#'
#' @param data          An R object that is passed to target()
#' @param target        The log density function of the target distribution, which takes two
#'                      arguments: the data and the parameters, i.e. target(data, params)
#' @param nparam        Numeric, the number of parameters (the length of params in 
#'                      target(data, params)). If missing, obtaining the number of parameters
#'                      is attempted by looking at groups, bounds, or chain_init if they
#'                      provided.
#' @param nmcmc         Numeric, the number of iterations the sampler should be run _post_
#'                      burn-in (this is not the total number of iterations)
#' @param nburn         Numeric, the number of iterations the sampler should be as a burn-in
#' @param nthin         Numeric, every nthin samples will be retained after the burn-in
#' @param window        Numeric, every window samples (during burn-in) there will be
#'                      an update to the variance-covariance matrix of the candidate
#'                      distribution(s) to improve the sampler performance. See autotune().
#' @param groups        List of numeric vectors corresponding to the indices of the
#'                      parameters that should be updated jointly. See details.
#' @param bounds        List containing two named vectors: "lower" and "upper". Each is a
#'                      numeric vector the same length as the number of parameters being
#'                      updated. "lower" ides the lower bound and "upper" the upper bound.
#'                      Defaults to list("lower" = rep(-Inf, nparam),
#'                      upper = rep(+Inf, nparam)).
#' @param chain_init    Numeric, same length as the number of parameters, gives the starting
#'                      point of the chain. Defaults to runif(nparam), since in many cases,
#'                      possible values for the parameters are in (0, 1].
#' @param cand_sig      List of diagonal matrices. See details.
#' @param acc_rate      Numeric within (0, 1), the desired acceptance rate. Passed to
#'                      autotune(). Defaults to 0.234.
#' @param k             Numeric greater than 1. The scale parameter passed to autotune().
#'                      Defaults to window / 50.
#' @param display       Numeric, the iteration count is displayed every display samples.
#'                      Setting display to 0 means the count is not displayed.
#' @details
#' This function is intended to be a common implementation of Markov chain Monte Carlo
#' (MCMC) sampling with Metropolis updates. It is assumed that each parameter is
#' characterized by a single value (e.g. a Wishart random variable is not acceptable).
#'
#' The argument target is a function of two arguments: the data and the parameters.
#' When target(data, params) is called, a single (possibly infinite) value is returned
#' which is the log density for those data and parameters. Caution should be taken if
#' infinite values are returned.
#'
#' The sampler is run a total of nburn + nmcmc times with the first nburn samples being
#' discarded. The remaining samples are then `thinned' so that every nthin sample is
#' ultimately returned to the user. It is necessary that nmcmc > 0, since because of
#' the `autotuning' process (described later), every draw during the burn-in cannot be
#' assumed to have come from the target distribution.
#'
#' To understand the group parameter, suppose we have a target distribution with four
#' parameters. Suppose we wish to update the second and third together, but the first
#' and forth by themselves. We would then specify groups = list(1, c(2,3), 4). This
#' will result in the function target() being called three times each iteration as
#' well as in the creation of three proposal distributions (each normal). The first
#' proposal distribution is used to update the first parameter, the second (which will be
#' bivariate) is for updating the second and third parameters, and the third updates
#' the fourth. Leaving groups missing will update all parameters together. To update
#' each parameter separately, set groups = 0.
#'
#' Proposal distributions are assumed to be the multivariate normal. To avoid the
#' potentially tedious process of tuning the sampler, the covariance matrices for the
#' proposal distributions are tuned automatically. During the burn-in phase only, every
#' window iterations, the acceptance rate is calculated from the most recent samples
#' and the covariances of the proposal distributions are adjusted accordingly. In general,
#' if the acceptance rate is too small this means the proposal covariances are too large,
#' so they will be decreased. The decrease is determined by the function autotune()
#' the covariance of the samples. Similarly, if the acceptance rate is too high, the
#' proposal covariances will be increased.
#'
#' TODO: Write about cand_sig.
#' @export
#' @example examples/ex_mcmc.R
#' 

mcmc_sampler = function(data, target, nparam, nmcmc = 10000, nburn = 10000, nthin = 1,
    window = 200, groups, bounds, chain_init, cand_sig, acc_rate = 0.234, k, display = 1000){

    require(MASS)
    if (display > 0 && display < 100)
        message("Note: setting 'display' too low (but non-zero) may increase sampling time.")

    trailing = function(x, digits = 4)
        formatC(x, digits=digits, format="f")
    nice_time = function(seconds){
        # floor() or round() would work as well
        seconds = ceiling(seconds)
        days = seconds %/% (60*60*24)
        seconds = seconds %% (60*60*24)
        hours = seconds %/% (60*60)
        seconds = seconds %% (60*60)
        minutes = seconds %/% (60)
        seconds = seconds %% (60)
        out = ""
        if (days > 0)
            out = paste0(out, days, "d ", hours, "h ", minutes, "m ", seconds, "s")
        if (days == 0 && hours > 0)
            out = paste0(out, hours, "h ", minutes, "m ", seconds, "s")
        if (days == 0 && hours == 0 && minutes > 0)
            out = paste0(out, minutes, "m ", seconds, "s")
        if (days == 0 && hours == 0 && minutes == 0)
            out = paste0(out, seconds, "s")
        return (out)
        }

    # Try to get what nparam should be
    if (missing(nparam)){
        if (!missing(chain_init))
            nparam = length(chain_init)
        if (!missing(groups))
            nparam = max(unlist(groups))
        if (!missing(bounds))
            nparam = length(bound$lower)
        }
    if (missing(nparam))
        stop("nparam not specified")

    if (missing(groups))
        groups = list(1:nparam)
    if (!is.list(groups) && groups == 0)
        groups = lapply(1:nparam, identity)

    if (length(unlist(groups)) != nparam)
        stop("Total number of indices in groups must be equal to the number of parameters")
    if (length(unlist(groups)) != length(unique(unlist(groups))))
        stop("Indices in groups should be exactly once")

    if (missing(bounds))
        bounds = list("lower" = rep(-Inf, nparam), "upper" = rep(Inf, nparam))

    if (missing(k))
        k = window / 50

    if (missing(chain_init))
        chain_init = runif(nparam)

    if (missing(cand_sig)){
        cand_sig = rep(list(NULL), length(groups))
        for (i in 1:length(groups))
            cand_sig[[i]] = 0.1*diag(length(groups[[i]]))
        }

    params = matrix(0, nburn + nmcmc, nparam)
    accept = matrix(0, nburn + nmcmc, length(groups))
    params[1,] = chain_init

    tval = target(data, params[1,])
    if (is.infinite(tval)){
        stop("The evaluaion of target(data, chain_init) cannot be infinite.\n",
            "    Try running the function again to get a new randomized chain_init, or\n",
            "    manually specify chain_init.")
        }

    begin_time = as.numeric(Sys.time())
    for (i in 2:(nburn + nmcmc)){
        if (floor(i/display) == i/display && display > 0){
            curr_time = as.numeric(Sys.time()) - begin_time
            cat("\r   ", i, " / ", nburn+nmcmc, " -- ",
                trailing(100*i/(nburn+nmcmc), 2),"% -- Remaining: ",
                nice_time(curr_time*(nmcmc+nburn-i)/(i-1)), "            ", sep = "")
            }
        params[i,] = params[i-1,]
        for (j in 1:length(groups)){
            cand = params[i,]
            cand[groups[[j]]] = mvrnorm(1, params[i-1, groups[[j]]], cand_sig[[j]])
            if (all(cand > bounds$lower) && all(cand < bounds$upper)){
                cand.tval = target(data, cand)
                if (log(runif(1)) <= cand.tval - tval){
                    tval = cand.tval
                    params[i, groups[[j]]] = cand[groups[[j]]]
                    accept[i, j] = 1
                    }
                }
            if ((floor(i/window) == i/window) && (i <= nburn))
                cand.sig[[j]] = autotune(mean(accept[(i-window+1):i, j]), target = acc_rate, k = k) *
                    (cand.sig[[j]] + window * var(params[(i-window+1):i, groups[[j]]]) / i)
            }
        if (i == (nburn + nmcmc) && display > 0){
            curr_time = as.numeric(Sys.time()) - begin_time
            cat("\r   ", i, " / ", nburn+nmcmc, " -- ",
                trailing(100, 2),"% -- Elapsed: ",
                nice_time(curr_time), "            \n", sep = "")
            }
        }

    # Discard the burn-in
    params = tail(params, nmcmc)
    accept = tail(accept, nmcmc)

    # Do the thinning
    params = params[seq(1, nmcmc, by = nthin),]
    accept = accept[seq(1, nmcmc, by = nthin),]

    return (list(
        "params" = params,
        "accept" = accept,
        "cand.sig" = cand.sig,
        "nparam" = nparam,
        "nmcmc" = nmcmc,
        "nthin" = nthin,
        "groups" = groups,
        "bounds" = bounds
        ))
    }
