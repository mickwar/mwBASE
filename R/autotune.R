#' autotune
#'
#' @description
#' Used to adjust candidate sigmas for normal candidate densities
#' Requires the calculation of acceptance rates within some given
#' window of mcmc iterations. For example, every 500 draws compute
#' the acceptance rate (0 <= A <= 1) for each parameter using the
#' last 500 draws. Multiply each candidate sigma by autotune().
#' 
#' @param accept - the acceptance rate within a given window
#' @param target - the acceptance rate goal
#' @param k - the maximum the candidate sigma can change by, 1/k is 
#'     minimum it can change by. For instance, if the current
#'     sigma is 2, and in some window of mcmc iterations every
#'     draw was accepted, the new candidate sigma would now
#'     be 2*k, which should serve to reduce the acceptance rate.
#'     On the other hand, if no draws are accepted, the new candidate
#'     sigma would then be 2/k. I recommend k = window/50
#'     Larger values of k will change sigma by a larger amount,
#'     and vice versa for smaller values (k -> 1, autotune() -> 1,
#'     everywhere)
#' @export

autotune = function(accept, target = 0.25, k = 2.5)
    (1+(cosh(accept-target)-1)*(k-1)/(cosh(target-
        ceiling(accept-target))-1))^sign(accept-target)
