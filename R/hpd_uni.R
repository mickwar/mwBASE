#' hpd_uni
#'
#' @description
#' Compute the highest density posterior interval from a (unimodal) sample of points
#'
#' @param x         vector of samples from some distribution
#' @param prob      numeric in (0, 1) the probability that the interval should be
#' @param precision numeric, determines how accurate the resulting hpd should be,
#'      larger is more precise
#' @seealso hpd_mult
#' @export
#' @examples
#' x = rnorm(100)
#' hpd.x = hpd_uni(x)
#' hpd.x

hpd_uni = function(x, prob = 0.95, precision = 1000){
    range = seq(0, 1-prob, length=precision)
    range = cbind(range, range+prob)
    best = range[which.min(apply(range, 1, function(y)
        diff(quantile(x, y)))),]
    return (quantile(x, best))
    }
