#' hpd_mult
#'
#' @description
#' Compute the highest density posterior region from a (possibly multi-modal)
#' sample of points. If the distribution of the points is known to be unimodal,
#' then hpd_uni() should be used instead as it will be faster and more accurate
#'
#' @param x         vector of samples from some distribution
#' @param dens      a density object based on x, defaults to density(x)
#' @param prob      numeric in (0, 1) the probability that the interval should be
#' @param klength   numeric, determines how accurate the resulting hpd should be,
#'      larger is more precise
#' @seealso hpd_uni
#' @export
#' @examples
#' set.seed(1)
#' x = c(rnorm(100), rnorm(100, 5)
#' hpd = hpd_mult(x)

hpd_mult = function(x, dens, prob = 0.95, klength = 5000){
    if (missing(dens))
        dens = density(x)
    c.prob = 1
    temp.prob = 0
    k = seq(min(dens$y), max(dens$y), length=klength)
    # i = 2 to prevent certain problems, test.x4 had an issue
    # with the probability 0 region in the middle, (doesn't always
    # occur) perhaps fix by doing f(x) > k, instead of f(x) >= k?
    i = 2
    zeros = function(y, k, return.max.min = FALSE){
        # y is expected to be density(x)$y
        out = NULL
        int = NULL
        for (i in 2:(length(y)-1)){
            # condition to check when the height crosses k
            if ((y[i] > k && y[i-1] < k) || (y[i] < k && y[i-1] > k)){
                # get the x closer to k
                out = c(out, ifelse(which.min(abs(y[c(i,i-1)]-k))==1,
                    i, i-1))
                # -1 if lower interval, +1 if upper
                int = c(int, -sign(y[i] - y[i-1]))
                }
            # check if the height exactly equals k
            if (y[i] == k){
                out = c(out, i)
                # 0 if a maximum or minimum, -1 if lower, +1 if upper
                # y[i] can only be max or min if y[i] = k, so don't
                # check this condition for when height crosses k
                int = c(int, -sign(sign(y[i]-y[i-1]) +
                    sign(y[i+1]-y[i])))
                }
            }
        # ensure that first value is lower end and last is upper end
        if (is.null(int))
            return (NULL)
        if (int[1] == 1){
            int = c(-1, int)
            out = c(1, out)
            }
        if (int[length(int)] == -1){
            int = c(int, 1)
            out = c(out, length(y))
            }
        if (return.max.min)
            return (out)
        return (out[as.logical(int)])
        }
    # repeat until area under curve is <= specified prob
    # (note 14 jun: perhaps do some kind of iterative
    # convergence to reduce the number of iterations;
    # start in the middle i = floor(klength/2), and if
    # temp.prob is too low, set i=floor((i + 0)/2), else
    # set i = floor((i + klength)/2), repeat until value
    # is sufficiently close to prob. need to keep track of
    # previous "lower" and "upper" bounds
    while (c.prob > prob){
        temp.prob = 0
        int = zeros(dens$y, k[i])
        if (!is.null(int)){
            if (length(int) > 0){
                # sum the area in the intervals
                for (j in 1:(length(int)/2))
                    temp.prob = temp.prob + mean(x >= dens$x[
                         int[2*j-1]] & x <= dens$x[int[2*j]])
                # update (i think this always occurs)
                if (c.prob > temp.prob)
                    c.prob = temp.prob
                }
            }
        i = i + 1
        }
    return (dens$x[int])
    }
