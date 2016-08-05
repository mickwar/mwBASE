#' plot_hpd
#'
#' @description
#' Plot a univariate density with its hpd shaded
#'
#' @param dens     density object to be plotted
#' @param hpd      vector containing end points of the hpd region, must have
#'                 an even length
#' @param col1     the color of the non-shaded portion of the plot
#' @param col2     the color of the shaded portion, defaults to gray50
#' @param multiply logical, if true multiply col1 with col2 and set new
#'                 color to col2, otherwise don't
#' @param fade     numeric in [0, 1], the degree of transparency to
#'                 affect col1 and col2, ranging from 0 (totally
#'                 transparent) to 1 (opaque), defaults to 1
#' @param add      logical, should the plot be added to the current one
#' @param ...      extra arguments to pass to plot()
#' @seealso hpd_uni, hpd_mult, col_dens, col_mult, col_fade
#' @export
#' @examples
#' x = rnorm(100)
#' y = rnorm(100, 1)
#' hpd.x = hpd_uni(x)
#' hpd.y = hpd_uni(y)
#'
#' # x and y on their own plots
#' plot_hpd(density(x), hpd.x, col1 = "dodgerblue")
#' plot_hpd(density(y), hpd.y, col1 = "firebrick1")
#'
#' # together on one plot
#' plot_hpd(density(x), hpd.x, col1 = "dodgerblue", fade = 0.6)
#' plot_hpd(density(y), hpd.y, col1 = "firebrick1", fade = 0.6, add = TRUE)

plot_hpd = function(dens, hpd, col1, col2 = "gray50",
    multiply = TRUE, fade = 1, add = FALSE, ...){

    if (multiply)
        col2 = col_mult(col1, col2)
    if (fade < 1){
        col1 = col_fade(col1, fade)
        col2 = col_fade(col2, fade)
        }
    if (!add)
        plot(dens, type='n', ...)

    # Get the points from hpd that are closest to those from dens$x
    new.hpd = sapply(hpd, function(y) dens$x[which.min(abs(dens$x - y))])
    mid = c(-Inf, new.hpd, Inf)

    # Draw each group
    for (i in 1:(length(new.hpd)/2+1))
        col_dens(dens, mid[2*i-1], mid[2*i], col1, NA)
    for (i in 1:(length(new.hpd)/2))
        col_dens(dens, new.hpd[2*i-1], new.hpd[2*i], col2, NA)
    }
