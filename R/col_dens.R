#' col_dens
#'
#' Original by Arthur Lui, adapted by Mickey
#'
#' Colors a specified area under a density within an interval by making
#' a call to the polygon function
#'
#' @param dens      a density object
#' @param xlim      numeric vector of length 2 specifying the interval
#' @param fill      color, the color that will fill in the area under the curve, defaults to black
#' @param border    color, the outline of the area receives this color, defaults to fill
#' @export
#' @examples
#' x = rnorm(100)
#' dens = density(x)
#' plot(dens)
#' col_dens(dens, c(0, 1), fill = "blue", border = NA)

col_dens = function(dens, xlim, fill = "black", border = NULL){
    if (is.null(border))
        border = fill
    index = which(dens$x >= xlim[1] & dens$x <= xlim[2])
    x0 = dens$x[index][1]
    x1 = dens$x[index][length(index)]
    x = dens$x[index]
    y = dens$y[index]
    
    # creating the shaded region
    polygon(c(x0, x, x1), c(0, y, 0), col = fill, border = border)
    }
