#' col_fade
#'
#' @description
#' Make a color transparent, simple call to rgb()
#'
#' @param color either a string of a valid R color name or a vector of
#'              length 3 of RGB values in [0, 1]^3
#' @param alpha the degree of transparency, 0 is invisible and 1 is no
#'              transparency
#' @seealso col_dens, col_mult, col_gray
#' @export
#' @examples
#' col_fade("lightgreen", 0.5)
#' col_fade(c(0.564, 0.933, 0.564), 0.5)

col_fade = function(color, alpha){
    if (length(color) == 1){
        x = col2rgb(color) / 255
    } else {
        x = color
        }
    rgb(x[1], x[2], x[3], alpha)
    }
