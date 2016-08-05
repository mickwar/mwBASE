#' col_gray
#'
#' Convert a given RGB color to gray-scale
#'
#' @param rgb       numeric vector in [0, 1]^3
#' @param method    either "yprime", "average", or "lightness". "yprime" attempts to approximate human vision, "average" simply takes the average of rgb and assigns this to all RGB values, "lightness" takes the average of the range of rgb.
#' @seealso col_dens, col_mult
#' @export
#' @examples
#' col_gray(c(0.8, 0.1, 0.8), "yprime")

col_gray = function(rgb, method = "yprime"){
    if (method == "yprime"){
        weight = c(0.2126, 0.7152, 0.0722) * rgb
        return (rep(sum(weight), 3))
        }
    if (method == "average"){
        return (rep(mean(rgb), 3))
        }
    if (method == "lightness"){
        return (rep(mean(range(rgb)), 3))
        }
    }
