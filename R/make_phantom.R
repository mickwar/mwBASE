#' make_phantom
#'
#' @description
#' Adapted from Matt Heiner
#'
#' Takes a character vector and can hide some components in that vector
#' when making a title. Also allows different colors to be in the text.
#'
#' @param text      a vector of characters, each part that is desired to
#'                  be either invisible or a different color should be
#'                  in a separate element
#' @param display   numeric vector containing the index of which elements
#'                  in text should be displayed, defaults to displaying all
#' @param colors    either a single color or a vector of colors having length
#'                  equal to the length of text so the colors match with
#'                  the elements of text, defaults to black
#' @param sep       a character string that separates the components in
#'                  text, defaults to "" which is no separation
#' @param ...       additional arguments used in title()
#' @export
#' @examples
#' set.seed(1)
#' x = rnorm(20)
#' y = rnorm(20)
#' 
#' # Figure 1
#' plot(x, type="l", col = "red", lwd = 2)
#' lines(y, col = "blue", lwd = 2)
#' make_phantom(text = c("Red", "Blue"), display = 1:2, colors = c("red", "blue"), sep = "  ")
#' 
#' # Figure 2
#' par(mfrow = c(1, 2))
#' plot(x, type="l", col = "red", lwd = 2)
#' make_phantom(text = c("Red", "Blue"), display = 1, colors = c("red", "blue"), sep = "  ")
#' 
#' plot(y, type="l", col = "blue", lwd = 2)
#' make_phantom(text = c("Red", "Blue"), display = 2, colors = c("red", "blue"), sep = "  ")
#' par(mfrow = c(1, 1))
#' # Notice the position of the text in the two titles compared with that of Figure 1

make_phantom = function(text, display, colors, sep = "", ...){
    # text: a character vector for 
    n = length(text)
    if (missing(display))
        display = 1:n
    if (missing(colors))
        colors = rep("black", n)
    if (length(colors))
        colors = rep(colors, n)
    for (i in display){
        if (i == 1)
            title(main = bquote( .(text[i]) * phantom(.(sep)) * phantom(.(paste0(text[-i], collapse = sep)))),
                col.main = colors[i], ...)
        if (i == n)
            title(main = bquote( phantom(.(paste0(text[-i], collapse = sep))) * phantom(.(sep)) * .(text[i])),
                col.main = colors[i], ...)
        if (i > 1 && i < n)
            title(main = bquote( phantom(.(paste0(text[1:(i-1)], collapse = sep))) * phantom(.(sep)) *
                .(text[i]) * phantom(.(sep)) * phantom(.(paste0(text[(i+1):n], collapse = sep)))),
                col.main = colors[i], ...)
        }
    }
