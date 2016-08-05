### fade()
# Make a color transparent, simple call to rgb()
#
# Params: color - either a string of a valid R color name or a vector of
#                 length 3 of RGB values in [0, 1]^3
#         alpha - the degree of transparency, 0 is invisible and 1 is no
#                 transparency
fade = function(color, alpha = 0.5){
    if (length(color) == 1){
        x = col2rgb(color) / 255
    } else {
        x = color
        }
    rgb(x[1], x[2], x[3], alpha)
    }

### make.phantom()
# Takes a character vector and can hide some components in that vector
# when making a title. Also allows different colors to be in the text.
# Adapted from Matt Heiner
#
# Params: text    - a vector of characters, each part that is desired to
#                   be either invisible or a different color should be
#                   in a separate element
#         display - numeric vector containing the index of which elements
#                   in text should be displayed, defaults to displaying all
#         colors  - either a single color or a vector of colors having length
#                   equal to the length of text so the colors match with
#                   the elements of text, defaults to black
#         sep     - a character string that separates the components in
#                   text, defaults to "" which is no separation
#         ...     - additional arguments used in title()
make.phantom = function(text, display, colors, sep = "", ...){
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

