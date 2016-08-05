#' col_mult
#'
#' Multiplies two colors together.
#' If A and B are vectors in [0,1]^3 (i.e. {R, G, B}), then to multiply the
#' colors A and B, do elementwise multiplication.
#'
#' @param col1 a color, either in name or integer/hexidecimal
#' @param col2 a color, either in name or integer/hexidecimal
#' @seealso col_dens, col_gray
#' @export
#' @examples
#' col_mult("red", "yellow")
#' # returns #FF0000, i.e. red
#'
#' col_mult("dodgerblue", "white")
#' # returns #1E90FF, i.e. dodgerblue ("white" is identity)
#'
#' col_mult("dodgerblue", "gray50")
#' # returns #0F487F, i.e. a darkened version of dodgerblue

col_mult = function(col1, col2){
    ### int2rgb()
    # Convert an integer/hexdecimal to an RGB. Returns the RGB values.
    # 
    # Params: x - an integer, between 0 and 16777215 = 256^3 - 1,
    #             or between 0x000000 and 0xFFFFFF
    int2rgb = function(x){
        hex = as.character(as.hexmode(x))
        hex = paste0("#", paste0(rep("0", 6-nchar(hex)), collapse=""), hex)
        col2rgb(hex)
        }

    if (is.character(col1))
        val1 = t(col2rgb(col1) / 255)
    if (is.numeric(col1))
        val1 = t(int2rgb(col1) / 255)
    if (is.character(col2))
        val2 = t(col2rgb(col2) / 255)
    if (is.numeric(col2))
        val2 = t(int2rgb(col2) / 255)

    return (rgb(val1 * val2))
    }
