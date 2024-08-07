#' colour_palette
#'
#' Return a colour palette for process steps
#'
#' @param n number of colours requested
#'
#' @return colours vector of colours in hex-format
#'
colour_palette <- function(n = 5) {
    colours <- expand_palette(base_colour_palette, n)
    return(colours)
}

#' text_palette
#'
#' Return a colour palette for text suitable for process step colours
#'
#' @param n number of text colours requested
#'
#' @return colours vector of colours in hex-format
#'
text_palette <- function(n = 5) {
    colours <- expand_palette(base_text_palette, n)
    return(colours)
}

#' expand_palette
#' Expand an vector of colours to be a specified length by repeating
#'
#' @param palette vector of colours to expand
#' @param n number of colours requested
#' @return colours vector of colours in hex-format
#'
expand_palette <- function(palette, n = 5) {
    if (n < 0) stop("Positive number of colours must be requested")
    v <- vector(length = n, mode = "character")
    colours <- paste0(v, palette)[1:n]
    return(colours)
}
