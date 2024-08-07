#' fmt_text wrap text to specified width using defined character separator
#'
#' utility function to wrap text
#'
#' @param text array containing text to wrap
#' @param wrap maximum width
#' @param sep separator string

#' @return fmted array of wrapped text
#'
fmt_text <- function(text, wrap, sep = "&#92;n") {
    fmted <- unlist(lapply(strwrap(text, width = wrap, simplify = FALSE), paste, collapse = sep))
    return(fmted)
}
