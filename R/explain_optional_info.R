#' explain meaning of the optional columns
#'
#' Explain all optional columns that can be added to a process tibble
#' @param info Vector of info to explain including RACI, Note and Branch
#'
#' @return df Dataframe with Info and Description
#' @examples
#' df <- explain_optional_info(info = "RACI")
#' df
#' df <- explain_optional_info(info = "all")
#' df
#'
#' @export
#'
explain_optional_info <- function(info = "all") {

    if (is.na(info) || info == "") return(NA)

    if ("all" %in% info) {
        info <- optional_info
    } else  if ("RACI" %in% info) {
        info <- c("R", "A", "C", "I", info)
        info <- info[info != "RACI"]
    }

    for (c in info) {
        if (!c %in% optional_info) stop("Invalid optional info ", c)
    }

    lout <- lapply(info, function(x) {
        z <- desc_optional_info[[x]]
        df <- data.frame(Info = x, Description = z)
        return(df)
    })
    df <- do.call(rbind, lout)

    return(df)

}
