#' add columns to process tibble
#'
#' Add all optional columns to process tibble
#' @param process Process tibble to update
#' @param add Vector of info to add including RACI, Note and Branch
#'
#' @return process Updated process
#' @examples
#' p <- create_process(steps = 5, sipoc_template = TRUE)
#' p <- add_optional_info(p, add = "RACI")
#' p
#' p <- add_optional_info(p, add = "Level")
#' p
#' p <- add_optional_info(p)
#' p
#'
#' @export
#'
add_optional_info <- function(process, add = "all") {
    # Make sure we haven't been given rubbish
    reorder <- FALSE
    if (!validate_process(process, allow_empty = TRUE)) stop("Unable to validate process")

    if (is.na(add) || add == "") return(process)

    if ("all" %in% add) {
        add <- optional_info
    } else  if ("RACI" %in% add) {
        add <- c("R", "A", "C", "I", add)
        add <- add[add != "RACI"]
    }

    # Add one more level
    if ("Level" %in% add) {
        cols <- colnames(process)
        levels <- cols[grepl("^L[0-9]+$", cols)]
        max_level <- max(as.numeric(sub("^L", "", levels)))
        next_level <- paste0("L", max_level + 1)
        add <- add[add != "Level"]
        add <- c(add, next_level)
        reorder <- TRUE
    }

    for (c in add) {
        if (!c %in% optional_info && !grepl("^L[0-9]+", c)) stop("Invalid info ", c)
        if (is.null(process[[c]])) process[[c]] <- as.character(NA)
    }

    if (reorder) {
        cols <- colnames(process)
        levels <- cols[grepl("^L[0-9]+$", cols)]
        notlevels <- cols[!cols %in% levels]
        process <- process[c(levels, notlevels)]
    }
    return(process)
}
