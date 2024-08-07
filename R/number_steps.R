#' process_label
#'
#' Create individual step numbers (eg 1.1, 1.2.3) at all levels
#' return a process with additional column IDdotted
#'
#' @param proces process in a standard format
#'
#' @return a process object with additional columns IDdotted and one LxID column for
#' each Lx column
#'
#' @examples
#' process <- number_steps(process)
#'
#' @export
number_steps <- function(process) {
    # Ignore blank lines
    blank <- apply(process, 1, function(x) {
        sum(!is.na(x) & x != "") == 0
    })
    pp <- process[!blank,]

    # Get column and levels
    cols <- colnames(pp)
    levels <- cols[grepl("^L[0-9]+$", cols)]
    if (length(levels) < 1) stop("At least two levels required")
    ndec <- floor(log10(nrow(pp))) + 1
    pp$Seq <- sprintf(paste0("P%0", ndec, "d"), 1:nrow(pp))
    pp$IDdotted <- ""
    pp$L1ID <- ""
    pp$Level <- ""
    pp$Parent <- ""
    pp$Step <- NA
    pp$Step[1] <- pp$L1[1]
    if (length(levels) == 1) return(pp)
    # Loop through the levels sequentially
    # Create LxID column with the ID
    for (i in 2:length(levels)) {
        level <- levels[i]
        parent <- levels[i - 1]
        parents <- levels[1:(i-1)]
        column <- paste0(levels[i], "ID")
        parent_column <- paste0(parent, "ID")
        pp[[column]] <- NA
        id <- 0
        # Loop through the process description finding and labelling steps at level i
        for (row in 1:nrow(pp)) {
            # Reset count if any parent has an entry
            for (prs in parents) {
                if (!is.na(pp[[prs]][row])) {
                    id <- 0
                    next
                }
            }
            if (!is.na(pp[[level]][row])) {
                id <- id + 1
            }
            # Write labels in LxID column and paste in IDdotted from levels above
            if (id != 0) {
                pp[[column]][row] <- id
                if (pp$IDdotted[row] != "") {
                    pp$IDdotted[row] <- paste0(pp$IDdotted[row], ".", id)
                } else {
                    pp$IDdotted[row] <- paste0(id)
                }
            }
        }
    }
    # Note level and parent for each row
    pp$Level <- stringi::stri_count_fixed(pp$IDdotted, ".") + 2
    pp$Level[pp$IDdotted == ""] <- 1
    pp$Parent <- sub("\\.[0-9]+$", "", pp$IDdotted)
    pp$Parent[pp$Level == 2] <- ""

    # Steps
    lev <- paste0("L", pp$Level)
    for (i in 1:nrow(pp)) {
        pp$Step[i] <- pp[[lev[i]]][i]
    }

    pp
}

#' @export
process_label <- function(...) {
    .Deprecated("number_steps")
    number_steps(...)
}
