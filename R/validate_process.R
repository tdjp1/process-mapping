#' Check process tibble for essential elements
#'
#' This function helps when loading a process to ensure it has the correct columns
#'
#' @param process Process tibble to check
#' @param allow_empty Allow an empty proces to validate
#'
#' @return status Logical
#'
#' @examples
#' p <- create_process(step = 5, sipoc_template = TRUE)
#' if (validate_process(p)) print("Process Valid")
#
#' @export
#'
validate_process <- function(process, allow_empty = FALSE) {

    status <- TRUE
    if (!tibble::is_tibble(process) && !is.data.frame(process)) {
        warning("Process is not a tibble or a data frame")
        status <- FALSE
        return(status)
    }

    cols <- colnames(process)
    levels <- cols[grepl("^L[0-9]+$", cols)]
    level_ids <- paste0(levels, "ID")

    # Check the sequence of levels makes sense
    max_level <- max(as.integer(sub("^L", "", levels)))
    if (length(levels) != max_level) {
        warning("Inconsistent use of process levels: ", length(levels),
                " levels but highest level is L", max_level)
        status <- FALSE
    }

    # Check for required columns
    for (c in required_info) {
        if (!c %in% cols) {
            warning("Required column ", c, " missing")
            status <- FALSE
        }
    }

    # Check that all columns are allowed
    for (c in cols) {
        if (!c %in% c(levels, level_ids, required_info, optional_info, internal_info)) {
            warning("Invalid column ", c, " found")
            status <- FALSE
        }
    }

    if (allow_empty) return(TRUE)

    # Ignore blank lines
    blank <- apply(process, 1, function(x) {
        sum(!is.na(x) & x != "") == 0
    })
    process <- process[!blank,]

    # Check we start with L1
    if (is.na(process$L1[1])) {
        warning("First process step must be at L1")
        status <- FALSE
    }

    # Only one L1 step
    if (sum(!is.na(process$L1)) != 1) {
        warning("Only one L1 process step allowed")
        status <- FALSE
    }

    # Check that only one level is used in each process step
    # Note that blank processes will fail this test
    counts <- apply(process[c(levels)], 1, function(x) {
        sum(!is.na(x))
    })
    if (sum(counts != 1)) {
        warning("Each process step must be at exactly one level only")
        status <- FALSE
    }

    status
}
