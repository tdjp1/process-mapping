#'
#' Clean a process
#'
#' Make sure all columns are present
#' Substitute out risky characters for their character code
#'
#'@export
#'
clean_process <- function(process) {

    # Remove strings that might mess up rendering
    cols <- colnames(process)
    for (c in cols) {
        process[[c]] <- gsub("\\r\\n", " ", process[[c]])
        process[[c]] <- gsub("[\']", "&#39;", process[[c]])
        process[[c]] <- gsub("[\"]", "&#34;", process[[c]])
        process[[c]] <- gsub("[<]", "&#60;", process[[c]])
        process[[c]] <- gsub("[>]", "&#62;", process[[c]])
        process[[c]] <- gsub("[{]", "&#123;", process[[c]])
        process[[c]] <- gsub("[}]", "&#125;", process[[c]])
        process[[c]] <- gsub("[|]", "&#124;", process[[c]])
        process[[c]] <- gsub(" +$", "", process[[c]])
        process[[c]][is.na(process[[c]])] <- ""
    }
    levels <- cols[grepl("^L[0-9]+$", cols)]
    branch <- cols[grepl("^Branch$", cols)]

    # For levels and Branch convert blanks back to NAs
    for (c in c(levels, branch)) {
        process[[c]][process[[c]] == ""] <- NA
    }

    # Skip rows where all levels are blank
    blank <- apply(process[levels], 1, function(x) {
        sum(!is.na(x)) == 0
    })
    process <- process[!blank,]

    # Add optional columns
    process <- add_optional_info(process)

    # Final validation
    if (!validate_process(process)) stop("Unable to validate process")
    process
}
