#' Get all subprocesses for a process
#'
#' Get a list of all subprocesses as dataframe
#' @param process Input process
#' @param depth maximum depth
#' @return subprocesses datafrane with matching Level and Parent
#'
#' @examples
#' p <- create_process(steps = 5, sipoc_template = TRUE)
#' list_subprocesses(p)
#'
#' @export

list_subprocesses <- function(process, depth = NA) {
    # Clean the process and validate it
    process <- clean_process(process)
    # Just use number_steps to get all Levels and Parents
    p <- number_steps(process = process)
    # Just unique the columns we want
    uniq <- !duplicated(p[c("Level", "Parent")])
    p$Title <- ""
    for (i in 1:nrow(p)) {
        if (!uniq[i]) next()
        if (p$Level[i] == 1) next()
        l <- paste0("L", p$Level[i] - 1)
        p$Title[i] <- p[[l]][i - 1]
    }
    subs <- p[c("Level", "Parent", "Title")]
    subs <- subs[uniq,]
    # Ideally also extract title ie name at current level
    # subs$Title <- ""
    # Limit the depth, if requested
    if (!is.na(depth)) subs <- subs[subs$Level <= depth,]
    subs
}
