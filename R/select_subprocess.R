#'
#' Select a subprocess
#'
#' Extract a coherent subprocess from a complete process
#'
#' @param process Complete process to extract subprocess from
#' @param level Top level to include
#' @param parent Parent of selected level
#' @param max_level Deepest level to include NA means no limit
#'
#' @export
select_subprocess <- function(process,
                              level = 1,
                              parent = "",
                              depth = NA) {
    # Check process validates and do basic preparation
    if (!validate_process(process)) stop("Unable to validate process")

    if (level > 2 && parent == "") stop("Must specify a parent for levels deeper than 2")
    p <- clean_process(process)
    p <- number_steps(p)

    # Get subprocesses
    s <- list_subprocesses(p, depth = depth)

    top_level <- min(s$Level)
    deepest_level <- max(s$Level)

    if (level < top_level || level > deepest_level) stop("Level ", level, " not found in process")
    if (!is.na(depth)) {
        if (depth < level || depth > deepest_level) stop("Depth ", depth,
                                                                 " not valid for process")
    } else {
        depth <- deepest_level
    }

    # Select the range of levels requested
    p <- p[p$Level >= level & p$Level <= depth,]

    # Select steps with right parent if we are in the depths
    if (level > 2) p <- p[p$Parent == parent,]

    if (nrow(p) < 1) stop("No steps available at level ", level, " for parent ", parent)

    # Remove parents - unless we just want level 1
    if (level == 1 && depth == 1) return(p)
    p <- p[!p$IDdotted %in% s$Parent,]
    if (nrow(p) < 1) stop("No steps available at level ", level, " for parent ", parent)

    p
}
