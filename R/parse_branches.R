## process_parse_branch ----
# make sense of the branch information
# is applied only to a process at a single level

#' @export
parse_branches <- function(process) {
    bp <- process
    # Make sure the Branch column is there and uses NAs when blank
    if (!"Branch" %in% colnames(bp)) {
        bp <- clean_process(bp)
    }
    # if (is.null(bp$Branch)) bp$Branch <- NA
    # bp$Branch[bp$Branch == ""] <- NA

    nstep <- nrow(process)

    # Set up defaults before we start
    current_label <- NA
    nbranch <- 0
    current_line <- 0
    in_branch <- FALSE
    bp$BranchID <- 0
    bp$BranchLine <- 0
    bp$BranchCount <- 0
    bp$Longest <- TRUE
    bp$BranchStart <- 0
    bp$BranchEnd <- 0

    # Loop through and mark branching status
    for (i in 1:nstep) {
        if (!is.na(bp$Branch[i])) {
            if (!in_branch) {
                in_branch <- TRUE
                nbranch <- nbranch + 1
                current_label <- bp$Branch[i]
                current_line <- 1
                start <- i
            } else if (bp$Branch[i] != current_label) {
                current_label <- bp$Branch[i]
                current_line <- current_line + 1
            }
            bp$BranchLine[i] <- current_line
            bp$BranchID[i] <- nbranch
            bp$BranchStart[i] <- start
        } else {
            if (in_branch) bp$BranchEnd[bp$BranchID == nbranch] <- i
            in_branch <- FALSE
            current_line <- 0
        }
    }

    # Identify longest line for each branch
    for (x in unique(bp$BranchID[bp$BranchID != 0])) {
        counts <- lines <- unique(bp$BranchLine[bp$BranchID == x])
        for (i in 1:length(lines)) {
            counts[i] <- sum(bp$BranchLine == lines[i] &
                                 bp$BranchID == x)
        }
        bp$BranchCount[bp$BranchID == x] <- length(counts)
        longest <- lines[which.max(counts)]
        bp$Longest[bp$BranchID == x & bp$BranchLine != longest] <- FALSE
    }
    # Error checks on branching
    branches <- unique(bp$BranchID[bp$BranchID > 0])
    # branches with just one branch
    for (i in branches) {
        start <- min(bp$BranchStart[bp$BranchID == i])
        if (bp$BranchCount[start] == 1) {
            warning("Unbalanced branch defined")
            sel <- bp$BranchID == i
            bp$BranchID[sel] <- 0
            bp$BranchCount[sel] <- 0
            bp$BranchStart[sel] <- 0
            bp$BranchEnd[sel] <- 0
        }
    }
    bp
}
