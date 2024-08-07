#' Creates edges from a process with labels
#'
#' Uses process dataframe including inputs and outputs to each step to create a
#' dataframe that can be used by DiagrammeR to create a graph. It is normally used within
#' process_plot_Lx to create labels at one specific process label
#'
#' @param process Process dataframe in the standard format
#' @param terminals specify whether to create initial output and final output edges
#'
#' @return pairs A dataframe with columns from, to and label indicating the edges of the graph
#'
#' @export
label_edges <- function(process, terminals = FALSE) {
    bp <- process
    nstep <- nrow(bp)
    ct <- 0
    edge_list <- list()
    prev_id <- 0
    prev_line <- 0
    latest_from <- 1

    # Find where each node comes from, from steps 2 to nstep
    for (i in 2:nstep) {
        if (nstep < 2) break()
        if (bp$Longest[i]) {
            to <- i
            from <- latest_from
            latest_from <- i
        } else  if (bp$BranchLine[i] != prev_line || bp$BranchID[i] != prev_id) {
            from <- bp$BranchStart[i] - 1
            prev_id <- bp$BranchID[i]
            prev_line <- bp$BranchLine[i]
        } else {
            from <- i - 1
        }
        to <- i
        label <- bp$Output[from]
        if (from > 0 && to > 0)  {
            ct <- ct + 1
            edge_list[[ct]] <- c(from, to, label, "Why", "forward", "normal")
        }
    }

    # Deal with branch ends
    for (x in unique(bp$BranchEnd[bp$BranchID != 0])) {
        ids <- unique(bp$BranchID[bp$BranchEnd == x])
        for (i in ids) {
            lines <-  unique(bp$BranchLine[bp$BranchID == i & !bp$Longest])
            for (j in lines) {
                from <- max(which(bp$BranchLine == j & bp$BranchID == i))
                to <- x
                if (to > 0) {
                    label <- bp$Output[from]
                    ct <- ct + 1
                    edge_list[[ct]] <- c(from, to, label, "Why", "forward", "normal")
                }
            }
        }
    }

    # Deal with terminals
    if (terminals) {
        # Start terminal
        n1 <- max(bp$BranchCount[1], 1)
        to <- 1     # if no branching at start terminal
        for (i in 1:n1) {
            if (n1 > 1) to <- min(which(bp$BranchID == 1 &  bp$BranchLine == i))
            ct <- ct + 1
            edge_list[[ct]] <- c(nstep + i, to, bp$Input[to], "When", "forward", "normal")
        }
        nc <- nrow(bp)
        n2 <- max(bp$BranchCount[nc], 1)
        from <- nstep   # if no branching at end terminal
        for (i in 1:n2) {
            last_branch <- max(bp$BranchID)
            if (n2 > 1) from <- max(which(bp$BranchID == last_branch &
                                              bp$BranchLine == i))
            ct <- ct + 1
            edge_list[[ct]] <- c(from, nstep + n1 + i, bp$Output[from], "Why", "forward", "normal")
        }
        ctfrom <- nstep + n1 + n2 + 1
    } else {
        ctfrom <- nstep + 1
    }

    # Deal with cycle times
    for (i in 1:nrow(bp)) {
        if (bp$CT[i] != "") {
            ct <- ct + 1
            edge_list[[ct]] <- c(ctfrom, ctfrom + 1, bp$CT[i], "Cycle time", "both", "tee")
            ctfrom <- ctfrom + 2
        }
    }

    pairs <- as.data.frame(do.call("rbind", edge_list))
    if (nrow(pairs) == 0) return(NULL)
    colnames(pairs) <- c("from", "to", "label", "tooltip", "dir", "type")
    pairs$from <- as.integer(pairs$from)
    pairs$to <- as.integer(pairs$to)
    return(pairs)
}
