#' Return coordinates for a process
#'
#' This function takes a process - which include branching - and returns xy coordinates as a
#' data frame which can be passed to DiagrammeR functions to specify node coordinates
#'
#' @param process Process in standard format
#' @param terminals Include initial input and final output
#' @param xscale Scale factor for x-dimension
#' @param yscale Scale factor for y-dimension
#' @param wrap Maximum number of steps before wrapping
#' @param branchoffset How much of scale to take up for multiple branches
#' @param tscale Relative location of terminal nodes
#' @param ctoffset Relative location of the cycle time double arrow in vertical direction
#'
#' @export
wrap_process <- function(process,
                         terminals = FALSE,
                         xscale = 2,
                         yscale = 1.5,
                         wrap = 5,
                         branchoffset = 0.5,
                         tscale = 1.0,
                         ctoffset = 0.3) {
    # Take a working copy of process and initialize
    bp <- process

    # Check parse_branches has been run
    if (!"Longest" %in% colnames(bp)) {
        bp <- parse_branches(bp)
    }

    # Set up xy dataframe to return coordinates
    xy <- data.frame(ID = 1:nrow(bp), x = NA, y = NA)
    main_ct <- 0

    # Generate coordinates for main pathway, ie longest branches
    for (i in 1:nrow(bp)) {
        if (bp$Longest[i]) {
            main_ct <- main_ct + 1
            xycurr <- wrap_process_path(main_ct, wrap = wrap, xscale = xscale, yscale = yscale)
            xy$x[i] <- xycurr$x[1]
            xy$y[i] <- xycurr$y[1]
        }
    }

    # Add coordinates for other branches
    prev_line <- 0
    prev_id <- 0
    line_ct <- 0
    discount <- 0
    shorten <- 0
    for (i in 1:nrow(bp)) {
        if (!bp$Longest[i]) {
            # Reset counter of steps used not in longest branch for this branch
            if (bp$BranchID[i] != prev_id) {
                shorten <- discount
            }
            # Reset counter for this line of this branch
            if (bp$BranchLine[i] != prev_line || bp$BranchID[i] != prev_id) {
                line_ct <- 0
            }
            # We use start to look up coorindates for branch lines that are nit the longest one
            start <- bp$BranchStart[i] - shorten
            xycurr <- wrap_process_path(start + line_ct, wrap = wrap, xscale = xscale, yscale = yscale)
            xy$x[i] <- xycurr$x[1]
            xy$y[i] <- xycurr$y[1]
            # Increment line count and helper variables
            line_ct <- line_ct + 1
            discount <- discount + 1
            prev_line <- bp$BranchLine[i]
            prev_id <- bp$BranchID[i]
        }
    }

    # Label each node generated as a process step "Step"
    xy$t <- "Step"

    # Find all branched parts and apply an offset in y-dimension
    uset <- bp$BranchID != 0
    xy$y[uset] <- xy$y[uset] + (-0.5 + (bp$BranchLine[uset] %% bp$BranchCount[uset]) /
                                    (bp$BranchCount[uset] - 1)) * yscale * branchoffset * bp$BranchCount[uset]/2

    # Add terminal nodes if necessary
    if (terminals)  xy <- rbind(xy, wrap_process_terminals(last = main_ct, bp$BranchCount,
                                                           wrap = wrap,
                                                           xscale = xscale, yscale = yscale,
                                                           tscale = tscale,
                                                           branchoffset = branchoffset))

    # Create a pair of nodes for each cycle time we want to display mid-point to mid-point between process step boxes
    # and offset upwards by a factor of ctoffset
    nct <- sum(bp$CT != "")
    if (nct > 0) {
        ictnodes <- 0
        ctnodes <- data.frame(ID = rep(1:nct, each = 2), x = NA, y = NA)
        for (i in 1:nrow(bp)) {
            if (bp$CT[i] != "") {
                ictnodes <- ictnodes + 1
                ctnodes$x[ictnodes] <- xy$x[i] - xscale / 2
                ctnodes$y[ictnodes] <- xy$y[i] + yscale * ctoffset
                ctnodes$t[ictnodes] <- "CT"
                ictnodes <- ictnodes + 1
                ctnodes$x[ictnodes] <- xy$x[i] + xscale / 2
                ctnodes$y[ictnodes] <- xy$y[i] + yscale * ctoffset
                ctnodes$t[ictnodes] <- "CT"
            }
        }
        # Append cycle nodes to the xy data frame
        xy <- rbind(xy, ctnodes)
    }
    return(xy)
}

## wrap_process_path ----
# This function converts a step number (position) in a process sequence to x and y coordinates
# suitable for plotting using a snaking path, starting top-left (0, 0). The maximum number of steps in one
# line is wrap while xscale and yscale are multiplers for x- and y-coordinate systems
# This function is used by wrap_process

wrap_process_path <- function(position, xscale, yscale, wrap) {
    i <- position - 1
    y <- (i %/% wrap)
    if (y %% 2) {sign <- -1} else {sign <- 1}
    x <- wrap * y %% 2 + sign * (i %% wrap) - y %% 2
    return(data.frame(x = x * xscale, y = y * -yscale))
}

## wrap_process_terminals
# This function returns a data frame of length two with the coordinates of the dummy nodes
# that are used to connect the first input and the last output of the process
# The y value will always be the same as the connected true node

wrap_process_terminals <- function(last, counts, xscale, yscale, wrap, tscale, branchoffset) {
    # Create coordindates for terminals
    # n1 and n2 are number of branch lines at start and end terminals
    nc <- length(counts)
    n1 <- max(counts[1], 1)
    n2 <- max(counts[nc], 1)

    tcounts <- c(counts[1:n1], counts[(nc - n2 + 1):nc])
    tline <- c(1:n1, 1:n2)

    # Basic coordindates
    final1 <- wrap_process_path(last, xscale = xscale, yscale = yscale, wrap = wrap)
    final2 <- wrap_process_path(last + 1, xscale = xscale, yscale = yscale, wrap = wrap)

    # Set x-coordinate of second (group)of terminals
    if (final1$y[1] == final2$y[1]) {
        x2 <- final1$x[1] + tscale * (final2$x[1] - final1$x[1])
    } else if (final1$x[1] == 0) {
        x2 <- final1$x[1] - tscale * xscale
    } else {
        x2 <- final1$x[1] + tscale * xscale
    }

    # Create data frame of right size
    xy <- data.frame(ID = 1:(n1 + n2),
                     x = c(rep(-tscale * xscale, n1), rep(x2, n2)),
                     y = c(rep(0, n1), rep(final1$y[1], n2)),
                     t = c(rep("T1", n1), rep("T2", n2)))

    uset <- tcounts > 1
    xy$y[uset] <- xy$y[uset] + (-0.5 + (tline[uset] %% tcounts[uset]) /
                                     (tcounts[uset] - 1)) * yscale * branchoffset * tcounts[uset]/2
    return(xy)
}
