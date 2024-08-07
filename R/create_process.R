#' Create a skeleton process tibble to use in process mapping
#'
#' This function makes it easier to get started with creating a process map by
#' creating a template data frame that can be saved and edited
#'
#' @param steps Sum of number of process steps across all levels
#' @param max_level Process sub-level depth
#' @param sipoc_template Include template information in SIPOC fields
#' @param add Vector of info to add including RACI, Note and Branch
#'
#' @return process with standard columns, as a tibble
#'
#' @examples
#' p <- create_process(steps = 5, sipoc_template = TRUE, add = "RACI")
#' p
#' p <- create_process(steps = 5, sipoc_template = TRUE, add = "RACI")
#' p
#
#' @export
#'
create_process <- function(steps,
                           levels = 2,
                           sipoc_template = FALSE,
                           add = NA) {
    if (steps < 1) stop("Process needs at least one process step - only ", steps, " requested")
    if (levels < 1) stop("Process needs at least one level")

    # Initialize data frame to desired steps
    df <- tibble::tibble(L1 = as.character(NA),
                         .rows = steps)

    # Add additional levels
    if (levels > 1) {
        for (i in 2:levels) {
            label <- paste0("L", i)
            df[[label]] <- as.character(NA)
        }
    }

    # Add remaining SIPOC components
    df$Supplier = as.character(NA)
    df$Input = as.character(NA)
    df$Output = as.character(NA)
    df$Customer = as.character(NA)

    # Add SIPOC tempplate labels, if requested
    if (sipoc_template) {
        if (steps < 2) stop("SIPOC needs at least one L2 process steps - only ", steps - 1, " requested")
        if (steps > 10) warning("SIPOC is best for high-level processes: ", steps, " is too many!")
        df$L1[1] <- "Process Title"
        df$Supplier[1] <- "Supplier of process input"
        df$Input[1] <- "Process input"
        df$Output[1] <- "Process output"
        df$Customer[1] <- "Customer of process output"
        inc <- 2:steps
        label <- inc - 1
        df$L2[inc] <- paste("Step", label)
        df$Supplier[inc] <- paste("Supplier of input", label)
        df$Input[inc] <- paste("Input for step", label)
        df$Output[inc] <- paste("Output from step", label)
        df$Customer[inc] <- paste("Customer of output", label)
    }

    # Add optional info
    df <- add_optional_info(df, add = add)

    return(df)
}
