#' get_loopback_edges generate edge dataframe for any loops
#'
#' Use To column to determine any additional edges to be created
#' called internally after clean_process and select_subprocess
#' To column includes destination in IDdotted format
#'
#' @param process Process in standard format
#' @return edge dataframe or NULL

get_loopback_edges <- function(process, edge_colour) {

    p <- process
    # Check the To column has already been added
    if (!"To" %in% colnames(p)) stop("To column not found")

    # Enforce blank is NA convention
    p$To[p$To == ""] <- NA
    # IS there anything in the To column
    if (sum(!is.na(p$To)) == 0) return(NULL)

    # Only numbers and dots allowed
    p$To <- sub("[^0-9.]+", "", p$To)

    # Keep just relevant info
    joins <- data.frame(IDTo = p$To, IDdotted = p$IDdotted, To = NA)
    joins$From <- 1:nrow(joins)

    # Find ID in sequence that matches To label
    for (i in 1:nrow(joins)) {
        if (is.na(joins$IDTo[i])) next()
        selected <- p$IDdotted %in% p$To[i]
        if (sum(selected) == 1) joins$To[i] <- joins$From[selected]
    }

    # Just keep the active edges
    joins <- joins[!is.na(joins$To),]
    if (nrow(joins) == 0) {
        warning("Connections in To column found - but none are valid")
        return(NULL)
    }

    edges <- DiagrammeR::create_edge_df(
        from = joins$From,
        to = joins$To,
        color = edge_colour,
        label = "",
        )
    edges
}
