#' Get named list of navigation nodes for a graph
#'
#' Get a list of all subprocesses as dataframe
#' @param graph dgr graph
#' @return named list
#'
#' @examples
#' names <- get_navigation_map(graph)
#'
#' @export

get_navigation_map <- function(graph) {

    # Check we have been given a directed graph
    if (!DiagrammeR::is_graph_directed(graph)) stop("Invalid graph")

    # Get node data frame
    df <- DiagrammeR::get_node_df(graph)
    df$node <- paste0("node", df$id)
    df <- df[df$navigate != "",]

    if (nrow(df) == 0) return(NULL)

    # Combine the information to a single named character vector
    nodes <- setNames(df$navigate, df$node)

    return(nodes)
}
