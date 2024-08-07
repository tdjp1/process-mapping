#' Create a tree graph from a process
#'
#' This function takes a process object and outputs the graph for the process as a tree
#'
#' @param process A Process data frame or tibble
#' @param depth The deepest level to plot
#' @param numbering Number the process steps
#' @param loopback whether to draw an edge from last sub-process
#' @param resource whether to show resources on the graph
#' @param step_colour set of colours to fill process steps
#' @param font_colour Set of colours for process step text
#' @param box_colour set of colours for edges of each process step and to show resources
#' @param arrow_colour Colour for the process step arrow
#' @param step_textwrap String length for wrapping text in process boxes (nodes)
#' @param step_width Minimum width for process step box
#' @param step_height Minimum height for process step box
#' @param fontname Font for text in the graph
#' @param orientation How to layout the tree LR (left-to-right), RL, TB (top-to-bottom) or BT
#' @param xsep Minimum separation between nodes horizontally
#' @param ysep Minimum separation between nodes vertically
#
#' @return A graph object of class dgr_graph
#'
#' @export
graph_tree <- function(process,
                       depth = NA,
                       numbering = FALSE,
                       loopback = FALSE,
                       resource = FALSE,
                       step_colour = base_colour_palette,
                       font_colour = base_text_palette,
                       box_colour = base_box_palette,
                       arrow_colour = base_arrow_palette,
                       step_textwrap = 18,
                       step_width = 1.25,
                       step_height = 0.5,
                       fontname = "Calibri,Arial,Helvetica",
                       orientation = "LR",
                       xsep = 0.25,
                       ysep = 0.25) {

    # Clean process and validate it
    process <- clean_process(process)
    process <- number_steps(process)

    # Check depth
    depth <- as.numeric(depth)
    if (!is.na(depth) && depth < 1) stop("depth must be a positive number or NA (meaning all)")

    # Check orientation
    if (orientation %in% c("LR", "RL")) {
        nodesep = ysep
        ranksep = xsep
        vertical = FALSE
    } else if (orientation %in% c("TB", "BT")) {
        nodesep = xsep
        ranksep = ysep
        vertical = TRUE
    } else {
        stop("orientation is invalid - use LR, RL, TB or BT")
    }

    # Get complete process tree
    tree <- list_subprocesses(process)

    # Create empty graph
    graph <- DiagrammeR::create_graph()

    # Build the colours for each level - will cycle through set of available colours in the palette
    levels <- max(tree$Level)
    fillcolvec <- expand_palette(step_colour, levels)
    fontcolvec <- expand_palette(font_colour,levels)
    colvec <- expand_palette(box_colour, levels)

    # Loop through the process tree
    for (i in 1:nrow(tree)) {

        if (!is.na(depth) && tree$Level[i] > depth) next
        # Find right colour
        fillcolor <- fillcolvec[tree$Level[i]]
        fontcolor <- fontcolvec[tree$Level[i]]
        color <- colvec[tree$Level[i]]

        # Get sub-process
        subp <- select_subprocess(process, level = tree$Level[i], parent = tree$Parent[i], depth = tree$Level[i])

        # Number if requested
        if (numbering) {
            do_number <- subp$IDdotted != ""
            subp$Step[do_number] <- paste0(subp$IDdotted[do_number], ". ", subp$Step[do_number])
        }

        # Wrap step text
        subp$wrap_step <- fmt_text(subp$Step, wrap = step_textwrap)

        # Add resources if defined
        if (resource) {
            role <- "R"
            if (i == 1) role <- "A"
            role_value <- fmt_text(subp[[role]], wrap = step_textwrap)
            has_role <- subp[[role]] != ""
            subp$wrap_step[has_role] <- paste0(subp$wrap_step[has_role], "|{", role, "|", role_value[has_role], "}")
            # Put resources underneath even in vertical layout
            if (vertical) subp$wrap_step[has_role] <- paste0("{", subp$wrap_step[has_role], "}")
        }

        # Get start and end nodes
        start <- DiagrammeR::count_nodes(graph) + 1
        end <- start + nrow(subp) - 1

        # Build node df and add to graph
        node_df <- DiagrammeR::create_node_df(nrow(subp),
                                              label = subp$wrap_step,
                                              color = color,
                                              fillcolor = fillcolor,
                                              fontcolor = fontcolor,
                                              IDdotted = subp$IDdotted)
        graph <- DiagrammeR::add_node_df(graph, node_df)

        # Get all nodes we have generated to date and use to work out the node of the parent
        all_nodes <- DiagrammeR::get_node_df(graph)
        if (tree$Level[i] == 1) {
            parentNode <- NA
        } else if (tree$Level[i] == 2) {
            parentNode <- 1
        } else {
            parentNode <- which(all_nodes$IDdotted == tree$Parent[i])
        }

        # Create edges
        if (!is.na(parentNode)) {
            # Create df for internal edegs and add to graph
            if (end > start) {
                edge_df <- DiagrammeR::create_edge_df(from = start:(end - 1), to = (start + 1):end)
                graph <- DiagrammeR::add_edge_df(graph, edge_df)
            }
            # Create edge fors link to parent
            edge_df <- DiagrammeR::create_edge_df(from = parentNode, to = start,
                                                  penwidth = 0.75,  arrowhead = "normal", arrowtail = "oinv", dir = "both")
            graph <- DiagrammeR::add_edge_df(graph, edge_df)

            # and loop back if requested
            if (loopback) {
                edge_df <- DiagrammeR::create_edge_df(from = end, to = parentNode,
                                                      penwidth = 0.75, arrowhead = "onormal", arrowtail = "inv", dir = "both")
                graph <- DiagrammeR::add_edge_df(graph, edge_df)
            }
        }
    }

    # Add global node properties
    graph <- DiagrammeR::add_global_graph_attrs(graph,
                                                attr = c("fixedsize", "width", "height", "fontname", "style", "shape"),
                                                value = c(FALSE, step_width, step_height, fontname, "filled", "record"),
                                                attr_type = "node")
    # Add global edge properyies
    graph <- DiagrammeR::add_global_graph_attrs(graph,
                                                attr = c("color", "penwidth"),
                                                value = c(arrow_colour, 1.5),
                                                attr_type = "edge")
    # Add global graph properties
    graph <- DiagrammeR::add_global_graph_attrs(graph,
                                                attr = c("layout", "nodesep", "ranksep", "rankdir", "splines"),
                                                value = c("dot", nodesep, ranksep, orientation, "spline"),
                                                attr_type = "graph")

    return(graph)
}
