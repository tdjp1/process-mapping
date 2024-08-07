#' Graph swimlane map
#'
#' Create a graph in the form of a swimlane map for the process
#'
#' @param process Process to graph as a swimlane map
#' @param level The process level to plot (2-5)
#' @param parent The level of the process to plot
#' @param depth The deepest level to plot
#' @param numbering Number the process steps
#' @param terminals Start and End circles be shown
#' @param xscale Scale factor for x-dimension
#' @param yscale Scale factor for y-dimension
#' @param step_textwrap String length for wrapping text in process boxes (nodes)
#' @param box_colour Color for the edge of the process step box
#' @param box_style Box style for use by DiagrammerR::create_node_df
#'    (rounded, solid, dotted are probably the most useful)
#' @param arrow_colour Colour for the process step arrow
#' @param arrow_style Arrow style for use by DiagrrammeR::create_edge_df
#'    (tapered and solid are probably the most useful)
#' @param colour_palette Colour palette to use for multiple lanes
#' @param lane_colour Colour for dividing line between lanes
#' @param title_fontsize Size of the title font in points (NA means no title)
#' @param fontname Name of font to use for process steps and arrows
#' @param title_fontname Name of font for title
#' @param ctoffset How much offset to use for cycle time arrows
#'
#' @return graph Graph that can be used by DiagrammeR for render or export
#'
#' @examples
#' # Create a template process with 5 steps and fill out a standard template for the SIPOC
#' # Note that row 1 contains L1 (level 1) information and the subsequent 4 rows contain L2
#' # steps that will would appear in a SIPOC
#' p <- create_process(steps = 5, sipoc_template = TRUE)
#'
#' # You could then edit the process in R or export to Excel then re-import
#' # graph_slm plots all the steps in the process. If an R column is included, that changes
#' # the name of the swinlane
#' # need to specify the parent step number
#' g <- graph_slm(p)
#'
#' # Visualize the process as a graph using Diagrammer
#' DiagrammeR::render_graph(g)

#' @export
#'
graph_slm <- function(process,
                      level = 1,
                      parent = "",
                      depth = NA,
                      numbering = FALSE,
                      terminals = TRUE,
                      xscale = 1.5,
                      yscale = 1.5,
                      step_textwrap = 18,
                      box_colour = "auto",
                      box_style = "rounded",
                      arrow_colour = base_arrow_palette,
                      arrow_style = "solid",
                      colour_palette = base_colour_palette,
                      lane_colour = base_text_palette[1],
                      title_fontsize = 14,
                      fontname = "Calibri,Arial,Helvetica",
                      title_fontname = "Calisto MT,Times New Roman",
                      ctoffset = 0.3) {

    # Check arguments OK
    if (is.na(level)) stop("Missing required argument level")

    # Clean process and validate it
    process <- clean_process(process)

    # Select correct subprocess
    subp <- list_subprocesses(process, depth = depth)
    # Reduce depth to match process if necessary
    depth <- min(depth, max(subp$Level))
    # Select subprocess
    p <- select_subprocess(process, level = level, parent = parent, depth = depth)
    nsteps <- nrow(p)
    if (nsteps < 1) stop("No level", level, " process steps for parent ", parent)

    # Create a graph title from the parent
    if (level > 2) {
        st <- subp[subp$Level == level & subp$Parent == parent,]
    } else if (!is.na(depth) && depth == 1) {
        st <- subp
    } else {
        st <- subp[subp$Level == 2,]
    }
    # st <- subp[subp$Level == min(p$Level),]
    graph_title <- paste(st$Parent[1], st$Title[1])

    # Find all the responsible roles
    p$R[is.na(p$R) | p$R == ""] <- "Default role"
    actors <- unique(p$R)

    # Build the lane information - ordered by first appearance with auto colouring
    lanes <- data.frame(Actor = actors, x = 0, y = -1 * 1:length(actors))
    lanes$Colour <- expand_palette(colour_palette, nrow(lanes))

    # Function to get lane number from the Responsible role
    get_lane <- function(name) {
        lookup <- setNames(1:nrow(lanes), lanes$Actor)
        val <- lookup[[name]]
        val
    }

    # Build coordinates
    # This algorithm may not give the most compact layout but it is simple and ensures no overlaps
    xy <- data.frame(ID = 1:nrow(p), x = NA, y = NA, colour = "", t = "Step")
    xy$periph <- p$Level - 1
    # Use a fixed colour if requested
    if (box_colour != "auto") xy$colour <- box_colour
    prev_x <- 1
    prev_y <- lanes$y[1]
    for (i in 1:nsteps) {
        lane <- get_lane(p$R[i])
        xy$colour[i] <- lanes$Colour[lane]
        xy$y[i] <- y <- lanes$y[lane]
        x <- max((lanes$x[lane:nrow(lanes)] + 1), prev_x)
        lanes$x[lane] <- x
        xy$x[i] <- x
        prev_x <- x
        prev_y <- y
    }
    # Apply scale factor
    xy$x <- xy$x * xscale
    xy$y <- xy$y * yscale


    # Number if requested
    if (numbering) p$Step <- paste0(p$IDdotted, ". ", p$Step)

    # Wrap the text
    p$Step <-
        unlist(lapply(
            strwrap(p$Step, width = step_textwrap, simplify = FALSE),
            paste,
            collapse = "\n"
        ))

    # Create nodes for the process steps
    nodes <-
        DiagrammeR::create_node_df(
            nsteps,
            label = p$Step,
            x = xy$x,
            y = xy$y,
            color = xy$colour,
            peripheries = xy$periph
        )

    # Build initial graph from nodes
    graph <-  DiagrammeR::create_graph(nodes_df = nodes)

    # Build edges (arrows between process steps) for linear process
    if (nsteps > 1) {
        from <- 1:(nsteps - 1)
        to <- 2:nsteps
        edges <- DiagrammeR::create_edge_df(
            from = from,
            to = to
            )
        graph <- DiagrammeR::add_edge_df(graph, edge_df = edges)
    }

    # Create additional nodes and edge if we want terminal start and end nodes
    if (terminals) {
        tn <- rbind(xy[1,], xy[nsteps,])
        tn$x[1] <- tn$x[1] - xscale
        tn$x[2] <- tn$x[2] + xscale
        tn$penwidth <- c(1, 2)
        tn$label <- c("Start", "End")
        tn$lp
        # Nodes
        terminal_nodes <- DiagrammeR::create_node_df(
            2,
            label = "",
            fixedsize = TRUE,
            x = tn$x,
            y = tn$y,
            penwidth = tn$penwidth,
            shape = "circle",
            color = tn$colour,
            width = 0.25,
            height = 0.25
        )
        # Edges
        te_from <- c(nsteps + 1, nsteps)
        te_to <- c(1, nsteps + 2)
        terminal_edges <- DiagrammeR::create_edge_df(
            from = te_from,
            to = te_to
        )

        # Add to the graph
        graph <- DiagrammeR::add_node_df(
            graph,
            node_df = terminal_nodes
        )
        graph <- DiagrammeR::add_edge_df(
            graph,
            edge_df = terminal_edges
        )

        # Create a single edge data frame to use in loopback
        edges <- DiagrammeR::get_edge_df(graph)
    }

    # Add nodes for cycle times

    # Create a pair of nodes for each cycle time we want to display mid-point to mid-point between process step boxes
    # and offset upwards by a factor of ctoffset
    nct <- sum(p$CT != "")
    if (nct > 0) {
        ictnodes <- 0
        ctnodes <- data.frame(ID = rep(1:nct, each = 2), x = NA, y = NA, t = "CT")
        for (i in 1:nrow(p)) {
            if (p$CT[i] != "") {
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
        ct_nodes <- DiagrammeR::create_node_df(
            nrow(ctnodes),
            x = ctnodes$x,
            y = ctnodes$y,
            shape = "point",
            label = "",
            width = 0.0,
            height = 0.0,
            style = "invisible"
        )
        graph_nodes <- DiagrammeR::count_nodes(graph)
        graph <- DiagrammeR::add_node_df(
            graph,
            node_df = ct_nodes
        )
        ct_from <- seq(graph_nodes + 1, graph_nodes + 1 + (nct  * 2 - 1), by = 2)
        ct_to <- seq(graph_nodes + 2, graph_nodes + 2 + (nct  * 2 - 1 ), by = 2)
        ct_edges <- DiagrammeR::create_edge_df(
            from = ct_from,
            to = ct_to
        )
        graph <- DiagrammeR::add_edge_df(
            graph,
            edge_df = ct_edges
        )
    }

    # Get edge df for loopbacks (ie explicit decisions for re-work)
    loop_edges <- get_loopback_edges(p, arrow_colour)

    # Add loop-backs to the graph as new edges and change shape of nodes to diamonds
    if (!is.null(loop_edges)) {
        # Create extra edges
        graph <- DiagrammeR::add_edge_df(graph, edge_df = loop_edges)
        # Modify shape of all nodes where there is a branch to a diamond
        graph <- DiagrammeR::set_node_attrs(graph, "shape", "diamond", loop_edges$from)
        # Modify to straight lines
        graph <- DiagrammeR::set_node_attrs(graph, "style", "solid", loop_edges$from)
        # Mark these edges as a "No" decision taken
        graph <- DiagrammeR::set_edge_attrs(graph, "label", "No",
                                            loop_edges$from, loop_edges$to)
        # Find normal next step and label as Yes
        loop_edges$orig <- NA
        s <- loop_edges$from %in% edges$from
        for (i in 1:nrow(loop_edges)) {
            if (!s[i]) next
            loop_edges$orig[i] <- edges$to[edges$from == loop_edges$from[i]]
        }
        graph <- DiagrammeR::set_edge_attrs(graph, "label", "Yes",
                                            loop_edges$from[s], loop_edges$orig[s])
    }


    # Expand page if we want terminals
    left <- 0 - 1 * terminals
    right <- max(lanes$x) + 1 * terminals

    # Create boxes for swimlanes using nodes at the corners joined by edges
    # Nodes
    lane_xy <- data.frame(x = 0, y = rep(0, (nrow(lanes) + 1) * 2))
    lane_xy$x[1] <- (left - 0.5) * xscale
    lane_xy$x[2] <- (right + 0.5) * xscale
    lane_xy$y[1] <- lane_xy$y[2] <- (lanes$y[1] + 0.5) * yscale
    for (i in 1:nrow(lanes)) {
        lane_xy$x[(2 * i) + 1] <- (left - 0.5) * xscale
        lane_xy$x[(2 * i) + 2] <- (right + 0.5) * xscale
        lane_xy$y[(2 * i) + 1] <- lane_xy$y[(2 * i) + 2] <- (lanes$y[i] - 0.5) * yscale
    }
    lane_nodes <-
        DiagrammeR::create_node_df(
            nrow(lane_xy),
            label = "",
            x = lane_xy$x,
            y = lane_xy$y,
            shape = "point",
            margin = 0,
            fixedsize = TRUE,
            width = 0,
            height = 0
        )
    graph <-
        DiagrammeR::add_node_df(
            graph,
            lane_nodes
            )
    # Edges
    lane_from <- c(seq(from = 1, to = nrow(lane_xy), by = 2),
                   seq(from = 1, to = nrow(lane_xy) - 2)) + nsteps + 2 * terminals + 2 * nct
    lane_to <- c(seq(from = 2, to = nrow(lane_xy), by = 2),
                 seq(from = 3, to = nrow(lane_xy))) + nsteps + 2 * terminals + 2 * nct

    lane_edges <- DiagrammeR::create_edge_df(
        from = lane_from,
        to = lane_to,
        color = lane_colour,
        penwidth = 0.25,
        arrowhead = "none"
        )
    graph <- DiagrammeR::add_edge_df(graph, lane_edges)

    # Nodes for actors -ie swim lanes
    actor_nodes <-
        DiagrammeR::create_node_df(
            nrow(lanes),
            label  = lanes$Actor,
            x = left,
            y = lanes$y * yscale,
            shape = "plain",
        )
    # Add into graph
    graph <-
        DiagrammeR::add_node_df(graph, actor_nodes)

    # Label cycle times
    if (nct > 0) {
        for (i in 1:nct) {
            graph <- DiagrammeR::set_edge_attrs(graph, "label", p$CT[p$CT != ""][i], ct_edges$from[i], ct_edges$to[i])
        }
        graph <- DiagrammeR::set_edge_attrs(graph, "arrowhead", "tee", ct_edges$from, ct_edges$to)
        graph <- DiagrammeR::set_edge_attrs(graph, "arrowtail", "tee", ct_edges$from, ct_edges$to)
        graph <- DiagrammeR::set_edge_attrs(graph, "dir", "both", ct_edges$from, ct_edges$to)
    }

    # Default styles for graph
    attrs <- c(
        "labelloc",
        "fontsize",
        "shape",
        "splines",
        "style",
        "fontcolor",
        "fixedsize",
        "fontname",
        "fontname",
        "fontname",
        "color",
        "style"
    )
    values <- c(
        "t",
        title_fontsize,
        "rectangle",
        "ortho",
        box_style,
        lane_colour,
        FALSE,
        fontname,
        fontname,
        title_fontname,
        arrow_colour,
        arrow_style
    )
    attr_types <- c(
        "graph",
        "graph",
        "node",
        "graph",
        "node",
        "node",
        "node",
        "edge",
        "node",
        "graph",
        "edge",
        "edge"
    )
    # Add in the defaults
    graph <- DiagrammeR::add_global_graph_attrs(graph,
                                                attr = attrs,
                                                value = values,
                                                attr_type = attr_types)

    # Make default edge label blank if adding loop-backs
    if (!is.null(loop_edges))  graph <- DiagrammeR::add_global_graph_attrs(graph,
                                                attr = "label",
                                                value = " ",
                                                attr_type = "edge")

    # Add graph title unless the size is NA
    if (is.na(title_fontsize)) return(graph)
    graph <- DiagrammeR::add_global_graph_attrs(graph,
                                                attr = "label",
                                                value = graph_title,
                                                attr_type = "graph")
    return(graph)
}
