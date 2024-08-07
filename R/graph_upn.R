#' Create a UPN graph from a process
#'
#' This function takes a process object and outputs the graph for the process in the style of
#' universal process notation (UPN) at a single  level. It calculates coordinates to wrap
#' the process  so that the process steps snake across the page. The first step shows its input
#' and other steps show the output of the step on the appropriate edge.
#'
#' @param process A Process data frame or tibble
#' @param level The process level to plot (eg 1, 3)
#' @param parentID The ID of the process to plot. The relevant sub-process must exist in the
#' process data frame and \code{parentID} must be consistent with \code{level}. This parameter is
#' needed for sub-processes at L3 and dpper. For example,
#' \code{level = 3, parentID = 1} plots the L3 processes under step 1 of the L2 process.
#' @param terminals Should initial input and final output be shown
#' @param notes Should Notes be added as comments near process steps
#' @param xscale Scale factor for x-dimension
#' @param yscale Scale factor for y-dimension
#' @param wrap Maximum number of steps before wrapping
#' @param step_textwrap String length for wrapping text in process boxes (nodes)
#' @param output_textwrap String length for wrapping text on connecting arrows (edges)
#' @param notes_textwrap String length for wrapping text on Notes
#' @param ratio_x_y Aspect ratio used to override yscale
#' @param offset How much of scale to take up for multiple branches
#' @param ctoffset How much offset to use for cycle time arrows
#' @param navoffset How much offset for navigation buttons
#' @param font_colour Colour for process step text
#' @param fontname Name of font to use
#' @param notes_colour Colour for notes text
#' @param box_colour Color for the edge of the process step box
#' @param title_fontsize Size of the title font in points (NA means no title)
#' @param title_fontname Name of font for title
#' @param step_width Minimum box width
#' @param step_height Minimum box height
#' @param nav_icons Add in icons to link to a parent process and L1 process
#'
#' @return A graph object of class dgr_graph
#'
#' @examples
#' # Create a template process with 5 steps and fill out a standard template for the SIPOC
#' # Note that row 1 contains L1 (level 1) information and the subsequent 4 rows contain L2
#' # steps that will would appear in a SIPOC
#' p <- create_process(steps = 5, sipoc_template = TRUE)
#'
#' # You could then edit the process in R or export to Excel then re-import
#' # graph_upn plots the process one level at a time. If the level is 3 or deeper then you also
#' # need to specify the parent step number
#' g <- graph_upn(p, level = 2, wrap = 2)
#'
#' # Visualize the process as a graph using Diagrammer
#' DiagrammeR::render_graph(g)
#'
#' @export
graph_upn <- function(process,
                      level = 2,
                      parentID = "",
                      terminals = TRUE,
                      notes = FALSE,
                      xscale = 2.2,
                      yscale = 1.5,
                      wrap = 5,
                      step_textwrap = 18,
                      output_textwrap = 15,
                      notes_textwrap = 22,
                      ratio_x_y = NULL,
                      offset = 0.5,
                      ctoffset = 0.35,
                      navoffset = 0.65,
                      font_colour = base_text_palette[1],
                      fontname = "Calibri,Arial,Helvetica",
                      notes_colour = base_text_palette[1],
                      box_colour = base_colour_palette[1],
                      edge_colour = base_box_palette,
                      title_fontsize = 14,
                      title_fontname = "Calisto MT,Times New Roman",
                      step_width = 1.25,
                      step_height = 0.60,
                      nav_icons = TRUE) {

    # Check required input
    if (is.na(level)) stop("Missing required argument level")

    # Check wrap
    if (wrap < 1) stop("Parameter wrap must be at least 1")

    # Clean process and validate it
    process <- clean_process(process)

    # Select correct subprocess
    s <- list_subprocesses(process, depth = level)
    p <- select_subprocess(process, level = level, depth = level, parent = parentID)
    Lx <- paste0("L", level)
    if (nrow(p) < 1) stop("No ", Lx, " process steps for parent ", parentID)

    # Create a graph title from the parent
    st <- s[s$Parent == p$Parent[1] & s$Level == p$Level[1],]
    stepnum <- st$Parent[1]
    if (stepnum != "") stepnum <- paste0(stepnum, ". ")
    graph_title <- paste0(stepnum, st$Title[1])

    # Which process steps have their own processes
    subp <- list_subprocesses(process)
    subp$ID <- paste0("UPN", 1:nrow(subp))
    if (level > 3) {
        parent_root <- sub("\\.[0-9]+$", "", parentID)
        nav_parent <- subp$ID[subp$Level == (level - 1) & subp$Parent == parent_root]
    } else if (level == 3) {
        nav_parent <- "UPN2"
    } else {
        nav_parent <- "UPN1"
    }
    subp <- subp[subp$Level == (level + 1),]
    if (nrow(subp) > 0) {
        subp$Exists <- FALSE
        for (i in 1:nrow(subp)) {
            if (subp$Parent[i] %in% p$IDdotted) subp$Exists[i] <- TRUE
        }
        subp <- subp[subp$Exists,]
    }

    # Parse branches - needed to get coordinates correct later
    pp <- parse_branches(p)
    if (level == 1) {
        pp$title <- pp$Step
    } else {
        pp$title <- paste0(pp$IDdotted, ". ", pp$Step)
    }
    pp$wrap_title <- fmt_text(pp$title, wrap = step_textwrap)

    # Pick right resource to plot, depending on level
    if (level == 1) {
        resource <- "A"
    } else {
        resource <- "R"
    }
    use_resource <- FALSE

    # Ignore resource plotting if nothing defined
    if (paste0(pp[[resource]], collapse = "") == "") {
        resource <- NA
        pp$use_roles <- ""
    } else {
        use_resource <- TRUE
        role <- fmt_text(pp[[resource]], wrap = step_textwrap)
        has_role <- pp[[resource]] != ""
        pp$wrap_title[has_role] <- paste0("{", pp$wrap_title[has_role], "|{", resource, "|", role[has_role], "}}")
        pp$wrap_title[!has_role] <- paste0("{", pp$wrap_title[!has_role], "}")
        pp$use_roles <- role
        step_height <- step_height + 0.25
    }

    # Fix scale factors
    nstep <- nrow(pp)
    if (!is.null(ratio_x_y)) yscale <- (wrap * xscale) / (ceiling(nstep/wrap) * ratio_x_y)

    # Generate coordinates of each process step
    node_props <- wrap_process(pp, wrap = wrap, xscale = xscale, yscale = yscale,
                               branchoffset = offset, terminals = terminals, tscale = 0.75,
                               ctoffset = ctoffset)

    # Add edges for the terminals
    nnode <- nrow(node_props)
    nterm <- sum(grepl("^T[12]", node_props$t))
    nct <- sum(node_props$t == "CT")
    node_props$label <- c(pp$wrap_title, rep("", nterm + nct))
    node_props$style <- "solid"
    if (use_resource) {
        node_props$shape = "record"
    } else {
        node_props$shape <- "rectangle"
    }
    node_props$color <- box_colour
    node_props$tooltip <- "What (process step)"
    if (use_resource) node_props$tooltip <- paste0(node_props$tooltip, " and Who (", resource, ")")
    node_props$use_roles <- c(pp$use_roles, rep("", nterm + nct))
    node_props$navigate <- ""

    node_props$height <- step_height
    node_props$width <- step_width

    pp$Output <- fmt_text(pp$Output,  wrap = output_textwrap)
    pp$Input <- fmt_text(pp$Input,  wrap = output_textwrap)

    # Create nodes - add in notes if requested
    if (notes) {
        notes_props <- node_props[1:nstep,]
        notes_props$tooltip <- paste0("Note (notes for ", pp$title, ")")
        notes_props$t <- "Note"
        notes_props$height <- 0.5
        notes_props$style <- "dotted"
        notes_props$x <- notes_props$x - 0.0
        notes_props$y <- notes_props$y + step_height / 2 + 0.4
        notes_props$color <- notes_colour
        notes_props$label <- fmt_text(pp$Notes, wrap = notes_textwrap)
        notes_props <- notes_props[notes_props$label != "",]
        node_props <- rbind(node_props, notes_props)
        nnode <- nnode + nrow(notes_props)
    }

    # Create nodes to indicate sub-process exists
    if (nrow(subp) > 0) {
        has_subp <- as.numeric(sub("^.*\\.([0-9]+)", "\\1", subp$Parent))
        subp_props <- node_props[1:nstep,]
        if (level == 1) {
            has_subp <- 1
            subp_props$navigate[1] <- "UPN2"
        } else {
            for (i in 1:nrow(subp)) {
                id <- has_subp[i]
                nav <- subp$ID[i]
                subp_props$navigate[id] <- nav
            }
        }
        # Insert any navigation down arrows inside curly brace on the left - which we get only if use_reource is active
        if (use_resource) {
            pattern <- "^[{]"
            replacement <- "{\u25bc"
        } else {
            pattern <- "^"
            replacement <- "\u25bc"
        }
        node_props$label[has_subp] <- sub(pattern, replacement, node_props$label[has_subp])
        node_props$navigate[has_subp] <- subp_props$navigate[has_subp]
        node_props$tooltip[has_subp] <- paste0("Navigate to sub-process\n",
                                               node_props$tooltip[has_subp])
    }

    # Create up navigation nodes if needed
    if (nav_icons && level > 1) {
        nav_props <- rbind(node_props[1,], node_props[1,])
        nav_props$label <- ""
        nav_props$style <- "filled"
        nav_props$width <- 0.25
        nav_props$height <- 0.25
        nav_props$t <- "Navigate"
        xmin <- min(node_props$x)
        nav_props$x[1] <- (-1 / 3) * (xmin + step_width / 2) + xmin
        nav_props$y[1] <- max(node_props$y[node_props$t == "Step"]) + step_height / 2 + navoffset
        nav_props$navigate[1] <- "UPN1"
        nav_props$shape[1] <- "house"
        nav_props$tooltip[1] <- "Navigate to L1 process"
        nav_props$shape[2] <- "triangle"
        nav_props$navigate[2] <- nav_parent
        nav_props$x[2] <- (-2 / 3) * (xmin + step_width / 2) + xmin
        nav_props$y[2] <- nav_props$y[1] - 0.0375
        nav_props$tooltip[2] <- "Navigate to parent process"
        node_props <- rbind(node_props, nav_props)
        nnode <- nnode + nrow(nav_props)
    }

    # Put together complete set of nodes as a node data frame from process steps,
    # resource boxes and notes
    nodes <-
        DiagrammeR::create_node_df(
            nnode,
            label = node_props$label,
            shape = node_props$shape,
            color = node_props$color,
            style = node_props$style,
            fontcolor = font_colour,
            x = node_props$x,
            y = node_props$y,
            height = node_props$height,
            width = node_props$width,
            tooltip = node_props$tooltip,
            navigate = node_props$navigate
        )

    # Set properties of terminal nodes
    if (terminals) {
        tnodes <- grepl("^T", node_props$t)
        nodes$width[tnodes] <- 0.02
        nodes$height[tnodes] <- 0.02
        nodes$style[tnodes] <- "invisible"
        nodes$tooltip[tnodes] <- "Terminal"
    }

    # Set properties of cycle-time nodes
    if (nct > 0) {
        ctnodes <- node_props$t == "CT"
        nodes$width[ctnodes] <- 0.02
        nodes$height[ctnodes] <- 0.02
        nodes$style[ctnodes] <- "invisible"
        nodes$tooltip[ctnodes] <- "Cycle time marker"
    }

    # Build edges for all steps including edges as edge data frame
    edge_pairs <- label_edges(pp, terminals)
    if (!is.null(edge_pairs)) {
        edges <- DiagrammeR::create_edge_df(
            from = edge_pairs$from,
            to = edge_pairs$to,
            dir = edge_pairs$dir,
            color = edge_colour,
            label = edge_pairs$label,
            tooltip = edge_pairs$tooltip,
            labeltooltip = edge_pairs$tooltip,
            arrowhead = edge_pairs$type,
            arrowtail = edge_pairs$type
        )
    } else {
        edges <- NULL
    }

    # Combine nodes and edges into a graph
    graph <-
        DiagrammeR::create_graph(nodes_df = nodes,
                                 edges_df = edges,
                                 graph_name = pp$L1[1])
    graph <-
        DiagrammeR::add_global_graph_attrs(graph,
                                           attr = "splines",
                                           value = "ortho",
                                           attr_type = "graph")

    # Get edge df for any loopbacks
    loop_edges <- get_loopback_edges(p, edge_colour = edge_colour)
    if (!is.null(loop_edges)) graph <- DiagrammeR::add_edge_df(graph,
                                                               edge_df = loop_edges)

    # Set default node parameters
    graph <- DiagrammeR::add_global_graph_attrs(graph,
                                                attr = c("fixedsize",
                                                         "fontname",
                                                         "style",
                                                         "fillcolor",
                                                         "fontcolor"),
                                                value = c(FALSE,
                                                          fontname,
                                                          "solid",
                                                          box_colour,
                                                          font_colour),
                                                attr_type = "node")

    # set edge parameters
    graph <- DiagrammeR::add_global_graph_attrs(graph,
                                                attr = c("fontname",
                                                         "fontcolor",
                                                         "color"),
                                                value = c(fontname,
                                                          font_colour,
                                                          edge_colour),
                                                attr_type = c("edge",
                                                              "edge",
                                                              "edge"))


    # Exit the function now if we don't want the title
    if (is.na(title_fontsize)) return(graph)

    # Add title to graph
    attrs <- c(
        "label",
        "labelloc",
        "fontsize",
        "fontcolor",
        "fontname"
    )
    values <- c(
        graph_title,
        "t",
        title_fontsize,
        font_colour,
        title_fontname
    )
    attr_types <- c(
        "graph",
        "graph",
        "graph",
        "graph",
        "graph"
    )
    graph <- DiagrammeR::add_global_graph_attrs(graph,
                                                attr = attrs,
                                                value = values,
                                                attr_type = attr_types)

    return(graph)
}
