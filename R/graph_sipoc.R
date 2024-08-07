#' Creates a SIPOC graph from a process
#'
#' This function is user-callable and requires DiagrammeR. It takes a process data frame, including
#' Inputs, Outputs, Supplier and Customers and the L2 process and creates the nodes, egdes and a graph
#' as a SIPOC form in AZ colours from this input
#'
#' @param process A process structure
#' @param xscale Scale factor in x-dimension
#' @param yscale Scale factor in y-dimension
#' @param sipoc_label_location Where to plot the SIPOC legend box
#' @param step_textwrap Specify longest line before wrapping
#' @param title_fontsize Size of the title font in points (NA means no title)
#' @param edge_colour Colour of edeges/arrows
#' @param fill_colours Five (or one) colours for SIPOC boxes
#' @param box_colours Five (or one) colours for SIPOC boxes (default match fill colours)
#' @param font_colours Five (or one) colours for text in SIPOC boxes
#' @param fontname Name of font to use
#' @param title_fontname Name of font for the title
#' @param width Standard box width
#'
#' @examples
#' p <- create_process(steps = 6, sipoc_template = TRUE)
#' g <- graph_sipoc(p)
#' DiagrammeR::render_graph(g)
#'
#' @export
graph_sipoc <- function(process,
                        xscale = 1.8,
                        yscale = 0.9,
                        sipoc_label_location = 0.25,
                        step_textwrap = 22,
                        title_fontsize = 14,
                        edge_colour = base_box_palette,
                        fill_colours = base_colour_palette,
                        box_colours = NULL,
                        font_colours = base_text_palette,
                        fontname = "Calibri,Arial,Helvetica",
                        title_fontname = "Calisto MT,Times New Roman",
                        width = 1.35) {

    # Set fonts

    # Clean process and Check it validates
    process <- clean_process(process)

    # Check colours are right
    if (length(fill_colours) != 5 &&
        length(fill_colours) != 1) stop("No to supply precisely one or five fill_colours")
    if (length(box_colours) != 5 &&
        length(box_colours) != 1 &&
        !is.null(box_colours)) stop("No to supply precisely one or five box_colours")
    if (length(font_colours) != 5 &&
        length(font_colours) != 1) stop("No to supply precisely one or five font_colours")

    # Select correct subprocess L2 only one allowed for SIPOC
    s <- list_subprocesses(process, depth = 2)
    p <- select_subprocess(process, level = 2, depth = 2)

    # Create a graph title from the parent
    st <- s[s$Level == p$Level[1],]
    graph_title <- st$Title[1]
    nproc <- nrow(p)
    nstep <- nproc + 1

    # Start to build single data frame for every node in the SIPOC
    pp <- data.frame(Label = p$Step)

    # Add the SI and OC boxes to complete the first column along with SIPOC boxes for the left
    pp <- rbind(
        pp,
        data.frame(Label = "P"),
        data.frame(Label = p$Input),
        data.frame(Label = "I"),
        data.frame(Label = p$Output),
        data.frame(Label = "O"),
        data.frame(Label = p$Supplier),
        data.frame(Label = "S"),
        data.frame(Label = p$Customer),
        data.frame(Label = "C")
    )

    # Add coordinates
    pp$x <- rep(c(1:(nstep - 1), sipoc_label_location), 5) * xscale
    pp$y <- rep(c(0, 1, -1, 2, -2), each = nstep) * yscale

    # Add width
    pp$width <- width
    adjust <- seq(from = nstep, by = nstep, length = 5)
    pp$width[adjust] <- 0.5


    # Set the fill colours
    pp$fillcolor <- rep(fill_colours, each = nstep)

    # Set box egde colours
    if (is.null(box_colours)) {
        pp$color = pp$fill
    } else {
        pp$color <- rep(box_colours, each = nstep)
    }

    # Set text folour depending on background to ensure better legibility
    pp$fontcolor <- rep(font_colours, each = nstep)

    # Add tooltips
    pp$tooltip <- ""
    # Process
    pp$tooltip[1:nproc] <- paste("Process step", 1:nproc)
    pp$tooltip[nstep] <- "Process"
    # Input
    pp$tooltip[(nstep + 1):(nstep + nproc)] <- paste("Input for",
                                                     pp$Label[1:nproc])
    pp$tooltip[(2 * nstep)] <- "Input"
    # Supplier
    pp$tooltip[(3 * nstep + 1):(3 * nstep + nproc)] <- paste("Supplier of",
                                                             pp$Label[(nstep + 1):(nstep + nproc)])
    pp$tooltip[(4 * nstep)] <- "Supplier"
    # Output
    pp$tooltip[(2 * nstep + 1):(2 * nstep + nproc)] <- paste("Output from",
                                                             pp$Label[1:nproc])
    pp$tooltip[(3 * nstep)] <- "Output"
    # Customer
    pp$tooltip[(4 * nstep + 1):(4 * nstep + nproc)] <- paste("Customer of",
                                                             pp$Label[(2 * nstep + 1):(nstep * 2 + nproc)])
    pp$tooltip[(5 * nstep)] <- "Customer"

    # Wrap the Label
    pp$Label <- fmt_text(pp$Label, wrap = step_textwrap)

    # Create DiagrameR nodes
    nodes <-
        DiagrammeR::create_node_df(
            nstep * 5,
            label = pp$Label,
            width = pp$width,
            color = pp$color,
            fontcolor = pp$fontcolor,
            fillcolor =  pp$fillcolor,
            tooltip = pp$tooltip,
            x = pp$x,
            y = pp$y
        )

    # Create DiagrameR edges
    edges <-
        DiagrammeR::create_edge_df(
            from = 1:(nstep - 2),
            to = 2:(nstep - 1),
            color = edge_colour
        )

    # Put the graph together
    graph <-
        DiagrammeR::create_graph(nodes_df = nodes,
                                 edges_df = edges,
                                 graph_name = "SIPOC")

    # Default values for every node
    node_attrs <- c(
        "fontname",
        "fixedsize",
        "shape",
        "style"
    )
    node_values <- c(
        fontname,
        FALSE,
        "rectangle",
        "filled"
    )

    graph <- DiagrammeR::add_global_graph_attrs(graph,
                                                attr = node_attrs,
                                                value = node_values,
                                                attr_type = "node")

    # Return here if we don't want the title
    if (is.na(title_fontsize)) return(graph)

    # Add title to graph
    attrs <- c(
        "label",
        "labelloc",
        "fontsize",
        "fontname"
    )
    values <- c(
        graph_title,
        "t",
        title_fontsize,
        title_fontname
    )
    graph <- DiagrammeR::add_global_graph_attrs(graph,
                                                attr = attrs,
                                                value = values,
                                                attr_type = "graph")

    return(graph)
}

#' @export
process_plot_sipoc <- function(...) {
    .Deprecated("graph_sipoc")
    graph_sipoc(...)
}
