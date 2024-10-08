% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graph_upn.R
\name{graph_upn}
\alias{graph_upn}
\title{Create a UPN graph from a process}
\usage{
graph_upn(
  process,
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
  step_height = 0.6,
  nav_icons = TRUE
)
}
\arguments{
\item{process}{A Process data frame or tibble}

\item{level}{The process level to plot (eg 1, 3)}

\item{parentID}{The ID of the process to plot. The relevant sub-process must exist in the
process data frame and \code{parentID} must be consistent with \code{level}. This parameter is
needed for sub-processes at L3 and dpper. For example,
\code{level = 3, parentID = 1} plots the L3 processes under step 1 of the L2 process.}

\item{terminals}{Should initial input and final output be shown}

\item{notes}{Should Notes be added as comments near process steps}

\item{xscale}{Scale factor for x-dimension}

\item{yscale}{Scale factor for y-dimension}

\item{wrap}{Maximum number of steps before wrapping}

\item{step_textwrap}{String length for wrapping text in process boxes (nodes)}

\item{output_textwrap}{String length for wrapping text on connecting arrows (edges)}

\item{notes_textwrap}{String length for wrapping text on Notes}

\item{ratio_x_y}{Aspect ratio used to override yscale}

\item{offset}{How much of scale to take up for multiple branches}

\item{ctoffset}{How much offset to use for cycle time arrows}

\item{navoffset}{How much offset for navigation buttons}

\item{font_colour}{Colour for process step text}

\item{fontname}{Name of font to use}

\item{notes_colour}{Colour for notes text}

\item{box_colour}{Color for the edge of the process step box}

\item{title_fontsize}{Size of the title font in points (NA means no title)}

\item{title_fontname}{Name of font for title}

\item{step_width}{Minimum box width}

\item{step_height}{Minimum box height}

\item{nav_icons}{Add in icons to link to a parent process and L1 process}
}
\value{
A graph object of class dgr_graph
}
\description{
This function takes a process object and outputs the graph for the process in the style of
universal process notation (UPN) at a single  level. It calculates coordinates to wrap
the process  so that the process steps snake across the page. The first step shows its input
and other steps show the output of the step on the appropriate edge.
}
\examples{
# Create a template process with 5 steps and fill out a standard template for the SIPOC
# Note that row 1 contains L1 (level 1) information and the subsequent 4 rows contain L2
# steps that will would appear in a SIPOC
p <- create_process(steps = 5, sipoc_template = TRUE)

# You could then edit the process in R or export to Excel then re-import
# graph_upn plots the process one level at a time. If the level is 3 or deeper then you also
# need to specify the parent step number
g <- graph_upn(p, level = 2, wrap = 2)

# Visualize the process as a graph using Diagrammer
DiagrammeR::render_graph(g)

}
