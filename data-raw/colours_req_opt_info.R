#
# Define the required (required_info) and optional (optional_info) columns for ProcessMapping
#
required_info <- c("L1",
                   "Supplier",
                   "Input",
                   "Output",
                   "Customer")

# Optional columns
optional_info <- c("R",
                   "A",
                   "C",
                   "I",
                   "Branch",
                   "Notes",
                   "To",
                   "CT")

# Descriptions of the optional columns
descriptions <- c("Responsible - a role of the person or people who has to make something happen",
                  "Accountable - a role of the person who has the ultimate authority to carry out an action or ensure that the action is carried out by others",
                  "Consulted - a role of the person whose input is required for the action to be completed",
                  "Informed - a role of the person who require information from the action in order to complete their responsibilities",
                  "Label for one branch to document parallel activities",
                  "Notes for a process step",
                  "Output goes to step To (in the current scope) as well as subsequent step",
                  "Cycle time")

# Create a named list to look up description from optional_info value
desc_optional_info <- setNames(descriptions, optional_info)

# Colours for ProcessMapping
base_colour_palette <- c("#c29870",
                         "#c0bfc4",
                         "#fdd7b0",
                         "#d4de7d",
                         "#dc9393")

base_text_palette <- c("#111111",
                       "#111111",
                       "#111111",
                       "#111111",
                       "#111111")

base_arrow_palette <- c("#999999")

base_box_palette <- c("#999999")

# Working columns for internal use
internal_info <- c("IDdotted",
                   "Seq",
                   "Level",
                   "Parent",
                   "Step")

# Save data
usethis::use_data(required_info,
                  optional_info,
                  desc_optional_info,
                  internal_info,
                  base_colour_palette,
                  base_text_palette,
                  base_arrow_palette,
                  base_box_palette,
                  internal = TRUE,
                  overwrite = TRUE)
