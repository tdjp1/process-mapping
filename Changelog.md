# ProcessMapping library

## Possible future enhancement

-   Add Branch functionality to graph_slm
-   Specify shape for particular boxes in SLM add_optional(add = "Type") and enter text such as Decision (diamond), Start, End - default is Activity need to label eg YN for a decision
-   Consistent comments throughout
-   Maybe look at yscale and ratio_x\_y and make consistent bewteen SLM and UPN
-   Add tooltips to swimlane maps
-   Investigate Rough.js and sketchviz.com

## Completed

-   add_optional_info(add = "CT") adds column for cycle times and these are plotted in UPN map
-   explain_optional_info - new function to explain possibly cryptic names of optional columns
-   Modify generate_dot output to use double-quotes rather than single in Shiny tool
-   Add tooltips to SIPOC
-   Update creation of upn to use graph_attr rather than writing for each node
-   Add tooltips to UPN maps
-   Add navigation tools to sub-process, parent and L1 in UPN maps
-   Add process metadata to Tibmatic (to-be/as-is, description, author)
-   Notes more visible as own node
-   Small Box added to top left of process step if it has a sub-process
-   Fixed width boxes now the default
-   added auto Diamond shape for steps with multiple outputs and label Yes or N
-   add Branch button for Shiny
-   use Try/Catch and modal dialogue in Shiny to let users see error messages
-   Add To functionality in graph_upn
-   Allow branches for first and last process steps (graph_upn)
-   add Tibmatic logo to Shiny tool
-   Find a way to add help/documention/tips - maybe as another tab in the SHiny interface
-   Add save state + resume state button/functionality in Shiny to store data and settings
-   Add arrow_style for SLM DONE
-   Colours for swimlanes DONE
-   add_optional(add = "Level") to add a sub-process level
-   Shiny app improve interface to choose between UPN and SLM with multiple levels
-   add Level button in Shiny
-   Multi-level rendering in swimlanes (level = 2, max_level = NA) NA means no limit
-   option for big arrow for process steps in graph_sipoc
-   use list_subprocesses() to allow any UPN map to be displayed
-   add_optional(add = "To") to add To column with info on extra connections from a node - enter as a relative number (+1, -3) perhaps need to only allow within same sub-process. Multiple values allowed separated by anything that isn't a number - maybe add optional labels here too such as N-1,Y+1 - maybe label is actually required - need to deal with overwriting edge that is already specified

## Rejected

-   Maybe update SLM x-value for next box
