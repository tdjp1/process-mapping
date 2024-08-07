#' process_title
#'
#' Find the name of the process as described in L1
#' @param process process in a standard format
#'
#' @return Name of the process as a character string
#'
#' @examples
#' \dontrun{
#' title <- process_title(process)
#' }
#'
#' @export
process_title <- function(process) {
    head(process$L1[!is.na(process$L1)], n = 1)
}

#' process_L2_title
#'
#' Find the name of an L2 process
#' @param process process in a standard format
#' @param L2ID the ID of the L2 process of interest
#'
#' @return Name of the L2 process as a character string
#'
#' @examples
#' \dontrun{
#' title <- process_L2_title(process, L2ID)
#' }
#' @export
process_L2_title <- function(process, L2ID) {
    paste0(L2ID, ". ", head(process$L2[!is.na(process$L2) & process$L2ID == L2ID], n = 1))
}

#' process_L2IDs
#'
#' Find all the L2 IDs for a process
#' @param process process in a standard format
#'
#' @return Array of L2 IDs for a process
#'
#' @examples
#' \dontrun{
#' title <- process_L2IDs(process)
#' }
#' @export
## process_L2IDs ----
process_L2IDs <- function(process) {
    ids <- unique(process$L2ID)
    ids <- ids[!is.na(ids)]
    ids
}
