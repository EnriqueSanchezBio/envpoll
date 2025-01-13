
#' Calculate Frequency Tables by Groups
#'
#' @param frequency_matrix A matrix that include frequency data
#' @param group_names A name vector that identify each group
#'
#' @return A list with filtered frequency tables per groups
#'
#'
#' @examples
#' frequency_matrix <- matrix(c(1, 2, 2, 3, 3, 1, 4, 1), nrow = 4, byrow = TRUE)
#' colnames(frequency_matrix) <- c("A", "B")
#' group_names <- c("Group1", "Group2", "Group3", "Group4")
#' calculateft (frequency_matrix, group_names)
#'
#' @importFrom dplyr filter
#'
#' @export
#'
calculateft <- function(frequency_matrix, group_names) {
  # Initializes the list that will contain the frequency tables.
  tablas_frecuencia <- list()

  # Traverses each row of the frequency matrix
  for (i in seq_len(nrow(frequency_matrix))) {
    grupo <- data.frame(frequency_matrix[i, ])  # Extract group as data frame
    colname_orig <- colnames(grupo)               # Save original name of columns
    colnames(grupo) <- "X"                        # Temporarily rename the column
    frecuencias <- dplyr::filter(grupo, X != 0)   # Filter rows where X is not 0
    colnames(frecuencias) <- colname_orig         # Restore original name of the columns
    tablas_frecuencia[[group_names[i]]] <- frecuencias  # Save to list
  }

  return(tablas_frecuencia)  # Returns the list of frequency tables
}
