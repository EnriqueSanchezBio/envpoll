
#' Title
#'
#' @description
#' Auxiliary function for calculating indvalgp for a group of samples
#'
#' @param x x is a matrix with samples in rows and taxa in columns
#'
#' @returns index calculated as the product of A and B, multiplied by 100
#'
#'
#' @examples
#' set.seed(123)
#' sample_matrix <- matrix(sample(0:5, 30, replace = TRUE), nrow = 6, ncol = 5)
#' colnames(sample_matrix) <- paste0("Taxon_", 1:5)
#' rownames(sample_matrix) <- paste0("Sample_", 1:6)
#'
#' # Mostrar la matriz de ejemplo
#' print("Matriz de muestras:")
#' print(sample_matrix)
#'
#' # Calcular el índice IndVal utilizando la función indval1
#' indval_result <- indval1(sample_matrix)
#'
#' # Mostrar el resultado
#' print("Índice IndVal por taxón:")
#' print(indval_result)
#'
#' @export
#'
indval1 <- function(x) {
  A <- apply(x, 2, sum) / sum(apply(x, 2, sum))   # A: Proportion of total abundance for each taxon in the group
  B <- apply(ifelse(x == 0, 0, 1), 2, sum) / nrow(x)    # B: Frequency of occurrence of each taxon in the group's samples
  IV <- round(A * B * 100, 2)   # IndVal index calculated as the product of A and B, multiplied by 100
  IV <- IV[which(IV != 0)]    # Filter IndVal values different from 0
  return(IV)
}
