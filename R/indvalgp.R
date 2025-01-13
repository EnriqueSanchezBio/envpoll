
#' Calculate IndVal Index for Taxa Across Provinces
#'
#' @param x A matrix where the rows are samples and the columns are taxa.
#' @param provinces A factor indicating to which province each sample belongs.
#'
#' @returns A data frame where rows represent provinces and columns represent taxa. Values indicate the IndVal index observed for each combination of province and taxon.
#'
#' @examples#
#' #Crear una matriz de datos (20 muestras, 5 taxones)
#'
#' set.seed(123)
#' x <- matrix(sample(0:10, 100, replace = TRUE), nrow = 20, ncol = 5)
#' colnames(x) <- paste0("Taxon_", 1:5)
#'
#' # Crear un factor para las provincias (4 provincias)
#' provinces <- factor(rep(c("Province_A", "Province_B", "Province_C", "Province_D"), each = 5))
#'
#' # Ver la matriz de datos y las provincias
#' print("Matriz de datos (x):")
#' print(x)
#' print("Provincias:")
#' print(provinces)
#'
#' # Definir la función indval1 (auxiliar)
#' indval1 <- function(x) {
#'   A <- apply(x, 2, sum) / sum(apply(x, 2, sum))
#'   B <- apply(ifelse(x == 0, 0, 1), 2, sum) / nrow(x)
#'   IV <- round(A * B * 100, 2)
#'   IV <- IV[which(IV != 0)]
#'   return(IV)
#' }
#'
#' # Calcular IndVal por provincia usando la función indval
#' result <- envpoll::indvalgp(x, provinces)
#'
#' # Mostrar el resultado
#' print("Resultado de IndVal por provincia:")
#' print(result)
#'
#'
#' @export
#'

indvalgp <- function(x, provinces) {
  # Create a list to store IndVal results for each province
  IV_list <- as.list(1:nlevels(provinces))
  names(IV_list) <- levels(provinces)

  # Calculate IndVal for each province
  for (i in 1:length(IV_list)) {  # 'i' represents a province
    prov_data <- x[provinces == levels(provinces)[i], ]  # Subset data for each province
    IV_list[[i]] <- envpoll::indval1(prov_data)  # Apply indval1 to the subset
  }

  # Create an empty data frame to store observed IndVal results
  IV_matrix <- data.frame(matrix(0, ncol = ncol(x), nrow = nlevels(provinces)))
  rownames(IV_matrix) <- names(IV_list)
  colnames(IV_matrix) <- colnames(x)

  # Fill the data frame with IndVal results
  for (i in 1:nrow(IV_matrix)) {
    IV_matrix[i, names(IV_list[[i]])] <- IV_list[[i]]
  }

  return(IV_matrix)  # Return the data frame with IndVal results by province and taxa
}
