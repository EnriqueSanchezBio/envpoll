
#' Perform Random Chi-Square Tests for Frequency Tables
#'
#' @param frequency_tables A list of frequency tables.
#' @param bioregions A dataframe containing a column with any bioregion type used to generate random samples.
#' @param n_iter The number of iterations for the random sampling (default is 1000).
#'
#' @return The function returns a list with two elements:
#'  random_table: A list of chi-square test results for each random sample.
#'  p_means: A list of mean p-values for each iteration across all frequency tables.
#'
#'
#' @importFrom stats chisq.test
#'
#' @examples
#'
#' set.seed(123)
#'
#' # Example frequency tables (simulated)
#'  frequency_tables <- list(
#'  data.frame(A = c(10, 20, 30), B = c(5, 15, 25)),
#'  data.frame(A = c(10, 20, 30), B = c(10, 20, 30))
#' )
#' #Simulating the 'bioregions' dataframe with a 'Province' column
#' bioregion <- data.frame(Province = sample(letters, 100, replace = TRUE))
#' # Call the function
#' result <- randomchi(frequency_tables, bioregion, n_iter = 1000)
#' # View the results
#' print(result$p_means)
#'
#'
#' @export


randomchi <- function(frequency_tables, bioregions, n_iter = 1000) {
  random_table <- list()
  random_names <- 1:n_iter
  p_values <- matrix()
  p_means <- list()

  for (i in 1:length(frequency_tables)) {
    tfrec <- frequency_tables[[i]]
    n <- sum(tfrec)
    equal <- all(tfrec == tfrec[1,])

    if (equal) {
      equal <- TRUE
    } else {
      provinces <- bioregions[[1]]
      for (j in 1:n_iter) {
        random <- provinces[sample(1:length(provinces), n)]
        random <- sort(table(random), decreasing = TRUE)
        are_equal <- all(random == random[1])

        if (are_equal) {
          are_equal <- TRUE
        } else {
          ncat <- max(c(length(random), nrow(tfrec)))
          comparison <- data.frame(matrix(0, ncol = 2, nrow = ncat))
          comparison[1:nrow(tfrec), 1] <- tfrec[, 1]
          comparison[1:length(random), 2] <- random
          chi_j <- chisq.test(comparison[, 1], comparison[, 2])
          random_table[[random_names[j]]] <- chi_j
          p_values[[j]] <- chi_j$p.value
        }
      }
    }
    p_means[[i]] <- mean(unlist(p_values))
  }

  return(list(random_table = random_table, p_means = p_means))
}
