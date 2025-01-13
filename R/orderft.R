
#' Order Frequency Tables by Decreasing Values
#'
#' @description This function orders the values of the frequency tables from highest to lowest in each group.
#' @param frequency_tables A list of data frames, where each data frame represents a frequency table.
#'
#' @return A list of data frames with the rows ordered in decreasing order of the first column values.
#'
#' @examples
#' freq_list <- list(
#'   data.frame(freq = c(5, 3, 8), row.names = c("A", "B", "C")),
#'   data.frame(freq = c(2, 6, 1), row.names = c("D", "E", "F"))
#' )
#' orderft(freq_list)
#'
#' @export
#'

orderft <- function(frequency_tables) {
  lapply(frequency_tables, function(x) {
    data.frame(
      row.names = rownames(x)[order(x[, 1], decreasing = TRUE)],
      x = as.numeric(x[order(x[, 1], decreasing = TRUE), ])
    )
  })
}
