#' Indicator Species Analysis with Randomization
#'
#' @param x A matrix or data frame where rows represent samples and columns represent taxa.
#' @param provinces A factor indicating the province to which each sample (row) belongs.
#' @param n An integer specifying the number of randomizations to perform. Default is 100.
#' @param alpha A numeric value indicating the significance threshold for p-values. Default is 0.05.
#'
#'
#' @return A list containing:
#' \describe{
#'   \item{indval_observed}{A matrix with the observed IndVal values, where rows represent provinces and columns represent taxa.}
#'   \item{p_values}{A matrix with p-values for each taxon (columns) in each province (rows).}
#'   \item{density_plots}{A list of density plots for the randomizations, saved as PDFs.}
#'   \item{significant_taxa}{A list where each element is a vector of significant taxa for a given province.}
#' }
#'
#'
#' @importFrom grDevices dev.off pdf
#' @importFrom graphics abline
#' @importFrom stats density
#' @importFrom utils setTxtProgressBar txtProgressBar
#'
#'
#' @examples
#'
#' #' # Example dataset
#' set.seed(123)
#' x <- matrix(sample(0:10, 50, replace = TRUE), nrow = 10, ncol = 5)
#' colnames(x) <- paste0("Taxon_", 1:5)
#' provinces <- factor(rep(c("Prov_A", "Prov_B"), each = 5))
#'
#' # Run the test
#' result <- test_indval(x, provinces, n = 100, alpha = 0.05)
#' print(result$significant_taxa)
#' test_indval <- function(x, provinces, n = 100, alpha = 0.05) {
#'   # Step 1: Calculate observed IndVal
#'   IVo <- envpoll::indvalgp(x, provinces)
#'
#'   # Step 2: Generate random realizations
#'   random <- vector("list", n)
#'   pb <- txtProgressBar(min = 0, max = n, style = 3, width = 50, char = "\u27a4")
#'   for (j in 1:n) {
#'     x1 <- x
#'     for (i in 1:ncol(x)) {
#'       x1[, i] <- x[sample(1:nrow(x), nrow(x)), i]
#'     }
#'     random[[j]] <- envpoll::indvalgp(x1, provinces)
#'     setTxtProgressBar(pb, j)
#'   }
#'   close(pb)
#'
#'   # Combine random realizations into a single data frame
#'   random <- do.call(rbind, random)
#'
#'   # Step 3: Organize random realizations by province
#'   random1 <- vector("list", nrow(IVo))
#'   names(random1) <- rownames(IVo)
#'   for (i in 1:length(random1)) {
#'     random1[[i]] <- random[seq(i, nrow(random), nlevels(provinces)), ]
#'   }
#'
#'   # Step 4: Estimate p-values based on random realizations
#'   pdf1 <- random1
#'   pval <- IVo
#'   pb <- txtProgressBar(min = 0, max = length(pdf1), style = 3, width = 50, char = "\u27a4")
#'   for (i in 1:length(pdf1)) {
#'     pdf1[[i]] <- vector("list", ncol(x))
#'     names(pdf1[[i]]) <- colnames(random1[[i]])
#'     for (j in 1:length(pdf1[[i]])) {
#'       pdf1[[i]][[j]] <- density(random1[[i]][, j])
#'       b <- pdf1[[i]][[j]]$x[2] - pdf1[[i]][[j]]$x[1]
#'       pval[i, j] <- sum(b * (pdf1[[i]][[j]]$y[which(pdf1[[i]][[j]]$x > IVo[i, j])]))
#'     }
#'
#'     # Save density plots to a PDF file for each province
#'     pdf(file = paste0(names(pdf1)[i], ".pdf"))
#'     par(mfrow = c(3, 3))
#'     for (j in 1:length(pdf1[[i]])) {
#'       plot(
#'         pdf1[[i]][[j]], sub = paste("p-val =", round(pval[i, j], 2)),
#'         main = names(pdf1[[i]])[j], xlim = range(c(IVo[i, j], pdf1[[i]][[j]]$x))
#'       )
#'       abline(v = IVo[i, j], col = "red")
#'     }
#'     dev.off()
#'     setTxtProgressBar(pb, i)
#'   }
#'   close(pb)
#'
#'   # Step 5: Identify significant taxa per province
#'   ind_taxa <- apply(pval, 1, function(x) names(which(x < alpha)))
#'
#'   # Return results as a list
#'   return(list(
#'     indval_observed = IVo,
#'     p_values = pval,
#'     density_plots = pdf1,
#'     significant_taxa = ind_taxa
#'   ))
#' }
#'
#' @export
#'

test_indval <- function(x, provinces, n, alpha = 0.05) {
  # Step 1: Calculate observed IndVal
  IVo <- envpoll::indvalgp(x, provinces)

  # Step 2: Generate random realizations
  random <- vector("list", n)
  pb <- txtProgressBar(min = 0, max = n, style = 3, width = 50, char = "\u27a4")
  for (j in 1:n) {
    x1 <- x
    for (i in 1:ncol(x)) {
      x1[, i] <- x[sample(1:nrow(x), nrow(x)), i]
    }
    random[[j]] <- envpoll::indvalgp(x1, provinces)
    setTxtProgressBar(pb, j)
  }
  close(pb)

  # Combine random realizations into a single data frame
  random <- do.call(rbind, random)

  # Step 3: Organize random realizations by province
  random1 <- vector("list", nrow(IVo))
  names(random1) <- rownames(IVo)
  for (i in 1:length(random1)) {
    random1[[i]] <- random[seq(i, nrow(random), nlevels(provinces)), ]
  }

  # Step 4: Estimate p-values based on random realizations
  pdf1 <- random1
  pval <- IVo
  pb <- txtProgressBar(min = 0, max = length(pdf1), style = 3, width = 50, char = "\u27a4")
  for (i in 1:length(pdf1)) {
    pdf1[[i]] <- vector("list", ncol(x))
    names(pdf1[[i]]) <- colnames(random1[[i]])
    for (j in 1:length(pdf1[[i]])) {
      pdf1[[i]][[j]] <- density(random1[[i]][, j])
      b <- pdf1[[i]][[j]]$x[2] - pdf1[[i]][[j]]$x[1]
      pval[i, j] <- sum(b * (pdf1[[i]][[j]]$y[which(pdf1[[i]][[j]]$x > IVo[i, j])]))
    }

    # Save density plots to a PDF file for each province
    pdf(file = paste0(names(pdf1)[i], ".pdf"))
    par(mfrow = c(3, 3))
    for (j in 1:length(pdf1[[i]])) {
      plot(
        pdf1[[i]][[j]], sub = paste("p-val =", round(pval[i, j], 2)),
        main = names(pdf1[[i]])[j], xlim = range(c(IVo[i, j], pdf1[[i]][[j]]$x))
      )
      abline(v = IVo[i, j], col = "red")
    }
    dev.off()
    setTxtProgressBar(pb, i)
  }
  close(pb)

  # Step 5: Identify significant taxa per province
  ind_taxa <- apply(pval, 1, function(x) names(which(x < alpha)))

  # Return results as a list
  return(list(
    indval_observed = IVo,
    p_values = pval,
    density_plots = pdf1,
    significant_taxa = ind_taxa
  ))
}
