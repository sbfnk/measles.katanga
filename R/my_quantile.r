##' Quantile function that works with NA
##'
##' @return quantiles
##' @inheritParams stats::quantile
##' @author Sebastian Funk
##' @keywords internal
my_quantile <- function(x, probs, ...) {
  if (any(is.na(x))) {
    vec <- rep(NA_real_, length(probs))
    names(vec) <- paste0(probs*100, "%")
    return(vec)
  } else {
    return(quantile(x, probs, ...))
  }
}

