#' @author Bennet Weiss
calcClinkerRatio <- function() {
  x <- readSource("GNRCement", subtype = "production")
  x <- setNames(x[, , "clinker_production"] / x[, , "cement_production"], "clinker_ratio")
  return(list(x = x, weight = NULL, unit = "ratio", description = "dummytext"))
}
