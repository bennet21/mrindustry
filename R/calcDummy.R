#' @author Bennet Weiss
calcDummy <- function() {
  x <- readSource("GNRCement", subtype = "production")
  x[is.na(x)] <- 0
  x <- setNames(x[, , "clinker_production"] + x[, , "cement_production"], "sum")
  return(list(x = x, weight = NULL, unit = "ratio", description = "dummytext"))
}
