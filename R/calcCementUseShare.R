#' Calculate end use product share of cement
#'
#' @author Bennet Weiss
calcCementUseShare <- function(){
  x <- readSource("Cao2024", subtype = "cement_use_share")

  weight <- NULL
  unit <- "ratio"
  description <- paste(
    "Share of cement that is used for the end-use categories concrete and mortar.",
    "Data from Cao2024."
  )
  output <- list(x = x, weight = weight, unit = unit, description = description)
}
