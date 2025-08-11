#' Calculate strength class distribution of concrete.
#'
#' @author Bennet Weiss
calcCementRatio <- function(subtype){
  x <- readSource("Cao2024", subtype = "cement_content")

  # create new magpie object and fill with ones
  weight <- new.magpie(cells_and_regions = NULL)
  weight <- toolCountryFill(weight, fill = 1, verbosity = 2)

  unit <- "ratio"
  description <- paste(
    "Concrete cement content.",
    "Data from Cao2024."
  )
  output <- list(x = x, weight = weight, unit = unit, description = description)
}
