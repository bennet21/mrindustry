#' Calculate what size distribution concrete/mortar waste has.
#'
#' @author Bennet Weiss
calcWasteSizeSplit <- function(){

  x <- readSource("Cao2024", subtype = "waste_size_split")

  # create new magpie object and fill with ones
  weight <- new.magpie(cells_and_regions = NULL)
  weight <- toolCountryFill(weight, fill = 1, verbosity = 2)

  unit <- "ratio"
  description <- paste(
    "Concrete/mortar waste particle size split.",
    "Data from Cao2024."
  )
  output <- list(x = x, weight = weight, unit = unit, description = description)
}
