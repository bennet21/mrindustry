#' Calculate carbonation rate of different strength concrete.
#'
#' @author Bennet Weiss
calcCarbonationRate <- function(subtype = "base"){

  full_name <- list(
    base      = "carbonation_rate",
    additives = "carbonation_rate_factor_additives",
    co2       = "carbonation_rate_factor_co2",
    coating   = "carbonation_rate_factor_coating"
  )

  x <- readSource("Cao2024", subtype = full_name[[subtype]])

  # create new magpie object and fill with ones
  weight <- new.magpie(cells_and_regions = NULL)
  weight <- toolCountryFill(weight, fill = 1, verbosity = 2)

  unit <- "mm/sqrt(a)"
  description <- paste(
    "Carbonation rate ", subtype, " of concrete of different strength classes.",
    "Data from Cao2024."
  )
  output <- list(x = x, weight = weight, unit = unit, description = description)
}
