#' Calculate end use product share of cement.
#'
#' @author Bennet Weiss
calcCaoCarbonationShare <- function(){
  x <- readSource("Cao2024", subtype = "cao_carbonation_ratio")

  # create new magpie object and fill with ones
  weight <- new.magpie(cells_and_regions = NULL)
  weight <- toolCountryFill(weight, fill = 1, verbosity = 2)

  unit <- "ratio"
  description <- paste(
    "Share of CaO in End-Use Product available for carbonation.",
    "Data from Cao2024."
  )
  output <- list(x = x, weight = weight, unit = unit, description = description)
}
