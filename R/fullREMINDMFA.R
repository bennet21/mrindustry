#' Function that produces the complete regional data set required for the REMIND-MFA model.
#' 
#' @author Bennet Weiss
fullREMINDMFA <- function() {

    past_years <- 1900:2023

    # Steel

    # Cement
    calcOutput("ClinkerRatio", file = "clinker_ratio.cs3r", years=past_years)

    # Plastics
}