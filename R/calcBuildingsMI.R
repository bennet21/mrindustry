#' Calculates global cement production as from Andrew's 2019 paper.
#' @author Bennet Weiss
#' @param subtype Material subtype. Possible values are "concrete", "steel", "plastics".
calcBuildingsMI <- function(subtype = "concrete") {
    x <- readSource("RASMI", subtype)
    x["USA",,]

    # use floor area for weight
    floor_area <- calcOutput()
}
