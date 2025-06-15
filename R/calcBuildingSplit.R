#' Calculates relative floor area of SF, MF and NR buildings by structure type.
#'
#' @author Bennet Weiss
calcBuildingSplit <- function() {
  data <- readSource("GEM")

  # transform to df for easier handling
  df <- as.data.frame(data, rev = 3)
  # remove Ind buildings
  df <- df[df$Stock_Type != "Ind", ]
  floorArea <- as.magpie(df, spatial = 1)

  # calc relative floor area
  df_byStockType <- group_by(df, ISO3, Stock_Type) %>%
    summarise(TOTALAREA_SQM = sum(.value, na.rm = TRUE), .groups = "drop")
  floorArea_byStockType <- as.magpie(df_byStockType, spatial = 1)
  #df_ByFunction <- group_by(df, ISO3, Function) %>%
   # summarise(TOTALAREA_SQM = sum(.value, na.rm = TRUE), .groups = "drop")
  #floorArea_byFunction <- as.magpie(df_ByFunction, spatial = 1)
  # TODO double check if it's really that simple (the values seem to be correct)
  relFloorArea <- floorArea / floorArea_byStockType

  # output
  floorArea[is.na(floorArea)] <- 0
  relFloorArea[is.na(relFloorArea)] <- 0
  floorArea_byStockType[is.na(floorArea_byStockType)] <- 0
  #floorArea_byFunction[is.na(floorArea_byFunction)] <- 0
  weight <- floorArea_byStockType # use normalizing floor area as weight
  unit <- "ratio"
  description <- paste(
    "Relative floor area of single-family (SF), multi-family (MF) and non-residential (NR) buildings by structure type.",
    "Calculated as (floor area of SF/MF/NR)/(total floor area of RES/NONRES in the same country).",
    "Data from GEM, aggregated by stock type and structure type."
  )
  output <- list(
    x = relFloorArea,
    weight = weight,
    unit = unit,
    description = description
  )
}
