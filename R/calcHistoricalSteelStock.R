#' Calculate Steel Stock from Mueller steel stock per capita and WDI population
#'
#' @author Falk Benke
calcHistoricalSteelStock <- function() {
  steel_stock_per_capita <- readSource("Mueller", "stocks", convert = TRUE)[, , "Steel stock per-capita|med (t)"]
  population <- calcOutput("PopulationPast", pastData = "UN_PopDiv", aggregate = FALSE)

  y <- intersect(getItems(steel_stock_per_capita, 2), getItems(population, 2))

  stock <- steel_stock_per_capita[, y, ] * population[, y, ]
  getNames(stock) <- c("Steel stock (million t)")

  list(
    x = stock,
    weight = NULL,
    unit = "million t",
    description = "Historical steel stock based on Mueller's medium stock per capita numbers and WDI population"
  )
}
