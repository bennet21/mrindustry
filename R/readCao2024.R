#' Read data received on 04.08.2025, personal communication.
#' Data used for Kaufmann et al. (2024), DOI: 10.1088/1748-9326/ad236b
#' "Society’s material stocks as carbon pool: an economy-wide quantification of global carbon stocks from 1900–2015"
#' The data is given as probability density functions.
#' For further use, they are translated to means before turned to magclass object.
#'
#' @author Bennet Weiss
#' @param subtype Variable to be read in.
readCao2024 <- function(subtype) {
  path <- file.path("v1", "data_cement_GAS_EoL_MISO_9regions.xlsx")
  data <- suppressMessages(readxl::read_xlsx(path, sheet = "Uptake"))
  data <- head(data, 11) # remove rows after row 11
  data <- data[-1, , drop = FALSE] # remove row 2
  normalize = FALSE

  if (subtype == "cement_use_share") {
    long_names <- c(
      "cement for concrete (distribution function)",
      "cement for mortar (distribution)"
    )
    labels <- c("concrete", "mortar")
    key <- "end_use_product"
    normalize = TRUE
  }

  if (subtype == "concrete_strength_class_split") {
    long_names <- c(
      "distribution of concrete by strength class ≤C15 (distribution)",
      "distribution of concrete by strength class C16-C23 (distribution)",
      "distribution of concrete by strength class C23-C35 (distribution)",
      "distribution of concrete by strength class >C35 (distribution)"
    )
    labels <- c("C15","C20","C30","C35")
    key <- "mortar_strength_category"
    normalize = TRUE
  }

  x <- calculate_means(data, long_names, labels, key, normalize = TRUE)
  x <- as.magpie(x, spatial = 1)
}

calculate_means <- function(data, long_names, labels, key, normalize = FALSE, tol = 3e-2) {
  if (length(labels) != length(long_names))
    stop("labels must have the same length as long_names")

  # Collect means (assumes calculate_mean(data, name) returns a numeric vector)
  cols <- lapply(long_names, function(nm) as.numeric(calculate_mean(data, nm)))
  X <- as.data.frame(cols, check.names = FALSE)
  names(X) <- labels

  # Normalize rows (exclude region)
  if (normalize) {
    rs <- rowSums(X) # calculate sum for each row
    X <- sweep(X, 1, rs, "/") # divide each value by the row sum

    err <- abs(rowSums(X) - 1)
    if (any(err > tol, na.rm = TRUE)) warning("Some rows deviate from 1 by more than tol.")
  }

  X$region <- data[[1]]

  # Wide -> long (base R)
  out <- reshape(X,
                 varying = labels, v.names = "value",
                 timevar = key, times = labels,
                 idvar = "region", direction = "long")
  rownames(out) <- NULL
  out <- out[, c("region", key, "value")]
  return(out)
}

calculate_mean <- function(data, start_column_name) {

  mean_functions <- list(
    "Weibull" = mean_trunc_weibull
  )

  distribution_parameter_number <- c(
    "Weibull" = 4
  )

  start_idx <- which(names(data) == start_column_name)
  distribution <-  data[[1, start_idx]]
  mean_function <- mean_functions[[distribution]]
  n_params <- distribution_parameter_number[[distribution]]

  parameters <- data[(start_idx + 1):(start_idx + n_params)]

  return(mean_function(parameters))
}

mean_trunc_weibull <- function(parameters) {
  parameters <- as.data.frame(parameters, optional = TRUE)

  if (!is.data.frame(parameters) || ncol(parameters) != 4L)
    stop("parameters must be a data.frame/matrix (4 columns) or a length-4 vector/list.")

  scale <- as.numeric(parameters[[1]])
  shape <- as.numeric(parameters[[2]])
  a <- as.numeric(parameters[[3]])
  b <- as.numeric(parameters[[4]])

  # Validate per-row
  bad <- !is.finite(shape) | !is.finite(scale) | shape <= 0 | scale <= 0 |
    !is.finite(a) | !is.finite(b) | a < 0 | !(b > a)
  if (any(bad)) {
    stop(sprintf("Invalid parameter rows: %s", paste(which(bad), collapse = ", ")))
  }

  k <- shape
  lambda <- scale
  ua <- (a / lambda)^k
  ub <- (b / lambda)^k
  s <- 1 + 1 / k

  # numerator: λ [γ(s, ub) - γ(s, ua)]  where γ is lower incomplete gamma
  num <- lambda * gamma(s) * (pgamma(ub, shape = s, rate = 1) - pgamma(ua, shape = s, rate = 1))
  # denominator: F(b) - F(a) = exp(-ua) - exp(-ub)
  den <- exp(-ua) - exp(-ub)
  return(num / den)
}
