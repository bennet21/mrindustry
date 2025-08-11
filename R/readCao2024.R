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
  normalize <- FALSE
  labels <- NULL
  key <- NULL
  fraction_mean <- FALSE

  if (subtype == "cement_use_share") {
    long_names <- c(
      "cement for concrete (distribution function)",
      "cement for mortar (distribution)"
    )
    labels <- c("concrete", "mortar")
    key <- "end_use_product"
    normalize = TRUE
  } else if (subtype == "concrete_strength_class_split") {
    long_names <- c(
      "distribution of concrete by strength class ≤C15 (distribution)",
      "distribution of concrete by strength class C16-C23 (distribution)",
      "distribution of concrete by strength class C23-C35 (distribution)",
      "distribution of concrete by strength class >C35 (distribution)"
    )
    labels <- c("C15","C20","C30","C35")
    key <- "concrete_strength_class"
    normalize = TRUE
  } else if (subtype == "carbonation_rate") {
    long_names <- c(
      "compressive strength class and exposure conditions (βc sec) ≤C15 (distribution)",
      "compressive strength class and exposure conditions (βc sec) C16-C23 (distribution)",
      "compressive strength class and exposure conditions (βc sec) C23-C35 (distribution)",
      "compressive strength class and exposure conditions (βc sec) >C35 (distribution)"
    )
    labels <- c("C15","C20","C30","C35")
    key <- "concrete_strength_class"
  } else if (subtype == "carbonation_rate_factor_additives") {
    long_names <- c("cement additives (βad) (distribution)")
  } else if (subtype == "carbonation_rate_factor_co2") {
    long_names <- c("CO2 concentration (βCO2) (distribution)")
  } else if (subtype == "carbonation_rate_factor_coating") {
    long_names <- c("coating and cover (βCC) (distribution)")
  } else if (subtype == "concrete_thickness") {
    long_names <- c("wall thickness (distribution)")
    fraction_mean <- TRUE
  } else if (subtype == "cement_content") {
    long_names <- c(
      "cement content of concrete in different strength classes (kg cement/m3) (Ci) ≤C15 (distribution)",
      "cement content of concrete in different strength classes (kg cement/m3) (Ci) C16-C23 (distribution)",
      "cement content of concrete in different strength classes (kg cement/m3) (Ci) C23-C35 (distribution)",
      "cement content of concrete in different strength classes (kg cement/m3) (Ci) >C35 (distribution)"
    )
    labels <- c("C15","C20","C30","C35")
    key <- "concrete_strength_class"
  } else if (subtype == "mortar_use_split") {
    long_names <- c(
      "percentage of mortar cement used for rendering, palstering and decorating (distribution)",
      "percentage of mortar cement used for masonry (distribution)",
      "percentage of mortar cement used for maintenance and repairing (distribution)"
    )
    labels <- c("finishing", "masonry", "M&R")
    key <- "mortar_use_type"
  } else if (subtype == "mortar_thickness") {
    long_names <- c(
      "thickness of mortar cement used for rendering, palstering and decorating (distribution)",
      "thickness of mortar cement used for masonry (distribution)",
      "thickness of mortar cement used for maintenance and repairing (distribution)"
    )
    labels <- c("finishing", "masonry", "M&R")
    key <- "mortar_use_type"
  } else if (subtype == "cement_loss_construction") {
    long_names <- c("cement wasted during construction (distribution)")
  } else if (subtype == "clinker_loss_production") {
    long_names <- c("CKD generation rate of clinker (distribution)")
  } else if (subtype == "cao_carbonation_ratio") {
    long_names <- c(
      "proportion of CaO within fully carbonated cement that converts to CaCO3 for concrete cement (γ) (distribution)",
      "proportion of CaO within fully carbonated cement that converts to CaCO3 for mortar cement (γ1) (distribution)"

    )
    labels <- c("concrete", "mortar")
    key <- "end_use_product"
  } else {
    long_names <- c(
      "CKD generation rate of clinker (distribution)"
    )

    long_names <- c(
      "carbonation rate coefficient of cement mortar (km) (distribution)"
    )

    long_names <- c(
      "cement wasted during construction (distribution)"
    )

    long_names <- c(
      "average CaO content of clinker in cement (fCaO) (distribution)"
    )

    long_names <- c(
      "ratio of CO2 element to CaO (Mr)"
    )

    long_names <- c(
      "fate of waste concrete (for new concrete)",
      "fate of waste concrete (for road base, backfills materials and other use)",
      "fate of waste concrete (landfill, dumped and stacking)",
      "fate of waste concrete (asphalt concrete)"
    )

    long_names <- c(
      "percentage of waste concrete (for new concrete) with particle size <5mm (min)",
      "percentage of waste concrete (for new concrete) with particle size 5-10mm (min)",
      "percentage of waste concrete (for new concrete) with particle size 10-20mm (min)",
      "percentage of waste concrete (for new concrete) with particle size 20-40mm (min)"
    )

    long_names <- c(
      "percentage of waste concrete (for road base, backfills materials and other use) with particle size <1mm (min)",
      "percentage of waste concrete (for road base, backfills materials and other use) with particle size 1-10mm (min)",
      "percentage of waste concrete (for road base, backfills materials and other use) with particle size 10-30mm (min)",
      "percentage of waste concrete (for road base, backfills materials and other use) with particle size 30-53mm (min)"
    )

    long_names <- c(
      "percentage of waste concrete (landfill, dumped and stacking) with particle size <10mm (min)",
      "percentage of waste concrete (landfill, dumped and stacking) with particle size 10-30mm (min)",
      "percentage of waste concrete (landfill, dumped and stacking) with particle size 30-50mm (min)",
      "percentage of waste concrete (landfill, dumped and stacking) with particle size >50mm (min)"
    )

    long_names <-c(
      "percentage of waste concrete (asphalt concrete) with particle size <5mm (min)",
      "percentage of waste concrete (asphalt concrete) with particle size 5-10mm (min)",
      "percentage of waste concrete (asphalt concrete) with particle size 10-20mm (min)",
      "percentage of waste concrete (asphalt concrete) with particle size 20-40mm (min)"
    )

    long_names <-c(
      "carbonation rate coefficient of buried concrete in strength class i (kli) ≤C15",
      "carbonation rate coefficient of buried concrete in strength class i (kli) C16-C23",
      "carbonation rate coefficient of buried concrete in strength class i (kli) C23-C35",
      "carbonation rate coefficient of buried concrete in strength class i (kli) >C35"
    )

    long_names <- c(
      "CKD generation rate of clinker (distribution)"
    )

    long_names <- c(
      "percentage of CKD sent to landfill (distribution)"
    )

    long_names <- c(
      "CaO content in CKD (distribution)"
    )

  }

  x <- calculate_means(data, long_names, labels, key, normalize = normalize, fmean = TRUE)
  x <- as.magpie(x, spatial = 1)
}

#' Calculate the data mean, do normalization check and reshape the data.
#'
#' @author Bennet Weiss
#' @param data original input from dataframe.
#' @param long_names long label of the first column of a variable.
#' @param labels names of the different categories of a key (dimension).
#' @param key name of the dimension.
#' @param normalize Bool if normalization step should be performed
#' @param tol Tolerance for check of normalization within rounding errors. Only performed if normalize = TRUE.
calculate_means <- function(data, long_names, labels = NULL, key = NULL, normalize = FALSE, tol = 3e-2, fmean=FALSE) {
  if (!is.null(labels) && length(labels) != length(long_names)) {
    stop("labels must have the same length as long_names")
  }

  # Collect means (assumes calculate_mean(data, name) returns a numeric vector)
  cols <- lapply(long_names, function(nm) as.numeric(calculate_mean(data, nm, fmean = fmean)))
  X <- as.data.frame(cols, check.names = FALSE)
  names(X) <- c(labels)

  # Normalize rows (exclude region)
  if (normalize) {
    rs <- rowSums(X) # calculate sum for each row
    X <- sweep(X, 1, rs, "/") # divide each value by the row sum

    err <- abs(rowSums(X) - 1)
    if (any(err > tol, na.rm = TRUE)) warning("Some rows deviate from 1 by more than tol.")
  }

  # add region as first column
  X <- cbind(Region = data[[1]], X)

  # Wide -> long
  if (!is.null(labels)) {
    out <- reshape(X,
                   varying = labels, v.names = "value",
                   timevar = key, times = labels,
                   idvar = "Region", direction = "long")
    rownames(out) <- NULL
    out <- out[, c("Region", key, "value")]
  } else out <- X

  return(out)

}

#' Function to calculate the mean by extracting the distribution function.
#'
#' @param data original input from dataframe.
#' @param start_column_name label of first column of variable.
calculate_mean <- function(data, start_column_name, fmean = FALSE) {

  if (!fmean){
    mean_functions <- list(
      "Weibull" = mean_trunc_weibull,
      "Uniform" = mean_uniform
    )
  } else {
    mean_functions <- list(
      "Weibull" = fmean_trunc_weibull,
      "Uniform" = fmean_uniform
    )
  }

  distribution_parameter_number <- c(
    "Weibull" = 4,
    "Uniform" = 2
  )

  start_idx <- which(names(data) == start_column_name)
  distribution <-  data[[1, start_idx]]
  mean_function <- mean_functions[[distribution]]
  n_params <- distribution_parameter_number[[distribution]]

  parameters <- data[(start_idx + 1):(start_idx + n_params)]

  return(mean_function(parameters))
}

#' Calculate the mean based on parameters from a truncated Weibull distribution.
#'
#' @param parmaters Array of the four parameters (scale, shape, min, max) of a Weibull distribution
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

#' Calculate the mean based on parameters from a uniform distribution.
#'
#' @param parameters Array of the two parameters (min, max) of a uniform distribution
mean_uniform <- function(parameters) {
  parameters <- as.data.frame(parameters, optional = TRUE)

  if (!is.data.frame(parameters) || ncol(parameters) != 2L)
    stop("parameters must be a data.frame/matrix (2 columns) or a length-2 vector/list.")

  b <- as.numeric(parameters[[1]])
  a <- as.numeric(parameters[[2]])

  # Validate per-row
  bad <- !is.finite(a) | !is.finite(b) | !(b > a)
  if (any(bad)) {
    stop(sprintf("Invalid parameter rows: %s", paste(which(bad), collapse = ", ")))
  }

  return((a + b) / 2)
}

#' Conditional harmonic mean (1 / E[1/X]) for truncated Weibull on [a,b].
#' Parameters: (scale, shape, min, max). Requires (shape>1 or a>0). Diverges if a=0 & shape<=1.
#' Returns 1 / E[1/X | a≤X≤b].
mean_trunc_weibull <- function(parameters) {
  parameters <- as.data.frame(parameters, optional = TRUE)
  if (!is.data.frame(parameters) || ncol(parameters) != 4L)
    stop("parameters must be a data.frame/matrix (4 columns) or a length-4 vector/list.")
  scale <- as.numeric(parameters[[1]])
  shape <- as.numeric(parameters[[2]])
  a <- as.numeric(parameters[[3]])
  b <- as.numeric(parameters[[4]])
  bad <- !is.finite(shape) | !is.finite(scale) | shape <= 0 | scale <= 0 |
    !is.finite(a) | !is.finite(b) | a < 0 | !(b > a)
  if (any(bad)) stop(sprintf("Invalid parameter rows: %s", paste(which(bad), collapse=", ")))

  out <- numeric(length(shape))
  for (i in seq_along(shape)) {
    k <- shape[i]; lambda <- scale[i]; ai <- a[i]; bi <- b[i]
    ua <- (ai / lambda)^k; ub <- (bi / lambda)^k
    den <- exp(-ua) - exp(-ub)              # F(b)-F(a)
    if (ai == 0 && k <= 1)
      stop(sprintf("Row %d: E[1/X] diverges (a=0, shape<=1).", i))

    # Compute numerator of E[1/X] (call it numEInv); then harmonic mean = den / numEInv.
    if (k > 1) {
      s <- 1 - 1 / k
      numEInv <- (1 / lambda) * gamma(s) *
        (pgamma(ub, shape = s, rate = 1) - pgamma(ua, shape = s, rate = 1))
    } else {
      f_rec <- function(x) (k / lambda) * (x / lambda)^(k - 1) * exp(-(x / lambda)^k) / x
      numEInv <- integrate(f_rec, lower = ai, upper = bi, rel.tol = 1e-8)$value
    }
    out[i] <- den / numEInv   # 1 / (numEInv / den)
  }
  out
}

#' Conditional harmonic mean (1 / E[1/X]) for Uniform(a,b).
#' Parameters layout kept: col1 = b (max), col2 = a (min). Requires 0 < a < b.
#' Returns (b - a)/(log b - log a).
fmean_uniform <- function(parameters) {
  parameters <- as.data.frame(parameters, optional = TRUE)
  if (!is.data.frame(parameters) || ncol(parameters) != 2L)
    stop("parameters must be a data.frame/matrix (2 columns) or a length-2 vector/list.")
  b <- as.numeric(parameters[[1]])
  a <- as.numeric(parameters[[2]])
  bad <- !is.finite(a) | !is.finite(b) | !(b > a) | a <= 0
  if (any(bad)) stop(sprintf("Invalid parameter rows (need 0 < a < b): %s", paste(which(bad), collapse=", ")))
  (b - a) / (log(b) - log(a))
}

