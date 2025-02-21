#' Read GCCA data from GNR project 
#' Getting the Numbers Right (GNR) is a Project of the Global Cement and Concrete Association (GCCA)
#' [GNR](https://gccassociation.org/sustainability-innovation/gnr-gcca-in-numbers/)
#' Data not publically available. Data received from Abhishek Shukla, Sustainability Program Manager at GCCA.
#' Data received on 21.01.2025
#' 
#' @author Bennet Weiss
#' @param subtype Currently only clinker_production is supported. Later, ...
#'
readGNRCement <- function(subtype) {
  sheets <- c(
    "CEMBUREAU members",
    "FICEM members",
    "EU 27",
    "South-Latin America",
    "World",
    "Austria",
    "Brazil",
    "Canada",
    "Czech Republic",
    "Egypt",
    "France",
    "Germany",
    "India",
    "Italy",
    "Morocco + Algeria + Tunisia",
    "Northeast Asia",
    "Philippines",
    "Poland",
    "Spain",
    "Thailand",
    "United Kingdom",
    "United States"
  )
  # TODO maybe I could use factor(range_8TG_clinker) here to save memory...
  range8TGClinker <- c(
    "A11:C31",
    "A11:C31",
    "A11:C31",
    "A11:C31",
    "A10:C30",
    "A10:C30",
    "A11:C31",
    "A11:C31",
    "A10:C30",
    "A10:C30",
    "A11:C31",
    "A13:C33",
    "A11:C31",
    "A10:C30",
    "A11:C31",
    "A10:C30",
    "A9:C29",
    "A11:C31",
    "A11:C31",
    "A11:C31",
    "A11:C31",
    "A11:C31"
  )
  # problem
  range8TGKClinker <- c(
    "A41:D159",
    "A38:D123",
    "A38:D156",
    "A40:D125",
    "A37:D157",
    "A38:D66",
    "A39:D84",
    "A40:D72",
    "A39:D50",
    "A39:D58",
    "A40:D100",
    "A42:D86",
    "A40:D60",
    "A39:D84",
    "A39:D62",
    "A37:D57",
    "A38:D76",
    "A37:D85",
    "A38:D57",
    "A38:D66",
    "A38:D125"
  )
  # problem
  range21TGWcmCement <- c(
    "A167:C187",
    "A130:C150",
    "A166:C186",
    "A134:C154",
    "A164:C184",
    "A75:C95",
    "A93:C113",
    "A80:C100",
    "A59:C79",
    "A67:C87",
    "A109:C129",
    "A95:C115",
    "A69:C89",
    "A93:C113",
    "A82:C102",
    "A71:C91",
    "A37:C57",
    "A83:C103",
    "A92:C112",
    "A64:C84",
    "A73:C93"
  )
  range21TGWctCementious <- c(
    "A197:C217",
    "A156:C176",
    "A192:C112",
    "A160:C180",
    "A190:C210",
    "A101:C121",
    "A119:C139",
    "A106:C126",
    "A159:C179",
    "A93:C113",
    "A135:C155",
    "A121:C141",
    "A95:C115",
    "A119:C139",
    "A108:C128",
    "A97:C117",
    "A93:C113",
    "A109:C129",
    "A118:C138",
    "A90:C110",
    "A99:C119",
    "A158:C178"
  )
  range21TGWceCement <- c(
    "A228:C239",
    "A188:C199",
    "A223:C234",
    "A192:C203",
    "A217:C228",
    "A133:C244",
    "A151:C163",
    "A106:C126",
    "A117:C128",
    "A124:C135",
    "A167:C178",
    "A153:C164",
    "A127:C138",
    "A151:C162",
    "A140:C151",
    "A129:C140",
    "A120:C113",
    "A138:C149",
    "A150:C161",
    "A122:C133",
    "A131:C142",
    "A190:C201"
  )

  if (subtype == "production") {
    # setwd("C:\\Users\\bennetwe\\Documents\\Code\\mrindustry")
    setwd("C:\\Users\\bennetwe\\Documents\\Code\\madrat_wd\\sources\\GNRCement")
    data <- NULL
    # loop over all excel sheets
    for (i in seq_along(sheets)) {
      clinkerI <- readxl::read_xlsx(
        file.path("v1", "Postdam_GCCA_GNR_2022.xlsx"),
        sheet = sheets[i],
        range = range8TGClinker[i]
      )
      data <- rbind(data, clinkerI)
    }
    clinker1 <- readxl::read_xlsx(file.path("v1", "Postdam_GCCA_GNR_2022.xlsx"), sheet = "World", range = "A10:C30")
    clinker2 <- readxl::read_xlsx(file.path("v1", "Postdam_GCCA_GNR_2022.xlsx"), sheet = "Austria", , range = "A10:C30")
    clinker3 <- readxl::read_xlsx(file.path("v1", "Postdam_GCCA_GNR_2022.xlsx"), sheet = "Brazil", , range = "A11:C31")
    clinker <- rbind(clinker1, clinker2, clinker3)
    clinker$variable <- "clinker_production"
    cement1 <- readxl::read_xlsx(file.path("v1", "Postdam_GCCA_GNR_2022.xlsx"), sheet = "World", , range = "A164:C184")
    cement2 <- readxl::read_xlsx(file.path("v1", "Postdam_GCCA_GNR_2022.xlsx"), sheet = "Austria", , range = "A75:C95")
    cement3 <- readxl::read_xlsx(file.path("v1", "Postdam_GCCA_GNR_2022.xlsx"), sheet = "Brazil", , range = "A93:C113")
    cement <- rbind(cement1, cement2, cement3)
    cement$variable <- "cement_production"
    data <- rbind(clinker, cement)
    data <- data[,c(1, 2, 4, 3)]
    x <- as.magpie(data, spatial = 1)
    return(x)
  } else {
    stop("Invalid subtype. Valid subtypes are: production")
  }
}
