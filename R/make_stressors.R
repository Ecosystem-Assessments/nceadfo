#' Prepare stressors data 
#'
#' @export
make_stressors <- function() {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Load & prepare data
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  uid <- c("3992e1a6", "6dba9a9f", "e2b7e6c4", "72312316", "16c0d3ad", "99bb2d51")
  dat <- pipedat::importdat(uid)
  nm <- data.frame(datname = names(dat))
  stringr::str_split(nm$datname, "-")
  
  loc <- function(dat, field, years = NULL) {
    nm <- names(dat)
    field <- stringr::str_detect(nm, field) |> which()
    years <- lapply(years, function(x) stringr::str_detect(nm, pattern = as.character(x))) |>
             lapply(which) |>
             unlist()
    years[years %in% field]
    dat[stringr::str_detect(names(dat), pattern)]
  }
  # ------------------------------
  # Climate stressors 
  ## Sea surface temperature anomalies
  sst <- importdat("3992e1a6")
  sst_pos <- loc(sst, "positive")
  sst_neg <- loc(sst, "negative")
  
  ## Sea bottom temperature anomalies 
  sbt <- importdat("6dba9a9f")
  
  # ------------------------------
  # Fisheries stressors 
  fish <- importdat("e2b7e6c4")

  ## DD
  dd <- loc(fish, "DD")
  
  ## DNH
  dnh <- loc(fish, "DNH")
  
  ## DNL
  dnl <- loc(fish, "DNL")
  
  ## PLB
  plb <- loc(fish, "PLB")
  
  ## PHB
  phb <- loc(fish, "PHB")

  # ------------------------------
  # Shipping 
  ## Traffic 
  ship <- importdat("72312316")
  
  ## Marine pollution 
  # marpol <- 
  
  ## Invasive species 
  inv <- importdat("84b6ea0b")
  
  
  # ------------------------------
  # Land-based
  halp <- importdat("16c0d3ad")
  
  ## Inorganic pollution
  inorg <- loc(halp, "inorganic")
  
  ## Nutrient import
  nut <- loc(halp, "plumes_fert")
  
  ## Organic pollution
  org <- loc(halp, "plumes_pest")
  
  ## Coastal development
  cd <- importdat("aba5e90a")
  
  ## Direct human impact
  dhi <- importdat("99bb2d51")
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Divide into periods
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Creating four periods for the assessment
  period_2010_2012 <- list()
  period_2013_2015 <- list()
  period_2016_2018 <- list()
  period_2019_2021 <- list()
    
  
  
  sst_pos <- list(
    sst_pos_2010_2012 = sst_pos[]
)  
  
}