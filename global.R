library(shiny)
library(faosws)
library(faoswsUtil)
library(ggplot2)
library(data.table)
library(faoswsFlag)
library(rhandsontable)

SERVER <- "https://sws.fao.org:443"

if ("HOSTNAME" %in% names(Sys.getenv()) && Sys.getenv()[['HOSTNAME']] == "hqlprsws1.hq.un.fao.org") {
  CONFIG_SETTINGS <- NA
  CONFIG_CERTIFICATES <- "/srv/shiny-server/PRODvalidation/files/certificates/production"
  LOCAL_PROD_FILES <- "/work/SWS_R_Share/PRODvalidation/data/production"
  LOCAL_TRADE_FILES <- "/work/SWS_R_Share/PRODvalidation/data/trade"
  ref_cpc <- readRDS('/srv/shiny-server/PRODvalidation/files/measuredItemCPC.rds')
  ref_m49 <- readRDS('/srv/shiny-server/PRODvalidation/files/geographicAreaM49.rds')
  SESSIONS_DIR <- '/work/SWS_R_Share/PRODvalidation/sessions'
  HELP_FILE <- "/srv/shiny-server/PRODvalidation/files/help.Rmd"
  REPORT_TEMPLATE <- "/srv/shiny-server/PRODvalidation/files/report.Rmd"
  REMOTE <- TRUE
  REFERENCE_ITEMS_CROP <- "/srv/shiny-server/PRODvalidation/files/reference/items_crop.txt"
} else {
  CONFIG_SETTINGS <- NA
  CONFIG_CERTIFICATES <- "C:/Users/mongeau.FAODOMAIN/Documents/certificates/production"
  LOCAL_PROD_FILES <- "C:/Users/mongeau.FAODOMAIN/Dropbox/FAO/FBS/production_outliers/data/production"
  LOCAL_TRADE_FILES <- "C:/Users/mongeau.FAODOMAIN/Dropbox/FAO/FBS/production_outliers/data/trade"
  ref_cpc <- readRDS('c:/Users/mongeau.FAODOMAIN/Dropbox/GitHub/SWS-Methodology/PRODvalidation/files/measuredItemCPC.rds')
  ref_m49 <- readRDS('c:/Users/mongeau.FAODOMAIN/Dropbox/GitHub/SWS-Methodology/PRODvalidation/files/geographicAreaM49.rds')
  SESSIONS_DIR <- 'r:/PRODvalidation/sessions'
  HELP_FILE <- "c:/Users/mongeau.FAODOMAIN/Dropbox/GitHub/SWS-Methodology/PRODvalidation/files/help.Rmd"
  REPORT_TEMPLATE <- "c:/Users/mongeau.FAODOMAIN/Dropbox/GitHub/SWS-Methodology/PRODvalidation/files/report.Rmd"
  REMOTE <- FALSE
  REFERENCE_ITEMS_CROP <- "c:/Users/mongeau.FAODOMAIN/Dropbox/GitHub/SWS-Methodology/PRODvalidation/files/reference/items_crop.txt"
}

REFERENCE_ITEMS_CROP <- read.delim(REFERENCE_ITEMS_CROP, header = FALSE, colClasses = "character")

ref_cpc[, id := paste(code, "-", description)]
ref_m49[, id := paste(code, "-", description)]

ref_cpc <- ref_cpc[code %in% REFERENCE_ITEMS_CROP[, 1]]

DIM_TIME <- as.character(2014:2018)

# 5510 = production [t]
# 5312 = area harvested [ha]
# 5421 = yield [t/ha]

DIM_ELEM_PROD <- c("5510", "5312", "5421") # 
# XXX; heads / 1000 heads
DIM_ELEM_TRADE <- c("5610", "5910")

THRESHOLD_PA <- 0.7
THRESHOLD_Y <- 0.3


compute_remaining_element <- function(x) {

  # NOTE: `Protected` should not have NAs

  res <- rep(NA_real_, 3)

  v_yield <- x[measuredElement == "5421", Value]
  v_area <- x[measuredElement == "5312", Value]
  v_prod <- x[measuredElement == "5510", Value]

  p_yield <- x[measuredElement == "5421", Protected]
  p_prod <- x[measuredElement == "5510", Protected]
  p_area <- x[measuredElement == "5312", Protected]

  if (sum(p_prod, p_area, p_yield) == 3) {
    return(res)
  } else if (sum(p_prod, p_area, p_yield) == 2) {
    non_protected <- x[Protected == FALSE, measuredElement]
    if (length(non_protected) == 0) {
      stop("Non explicit non-protected figures")
    } else {
      if (non_protected == "5421") {
        if (!is.na(v_area) && v_area > 0 && !is.na(v_prod)) {
          res[x$measuredElement == "5421"] <- v_prod / v_area
        }
      } else if (non_protected == "5312") {
        if (!is.na(v_yield) && v_yield > 0 && !is.na(v_prod)) {
          res[x$measuredElement == "5312"] <- v_prod / v_yield
        }
      } else if (non_protected == "5510") {
        if (!is.na(v_yield) && !is.na(v_area)) {
          res[x$measuredElement == "5510"] <- v_area * v_yield
        }
      }
    }
  } else if (sum(p_prod, p_area, p_yield) == 1) {
    # check the I,i flag
    computed <- x[flagObservationStatus == "I" & flagMethod == "i"]
    if (nrow(computed) > 1) {
      stop("More than one are identities. This cannot happen.")
    } else if (nrow(computed) == 1) {
      if (computed$measuredElement == "5421") {
        if (!is.na(v_area) && v_area > 0 && !is.na(v_prod)) {
          res[x$measuredElement == "5421"] <- v_prod / v_area
        }
      } else if (computed$measuredElement == "5312") {
        if (!is.na(v_yield) && v_yield > 0 && !is.na(v_prod)) {
          res[x$measuredElement == "5312"] <- v_prod / v_yield
        }
      } else if (computed$measuredElement == "5510") {
        if (!is.na(v_yield) && !is.na(v_area)) {
          res[x$measuredElement == "5510"] <- v_area * v_yield
        }
      }
    } else { # all I,e probably...
      # We use some order. 1) prod, 2) area, 3) yield
      if (p_prod == FALSE && !is.na(v_yield) && !is.na(v_area)) {
        res[x$measuredElement == "5510"] <- v_area * v_yield
      } else if (p_area == FALSE && !is.na(v_yield) && v_yield > 0 && !is.na(v_prod)) {
        res[x$measuredElement == "5312"] <- v_prod / v_yield
      } else if (p_yield == FALSE && !is.na(v_area) && v_area > 0 && !is.na(v_prod)) {
        res[x$measuredElement == "5421"] <- v_prod / v_area
      }
    }
  }
  # if nothing is protected, we shouldn't care, the imputation module should

  return(res)
}

send_mail <- function(from = NA, to = NA, subject = NA,
                      body = NA, remove = FALSE) {
  
  if (missing(from)) from <- 'no-reply@fao.org'
  
  if (missing(to)) {
    if (exists('swsContext.userEmail')) {
      to <- swsContext.userEmail
    }
  }
  
  if (is.null(to)) {
    stop('No valid email in `to` parameter.')
  }
  
  if (missing(subject)) stop('Missing `subject`.')
  
  if (missing(body)) stop('Missing `body`.')
  
  if (length(body) > 1) {
    body <-
      sapply(
        body,
        function(x) {
          if (file.exists(x)) {
            # https://en.wikipedia.org/wiki/Media_type 
            file_type <-
              switch(
                tolower(sub('.*\\.([^.]+)$', '\\1', basename(x))),
                txt  = 'text/plain',
                csv  = 'text/csv',
                png  = 'image/png',
                jpeg = 'image/jpeg',
                jpg  = 'image/jpeg',
                gif  = 'image/gif',
                xls  = 'application/vnd.ms-excel',
                xlsx = 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
                doc  = 'application/msword',
                docx = 'application/vnd.openxmlformats-officedocument.wordprocessingml.document',
                pdf  = 'application/pdf',
                zip  = 'application/zip',
                # https://stackoverflow.com/questions/24725593/mime-type-for-serialized-r-objects
                rds  = 'application/octet-stream'
              )
            
            if (is.null(file_type)) {
              stop(paste(tolower(sub('.*\\.([^.]+)$', '\\1', basename(x))),
                         'is not a supported file type.'))
            } else {
              return(sendmailR:::.file_attachment(x, basename(x), type = file_type))
            }
            
            if (remove) {
              unlink(x)
            }
          } else {
            return(x)
          }
        }
      )
  } else if (!is.character(body)) {
    stop('`body` should be either a string or a list.')
  }
  
  sendmailR::sendmail(from, to, subject, as.list(body))
}

