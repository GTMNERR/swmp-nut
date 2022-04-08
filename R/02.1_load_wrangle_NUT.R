# load libraries and data files 
# updated 2022-04-08 SKD to add 2022 data
# uncomment below if necessary
# source('R/00_loadpackages.R')

# 01 load data --------------------------------------------------
## 01.1 load 2002-2022 Nutrient Data


## 01.1 load 2002-2020 Nutrient Data ------------------------------------------------------

nms <- names(read_excel(here::here('data',
                                    '2001_2020_WQ_MET_NUT_FilesCDMO',
                                    'All_inclusive_NUT',
                                    'gtmnut2002-2022_QC.xlsx'), n_max = 0)) # pull out all the column names in this file

class <- ifelse(grepl("^F_", nms), "text", "numeric") # read everything with F_ as a character
class2 <- class[-(1:5)] # remove the first five elements of the vector because they are different

nut <- readxl::read_xlsx(here::here('data',
                                         '2001_2020_WQ_MET_NUT_FilesCDMO',
                                         'All_inclusive_NUT',
                                         'gtmnut2002-2022_QC.xlsx'),
                              col_types = c("text", "date", "numeric", "numeric", "text", class2))  # specify how to read in these columns
            
  # clean environment
rm(nms, class, class2)

# 02 wrangle data for merging ------------------------------------------------

# 04 wrangle to swmpr -----------------------------------------------------
# # The `swmpr()` call needs to have just datetimestamp and data+qa columns, so remove the extras, while also making names lower case.  
# timezone <- "America/Jamaica" # needs a timezone
# 
# names(NUT) <- tolower(names(NUT))
# PC_nut <- NUT %>% 
#   filter(station_code == "gtmpcnut") %>% 
#   select(-station_code) %>% 
#   mutate(date_time_stamp = as.POSIXct(PC_nut$date_time_stamp, 
#                                       tz = timezone, 
#                                       format = '%m/%d/%Y %H:%M')) %>% 
#   rename(datetimestamp = date_time_stamp)
# 
# # swmpr wants data frame, not tibble
# swmp_pc <- swmpr(as.data.frame(PC_nut), "gtmpcnut")
# 
# # check object
# class(swmp_pc)
# str(swmp_pc)
# 
# # try the qaqc functions
# swmp_pc %>% SWMPr::qaqc(qaqc_keep = c('0', '1', '2', '3', '4', '5'))


# 05 wrangle for nutrient indicators --------------------------------------

## 05.1 parameters of interest ----

TN_f <- NUT %>% 
  dplyr::select(1:4, TN, F_TN) %>% 
  dplyr::filter(grepl("0|<1>|<2>|<3>|<4>|<-4>|<5>", F_TN) | is.na(F_TN)) %>%
  dplyr::select(-F_TN) %>% 
  dplyr::filter(MONITORING_PROGRAM == 1) %>% 
  dplyr::mutate(DATE = as.Date(DATE_TIME_STAMP)) %>% 
  dplyr::select(STATION_CODE, DATE, REP, TN) 

TP_f <- NUT %>% 
  dplyr::select(1:4, TP, F_TP) %>%
  dplyr::filter(grepl("0|<1>|<2>|<3>|<4>|<-4>|<5>", F_TP) | is.na(F_TP)) %>% 
  dplyr::select(-F_TP) %>% 
  dplyr::filter(MONITORING_PROGRAM == 1) %>% 
  dplyr::mutate(DATE = as.Date(DATE_TIME_STAMP)) %>% 
  dplyr::select(-MONITORING_PROGRAM, -DATE_TIME_STAMP)     

CHLA_f <- NUT %>% 
  dplyr::select(1:4, CHLA_N, F_CHLA_N) %>% 
  dplyr::filter(grepl("0|<1>|<2>|<3>|<4>|<-4>|<5>", F_CHLA_N) | is.na(F_CHLA_N)) %>% 
  dplyr::select(-F_CHLA_N) %>% 
  dplyr::filter(MONITORING_PROGRAM == 1) %>% 
  dplyr::mutate(DATE = as.Date(DATE_TIME_STAMP)) %>% 
  dplyr::select(-MONITORING_PROGRAM, -DATE_TIME_STAMP) 

ENTERO_f <- NUT %>% 
  dplyr::select(1:4, ENTERO_MPN, F_ENTERO_MPN) %>% 
  dplyr::filter(grepl("0|<1>|<2>|<3>|<4>|<-4>|<5>", F_ENTERO_MPN) | is.na(F_ENTERO_MPN)) %>% 
  dplyr::select(-F_ENTERO_MPN) %>% 
  dplyr::filter(MONITORING_PROGRAM == 1) %>% 
  dplyr::mutate(DATE = as.Date(DATE_TIME_STAMP)) %>% 
  dplyr::select(-MONITORING_PROGRAM, -DATE_TIME_STAMP) 

FECAL_f <- NUT %>% 
  dplyr::select(1:4, FECCOL_CFU, F_FECCOL_CFU) %>% 
  dplyr::filter(grepl("0|<1>|<2>|<3>|<4>|<-4>|<5>", F_FECCOL_CFU) | is.na(F_FECCOL_CFU)) %>% 
  dplyr::select(-F_FECCOL_CFU) %>% 
  dplyr::filter(MONITORING_PROGRAM == 1) %>% 
  dplyr::mutate(DATE = as.Date(DATE_TIME_STAMP)) %>% 
  dplyr::select(-MONITORING_PROGRAM, -DATE_TIME_STAMP) 


## 05.2 merge all parameters into one new df --------------------------------

NUT_f <- dplyr::left_join(TN_f, TP_f, by = c("STATION_CODE", "DATE", "REP")) %>% 
  dplyr::left_join(CHLA_f, by = c("STATION_CODE", "DATE", "REP")) %>% 
  dplyr::left_join(ENTERO_f, by = c("STATION_CODE", "DATE", "REP")) %>% 
  dplyr::left_join(FECAL_f, by = c("STATION_CODE", "DATE", "REP"))

# clean environment
rm(CHLA_f, TN_f, TP_f, ENTERO_f, FECAL_f)


## 05.3 calculations --------------------------------------------------------------

### 05.3.1 monthly average ----
NUT_monthly <- NUT_f %>% 
  dplyr::group_by(STATION_CODE, DATE) %>% 
  dplyr::summarise(TN_avg = mean(TN, na.rm = TRUE),
                   TP_avg = mean(TP, na.rm = TRUE),
                   CHLA_avg = mean(CHLA_N, na.rm = TRUE),
                   ENTERO_avg = mean(ENTERO_MPN, na.rm = TRUE),
                   FECAL_avg = mean(FECCOL_CFU, na.rm = TRUE),
                   .groups = "keep") %>% 
  dplyr::mutate(YEAR = lubridate::year(DATE), 
                MONTH_abb = lubridate::month(DATE, label = TRUE, abbr = TRUE),
                MONTH = lubridate::month(DATE),
                STATION_CODE = factor(STATION_CODE,
                                      levels = c("gtmpinut",
                                                 "gtmssnut",
                                                 "gtmfmnut",
                                                 "gtmpcnut")))

### 05.3.2 monthly averages to yearly ----
# annual geometric mean function
# gmean <- function(x) exp(mean(log(x), na.rm = TRUE))
# or use the psych::geometric.mean() function

NUT_yearly <- NUT_monthly %>% 
  dplyr::group_by(STATION_CODE, YEAR) %>% 
  dplyr::summarise(TN_agm = psych::geometric.mean(TN_avg, na.rm = T),
                   TP_agm = psych::geometric.mean(TP_avg, na.rm = T),
                   CHLA_agm = psych::geometric.mean(CHLA_avg, na.rm = T)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(YEAR = forcats::as_factor(YEAR))


## 99 export .RData ----
## uncomment below to export nutrients as .RData for use later.
# save(NUT, file = here::here('output', 'data', 'NUT.RData'))
