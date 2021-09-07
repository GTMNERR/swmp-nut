# load libraries and data files 
# uncomment below if necessary
# source('R/00_loadpackages.R')

# 01 load data --------------------------------------------------
## 01.1 load 2002-2020 Nutrient Data
## 01.2 load 2021 Nutrient Data
## 01.3 load 2001-2020 WQ and MET files
## 01.4 load 2021 WQ and MET files


## 01.1 load 2002-2020 Nutrient Data ------------------------------------------------------

nms <- names(read_excel(here::here('data',
                                    '2001_2020_WQ_MET_NUT_FilesCDMO',
                                    'All_inclusive_NUT',
                                    'gtmnut2002-2020_QC.xlsx'), n_max = 0)) # pull out all the column names in this file

class <- ifelse(grepl("^F_", nms), "text", "numeric") # read everything with F_ as a character
class2 <- class[-(1:5)] # remove the first five elements of the vector because they are different

nut_hist <- readxl::read_xlsx(here::here('data',
                                         '2001_2020_WQ_MET_NUT_FilesCDMO',
                                         'All_inclusive_NUT',
                                         'gtmnut2002-2020_QC.xlsx'),
                              col_types = c("text", "date", "numeric", "numeric", "text", class2))  # specify how to read in these columns
            
  # clean environment
rm(nms, class, class2)

## 01.2 load 2021 Nutrient Data ------------------------------------------
nms <- names(read_excel(here::here('data',
                                   '2021',
                                   'gtmnut2021.xlsx'),
                        sheet = "Data", 
                        n_max = 0)) # pull out all the column names in this file

class <- ifelse(grepl("^F_", nms), "text", "numeric") # read everything with F_ as a character
class2 <- class[-(1:5)] # remove the first five elements of the vector because they are different



nut_2021 <- readxl::read_xlsx(here::here('data',
                                         '2021',
                                         'gtmnut2021.xlsx'),
                              sheet = 'Data',
                              col_types = c("text", "text", "date", "numeric", "numeric", class2)) # specify how to read in these columns)
# clean environment 
rm(nms, class, class2)

## 01.3 load 2001-2020 WQ and MET files ------------------------------------------
## import data with `SWMPr::import_local()` and then clean it with `SWMPr::qaqc()` to screen observations
## check what the flags mean used in the `SWMPr::qaqc()` fxn here:  https://cdmo.baruch.sc.edu/data/qaqc.cfm.
## add in station name (for combining)
pi_hist <- SWMPr::import_local(path = here::here('data',
                                          '2001_2020_WQ_MET_NUT_FilesCDMO'), 
                               station_code = 'gtmpiwq') %>% 
           SWMPr::qaqc(qaqc_keep = c('0', '1', '2', '3', '4', '5')) %>% 
           dplyr::mutate(station = 'gtmpiwq')
ss_hist <- SWMPr::import_local(path = here::here('data',
                                                 '2001_2020_WQ_MET_NUT_FilesCDMO'), 
                               station_code = 'gtmsswq') %>% 
           SWMPr::qaqc(qaqc_keep = c('0', '1', '2', '3', '4', '5')) %>% 
           dplyr::mutate(station = 'gtmsswq')
fm_hist <- SWMPr::import_local(path = here::here('data',
                                                 '2001_2020_WQ_MET_NUT_FilesCDMO'), 
                               station_code = 'gtmfmwq') %>% 
           SWMPr::qaqc(qaqc_keep = c('0', '1', '2', '3', '4', '5')) %>% 
           dplyr::mutate(station = 'gtmfmwq')
pc_hist <- SWMPr::import_local(path = here::here('data',
                                                 '2001_2020_WQ_MET_NUT_FilesCDMO'), 
                               station_code = 'gtmpcwq') %>% 
           SWMPr::qaqc(qaqc_keep = c('0', '1', '2', '3', '4', '5')) %>% 
           dplyr::mutate(station = 'gtmpcwq')

WQ_hist <- dplyr::bind_rows(pi_hist, ss_hist, fm_hist, pc_hist)

MET_hist <- SWMPr::import_local(path = here::here('data',
                                                  '2001_2020_WQ_MET_NUT_FilesCDMO'), 
                                station_code = 'gtmpcmet') %>% 
            SWMPr::qaqc(qaqc_keep = c('0', '1', '2', '3', '4', '5')) %>% 
            dplyr::mutate(station = 'gtmpcmet')

# choose to keep or remove individual stations
rm(pi_hist, ss_hist, fm_hist, pc_hist)

## 01.4 load 2021 WQ and MET files ------------------------------------------
pi_2021 <- SWMPr::import_local(path = here::here('data',
                                                 '2021',
                                                 'WQ-MET'), 
                               station_code = 'gtmpiwq') %>% 
           SWMPr::qaqc(qaqc_keep = c('0', '1', '2', '3', '4', '5')) %>% 
           dplyr::mutate(station = 'gtmpiwq')
ss_2021 <- SWMPr::import_local(path = here::here('data',
                                                 '2021',
                                                 'WQ-MET'), 
                               station_code = 'gtmsswq') %>% 
           SWMPr::qaqc(qaqc_keep = c('0', '1', '2', '3', '4', '5')) %>% 
           dplyr::mutate(station = 'gtmsswq')
fm_2021 <- SWMPr::import_local(path = here::here('data',
                                                 '2021',
                                                 'WQ-MET'), 
                               station_code = 'gtmfmwq') %>% 
           SWMPr::qaqc(qaqc_keep = c('0', '1', '2', '3', '4', '5')) %>% 
           dplyr::mutate(station = 'gtmfmwq')
pc_2021 <- SWMPr::import_local(path = here::here('data',
                                                 '2021',
                                                 'WQ-MET'), 
                               station_code = 'gtmpcwq') %>% 
           SWMPr::qaqc(qaqc_keep = c('0', '1', '2', '3', '4', '5')) %>% 
           dplyr::mutate(station = 'gtmpcwq')

WQ_2021 <- dplyr::bind_rows(pi_2021, ss_2021, fm_2021, pc_2021)

MET_2021 <- SWMPr::import_local(path = here::here('data',
                                                  '2021',
                                                  'WQ-MET'), 
                                station_code = 'gtmpcmet') %>% 
            SWMPr::qaqc(qaqc_keep = c('0', '1', '2', '3', '4', '5')) %>% 
            dplyr::mutate(station = 'gtmpcmet')

# choose to keep or remove individual stations
rm(pi_2021, ss_2021, fm_2021, pc_2021)


# 02 wrangle data for merging ------------------------------------------------
## 02.1 Nutrients
## 02.2 WQ and MET


## 02.1 Nutrient file wrangling --------------------------------------------

# # check the two data frames using `janitor::compare_df_cols()`
# janitor::compare_df_cols(nut_hist, nut_2021,
#                          bind_method = "bind_rows",
#                          return = "mismatch")

# make some changes to the 2021 data to match the hist data:
nut_2021 <- nut_2021 %>% 
              dplyr::rename(COLOR = Color, 
                            F_COLOR = F_Color,
                            REP = Rep) %>% 
              dplyr::select(-`Station Code_GTM`)


# 03 combine historic and 2021 files --------------------------------------
## 03.1 Nutrient
## 03.2 WQ and MET

## 03.1 Nutrient files merge -----------------------------------------------
# `bind_rows()` the hist and 2021 data and also clean up the column names using `clean_names()`
NUT <- dplyr::bind_rows(nut_hist, nut_2021) %>% 
  janitor::clean_names(case = "screaming_snake")

# view the datafile
dplyr::glimpse(NUT)

# clean up environment
rm(nut_hist, nut_2021)

