# load libraries and data files 
# uncomment below if necessary
# source('R/00_loadpackages.R')

# 01 load data --------------------------------------------------
## 01.1 load 2001-2020 WQ and MET files
## 01.2 load 2021 WQ and MET files

## 01.1 load 2001-2020 WQ and MET files ------------------------------------------
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

## 01.2 load 2021 WQ and MET files ------------------------------------------
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

MET <- dplyr::bind_rows(MET_hist, MET_2021)
WQ <- dplyr::bind_rows(WQ_hist, WQ_2021)


# 03 export as .RData -----------------------------------------------------

## uncomment below to export nutrients as .RData for use later.
# save(WQ, file = here::here('output', 'data', 'WQ.RData'))
# save(MET, file = here::here('output', 'data', 'MET.RData'))

rm(MET_2021, MET_hist, 
   WQ_2021, WQ_hist)