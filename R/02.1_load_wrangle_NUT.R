# load libraries and data files 
# updated 2022-07-19 SKD to convert to swmpr object and filter using qaqc from swmpr
# uncomment below if necessary
# source('R/00_loadpackages.R')

# 01 load data --------------------------------------------------
## 01.1 load 2002-2022 Nutrient Data

nms <- names(read_excel(here::here('data',
                                   '2001_2020_WQ_MET_NUT_FilesCDMO',
                                   'All_inclusive_NUT',
                                   'gtmnut2002-2022_QC_zeros-corrected.xlsx'), 
                        n_max = 0)) # pull out all the column names in this file

class <- ifelse(grepl("^F_", nms), "text", "numeric") # read everything with F_ as a character
class2 <- class[-(1:5)] # remove the first five elements of the vector because they are different

NUT <- readxl::read_xlsx(here::here('data',
                                    '2001_2020_WQ_MET_NUT_FilesCDMO',
                                    'All_inclusive_NUT',
                                    'gtmnut2002-2022_QC_zeros-corrected.xlsx'),
                              col_types = c("text", 
                                            "date", 
                                            "numeric", 
                                            "numeric", 
                                            "text", 
                                            class2)) %>% # specify how to read in these columns
  janitor::clean_names()
            
  # clean environment
rm(nms, class, class2)

# 02 wrangle data for merging ------------------------------------------------
NUT <- NUT %>% filter(!is.na(rep)) # remove "S" reps in dataset

# 04 wrangle to swmpr -----------------------------------------------------
# The `swmpr()` call needs to have just datetimestamp and data+qa columns, so remove the extras, while also making names lower case.
timezone <- "America/Jamaica" # needs a timezone

# Pine Island
PI_nut <- NUT %>%
  filter(station_code == "gtmpinut") %>%
  select(-station_code) %>%
  mutate(date_time_stamp = as.POSIXct(date_time_stamp,
                                      tz = timezone,
                                      format = '%m/%d/%Y %H:%M')) %>%
  rename(datetimestamp = date_time_stamp) %>% 
  select(-monitoring_program, -rep)

# swmpr wants data frame, not tibble
swmp_pi <- swmpr(as.data.frame(PI_nut), "gtmpinut")
rm(PI_nut)

# San Sebastian
SS_nut <- NUT %>%
  filter(station_code == "gtmssnut") %>%
  select(-station_code) %>%
  mutate(date_time_stamp = as.POSIXct(date_time_stamp,
                                      tz = timezone,
                                      format = '%m/%d/%Y %H:%M')) %>%
  rename(datetimestamp = date_time_stamp) %>% 
  select(-monitoring_program, -rep)

# swmpr wants data frame, not tibble
swmp_ss <- swmpr(as.data.frame(SS_nut), "gtmssnut")
rm(SS_nut)

# Fort Matanzas
FM_nut <- NUT %>%
  filter(station_code == "gtmfmnut") %>%
  select(-station_code) %>%
  mutate(date_time_stamp = as.POSIXct(date_time_stamp,
                                      tz = timezone,
                                      format = '%m/%d/%Y %H:%M')) %>%
  rename(datetimestamp = date_time_stamp) %>% 
  select(-monitoring_program, -rep)

# swmpr wants data frame, not tibble
swmp_fm <- swmpr(as.data.frame(FM_nut), "gtmfmnut")
rm(FM_nut)

# Pellicer Creek
PC_nut <- NUT %>%
  filter(station_code == "gtmpcnut") %>%
  select(-station_code) %>%
  mutate(date_time_stamp = as.POSIXct(date_time_stamp,
                                      tz = timezone,
                                      format = '%m/%d/%Y %H:%M')) %>%
  rename(datetimestamp = date_time_stamp) %>% 
  filter(monitoring_program == 1) %>% 
  select(-monitoring_program, -rep)

# swmpr wants data frame, not tibble
swmp_pc <- swmpr(as.data.frame(PC_nut), "gtmpcnut")
rm(PC_nut)
# check object
# class(swmp_pc)
# str(swmp_pc)

rm(timezone)

## 04.2 qaqc swmpr --------------------------------------------------------

# use the qaqc functions on the data
pi_nut <- swmp_pi %>% SWMPr::qaqc(qaqc_keep = c('0', '2', '3', '4', '5'))
ss_nut <- swmp_ss %>% SWMPr::qaqc(qaqc_keep = c('0', '2', '3', '4', '5'))
fm_nut <- swmp_fm %>% SWMPr::qaqc(qaqc_keep = c('0', '2', '3', '4', '5'))
pc_nut <- swmp_pc %>% SWMPr::qaqc(qaqc_keep = c('0', '2', '3', '4', '5'))


rm(swmp_pi, swmp_ss, swmp_fm,swmp_pc)


# 05 aggregate to monthly -------------------------------------------------

pi_nut_mo <- pi_nut %>% aggreswmp(by = "months")
ss_nut_mo <- ss_nut %>% aggreswmp(by = "months")
fm_nut_mo <- fm_nut %>% aggreswmp(by = "months")
pc_nut_mo <- pc_nut %>% aggreswmp(by = "months")

# merge together
NUT_f <- bind_rows("gtmpinut" = pi_nut_mo, 
                   "gtmssnut" = ss_nut_mo, 
                   "gtmfmnut" = fm_nut_mo, 
                   "gtmpcnut" = pc_nut_mo, 
                   .id = "station_code")

NUT_monthly <- NUT_f %>%
  select(station_code, datetimestamp, tn, tp, chla_n) %>% 
  dplyr::mutate(year = lubridate::year(datetimestamp), 
                month_abb = lubridate::month(datetimestamp, label = TRUE, abbr = TRUE),
                month = lubridate::month(datetimestamp),
                station_code = factor(station_code,
                                      levels = c("gtmpinut",
                                                 "gtmssnut",
                                                 "gtmfmnut",
                                                 "gtmpcnut")))

### 05.3.2 monthly averages to yearly ----
# annual geometric mean function
# gmean <- function(x) exp(mean(log(x), na.rm = TRUE))
# or use the psych::geometric.mean() function

NUT_yearly <- NUT_monthly %>% 
  dplyr::group_by(station_code, year) %>% 
  dplyr::summarise(TN_agm = psych::geometric.mean(tn, na.rm = T),
                   TP_agm = psych::geometric.mean(tp, na.rm = T),
                   CHLA_agm = psych::geometric.mean(chla_n, na.rm = T)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(year = forcats::as_factor(year))