# load libraries and data files 
# uncomment below if necessary
# source('R/00_loadpackages.R')

# 01 load data --------------------------------------------------
## 01.1 load 2002-2020 Nutrient Data
## 01.2 load 2021 Nutrient Data


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



# 02 wrangle data for merging ------------------------------------------------


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



# 04 wrangle to swmpr -----------------------------------------------------
# The `swmpr()` call needs to have just datetimestamp and data+qa columns, so remove the extras, while also making names lower case.  
timezone <- "America/Jamaica" # needs a timezone

names(NUT) <- tolower(names(NUT))
PC_nut <- NUT %>% filter(station_code == "gtmpcnut") %>% select(-station_code)
PC_nut$date_time_stamp <- as.POSIXct(PC_nut$date_time_stamp, tz = timezone, format = '%m/%d/%Y %H:%M')
swmp_pc <- swmpr(PC_nut, "gtmpcnut")

class(swmp1)
str(swmp1)




## 99 export .RData ----
## uncomment below to export nutrients as .RData for use later.
# save(NUT, file = here::here('output', 'data', 'NUT.RData'))