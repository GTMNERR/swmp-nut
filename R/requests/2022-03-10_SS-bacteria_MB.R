# 2022-03-10 Data Request for Matt Brown 
# bacteria data (Entero and Fecal) for SS for the past 3 years.
# load libraries and data files 
# uncomment below if necessary
# source('R/00_loadpackages.R')
# source('R/02.1_load_wrangle_NUT.R')

req <- NUT %>% 
        select(1:5, 
               ENTERO_MPN, F_ENTERO_MPN,
               FECCOL_CFU, F_FECCOL_CFU
               ) %>% 
  filter(STATION_CODE == "gtmssnut" & DATE_TIME_STAMP > "2012-12-31 23:45:00")

# remove the NAs that will come through on file export
req <- sapply(req, as.character)
req[is.na(req)] <- " "
req <- as.data.frame(req)

# careful, if running this code twice in the same day, you will get a warning that a sheet of that name already exists. 
# may need to delete sheet of same date if you ran into error
xlsx::write.xlsx(req, here::here('output', 'data', '2022-03-10_SS-bacteria_MB.xlsx'))