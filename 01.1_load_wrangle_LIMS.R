# load libraries and data files 
# uncomment below if necessary
# source('R/00_loadpackages.R')

# 01 load data --------------------------------------------------
## load LIMS File
## this file should be in the 'data/2021' folder
## if kept generic, rename file to simply 'LIMS_Download.xlsx', but if using file as is
## edit file name in import code below
## EDIT and/or REVIEW LIMS file prior to loading it. Correct all mistakes in the LIMS file. If new parameters are added
## or removed the code will need to be edited. Code is currently based on the 2022 LIMS file format.
## LIMS file must also be in the in the format used in the GTM_LIMS_V2 template. If running code and encounters an error then there is probably a mistake in the LIMS file.
lims <- readxl::read_xlsx(here::here('data', 
                                     '2022',
                                     'LIMS_Download.xlsx'), # this is where you'd want to rename the file
                          sheet = "BrowseReportPage") %>% 
        janitor::clean_names()

# inspect the data
dplyr::glimpse(lims)

# load CDMO names file
names <- readxl::read_xlsx(here::here('data', 
                                      'componentnames.xlsx')) %>%
         janitor::clean_names()

# 02 wrangle-tidy data ------------------------------------------------------
# rename some columns in lims to what we use in SWMP
# convert datetimes into POSIXct format
# make all entries in station_code and component_long columns lowercase (easier coding)
# remove field blanks
lims2 <- lims %>%
          dplyr::rename(station_code = field_id,
                        component_long = component,
                        datetimestamp = date_sampled) %>% 
          dplyr::mutate(datetimestamp = as.POSIXct(strptime(datetimestamp, 
                                                           "%d-%b-%Y %H:%M", tz='EST')),
                        date_analyzed = as.POSIXct(strptime(date_analyzed, 
                                                            "%d-%b-%Y %H:%M", tz='EST')),
                        station_code = tolower(station_code),
                        component_long = tolower(component_long)) %>% 
          dplyr::filter(station_code != "field blank") 

# correct so that TKN and TKN-F are different 
# fixing the LIMS entry so that kjeldahl nitrogen, dissolved is different from kjeldahl nitrogen. they have the same component name.
tkn_f <- lims2 %>% 
          dplyr::filter(analysis == "W-TKN-F") %>% 
          dplyr::mutate(component_long = "kjeldahl nitrogen, dissolved") 

# fixing the LIMS entry so that total-p, filtered is different from total-p. they have the same component name.
tp_f <- lims2 %>% 
  dplyr::filter(analysis == "W-S-A-TP-F") %>% 
  dplyr::mutate(component_long = "total-p, dissolved") 


# merge the renamed TKNF data with all the other data and then join with the `names` df to get the CDMO format names
lims3 <- lims2 %>% 
          dplyr::filter(analysis != "W-TKN-F") %>% 
          dplyr::filter(analysis != "W-S-A-TP-F") %>%
          dplyr::bind_rows(tkn_f) %>%  
          dplyr::bind_rows(tp_f) %>%
          dplyr::left_join(names, by = "component_long") %>% 
          dplyr::mutate(cdmo_name = forcats::as_factor(cdmo_name)) 

## clean up environment ---
rm(tkn_f, tp_f, lims2, names)

# 03 make LIMS data wide for hold time tables --------------------------------------------------
# Get information for sample hold time tables

holdTimes <- lims3 %>% 
  dplyr::select(station_code, datetimestamp, date_analyzed, cdmo_name, component_long) %>% 
  tidyr::pivot_wider(id_cols = c('station_code',
                                 'datetimestamp'), 
                     names_from = cdmo_name,
                     values_from = date_analyzed)

# 04 Create a new column called Program_Type to help complete the sample hold tables --------------------------------------------------
# Get information for Program_Type
holdTimes$ProgramType<-substr(holdTimes$station_code, 9, 9)

View(holdTimes)

holdTimes2<-holdTimes %>%
  mutate(across('ProgramType', str_replace, '2','Diel')) %>%
  mutate(across('ProgramType', str_replace, '1','Grab'))

View(holdTimes2)

## Reorder output to match metadata document

tier1<- holdTimes2 %>% 
  dplyr::select(1, # reorder everything, 1 is the first column
                ProgramType,
                datetimestamp,
                PO4F,
                NO23F,
                NH4F,
                CHLA_N)

tier2a<- holdTimes2 %>% 
  dplyr::select(1, # reorder everything, 1 is the first column
                ProgramType,
                datetimestamp,
                DOC,            
                TP,
                TDP,
                TKN,
                TKNF)

tier2b<- holdTimes2 %>% 
  dplyr::select(1, # reorder everything, 1 is the first column
                ProgramType,
                datetimestamp,
                UncCHLa_N,
                PHEA,
                TSS,
                color,
                ENTERO_MPN,
                FECCOL_CFU)

# Write as .csv files. USE THIS ONE. File exports as .csv so when imported into Excel it reformats the columns to numbers and the date columns as dates.
write.csv(tier1, here::here('output', 'tier1_holdtimes.csv'), row.names = FALSE)
write.csv(tier2a, here::here('output', 'tier2a_holdtimes.csv'), row.names = FALSE)
write.csv(tier2b, here::here('output', 'tier2b_holdtimes.csv'), row.names = FALSE)


# 05 make LIMS data wide for data file --------------------------------------------------
# to make the LIMS data in wide format for entry into in-house datafile

lims_wide_results <- lims3 %>% 
                        dplyr::select(station_code, datetimestamp, cdmo_name, component_long, result) %>% 
                        tidyr::pivot_wider(id_cols = c('station_code',
                                                       'datetimestamp'), 
                                           names_from = cdmo_name,
                                           values_from = result)

lims_wide_remarks <- lims3 %>% 
                        dplyr::select(station_code, datetimestamp, cdmo_name, remark) %>% 
                        dplyr::mutate(cdmo_name = paste0('F_', cdmo_name)) %>% 
                        tidyr::pivot_wider(id_cols = c('station_code',
                                                       'datetimestamp'), 
                                           names_from = cdmo_name,
                                           values_from = remark)

lims_wide <- lims_wide_results %>% 
                left_join(lims_wide_remarks, 
                          by = c("station_code", "datetimestamp")) %>%
                dplyr::mutate(fullstationname = station_code) %>% 
                tidyr::separate(station_code, 
                                into = c("station_code", "num"), 
                                sep = "(?<=[A-Za-z])(?=[0-9])") %>%
                tidyr::separate(num,
                                into = c("monitoringprogram", "rep"),
                                sep = "[.]") %>% 
                    dplyr::select(1, 103, 4, 2:3, 5:102) # Code provides a warning here. Just ignore.
                    #dplyr::select(1, 103, 4, 2:3, 5:102)
  

# to look up column numbers for easier reordering used for `select()` above          
# data.frame(colnames(lims_wide))
# data.frame(colnames(lims_wide_results)). Reordering columns. Station Code =1, full station name = 101, datetime = 2, monitoring program = 2
# rep = 3, Data starts in column 5 and data ends in column 100

## clean up environment 
rm(lims_wide_remarks, lims_wide_results)


# env variables in LIMS file if wanted ----------------------------------------------

# env <- lims3 %>% 
#         select(station_code, temperature, specific_conductance, salinity, ph, dissolved_o2) %>% 
#         tidyr::separate(temperature,
#                         into = c("WTEM_N", "tempunits"),
#                         sep = "(\\s+)") %>%
#         tidyr::separate(specific_conductance,
#                         into = c("specificconductance", "spcunits"),
#                         sep = "(\\s+)") %>%
#         tidyr::separate(dissolved_o2,
#                         into = c("DO_N", "o2units"),
#                         sep = "(\\s+)") %>%
#         dplyr::select(-spcunits, -tempunits, -o2units)  
#   


# 06 export lims-wide to csv or xlsx ------------------------------------------------

# 06.1 make file the same parameters and order as the qaqc file in-house
# Added TDP
lims_wide2 <- lims_wide %>% 
                dplyr::mutate(F_Record = '', # build in blank columns for easier copy & paste
                              IRR0_N = '',
                              F_IRR0_N = '',
                              IRR1_N = '',	
                              F_IRR1_N = '',
                              Kd_N = '',	
                              F_Kd_N = '',
                              DOP = '',	
                              F_DOP = '',
                              PHOSP	= '',
                              F_PHOSP= '',
                              PN = '',
                              F_PN = '',
                              TURB_N = '',
                              F_TURB_N = '',
                              DIN = '',
                              F_DIN = '',
                              TN = '',
                              F_TN = '',
                              TON = '',
                              F_TON = '',
                              PON = '',
                              F_PON = '',
                              TDN = '',
                              F_TDN = '',
                              DON = '',
                              F_DON = '',
                              SECCHI = '',
                              F_SECCHI = '',
                              WTEM_N = '',
                              F_WTEM_N = '',
                              SALT_N = '',
                              F_SALT_N = '',
                              PH_N = '',
                              F_PH_N = '',
                              DO_N = '',
                              F_DO_N = '') %>% 
                    dplyr::select(1:5, 104, # reorder everything, 1:5 are the station code, etc. columns. Second number is one more than the total number of columns i.e. number of data columns +1
                                  PO4F, F_PO4F,
                                  TP, F_TP,
                                  TDP, F_TDP,
                                  NH4F, F_NH4F,
                                  NO23F, F_NO23F,
                                  TKN, F_TKN,
                                  TKNF, F_TKNF,
                                  CHLA_N, F_CHLA_N,
                                  UncCHLa_N, F_UncCHLa_N,
                                  PHEA, F_PHEA,
                                  TSS, F_TSS,
                                  TURB_N, F_TURB_N,
                                  color, F_color,
                                  FECCOL_CFU, F_FECCOL_CFU,
                                  ENTERO_MPN, F_ENTERO_MPN,
                                  WTEM_N, F_WTEM_N,
                                  SALT_N, F_SALT_N,
                                  DO_N, F_DO_N,
                                  PH_N, F_PH_N,
                                  SECCHI,	F_SECCHI,
                                  DOC, F_DOC,
                                  IRR0_N,	F_IRR0_N,
                                  IRR1_N,	F_IRR1_N,
                                  Kd_N,	F_Kd_N,
                                  DIN, F_DIN,
                                  DON, F_DON,
                                  DOP, F_DOP,
                                  PHOSP, F_PHOSP,
                                  PON, F_PON,
                                  TDN, F_TDN,
                                  TN, F_TN,
                                  PN,	F_PN,
                                  TON, F_TON)
                    #dplyr::select(1:5, 104, # reorder everything
                              # PO4F, F_PO4F,
                              # TP, F_TP,
                              # NH4F, F_NH4F,
                              # NO2F, F_NO2F,
                              # NO3F, F_NO3F,
                              # NO23F, F_NO23F,
                              # DIN, F_DIN,
                              # TN, F_TN,
                              # TKN, F_TKN,
                              # TKNF, F_TKNF, 
                              # TON, F_TON,
                              # DON, F_DON,
                              # TDN, F_TDN,
                              # PON, F_PON,
                              # CHLA_N, F_CHLA_N,
                              # UncCHLa_N, F_UncCHLa_N,
                              # PHEA, F_PHEA,
                              # TSS, F_TSS,
                              # TURB_N, F_TURB_N,
                              # color, F_color,
                              # FECCOL_CFU, F_FECCOL_CFU,
                              # WTEM_N, F_WTEM_N,
                              # SALT_N, F_SALT_N,
                              # DO_N, F_DO_N,
                              # PH_N, F_PH_N, 
                              # DOC, F_DOC)

## reformat datetimestamp to CDMO format. Column will be last. NOTE: If exporting as .csv you don't need to reformat datetimestamp
## create Date objects using base R
lims_wide2$timestamp <- strptime(lims_wide2$datetimestamp, "%Y-%m-%d %H:%M")

## format them to spec
lims_wide2$CDMO_dates <- format(lims_wide2$timestamp, "%m/%d/%Y %H:%M")


# remove the NAs that will come through on file export
lims_wide2 <- sapply(lims_wide2, as.character)
lims_wide2[is.na(lims_wide2)] <- " "
lims_wide2 <- as.data.frame(lims_wide2)

# Write as .csv file. USE THIS ONE. File exports as .csv so when imported into Excel it reformats the columns to numbers and the date columns as dates.
write.csv(lims_wide2, here::here('output', 'lims_wide.csv'), row.names = FALSE)


# careful, if running this code twice in the same day, you will get a warning that a sheet of that name already exists. 
# may need to delete sheet of same date if you ran into error
# xlsx::write.xlsx(lims_wide2, here::here('output', 'lims_wide.xlsx'),
#                  sheetName = paste(Sys.Date()), # change this date
#                  append = TRUE)



