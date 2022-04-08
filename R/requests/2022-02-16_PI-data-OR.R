# Olivia Roorbach requested Pine Island nutrient data for inclusion into her Guana analysis
# 2022-02-16 SKD

# load libraries and data files 
# uncomment below if necessary
# source('R/00_loadpackages.R')

# 01 load data --------------------------------------------------
# source('R/02.1_load_wrangle_NUT.R')
# source('R/working/N_P_forms_statistics.R') # lines 1-172

# 02 wrangle and export data ----
NUT_or <- sites %>% filter(STATION_CODE == "gtmpinut")  
  

# remove the NAs that will come through on file export
NUT_or <- sapply(NUT_or, as.character)
NUT_or[is.na(NUT_or)] <- " "
NUT_or <- as.data.frame(NUT_or)

# 03 export data ----
xlsx::write.xlsx(NUT_or, 
                 here::here('output', 'data', '2022-02-16_PI-data-OR.xlsx'))