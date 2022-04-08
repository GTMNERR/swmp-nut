# 2022-03-10 Data Request for Adam Langley 
# the longest records you have of [Total N] or [Mineral N] of any water body in or around GTM. 
# all the forms of N over all time at all stations 
# load libraries and data files 
# uncomment below if necessary
# source('R/00_loadpackages.R')
# source('R/02.1_load_wrangle_NUT.R')

req <- NUT %>% 
        select(1:5, 
               NH4F, F_NH4F,
               NO2F, F_NO2F,
               NO3F, F_NO3F,
               NO23F, F_NO23F,
               DIN, F_DIN,
               TDN, F_TDN,
               TN, F_TN,
               TKN, F_TKN,
               TKNF, F_TKNF,
               TON, F_TON,
               DON, F_DON,
               PON, F_PON
               )

# remove the NAs that will come through on file export
req <- sapply(req, as.character)
req[is.na(req)] <- " "
req <- as.data.frame(req)

# careful, if running this code twice in the same day, you will get a warning that a sheet of that name already exists. 
# may need to delete sheet of same date if you ran into error
xlsx::write.xlsx(req, here::here('output', 'data', '2022-03-10_GTM-N-data_AL.xlsx'))