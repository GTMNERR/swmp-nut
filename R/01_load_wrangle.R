# load libraries and data files 
# uncomment below if necessary
# source('R/00_loadpackages.R')

# 01 load data --------------------------------------------------
## 01.1 load GTMNERR SWMP nutrient datasets
## 01.2 load GTMNERR SWMP field data


## 01.1 2021 nutrient data ------------------------------------------------------

nut_2021 <- readxl::read_xlsx(here::here('data', 
                                         'gtmnut2021Q1-Q2.xlsx'), 
                              sheet = "Data") %>% 
            janitor::clean_names(case = "screaming_snake")

# inspect the data
glimpse(nut)


## 01.2 2021 field data loop ----------------------------------------------------

# initialize readin listing
mysheets_fromexcel <- list()

# get the list of all the sheet names
mysheetlist <- readxl::excel_sheets(path = here::here('data', 
                                                      '2021_FIELDDATA_v1.xlsx'))

# create loop for the sheets
i = 1

for (i in 1:length(mysheetlist)){
  
  tempdf <- readxl::read_excel(path = here::here('data', 
                                                 '2021_FIELDDATA_v1.xlsx'), 
                               sheet = mysheetlist[i])
  
  tempdf$sheetname <- mysheetlist[i]
  
  mysheets_fromexcel[[i]] <- tempdf 
}

mysheets_fromexcel


# merge all the lists into one tibble using dplyr::bind_rows()
env <- purrr::reduce(mysheets_fromexcel, dplyr::bind_rows) %>% 
  janitor::clean_names()

# inspect the data
glimpse(env)

# clear environment

rm(mysheets_fromexcel, 
   tempdf, i, mysheetlist)


## 01.3 2020 nut data ------------------------------------------------------
nut_2020 <- readxl::read_xlsx(here::here('data', 
                                         '2002-2020', 
                                         'gtmnut2020.xlsx'), 
                              sheet = "Data") %>% 
            janitor::clean_names(case = "screaming_snake")


## 01.4 2002-2019 nut data -------------------------------------------------
nut_2002_2019 <- readxl::read_xlsx(here::here('data', 
                                              '2002-2020', 
                                              'gtmnut2002-2019_QC.xlsx'), 
                                  ) %>% 
                 janitor::clean_names(case = "screaming_snake")



# 02 wrangle-tidy data ------------------------------------------------------


## 02.1 combine 2020-2021 files --------------------------------------------

nut_2020_2021 <- bind_rows(nut_2020, nut_2021)

glimpse(nut_2020_2021)



## 02.2 combine 2002-2021 --------------------------------------------------

janitor::compare_df_cols_same(nut_2002_2019, nut_2020_2021,
                              bind_method = "bind_rows")

nut_2002_2019 <- nut_2002_2019 %>% 
                  dplyr::mutate(DOC = as.numeric(DOC),
                                DON = as.numeric(DON),
                                ENTERO_MPN = as.numeric(ENTERO_MPN),
                                F_DOC = as.character(F_DOC),
                                F_DON = as.character(F_DON),
                                F_ENTERO_MPN = as.character(F_ENTERO_MPN),
                                F_FECCOL_CFU = as.character(F_FECCOL_CFU),
                                F_PON = as.character(F_PON),
                                F_TKN = as.character(F_TKN),
                                F_TKNF = as.character(F_TKNF),
                                F_TON = as.character(F_TON),
                                FECCOL_CFU = as.numeric(FECCOL_CFU),
                                PON = as.numeric(PON),
                                REP = as.numeric(REP),
                                TKN = as.numeric(TKN),
                                TKNF = as.numeric(TKNF),
                                TON = as.numeric(TON)
                  )

nut_2020_2021 <- nut_2020_2021 %>% 
                  dplyr::mutate(F_PH_N = as.character(F_PH_N),
                                F_SALT_N = as.character(F_SALT_N),
                                F_WTEM_N = as.character(F_WTEM_N))

nut <- bind_rows(nut_2002_2019, nut_2020_2021)

## 02.1 wrangle-tidy field data ---------------------------------------------

env2 <- env %>%
  dplyr::mutate(date_sampled = lubridate::ymd_hm(paste(date, time_24_hr)),
                site = tolower(site),
                component_long = tolower(component_long),
                cdmo_name = forcats::as_factor(component_short)
                ) %>% 
  dplyr::select(station_code, date_sampled, cdmo_name, component_long, result, remark) %>% 
  tidyr::separate(station_code, 
                  into = c("station_code", "num"), 
                  sep = "(?<=[A-Za-z])(?=[0-9])") %>%
  tidyr::separate(num,
                  into = c("monitoringprogram", "replicate"),
                  sep = "[.]")


# merge env and nut -------------------------------------------------------

dat <- dplyr::bind_rows(nut2, env2)
