# be sure to check for packages conflicts!
# 01 import/export ----
library(readxl) # read excel files
library(janitor) # simple tools to clean dirty data
library(here) # a simpler way to find your files
library(SWMPr) # working with SWMP data from the NERRS
library(SWMPrExtension) # expanded plotting and analysis of SWMP data from NERRS
library(xlsx) # to export df as .xlsx files

# 02 tidy and wrangle ----
library(tidyverse) # because...tidyverse (ggplot2, tidyr, dplyr)
library(lubridate) # dates and times

# 03 pulling information and statistics ----
library(broom) # convert statistical analysis objects into tidy tibbles
library(psych)
library(wql)
library(car) # expanded statistical tests

# 04 markdown ----
library(knitr)
library(rmarkdown)
library(kableExtra) # https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html 

# 05 graphics ----
library(cowplot) # additional themes for ggplot2
library(ggpubr) # stats and additional plotting
library(khroma) # color-blind friendly palettes
library(patchwork) # grid graphics
library(scales) # scale functions for visualization
library(plotly) # create interactive web graphics - use for html output files
library(gganimate) # create animated graphs
library(leaflet) # making maps
library(ggcorrplot) # visualization of correlation matrix using ggplot2
# or install the latest ggcorrplot from GitHub
# Install
# if(!require(devtools)) install.packages("devtools")
# devtools::install_github("kassambara/ggcorrplot")

# 06 mapping ---------------------------------------------------------------
library(leaflet)
library(htmltools)
library(sf)

