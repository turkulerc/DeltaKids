#----Script Header----------------------------------------------------------####
# Date: 2022-05-30
# Author: Turkuler Ozgumus
#Filename: 00_read_data.R
# Description: Reading data files
#               
#              
# Project: Delta Kids
#----------------------------------------------------------------------------###

library(here)

#----read data--------------------------------------------------------------####
allData <- readxl::read_xlsx(here("data_original", "Delta infected results incl. serology 100822.xlsx"), skip = 1, n_max = 276)
info_allData <- readxl::read_xlsx(here("data_original", "Delta infected results incl. serology 100822.xlsx"), n_max = 1)
#----------------------------------------------------------------------------###
