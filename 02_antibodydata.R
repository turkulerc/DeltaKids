#----Script Header----------------------------------------------------------####
# Date: 2022-07-21
# Author: Turkuler Ozgumus
#Filename: 02_antibodydata.R
# Description: Reading and processing antibody data
#               
#              
# Project: Delta Kids
#----------------------------------------------------------------------------###

library(here)
library(tidyverse)
source(here("scripts", "00_read_data.R"))

df <- allData %>% select(1:9) %>% slice(1:276)

colnames(df) <- c("record_id", 
                  "BP_6w",
                  "Delta_t1",
                  "Wuhan_t1",
                  "Omicron_t1",
                  "BP_6m",
                  "Delta_t2",
                  "Wuhan_t2",
                  "Omicron_t2")
