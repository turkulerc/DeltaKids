#----Script Header----------------------------------------------------------####
# Date: 2022-05-30
# Author: Turkuler Ozgumus
#Filename: 01_process_data.R
# Description: Processing baseline file
#               
#              
# Project: Delta Kids
#----------------------------------------------------------------------------###

library(tidyverse)
library(here)

source(here("scripts", "00_read_data.R"))
df_baseline <- allData %>% select(1, 10:94)
info_baseline <- info_allData %>% select(1, 10:94)

# t0 refers to baseline
# t1 refers to 6-8 weeks after baseline

info_baseline <- info_baseline %>% 
  pivot_longer(cols = everything(), names_to = "info_col", values_to = "column_name") %>% 
  filter(!str_detect(info_col, "^\\.\\.\\.")) %>% 
  mutate(info_col = if_else(str_detect(column_name, "t0"), paste0(info_col, "_t0"), paste0(info_col, "_t1"))) %>% 
  mutate(info_col = str_remove(info_col, "\\.\\.\\...")) %>% 
  mutate(info_col = str_replace(info_col, " ", "_")) %>% 
  mutate(info_col = str_replace(info_col, "/", "_")) %>% 
  filter(!info_col %in% c("Baseline_t0", "6-8_veker_t1")) %>% 
  mutate(info_col = if_else(str_detect(column_name, "_ch_"), paste0("ch_", info_col), info_col))
info_baseline$info_col[19] <- "Sovnproblem_t1"
  

df_baseline <- df_baseline %>% 
#  filter(record_id != "delta082") %>%  # commented on unge_kommentar_t0 that this person is not in the study anymore # Nina is sure that this person did not want her data to be deleted
  mutate(covid_before_baseline = if_else(!is.na(unge_tidlpostest_dato_t0), 1, 0)) %>% 
  rename_at(vars(info_baseline$column_name), ~info_baseline$info_col) %>% 
  mutate(unge_sympt_annen_t0 = tolower(unge_sympt_annen_t0)) %>% 
  rename_with(tolower) %>% 
  rename(
         red_hukommelse_t1 = red._hukommelse_t1)
#----other symptoms---------------------------------------------------------####
#----baseline (t0)----------------------------------------------------------####
andre_symp_t0 <- df_baseline %>% 
  select(unge_sympt_annen_t0) %>% 
  filter(!is.na(unge_sympt_annen_t0)) %>% 
  mutate(unge_sympt_annen_t0 = str_remove(unge_sympt_annen_t0, "\\s+$")) %>% 
  distinct() %>%
  unlist()

# original variables are not changed, new variables have _mod at the end of their names to mark them as modified/newly created
# "oppkast", "kastet opp" and "kvalm" in other symptoms column (unge_sympt_annen_t0) are combined with diare_mage_t0 column (new variable diare_mage_t0_mod)
# the entries with "vond" or "sår" or "smerte" with "hals" are made into a new variable as vondt_hals_t0_mod
# the entries with "nese" or "snue" or "svie" are made into a new variable as tett_rennede_nese_t0_mod
# the entries with "forkjøle" or "snufsete" or "øli" are made into a new variable as forkjolet_t0_mod
# the entries with "bryst" are made into a new variable as brystsmerter_t0_mod
# the entries with "hjerte" are made into a new variable as hjertebank_t0_mod
# the entries with "heshet" are made into a new variable as heshet_t0_mod
# the entries with "svimmel" are made into a new variable as svimmel_t0_mod

df_baseline <- df_baseline %>% 
  mutate(diare_mage_t0_mod = if_else(!is.na(unge_sympt_annen_t0) & str_detect(unge_sympt_annen_t0, "oppkast"), 1, diare_mage_t0)) %>% 
  mutate(diare_mage_t0_mod = if_else(!is.na(unge_sympt_annen_t0) & str_detect(unge_sympt_annen_t0, "kastet opp"), 1, diare_mage_t0_mod))  %>% 
  mutate(diare_mage_t0_mod = if_else(!is.na(unge_sympt_annen_t0) & str_detect(unge_sympt_annen_t0, "kvalm"), 1, diare_mage_t0_mod)) %>% 
  mutate(vondt_hals_t0_mod = case_when(!is.na(unge_sympt_annen_t0) & str_detect(unge_sympt_annen_t0, "hals")  ~ 1, 
                                       !is.na(unge_sympt_annen_t0) & !str_detect(unge_sympt_annen_t0, "hals")  ~ 0)) %>% 
  mutate(tett_rennede_nese_t0_mod = case_when(!is.na(unge_sympt_annen_t0) & str_detect(unge_sympt_annen_t0, "nese") ~ 1, 
                                              !is.na(unge_sympt_annen_t0) & !str_detect(unge_sympt_annen_t0, "nese") ~ 0)) %>% 
  mutate(tett_rennede_nese_t0_mod = if_else(!is.na(unge_sympt_annen_t0) & str_detect(unge_sympt_annen_t0, "snue"), 1, tett_rennede_nese_t0_mod)) %>% 
  mutate(tett_rennede_nese_t0_mod = if_else(!is.na(unge_sympt_annen_t0) & str_detect(unge_sympt_annen_t0, "snufsete"), 1, tett_rennede_nese_t0_mod)) %>% 
  mutate(tett_rennede_nese_t0_mod = if_else(!is.na(unge_sympt_annen_t0) & str_detect(unge_sympt_annen_t0, "heshet"), 1, tett_rennede_nese_t0_mod)) %>% 
  mutate(tett_rennede_nese_t0_mod = if_else(!is.na(unge_sympt_annen_t0) & str_detect(unge_sympt_annen_t0, "forkjøle"), 1, tett_rennede_nese_t0_mod)) %>% 
  mutate(tett_rennede_nese_t0_mod = if_else(!is.na(unge_sympt_annen_t0) & str_detect(unge_sympt_annen_t0, "bihule"), 1, tett_rennede_nese_t0_mod)) %>% 
  mutate(tett_rennede_nese_t0_mod = if_else(!is.na(unge_sympt_annen_t0) & str_detect(unge_sympt_annen_t0, "øli"), 1, tett_rennede_nese_t0_mod)) %>% 
  mutate(forkjolet_t0_mod = case_when(!is.na(unge_sympt_annen_t0) & str_detect(unge_sympt_annen_t0, "forkjøle") ~ 1, 
                                      !is.na(unge_sympt_annen_t0) & !str_detect(unge_sympt_annen_t0, "forkjøle") ~ 0)) %>% 
  mutate(forkjolet_t0_mod = if_else(!is.na(unge_sympt_annen_t0) & str_detect(unge_sympt_annen_t0, "snufsete"), 1, forkjolet_t0_mod)) %>% 
  mutate(forkjolet_t0_mod = if_else(!is.na(unge_sympt_annen_t0) & str_detect(unge_sympt_annen_t0, "øli"), 1, forkjolet_t0_mod)) %>% 
  mutate(brystsmerter_t0_mod = case_when(!is.na(unge_sympt_annen_t0) & str_detect(unge_sympt_annen_t0, "bryst") ~ 1, 
                                         !is.na(unge_sympt_annen_t0) & !str_detect(unge_sympt_annen_t0, "bryst") ~ 0)) %>% 
  mutate(hjertebank_t0_mod = case_when(!is.na(unge_sympt_annen_t0) & str_detect(unge_sympt_annen_t0, "hjerte") ~ 1, 
                                       !is.na(unge_sympt_annen_t0) & !str_detect(unge_sympt_annen_t0, "hjerte") ~ 0)) %>% 
  mutate(heshet_t0_mod = case_when(!is.na(unge_sympt_annen_t0) & str_detect(unge_sympt_annen_t0, "heshet") ~ 1, 
                                   !is.na(unge_sympt_annen_t0) & !str_detect(unge_sympt_annen_t0, "heshet") ~ 0)) %>% 
  mutate(svimmel_t0_mod = case_when(!is.na(unge_sympt_annen_t0) & str_detect(unge_sympt_annen_t0, "svimmel") ~ 1, 
                                    !is.na(unge_sympt_annen_t0) & !str_detect(unge_sympt_annen_t0, "svimmel") ~ 0))
#----------------------------------------------------------------------------###
#----6-8 weeks followup (t1)------------------------------------------------####
andre_symp_t1 <- df_baseline %>% 
  select(unge_vedvarende_annen_t1) %>% 
  filter(!is.na(unge_vedvarende_annen_t1)) %>% 
  mutate(unge_vedvarende_annen_t1 = str_remove(unge_vedvarende_annen_t1, "\\s+$")) %>% 
  distinct() %>%
  unlist()

# "lukt" in other symptoms column (unge_vedvarende_annen_t1) are combined with lukt_smak_t1 (new variable lukt_smak_t1_mod)
# "utmattelse" or "form" (redusert form in the entry) in other symptoms column (unge_vedvarende_annen_t1) are combined with sliten_t1 (new variable sliten_t1_mod)
# the entries with "nese" are made into a new variable as tett_rennede_nese_t1_mod
# the entries with "host" are made into a new variable as hoste_t1_mod

df_baseline <-
  df_baseline %>% 
  mutate(lukt_smak_t1_mod = case_when(!is.na(unge_vedvarende_annen_t1) & str_detect(unge_vedvarende_annen_t1, "lukt") ~ 1, 
                                      TRUE ~ lukt_smak_t1)) %>% 
  mutate(sliten_t1_mod = case_when(!is.na(unge_vedvarende_annen_t1) & str_detect(unge_vedvarende_annen_t1, "utmattelse") ~ 1, 
                                   TRUE ~ sliten_t1)) %>% 
  mutate(sliten_t1_mod = case_when(!is.na(unge_vedvarende_annen_t1) & str_detect(unge_vedvarende_annen_t1, "form") ~ 1,
                                   TRUE ~ sliten_t1_mod)) %>% 
  mutate(tett_rennede_nese_t1_mod = case_when(!is.na(unge_vedvarende_annen_t1) & str_detect(unge_vedvarende_annen_t1, "nese") ~ 1, 
                                              !is.na(unge_vedvarende_annen_t1) & !str_detect(unge_vedvarende_annen_t1, "nese") ~ 0)) %>% 
  mutate(hoste_t1_mod = case_when(!is.na(unge_vedvarende_annen_t1) & str_detect(unge_vedvarende_annen_t1, "host") ~ 1, 
                                  !is.na(unge_vedvarende_annen_t1) & !str_detect(unge_vedvarende_annen_t1, "host") ~ 0))

rm(andre_symp_t0)
rm(andre_symp_t1)
#----------------------------------------------------------------------------###

  

  



