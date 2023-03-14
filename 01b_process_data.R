#----Script Header----------------------------------------------------------####
# Date: 2022-06-13
# Author: Turkuler Ozgumus
#Filename: 01b_process_data.R
# Description: Processing 6m follow-up data
#               
#              
# Project: Delta Kids
#----------------------------------------------------------------------------###

library(tidyverse)
library(here) 
library(lubridate)

df_6m <- allData %>% select(1, 95:154, 161)
info_6m <- info_allData %>% select(1, 95:154, 161)

info_6m <- cbind.data.frame(info_col = colnames(df_6m),
                            variable_name = c("record_id",
                                              "unge_ansvarlig_t2",
                                              "unge_alder_t2",
                                              "feber_t2",
                                              "smak_lukt_t2",
                                              "hoste_t2",
                                              "prikking_numm_t2",
                                              "hodepine_t2",
                                              "svimmel_t2",
                                              "sovnproblem_t2",
                                              "hjertebank_t2",
                                              "brystsmerter_t2",
                                              "diare_mage_t2",
                                              "ledd_t2",
                                              "muskel_t2",
                                              "andre_plager_t2",
                                              "ingen_symp_t2",
                                              "andre_plager_tekst",
                                              "unge_medisiner_t2",
                                              "prednisolon_steroider_t2",
                                              "annen_immundemping_t2",
                                              "ingen_medisiner_t2",
                                              "komorb_nyresykdom_t2",
                                              "komorb_leversykdom_t2",
                                              "komorb_lungesykdom_t2",
                                              "komorb_astma_t2",
                                              "komorb_kreft_t2",
                                              "komorb_diabetes_t2",
                                              "komorb_pollenallergi_t2",
                                              "komorb_høytblodtrykk_t2",
                                              "komorb_hjertekarsykdom_t2",
                                              "komorb_revmatisksykdom_t2",
                                              "komorb_nevrologisksykdom_t2",
                                              "komorb_ingen_t2",
                                              "røyking_t2",
                                              "unge_hoeyde_t2",
                                              "unge_vekt_t2",
                                              "sliten_t2",
                                              "unge_ch_slitenprosent_t2",
                                              "konsentrasjon_t2",
                                              "red_hukommelse_t2",
                                              "deprimert_t2",
                                              "tungpust_t2",
                                              "tungpust_tekst_t2",
                                              "utejobb_t2",
                                              "utejobb_dager_t2",
                                              "utefritidsaktiviteter_t2",
                                              "utefritidsaktiviteter_dager_t2",
                                              "reinfeksjon_t2",
                                              "reinfection_date_t2",
                                              "unge_vaksine_t2",
                                              "unge_vaksinetype_dose1_t2",
                                              "dose1_date_t2",
                                              "unge_vaksine_dose2_t2",
                                              "unge_vaksinetype_dose2_t2",
                                              "dose2_date_t2",
                                              "unge_vaksine_dose3_t2",
                                              "unge_vaksinetype_dose3_t2",
                                              "dose3_date_t2",
                                              "ny_komorb_t2",
                                              "ny_komorb_tekst_t2",
                                              "unge_dato_t2"))

df_6m <- df_6m %>% 
  rename_at(vars(info_6m$info_col), ~info_6m$variable_name) %>% 
  mutate(reinfeksjon_t2 = case_when(reinfeksjon_t2 == "Ja (angi dato)" ~ 1,
                                    reinfeksjon_t2 == "Nei" ~ 0),
         unge_vaksine_t2 = case_when(unge_vaksine_t2 == "Ja" ~ 1,
                                     unge_vaksine_t2 == "Nei" ~ 0))

df_all <- df_baseline %>% 
  left_join(df_6m, by="record_id") %>% 
  mutate(dose1_date_t2 = openxlsx::convertToDate(dose1_date_t2),
         dose2_date_t2 = openxlsx::convertToDate(dose2_date_t2),
         dose3_date_t2 = as.Date(dose3_date_t2),
         unge_dato_t0 = lubridate::as_date(unge_dato_t0),
         unge_dato_t1 = lubridate::as_date(unge_dato_t1),
         unge_dato_t2 = lubridate::as_date(unge_dato_t2),
         unge_testdato_t0 = lubridate::as_date(unge_testdato_t0),
         reinfection_date_t2 = openxlsx::convertToDate(reinfection_date_t2)) %>% 
  mutate(tungpust_t2 = case_when(tungpust_t2 == "Ja" ~ 1,
                                   tungpust_t2 == "Nei" ~  0)) %>% 
  mutate(sliten_t2 = case_when(sliten_t2 %in% c("Ikke mer enn vanlig", "Mindre enn vanlig") ~ 0,
                                   sliten_t2 == "Mer enn vanlig" ~ 1, # more than usual
                                   sliten_t2 == "Mye mer enn vanlig" ~ 2), # much more than usual
         red_hukommelse_t2 = case_when(red_hukommelse_t2 %in% c("Ikke verre enn vanlig", "Bedre enn vanlig") ~ 0,
                                            red_hukommelse_t2 == "Verre enn vanlig" ~ 1, # more than usual
                                            red_hukommelse_t2 == "Mye verre enn vanlig" ~ 2), # much more than usual
         konsentrasjon_t2 = case_when(konsentrasjon_t2 %in% c("Ikke mer enn vanlig", "Mindre enn vanlig") ~ 0,
                                          konsentrasjon_t2 == "Mer enn vanlig" ~ 1, # more than usual
                                          konsentrasjon_t2 == "Mye mer enn vanlig" ~ 2), # much more than usual
         deprimert_t2 = case_when(deprimert_t2 %in% c("Ikke mer enn vanlig", "Mindre enn vanlig") ~ 0,
                                      deprimert_t2 == "Mer enn vanlig" ~ 1, # more than usual
                                      deprimert_t2 == "Mye mer enn vanlig" ~ 2)) %>%  # much more than usual
  mutate(sliten_t2_d = case_when(sliten_t2 == 2 ~ 1,
                                 TRUE ~ sliten_t2),
         konsentrasjon_t2_d = case_when(konsentrasjon_t2 == 2 ~ 1,
                                        TRUE ~ konsentrasjon_t2),
         red_hukommelse_t2_d = case_when(red_hukommelse_t2 == 2 ~ 1,
                                          TRUE ~ red_hukommelse_t2),
         deprimert_t2_d = case_when(deprimert_t2 == 2 ~ 1,
                                    TRUE ~ deprimert_t2)) %>% 
  mutate(sliten_t1_d = case_when(ch_sliten_t1 %in% 1:2 ~ 0,
                                 ch_sliten_t1 %in% 3:4 ~ 1),
         konsentrasjon_t1_d = case_when(ch_konsentrasjon_t1 %in% 1:2 ~ 0,
                                        ch_konsentrasjon_t1 %in% 3:4 ~ 1),
         hukommelse_t1_d = case_when(ch_hukommelse_t1 %in% 1:2 ~ 0,
                                     ch_hukommelse_t1 %in% 3:4 ~ 1)) %>% 
  rowwise() %>% 
  mutate(muskel_ledd_t2 = max(muskel_t2, ledd_t2),
         ingen_symp_t0 = case_when(all(feber_t0 == 0, 
                                     hoste_t0 == 0, 
                                     tungpust_t0 == 0, 
                                     slapphet_t0 == 0, 
                                     muskel_ledd_t0 == 0, 
                                     hodepine_t0 == 0, 
                                     smak_lukt_t0 == 0, 
                                     diare_mage_t0 == 0, 
                                     andre_sympt_t0 == 0) ~ 1,
                                   any(feber_t0 == 1, 
                                       hoste_t0 == 1, 
                                       tungpust_t0 == 1, 
                                       slapphet_t0 == 1, 
                                       muskel_ledd_t0 == 1, 
                                       hodepine_t0 == 1, 
                                       smak_lukt_t0 == 1, 
                                       diare_mage_t0 == 1, 
                                       andre_sympt_t0 == 1) ~ 0),
         ingen_symp_t1 = case_when(all(lavgradig_feber_t1 == 0, 
                                       tungpust_t1 == 0, 
                                       prikking_numm_t1 == 0, 
                                       hodepine_t1 == 0, 
                                       svimmel_t1 == 0, 
                                       hjertebank_t1 == 0, 
                                       mageproblem_t1 == 0, 
                                       lukt_smak_t1 == 0, 
                                       sliten_t1 == 0,
                                       sovnproblem_t1 == 0,
                                       red_hukommelse_t1 == 0,
                                       konsentrasjon_t1 == 0,
                                       andre_plager_t1 == 0) ~ 1,
                                   any(lavgradig_feber_t1 == 1, 
                                       tungpust_t1 == 1, 
                                       prikking_numm_t1 == 1, 
                                       hodepine_t1 == 1, 
                                       svimmel_t1 == 1, 
                                       hjertebank_t1 == 1, 
                                       mageproblem_t1 == 1, 
                                       lukt_smak_t1 == 1, 
                                       sliten_t1 == 1,
                                       sovnproblem_t1 == 1,
                                       red_hukommelse_t1 == 1,
                                       konsentrasjon_t1 == 1,
                                       andre_plager_t1 == 1) ~ 0),
         ingen_symp_t2 = case_when(all(feber_t2 == 0, 
                                     smak_lukt_t2 == 0, 
                                     hoste_t2 == 0, 
                                     prikking_numm_t2 == 0, 
                                     hodepine_t2 == 0, 
                                     svimmel_t2 == 0,                    
                                     sovnproblem_t2 == 0, 
                                     hjertebank_t2 == 0, 
                                     brystsmerter_t2 == 0,
                                     diare_mage_t2 == 0, 
                                     ledd_t2 == 0, 
                                     muskel_t2 == 0, 
                                     andre_plager_t2 == 0, 
                                     tungpust_t2 == 0) ~ 1, 
                                   any(feber_t2 == 1, 
                                       smak_lukt_t2 == 1, 
                                       hoste_t2 == 1, 
                                       prikking_numm_t2 == 1, 
                                       hodepine_t2 == 1, 
                                       svimmel_t2 == 1,                    
                                       sovnproblem_t2 == 1, 
                                       hjertebank_t2 == 1, 
                                       brystsmerter_t2 == 1,
                                       diare_mage_t2 == 1, 
                                       ledd_t2 == 1, 
                                       muskel_t2 == 1, 
                                       andre_plager_t2 == 1, 
                                       tungpust_t2 == 1) ~ 0)) %>% 
  mutate(ingen_symp_t2_inclCh = case_when(all(ingen_symp_t2 == 1,
                                            sliten_t2 == 0,
                                            konsentrasjon_t2 == 0,
                                            deprimert_t2 == 0,
                                            red_hukommelse_t2 == 0) ~ 1,
                                            any(ingen_symp_t2 == 0,
                                                sliten_t2 %in% 1:2,
                                                konsentrasjon_t2 %in% 1:2,
                                                deprimert_t2 %in% 1:2,
                                                red_hukommelse_t2 %in% 1:2) ~ 0),
         ingen_symp_t1_inclCh = case_when(all(ingen_symp_t1 == 1,
                                              konsentrasjon_t1_d == 0 | is.na(konsentrasjon_t1_d),
                                              hukommelse_t1_d   == 0 | is.na(hukommelse_t1_d)) ~ 1,
                                          any(ingen_symp_t1 == 0,
                                              konsentrasjon_t1_d %in% 1:2 | is.na(konsentrasjon_t1_d),
                                              hukommelse_t1_d %in% 1:2 | is.na(hukommelse_t1_d)) ~ 0)) %>% 
  ungroup() %>% 
  mutate(any_symp_t0 = case_when(ingen_symp_t0 == 0 ~ 1,
                                 ingen_symp_t0 == 1 ~ 0),
         any_symp_t1 = case_when(ingen_symp_t1 == 0 ~ 1,
                                 ingen_symp_t1 == 1 ~ 0),
         any_symp_t2 = case_when(ingen_symp_t2 == 0 ~ 1,
                                 ingen_symp_t2 == 1 ~ 0),
         any_symp_t2_inclCh = case_when(ingen_symp_t2_inclCh == 0 ~ 1,
                                        ingen_symp_t2_inclCh == 1 ~ 0),
         any_symp_t1_inclCh = case_when(ingen_symp_t1_inclCh == 0 ~ 1,
                                        ingen_symp_t1_inclCh == 1 ~ 0)) %>% 
  mutate(any_symp_t1_inclCh = if_else(is.na(any_symp_t1), NA_real_, any_symp_t1_inclCh),
         ingen_symp_t1_inclCh = if_else(is.na(ingen_symp_t1), NA_real_, ingen_symp_t1_inclCh)) %>% 
  mutate(unge_alder_t2 = as.numeric(unge_alder_t2))  %>% 
  mutate(unge_vaksine_t2_mod = case_when(!is.na(dose1_date_t2) & dose1_date_t2 > unge_dato_t2 ~ 0, 
                                         TRUE ~ unge_vaksine_t2)) %>% 
  mutate(dose2_date_t2 = case_when(record_id == "delta096" ~ ymd("2022-01-27"),
                                   TRUE ~ dose2_date_t2)) %>% 
#  filter(record_id != "delta156") %>% # reinfection with Delta
  mutate(unge_alder_t2 = case_when(unge_alder_t2 == 10 ~ 11, TRUE ~ unge_alder_t2 )) |># only 1 10 years old child - included in 11 year-olds
  mutate(unge_vaksine_t0_eff = if_else(!is.na(unge_datodose1_t0) & openxlsx::convertToDate(unge_datodose1_t0) + 14 <= unge_testdato_t0, 1, 2)) |>
  mutate(unge_vaksine_t0_eff = if_else(record_id == "delta080", 1, unge_vaksine_t0_eff)) |>
  mutate(unge_vaksine_t2_eff = if_else(!is.na(dose1_date_t2) & dose1_date_t2 + 14 < unge_dato_t2, 1, 0)) |>
  mutate(unge_vaksine_t2_eff = if_else(is.na(unge_vaksine_t2), NA_real_, unge_vaksine_t2_eff)) |>
  mutate(vaccine_after_delta = if_else(!is.na(dose1_date_t2) & ((dose1_date_t2 + 14 > unge_testdato_t0 & dose1_date_t2 + 14 < unge_dato_t2) 
                                                                | (!is.na(dose2_date_t2) & dose2_date_t2 + 14 > unge_testdato_t0 & dose2_date_t2 + 14 < unge_dato_t2)
                                                                | (!is.na(dose3_date_t2) & dose3_date_t2 + 14 > unge_testdato_t0 & dose3_date_t2 + 14 < unge_dato_t2)), 1, 0)) |>
  mutate(vaccine_after_delta_before_omicron = if_else(vaccine_after_delta == 1 & !is.na(reinfection_date_t2) & 
                                                        (dose1_date_t2 < reinfection_date_t2
                                                         | dose2_date_t2 < reinfection_date_t2
                                                         | dose3_date_t2 < reinfection_date_t2), 1, 0)) %>% 
  mutate(vaccine_after_delta = if_else(is.na(unge_vaksine_t2), NA_real_, vaccine_after_delta),
         vaccine_after_delta_before_omicron = if_else(is.na(unge_vaksine_t2), NA_real_, vaccine_after_delta_before_omicron))
                                       

df_all <- df_all %>% 
  mutate(tungpust_t1_mod = case_when(unge_tungpust_mmrc_t1 %in% 2:4  ~  1,
                                     unge_tungpust_mmrc_t1 %in% 0:1  ~ 0)) %>% 
  mutate(resp_symp_t2 = case_when(!is.na(andre_plager_tekst) & (str_detect(andre_plager_tekst, "oste") |
                                                                str_detect(andre_plager_tekst, "hals") |
                                                                str_detect(andre_plager_tekst, "nese") |
                                                                str_detect(andre_plager_tekst, "forkjøle") |
                                                                str_detect(andre_plager_tekst, "nase")) ~ 1, 
                                  !is.na(andre_plager_tekst) & !(str_detect(andre_plager_tekst, "oste") |
                                                                  str_detect(andre_plager_tekst, "hals") |
                                                                  str_detect(andre_plager_tekst, "nese") |
                                                                  str_detect(andre_plager_tekst, "forkjøle") |
                                                                  str_detect(andre_plager_tekst, "nase")) ~ 0)) %>% 
  mutate(general_symp_t0 = pmax(feber_t0, slapphet_t0, hodepine_t0, diare_mage_t0_mod, muskel_ledd_t0, na.rm = T),
         general_symp_t1 = pmax(lavgradig_feber_t1, sliten_t1, sliten_t1_d, hodepine_t1, mageproblem_t1, hjertebank_t1, na.rm = T),
         general_symp_t2 = pmax(feber_t2, sliten_t2_d, hodepine_t2, diare_mage_t2, muskel_ledd_t2, hjertebank_t2, na.rm = T),
         resp_symp_t0 = pmax(tungpust_t0, hoste_t0, tett_rennede_nese_t0_mod, vondt_hals_t0_mod, na.rm = T),
         resp_symp_t1 = pmax(tungpust_t1, tett_rennede_nese_t1_mod, hoste_t1_mod, na.rm = T),
         resp_symp_t2 = pmax(resp_symp_t2, tungpust_t2, hoste_t2, brystsmerter_t2, na.rm = T),
         smell_taste_t0 = smak_lukt_t0,
         smell_taste_t1 = lukt_smak_t1, 
         smell_taste_t2 = smak_lukt_t2,
         neuro_symp_t1 = pmax(prikking_numm_t1, svimmel_t1, sovnproblem_t1, na.rm = T),
         neuro_symp_t2 = pmax(prikking_numm_t2, svimmel_t2, sovnproblem_t2, na.rm = T),
         cogn_symp_t1 = pmax(red_hukommelse_t1, konsentrasjon_t1, konsentrasjon_t1_d, hukommelse_t1_d, na.rm = T), 
         cogn_symp_t2 = pmax(red_hukommelse_t2_d, konsentrasjon_t2_d, na.rm = T)) %>% 
  mutate(across(c(unge_dato_t0, unge_testdato_t0, unge_symptstart_t0, unge_symptslutt_t0, unge_tidlpostest_dato_t0, unge_datodose2_t0, unge_datodose3_t0, unge_dato_t1), ~ as.Date(.x)),
         age_group = if_else(unge_alder_t0 < 16, "10-15", "16-20"),
         unge_datodose1_t0 = openxlsx::convertToDate(unge_datodose1_t0),
         unge_vekt_t2 = as.numeric(unge_vekt_t2),
         unge_hoeyde_t2 = as.numeric(unge_hoeyde_t2)) %>% 
  mutate(vacc_6m = case_when((as.numeric(unge_dato_t2 - dose1_date_t2) <= 180 & as.numeric(unge_dato_t2 - dose1_date_t2 >= 14)) |
                             (as.numeric(unge_dato_t2 - dose2_date_t2) <= 180 & as.numeric(unge_dato_t2 - dose2_date_t2 >= 14)) |
                             (as.numeric(unge_dato_t2 - dose3_date_t2) <= 180 & as.numeric(unge_dato_t2 - dose3_date_t2 >= 14)) ~ 1,
                             is.na(unge_vaksine_t2_eff) ~ NA_real_,
                             TRUE ~ 0),
         vacc_2m = case_when((as.numeric(unge_dato_t2 - dose1_date_t2) <= 60 & as.numeric(unge_dato_t2 - dose1_date_t2 >= 14)) |
                               (as.numeric(unge_dato_t2 - dose2_date_t2) <= 60 & as.numeric(unge_dato_t2 - dose2_date_t2 >= 14)) |
                               (as.numeric(unge_dato_t2 - dose3_date_t2) <= 60 & as.numeric(unge_dato_t2 - dose3_date_t2 >= 14)) ~ 1,
                             is.na(unge_vaksine_t2_eff) ~ NA_real_,
                             TRUE ~ 0),
         days_since_vaccination = case_when(!is.na(dose1_date_t2) & dose1_date_t2 < unge_dato_t2  ~ as.numeric(unge_dato_t2 - dose1_date_t2)),
         days_since_vaccination = case_when(!is.na(dose2_date_t2) & dose2_date_t2 < unge_dato_t2  ~ as.numeric(unge_dato_t2 - dose2_date_t2),
                                            TRUE ~ days_since_vaccination),
         days_since_vaccination = case_when(!is.na(dose3_date_t2) & dose3_date_t2 < unge_dato_t2  ~ as.numeric(unge_dato_t2 - dose3_date_t2),
                                            TRUE ~ days_since_vaccination),
         days_since_vaccination_6m = case_when(days_since_vaccination <= 180 ~ days_since_vaccination))
