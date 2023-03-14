#----Script Header----------------------------------------------------------####
# Date: 2022-08-25
# Author: Turkuler Ozgumus
#Filename: 03b_KMplot.R
# Description: KM plot
#               
#              
# Project: Delta Kids
#----------------------------------------------------------------------------###

library(here)
library(tidyverse)
library(survminer)
library(survival)
library(ggpmisc)
library(patchwork)


source(here("scripts", "01_process_data.R"))
source(here("scripts", "01b_process_data.R"))
source(here("scripts", "functions.R"))

df_use <-
  df_all |>
  select(record_id, reinfeksjon_t2, unge_testdato_t0, reinfection_date_t2, unge_vaksine_t2, dose1_date_t2, dose2_date_t2, dose3_date_t2, unge_dato_t2) |>
  filter(!is.na(dose1_date_t2), dose1_date_t2 + 14  < unge_dato_t2, dose1_date_t2 + 14  > unge_testdato_t0 |
           dose2_date_t2 + 14  > unge_testdato_t0 | dose3_date_t2 + 14  > unge_testdato_t0,
         is.na(reinfection_date_t2) | (dose1_date_t2  < reinfection_date_t2 | dose2_date_t2  < reinfection_date_t2 | dose3_date_t2  < reinfection_date_t2))  |>
  mutate(days_since_last_dose1_t2 = if_else(dose1_date_t2 + 14  > unge_testdato_t0 & dose1_date_t2 + 14  < unge_dato_t2, time_length(unge_dato_t2 - dose1_date_t2, unit = "day"), NA_real_),
         days_since_last_dose2_t2 = if_else(dose2_date_t2 + 14  > unge_testdato_t0 & dose2_date_t2 + 14  < unge_dato_t2, time_length(unge_dato_t2 - dose2_date_t2, unit = "day"), NA_real_),
         days_since_last_dose3_t2 = if_else(dose3_date_t2 + 14  > unge_testdato_t0 & dose3_date_t2 + 14  < unge_dato_t2, time_length(unge_dato_t2 - dose3_date_t2, unit = "day"), NA_real_)) |>
  rowwise() |>
  mutate(days_since_last_vaccine = min(days_since_last_dose1_t2, days_since_last_dose2_t2, days_since_last_dose3_t2, na.rm=T)) |>
  ungroup() |>
  mutate(days_since_last_dose1_reinf = if_else(dose1_date_t2 + 14  > unge_testdato_t0 & reinfeksjon_t2 == 1 & dose1_date_t2 + 14  < reinfection_date_t2, time_length(reinfection_date_t2 - dose1_date_t2, unit = "day"), NA_real_),
         days_since_last_dose2_reinf = if_else(dose2_date_t2 + 14  > unge_testdato_t0 & reinfeksjon_t2 == 1 & dose2_date_t2 + 14  < reinfection_date_t2, time_length(reinfection_date_t2 - dose2_date_t2, unit = "day"), NA_real_),
         days_since_last_dose3_reinf = if_else(dose3_date_t2 + 14  > unge_testdato_t0 & reinfeksjon_t2 == 1 & dose3_date_t2 + 14  < reinfection_date_t2, time_length(reinfection_date_t2 - dose3_date_t2, unit = "day"), NA_real_)) |>
  rowwise() |>
  mutate(days_since_last_vaccine = if_else(!is.na(days_since_last_dose1_reinf) | !is.na(days_since_last_dose2_reinf) | !is.na(days_since_last_dose3_reinf), 
                                           min(days_since_last_dose1_reinf, days_since_last_dose2_reinf, days_since_last_dose3_reinf, na.rm = T),
                                           days_since_last_vaccine)) |>
  ungroup() 

fit <- survfit(Surv(days_since_last_vaccine, reinfeksjon_t2) ~ 1, data = df_use)
p <- ggsurvplot(fit, risk.table = T, conf.int = T, censor = F, conf.int.style = "step", xlim = c(0,100), ylim = c(0, 1), break.x.by = 25, theme = theme_classic2())

#summary(fit, times = 100)


riskTable <- p$data.survtable %>% select(n.risk, n.event, n.censor) %>% t(.)
colnames(riskTable) <- p$data.survtable$time
riskTable <- riskTable[,1:5]

p_plot <- p$plot + 
  geom_segment(x = 0, xend = 30, y  = 0.9, yend = 0.9, linetype = "dashed", col = "#0570b0", size = 1) + 
  geom_segment(x = 0, xend = 100, y  = 0.5, yend = 0.5, linetype = "dashed", size = 1) +
  geom_segment(x = 30, xend = 30, y  = 0, yend = 0.9, linetype = "dashed", col = "#0570b0", size = 1) +
  geom_segment(x = 100, xend = 100, y  = 0, yend = 0.5, linetype = "dashed", size = 1) +
  annotate("text", x = 1, y = 0.87, label = "90% at 30 d", col = "#0570b0", size = 3, hjust = 0) +
  annotate("text", x = 1, y = 0.47, label = "50% at 100 d", size = 3, hjust = 0) +
  theme(legend.position = "none",
        text = element_text(size = 20, colour = "black"),
        axis.title.y = element_text(margin = margin(r=-70))) +
  scale_y_continuous(expand= c(0, 0)) + 
  scale_x_continuous(expand= expand_scale(mult=c(0, 0.05))) +
  ylab("Proportion of non-reinfected") +
  xlab("Days since last vaccination")

p_table <- 
  ggplot(riskTable%>% as.data.frame() %>% slice(1) %>% 
           bind_rows(c(`0` = 0, 
                       `25` = nrow(df_use %>% filter(days_since_last_vaccine < 25, reinfeksjon_t2 == 1)), 
                       `50` = nrow(df_use %>% filter(days_since_last_vaccine >= 25, days_since_last_vaccine < 50, reinfeksjon_t2 == 1)), 
                       `75` = nrow(df_use %>% filter(days_since_last_vaccine >= 50, days_since_last_vaccine < 75, reinfeksjon_t2 == 1)),
                       `100` = nrow(df_use %>% filter(days_since_last_vaccine >= 75, days_since_last_vaccine < 100, reinfeksjon_t2 == 1)))) %>% 
           bind_rows(c(`0` = 0, 
                       `25` = nrow(df_use %>% filter(days_since_last_vaccine < 25, reinfeksjon_t2 == 0)), 
                       `50` = nrow(df_use %>% filter(days_since_last_vaccine >= 25, days_since_last_vaccine < 50, reinfeksjon_t2 == 0)), 
                       `75` = nrow(df_use %>% filter(days_since_last_vaccine >= 50, days_since_last_vaccine < 75, reinfeksjon_t2 == 0)),
                       `100` = nrow(df_use %>% filter(days_since_last_vaccine >= 75, days_since_last_vaccine < 100, reinfeksjon_t2 == 0)))) %>% 
           mutate(group = c( "n at risk", "n events", "n censored")) %>% pivot_longer(cols = c(`0`:`100`), names_to = "time", values_to = "n" ) %>% mutate(time = as.numeric(time)), 
         aes(x = time, y = group)) + 
  geom_text(aes(label=n), size = 3.5) + 
  ylab("") +
  xlab("") +
  theme_minimal() + 
  theme(panel.border = element_rect(fill=NA, color="black"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(colour = "black", margin = margin(r = 10)),
        axis.title.x = element_text(colour = "black"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        text = element_text(size = 15, hjust=0)) +
  scale_x_continuous(expand= expand_scale(mult=c(0, 0.02)), limits = c(-4, 103)) 

p_plot / p_table + plot_layout(design = c(area(t = 1, l = 1, b = 7, r = 8), area(t = 8, l = 1, b = 9, r = 8)))

ggsave(file = here("plots", paste0("KM_plot_", Sys.Date(), ".tiff")), dpi = 300, width = 1259, height = 1456, unit = "px")

