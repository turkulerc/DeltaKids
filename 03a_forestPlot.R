#----Script Header----------------------------------------------------------####
# Date: 2022-11-24
# Author: Turkuler Ozgumus
#Filename: 03a_forestPlot.R
# Description: forest plots
#               
#              
# Project: Delta Kids
#----------------------------------------------------------------------------###

library(here)
library(tidyverse)
library(patchwork)

source(here("scripts", "01_process_data.R"))
source(here("scripts", "01b_process_data.R"))
source(here("scripts", "functions.R"))

df_use <- df_all %>% 
  select(record_id, any_symp_t2_inclCh, unge_alder_t0, unge_kjonn_t0, unge_vaksine_t2_eff, age_group,
         reinfeksjon_t2, any_symp_t0, vaccine_after_delta, unge_vaksine_t0_eff, vaccine_after_delta_before_omicron, 
         sliten_t2_d, cogn_symp_t2, age_group, resp_symp_t2, neuro_symp_t2, smell_taste_t2, hodepine_t0, tungpust_t2, 
         general_symp_t0, resp_symp_t0, smell_taste_t0, any_symp_t1, tungpust_t1, cogn_symp_t1, neuro_symp_t1, sliten_t1_d, 
         sliten_t1, slapphet_t0, tungpust_t0, ingen_symp_t0, ingen_symp_t1, ingen_symp_t2_inclCh, resp_symp_t1) %>% 
  rename(any_symp_t2 = any_symp_t2_inclCh, 
         ingen_symp_t2 = ingen_symp_t2_inclCh,
         sliten_t2 = sliten_t2_d) %>% 
  mutate(sex = if_else(unge_kjonn_t0 == 2, 0, unge_kjonn_t0),
         age_group_2 = if_else(age_group == "10-15", 1, 0),
         no_reinfeksjon_t2 = as.numeric(!reinfeksjon_t2),
         sex2 = unge_kjonn_t0 - 1)

#----------------------------------------------------------------------------###
  
symp <- c("sliten_t2", "tungpust_t2", "cogn_symp_t2" )
covar <- c("age_group_2", "sex2", "ingen_symp_t0", #"unge_vaksine_t2_eff", 
           "reinfeksjon_t2")
covar_name <- c("Age", "Sex", "Baseline symptoms", #"Vaccination", 
                "Omicron reinfection")

table1 <- tibble()
tables <- list()
for (i in 1:length(symp)) {
  for (j in 1:length(covar)) {
    table1 <- table1 %>% 
      bind_rows(df_use %>% 
      count(!!as.symbol(covar[j]), !!as.symbol(symp[i])) %>% 
      filter(!is.na(!!as.symbol(symp[i]))) %>% 
      pivot_wider(names_from = !!as.symbol(symp[i]), values_from = n, names_prefix = paste0(symp[i], "_")) %>% 
      mutate(term = covar[j]) %>% 
      mutate(groups = as.character(!!as.symbol(covar[j])) ,
             prop = round(!!as.symbol(paste0(symp[i], "_1"))*100/(!!as.symbol(paste0(symp[i], "_1")) + !!as.symbol(paste0(symp[i], "_0"))))) %>% 
        select(-1) %>% 
        bind_cols(df_use %>% 
                    mutate(OR = regressionLogistic(outcome = symp[i], df=df_use, covariates = covar[j], var = covar[j])) %>% 
                    slice(1:2) %>% 
                    select(OR)) %>% 
        mutate(#prop_CIL = round(100*binom::binom.confint(!!as.symbol(paste0(symp[i], "_1")), !!as.symbol(paste0(symp[i], "_1")) + !!as.symbol(paste0(symp[i], "_0")), methods = "ac")$lower),
               #prop_CIU = round(100*binom::binom.confint(!!as.symbol(paste0(symp[i], "_1")), !!as.symbol(paste0(symp[i], "_1")) + !!as.symbol(paste0(symp[i], "_0")), methods = "ac")$upper),
               n = !!as.symbol(paste0(symp[i], "_1")) + !!as.symbol(paste0(symp[i], "_0"))))
      
  }
  tables[[i]] <- 
  table1 %>% 
    arrange(desc(groups)) %>% arrange(factor(term, levels=c("age_group_2", "sex2", "ingen_symp_t0", "reinfeksjon_t2"))) %>% 
    mutate(risk_factor = c("     <16", "     \u2265 16", "     Female", "     Male", "     No", "     Yes", #"Unvaccinated", "Vaccinated", 
                           "     Yes ", "     No "),
           n_N = paste0(round(!!as.symbol(paste0(symp[i], "_1"))*100/(!!as.symbol(paste0(symp[i], "_1")) + !!as.symbol(paste0(symp[i], "_0")))), "% (", !!as.symbol(paste0(symp[i], "_1")), "/", (!!as.symbol(paste0(symp[i], "_1")) + !!as.symbol(paste0(symp[i], "_0"))), ")")) %>% 
    select(term, risk_factor, n_N, OR, n, prop) %>% 
    group_by(term) %>% mutate(OR = if_else(row_number() == n(), "", OR)) %>% 
    rowwise() %>% 
    mutate(OR_val = str_split(OR, " ") %>% unlist() %>% .[1],
           OR_CI = str_split(OR, " ") %>% unlist() %>% .[2],
           p = str_split(OR, " ") %>% unlist() %>% .[3]) %>% 
    mutate(OR_CI = str_remove(OR_CI, "\\("),
           OR_CI = str_remove(OR_CI, "\\)")) %>% 
    mutate(OR_CIL = str_split(OR_CI, "-") %>% unlist() %>% .[1],
           OR_CIU = str_split(OR_CI, "-") %>% unlist() %>% .[2]) %>% 
    ungroup() %>% 
    mutate(p = sprintf("%4.3f", as.numeric(p))) %>% 
    mutate(p = if_else(str_detect(p, "NA"), NA_character_, p)) %>% 
    mutate(across(c(OR_CIL, OR_CIU), ~as.numeric(.x))) %>% 
    mutate(face = case_when(p <= 0.05 ~ "bold",
                            p > 0.05 ~ "plain")) %>% 
    mutate(OR2 = if_else(!is.na(OR_val), paste0(OR_val, " (", OR_CI, ")"), NA_character_)) %>% 
    mutate(OR2= if_else(OR2 == " (NA)", NA_character_, OR2)) %>% 
    add_row(term=NA, risk_factor="Age", n_N=NA, OR=NA, n=NA, prop=NA, OR_val=NA, OR_CI=NA, OR_CIL=NA, OR_CIU=NA, .before=1) %>% 
    add_row(term=NA, risk_factor="Sex", n_N=NA, OR=NA, n=NA, prop=NA, OR_val=NA, OR_CI=NA, OR_CIL=NA, OR_CIU=NA, .before=4) %>% 
    add_row(term=NA, risk_factor="Baseline symp.", n_N=NA, OR=NA, n=NA, prop=NA, OR_val=NA, OR_CI=NA, OR_CIL=NA, OR_CIU=NA, .before=7) %>% 
    add_row(term=NA, risk_factor="Omicron reinf.", n_N=NA, OR=NA, n=NA, prop=NA, OR_val=NA, OR_CI=NA, OR_CIL=NA, OR_CIU=NA, .before=10) %>% 
    mutate(OR_val = as.numeric(OR_val))
  
  
  table1 <- tibble()
}

table_plots <- list()
table_plots2 <- list()
table_plots3 <- list()
forest_plots <- list()

x_axis_min <- -0.1
x_axis_max <- 3.5

size1 <- 3 # 5 previous
size2 <- 9 # 15 previous

for (i in 1:3){
table_plots[[i]] <- ggplot(tables[[i]] %>% mutate(x=1), aes(y = factor(risk_factor, levels = rev(risk_factor )))) +
  geom_text(aes(label=risk_factor, x = x), size = size1, hjust=0) +
  theme_minimal() + 
  xlab("") +
  theme(panel.grid = element_blank(),
        text = element_text(hjust=0, colour="black"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(size = size2, colour="black", face="bold"),
        plot.margin = margin(r=-3, unit = "cm"),
        axis.title.y = element_blank()) +
  xlim(c(1,1.75)) +
  ggtitle("Risk factor") 

table_plots2[[i]] <- ggplot(tables[[i]] %>% mutate(x=1), aes(y = factor(risk_factor, levels = rev(risk_factor )))) +
  geom_text(aes(label=n_N, x = x), size = size1, hjust=0) +
  theme_minimal() + 
  xlab("") +
  theme(panel.grid = element_blank(),
        text = element_text(hjust=0, colour="black"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(size = size2, colour="black", face="bold", hjust=0),
        plot.margin = margin(l = -4, r=-3, unit = "cm"),
        axis.title.y = element_blank()) +
#  scale_x_continuous(expand(add=c(0,-10)))
  xlim(c(1,1.0001)) +
  ggtitle("% (n/N)")

table_plots3[[i]] <- ggplot(tables[[i]] %>% mutate(x=1), aes(y = factor(risk_factor, levels = rev(risk_factor )))) +
  geom_text(aes(label=OR2, x = x), size = size1, hjust=0) +
  geom_text(aes(label=p, x = x+1), size = size1, hjust=0) +
  theme_minimal() + 
  xlab("") +
  theme(panel.grid = element_blank(),
        text = element_text(hjust=0, colour="black"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(size = size2-1, colour="black", face="bold", hjust=0.8),
        plot.margin = margin(l = -4, r=-3, unit = "cm"),
        axis.title.y = element_blank()) +
  xlim(c(1,2.5)) +
  ggtitle("p value")

df_plot <- tables[[i]] %>% 
  mutate(OR_CIL2 = if_else(as.numeric(OR_CIL) < x_axis_min, x_axis_min, as.numeric(OR_CIL)),
         OR_CIU2 = if_else(as.numeric(OR_CIU) > x_axis_max, x_axis_max, as.numeric(OR_CIU))) %>% 
  mutate(arrow_l = if_else(as.numeric(OR_CIL) == OR_CIL2, 0, 1),
         arrow_u = if_else(as.numeric(OR_CIU) == OR_CIU2, 0, 1))
  

forest_plots[[i]] <- ggplot(df_plot, aes(y = factor(risk_factor, levels = rev(risk_factor )), x = as.numeric(OR_val))) +
  geom_point(shape = 18, size = size1) +
  geom_errorbarh(aes(xmin = OR_CIL2, xmax = OR_CIU2), height = 0.25,  alpha = 0.4) +
  geom_vline(xintercept = 1, alpha = 0.7, linetype = "dotted") +
  xlab("") +
  theme_bw() + 
  theme(axis.title = element_text(size = size2),
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x =  element_line(colour = "black"),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = size2 - 2, colour = "black"),
        plot.title = element_text(size = size2, colour="black", face="bold", hjust=0.5),
        panel.grid.major.y = element_line(color = "#f0f0f0", linetype = 2, size = 0.75),
        plot.margin = margin(l = -4, r=-3, unit = "cm"),
        axis.title.y = element_blank()) +
  xlim(c(x_axis_min, x_axis_max)) +
  scale_x_continuous(trans = "log2", breaks = c(0.1, 0.2, 0.5,1,2), limits = c(0.01, 4)) +
  ggtitle("OR (95% CI)")

}

thm <- theme(plot.title = element_text(face = 2, size = size2 + 1))

p1 <- wrap_elements(table_plots[[1]] + table_plots2[[1]] + forest_plots[[1]] + table_plots3[[1]] + plot_layout(ncol=4, widths = c(1.1,1,2,2)) + plot_annotation(title = "a Fatigue", theme = thm))
p2 <- wrap_elements(table_plots[[2]] + table_plots2[[2]] + forest_plots[[2]] + table_plots3[[2]] + plot_layout(ncol=4, widths = c(1.1,1,2,2)) + plot_annotation(title = "b Dyspnea", theme = thm))
p3 <- wrap_elements(table_plots[[3]] + table_plots2[[3]] + forest_plots[[3]] + table_plots3[[3]] + plot_layout(ncol=4, widths = c(1.1,1,2,2)) + plot_annotation(title = "c Cognitive symptoms", theme = thm))

p <- p1 / p2 / p3
p

ggsave(file = here("plots", paste0("forest_plot_panel_", Sys.Date(), ".tiff")), dpi = 300, width = 1574, height = 2428, units = "px")


