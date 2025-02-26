# Authors: Abigail O. Asare and Laura Schürer 

# Date: 26/02/2025

# Load relevant Packages------------------------------------------
library("cowplot")
library("dplyr")
library("data.table")
library("foreign")
library("ggthemes")
library("ggpubr")
library("ggsci")
library("Hmisc")
library("janitor")
library("officer")
library("openxlsx")
library("quantreg")
library("rattle")
library("reshape2")
library("scales")
library("tidyverse")
library("utils")
library("wesanderson")
library("weights")
library("readr")
library("readxl")
library("ggrepel")
library("grid")
library("broom")
library("colorspace")
library("srvyr")
library(survey)
library(ggforce)
library(ggmagnify)
library(ggfx)
library("xtable")
options(scipen = 999)

# Currency is in TSH
# Year = 2020-2021

# Setup -------------------------------------------------------------------
source("./00_setup.R")


# Read Incidence data set --------------------------------------------------

tza_final <-
  read_csv(file.path(dir[["analysis"]], "inci_analysis_tza.csv"))

tza_info <-
  read_csv(file.path(dir[["analysis"]], "hh_information_tza.csv")) |> 
  mutate(Urban=ifelse(urban==1,"Rural","Urban")) |> 
  filter(!is.na(hh_id)) |> 
  filter(!is.na(hh_weights)) |>
  filter(!is.na(hh_size)) 

# Summary Statistics (SS) for NPS 2020 Appendix Table ------------------------------

## Appendix Table A.11 -------------------------------------------
## SS Urban/Rural and IG

Sum_tza_ur_IG <- tza_final |>
  mutate(population = hh_weights*hh_size) |> 
  group_by(Urban, incg_5) |> 
  summarise(units = n(),
            hh_size_mean        = wtd.mean(hh_size, hh_weights),
            hh_size_median      = wtd.quantile(hh_size, hh_weights, 
                                               probs = 0.5),
            hh_size_sd          = sqrt(wtd.var(hh_size, hh_weights)),
            exp_mean   = wtd.mean(hh_exp_USD, hh_weights),
            exp_median = wtd.quantile(hh_exp_USD, 
                                      hh_weights, probs = 0.5),
            exp_sd     = sqrt(wtd.var(hh_exp_USD, 
                                      hh_weights)),
            exp_pc_mean   = wtd.mean(hh_exp_USD_pc, 
                                     hh_weights),
            exp_pc_median = wtd.quantile(hh_exp_USD_pc, 
                                         hh_weights, probs = 0.5),
            exp_pc_sd     = sqrt(wtd.var(hh_exp_USD_pc,
                                         hh_weights))
  )|>
  select(everything())


write.xlsx(Sum_tza_ur_IG ,file.path(dir[["tables"]],
                                    "Appendix_Table_11.xlsx"))


# export as latex
table_latex_1<-xtable(Sum_tza_ur_IG, label="SS_HH_EQ",
                      caption="Summary Statistics of Household 
                      Data for each Expenditure Quintile Uraban and Rural")

print(table_latex_1, file =file.path(dir[["tables"]],"Appendix_Table_11.tex")
)


## Appendix Table A.12 -------------------------------------------
## SS Carbon Foot print by UR and IG

Sum_tza_UR_IG <- tza_final |>
  group_by(Urban, incg_5)|>
  summarise(
    number = n(),
    CO2_intl_mean   = (wtd.mean(tCO2_pc, hh_weights)),
    CO2_intl_median = wtd.quantile(tCO2_pc, hh_weights, probs = 0.5),
    CO2_intl_sd     = sqrt(wtd.var(tCO2_pc, hh_weights)),
    CO2_wthn_mean   = wtd.mean(tCO2_within_pc, hh_weights),
    CO2_wthn_median = wtd.quantile(tCO2_within_pc, hh_weights, probs = 0.5),
    CO2_wthn_sd     = sqrt(wtd.var(tCO2_within_pc, hh_weights))
  )|>
  ungroup()|>
  select(everything())


write.xlsx(Sum_tza_UR_IG ,file.path(dir[["tables"]],"Appendix_Table_A12.xlsx"))

# export as latex
table_latex_1<-xtable(Sum_tza_UR_IG, label="Sum_HHs_footprints_UR_IG",
                      caption="Summary Statistics on Carbon Footprints By 
                      Location and Expenditure Quintiles")

print(table_latex_1, file =file.path(dir[["tables"]],"Appendix_Table_A12.tex")
)


## Appendix Table A.13 -------------------------------------------
## SS Statistics Incidence (burden) by UR and IG

Sum_tza_IC_UR_IG <- tza_final |>
  group_by(Urban, incg_5)|>
  summarise(
    number = n(),
    CO2_intl_mean   = wtd.mean(burden_CO2_pc, hh_weights),
    CO2_intl_median = wtd.quantile(burden_CO2_pc, hh_weights,
                                   probs = 0.5),
    CO2_intl_sd     = sqrt(wtd.var(burden_CO2_pc, hh_weights)),
    CO2_wthn_mean   = wtd.mean(burden_CO2_within_pc, hh_weights),
    CO2_wthn_median = wtd.quantile(burden_CO2_within_pc, hh_weights,
                                   probs = 0.5),
    CO2_wthn_sd     = sqrt(wtd.var(burden_CO2_within_pc, hh_weights))
  )|>
  ungroup()|>
  select(everything())

write.xlsx(Sum_tza_IC_UR_IG ,file.path(dir[["tables"]],"Appendix_Table_A13.xlsx"))

# export as latex
table_latex_1<-xtable(Sum_tza_IC_UR_IG , label="SS_HHs_incidences_UR_IG",
                      caption="Summary Statistics of 
                      Distributional Incidence by Location 
                      and Expenditure Qintals")

print(table_latex_1, file =file.path(dir[["tables"]],"Appendix_Table_A13.tex")
)



rm(list = ls())













