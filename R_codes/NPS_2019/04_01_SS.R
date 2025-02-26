# Authors: Abigail O. Asare and  Laura Schürer

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
# Year = 2019-2020

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



# Summary Statistics Table Appendix ------------------------------

## Appendix Table A2 -------------------------------------------
#  SS Urban/Rural and IG 

Sum_tza_ur_IG <- tza_final |>
  mutate(population = hh_weights*hh_size)|> 
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
                                    "Appendix_Table_A2.xlsx"))


# export as latex
table_latex_1<-xtable(Sum_tza_ur_IG, label="SS_HH_EQ",
                      caption="Summary Statistics of Household 
                      Data for each Expenditure Quintile Uraban and Rural")

print(table_latex_1, file =file.path(dir[["tables"]],"Appendix_Table_A2.tex")
)


## Appendix Table A3 -------------------------------------------
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


write.xlsx(Sum_tza_UR_IG ,file.path(dir[["tables"]],"Appendix_Table_A3.xlsx"))

# export as latex
table_latex_1<-xtable(Sum_tza_UR_IG, label="Sum_HHs_footprints_UR_IG",
                      caption="Summary Statistics on Carbon Footprints By 
                      Location and Expenditure Quintiles")

print(table_latex_1, file =file.path(dir[["tables"]],"Appendix_Table_A3.tex")
)


## Appendix Table A4 -------------------------------------------
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

write.xlsx(Sum_tza_IC_UR_IG ,file.path(dir[["tables"]],"Appendix_Table_A4.xlsx"))

# export as latex
table_latex_1<-xtable(Sum_tza_IC_UR_IG , label="SS_HHs_incidences_UR_IG",
                      caption="Summary Statistics of 
                      Distributional Incidence by Location 
                      and Expenditure Qintals")

print(table_latex_1, file =file.path(dir[["tables"]],"Appendix_Table_A4.tex")
)

# Appendix Table A9 -------------------------------------------
gtap_hh<-read_excel("./data-raw/GTAP_matching_sectors.xlsx", 
                    sheet = "matching_edited", skip=1) 

gtap_hh <- gtap_hh |>
  select("GTAP sector","orig. code","unam. code",  "description...5") |>
  rename(itemcode = "unam. code",
         orig.code = "orig. code",
         definition="description...5",
         GTAP = "GTAP sector") |> 
  filter(!(is.na(itemcode))) |> 
  mutate(definition = str_to_title(definition))


# export as latex
table_latex_1<-xtable(gtap_hh, label="match_itemcodes",
                      caption=" HH itemcodes with GTAP Sectors")

print(table_latex_1, file =file.path(dir[["tables"]],"match_itemcodes.tex")
)


# Appendix Table A8 -------------------------------------------

## Gtap Sectors------------------------------------

gtap_sectors<-
  read_excel("./data-raw/GTAP_matching_sectors.xlsx", 
             sheet = "matching_tza", skip=1) 



# Select Variables needed and delete sectors with no match

gtap_sectors <- gtap_sectors|>
  select("GTAP sector", "description...2") |>
  rename(
    GTAP = "GTAP sector",
    Explanation = "description...2") |> 
  distinct(GTAP, .keep_all=T) |> 
  mutate(GTAP = ifelse(GTAP == "gas" | GTAP == "gdt", "gasgdt", GTAP)) |> 
  filter(!(is.na(GTAP)|GTAP=="other"|GTAP=="deleted")) |> 
  distinct(GTAP, .keep_all=T) 


## Read Carbon intensities ------------------------------------------------------
# CO2 are in tonnes per USD (CO2_t_per_dollar)

## International/Global Carbon intensities ----------------------------------
intensity_int <-
  read_excel("./data-raw/hh-CO2-intensities-11-int.xlsx") |>
  rename(GTAP="...1",
         CO2_t=TZA) |> 
  select(GTAP,CO2_t) 

## National Carbon intensities ----------------------------------
intensity_nat <-
  read_excel("./data-raw/hh-CO2-intensities-TZA.xlsx") |>
  rename(GTAP="...1",
         CO2_t_within=TZA) |> 
  select(GTAP,CO2_t_within) 



## Merge Intensities ----------------------------------------------

carbon_intensities <- intensity_int |>
  left_join(intensity_nat, by = "GTAP", relationship = "one-to-one")

rm(intensity_int,intensity_nat)


test<- carbon_intensities |>
  left_join(gtap_sectors, by = "GTAP", relationship = "one-to-one") |> 
  relocate(GTAP, Explanation)

test<- test |>
  mutate(CO2_t=CO2_t*1000,
         CO2_t_within=CO2_t_within*1000)

# Keep only merged sectors

test2 <- test |> 
  filter(GTAP %in% gtap_hh$GTAP|GTAP=="gasgdt")
# note that gas and gdt is not combine in the gtap_hh data do not filter it out

# export as latex
table_latex_1<-xtable(test2 , label="GTAP_explanation_intensities",
                      caption="Sectoral carbon intensities in tCO2 per USD")

print(table_latex_1, file =file.path(dir[["tables"]],"Appendix_Table_A8.tex")
)




rm(list = ls())













