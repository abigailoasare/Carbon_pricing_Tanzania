Authors: Abigail O. Asare and Laura Schürer

# Date:26/02/2025

# Sys.getlocale()

# Load relevant Packages------------------------------------------
library("data.table")
library("foreign")
library("ggsci")
library("haven")
library("dplyr")
library("Hmisc")
library("expss")
library("ggplot2")
library("officer")
library("openxlsx")
library("rattle")
library("reshape2")
library("scales")
library("tidyverse")
library("srvyr")
library("tidyr")
library("readr")
library("readxl")

# Currency is in TSH
# Year = 2019


# Setup -------------------------------------------------------------------
source("./00_setup.R")


Urban_New <- data.frame("urban" = c(1,2), "Urban" = c("Rural", "Urban"))    

# HH data ----------------------------------------------------------------

tza_info <-
  read_csv(file.path(dir[["tza_processed"]], "clean_tanzania_2020.csv"))|> 
  mutate(Urban=ifelse(urban==1,"Rural","Urban"))|> 
  filter(!is.na(hh_id))|> 
  filter(!is.na(hh_weights))|>
  filter(!is.na(hh_size))|> 
  relocate(hh_id, hh_weights,urban, Urban)


tza_IG <-tza_info|> 
  dplyr::group_by(hh_id)|>
  dplyr::mutate(hh_exp_0   = sum(expenditures,na.rm = T))|>
  dplyr::ungroup()|>
  dplyr::mutate(hh_exp_0_pc = hh_exp_0 / hh_size)|>
  dplyr::select(hh_id,
                hh_exp_0,
                hh_exp_0_pc,
                hh_weights,
                Urban,
                hh_size)|>
  dplyr::filter(!duplicated(hh_id))|>
  dplyr::mutate(incg_5   = as.numeric(
    rattle::binning(
      hh_exp_0_pc,
      bins = 5,
      method = c("wtd.quantile"),
      labels = seq(1, 5, length.out = 5),
      weights = hh_weights
    )
  ))|>
  dplyr::mutate(incg_10  = as.numeric(
    rattle::binning(
      hh_exp_0_pc,
      bins = 10,
      method = c("wtd.quantile"),
      labels = seq(1, 10, length.out = 10),
      weights = hh_weights
    )
  )) 

write_csv(
  tza_IG,
  file.path(dir[["analysis"]],"tza_IG.csv")
)

# save HH Information
tza_infras <-tza_info|> 
  select(1:31)|> 
  distinct(hh_id,.keep_all = T)

write_csv(
  tza_infras,
  file.path(dir[["analysis"]],"hh_information_tza.csv")
)

# Income Group 5

tza_infras <-tza_infras|> 
  select(-hh_weights, -hh_size,-Urban)|> 
  left_join(tza_IG,by="hh_id") 

# Electricity Access ----------------------------------------------------------

# Count the number of individuals with access to infrastructure IG
tza_infras_svy <- tza_infras|>
  as_survey_design(weights = hh_weights)

tza_infras_IG5 <- tza_infras_svy|>
  group_by(incg_5)|>
  summarise(
    access_ely_bd = survey_mean(access_ely_bd, na.rm = TRUE),
    access_ely_st = survey_mean(access_ely_st, na.rm = TRUE),
    access_ely_grid = survey_mean(access_ely_grid, na.rm = TRUE),
    access_ely_solar = survey_mean(access_ely_solar, na.rm = TRUE),
    access_ely_other = survey_mean(access_ely_other, na.rm = TRUE),
    access_sanit_st = survey_mean(access_sanit_st, na.rm = TRUE),
    access_water_st = survey_mean(access_water_st, na.rm = TRUE)
  )|>
  ungroup() |> 
  dplyr::select(-ends_with("_se"))


write_csv(
  tza_infras_IG5,
  file.path(dir[["analysis"]],"hh_infrastructure_IG5_tza.csv")
)

# Count the number of individuals with access to infrastructure UR-IG

tza_infras_IG5UR <-tza_infras_svy|> 
  group_by(Urban,incg_5)|> 
  summarise(
    access_ely_bd = survey_mean(access_ely_bd, na.rm = TRUE),
    access_ely_st = survey_mean(access_ely_st, na.rm = TRUE),
    access_ely_grid = survey_mean(access_ely_grid, na.rm = TRUE),
    access_ely_solar = survey_mean(access_ely_solar, na.rm = TRUE),
    access_ely_other = survey_mean(access_ely_other, na.rm = TRUE),
    access_sanit_st = survey_mean(access_sanit_st, na.rm = TRUE),
    access_water_st = survey_mean(access_water_st, na.rm = TRUE)
  )|>
  ungroup() |> 
  dplyr::select(-ends_with("_se"))




write_csv(
  tza_infras_IG5UR ,
  file.path(dir[["analysis"]],"hh_infrastructure_IG5UR_tza.csv")
)


# Cooking Fuel Types ----------------------------------------------------------

cooking_code <-
  data.frame(
    "cooking_fuel" = c(seq(1:8)),
    "cooking_Label" = c(
      "firewood",
      "paraffin",
      "electricity",
      "gas",
      "charcoal",
      "ANIMAL RESIDUAL",
      "GAS (BIOGAS)",
      "others"
    )
  )

# No. of households with different types of cooking fuels IG
tza_cook_IG5 <-tza_infras|> 
  left_join(cooking_code, by=c("cooking_fuel"="cooking_fuel"))|> 
  dplyr::mutate(cooking_Label=ifelse(cooking_Label=="paraffin","others",
                                     cooking_Label)
  )

tza_cook_IG5_svy <- tza_cook_IG5 |>
  as_survey_design(weights = hh_weights)

tza_cook_IG5 <-tza_cook_IG5_svy|> 
  group_by(incg_5,cooking_Label)|>
  summarise(num_households = n()
  )%>% 
  ungroup()|> 
  group_by(incg_5)|>
  mutate(share = num_households / sum(num_households,na.rm = TRUE))|> 
  ungroup()|> 
  dplyr::select(-num_households)


write_csv(
  tza_cook_IG5,
  file.path(dir[["analysis"]],"hh_cooking_IG5_tza.csv")
)

# Count the number of individuals cooking fuel type UR-IG

# No. of households with different types of cooking fuels IG

tza_cook_IG5UR <-tza_infras|> 
  left_join(cooking_code, by=c("cooking_fuel"="cooking_fuel"))|> 
  dplyr::mutate(cooking_Label=ifelse(cooking_Label=="paraffin","others",
                                     cooking_Label)
  )

tza_cook_IG5UR_svy <- tza_cook_IG5UR |>
  as_survey_design(weights = hh_weights)

tza_cook_IG5UR <-tza_cook_IG5UR_svy|> 
  group_by(Urban,incg_5,cooking_Label)|>
  summarise(num_households = n()
  )%>% 
  ungroup()|> 
  group_by(Urban,incg_5)|>
  mutate(share = num_households / sum(num_households,na.rm = TRUE))|> 
  ungroup()|> 
  dplyr::select(-num_households)



unique(tza_cook_IG5UR$cooking_Label)

write_csv(
  tza_cook_IG5UR ,
  file.path(dir[["analysis"]],"hh_cooking_IG5UR_tza.csv")
)



# Lighting Fuel Types ----------------------------------------------------------
unique(tza_infras$lighting_fuel)

lighting_code <-
  data.frame(
    "lighting_fuel" = c(seq(1:10)),
    "lighting_Label" = c(
      "electricity",
      "solar",
      "gas",
      "gas (biogas)",
      "lamp oil",
      "candle",
      "firewood",
      "private generator",
      "torch",
      "others"
    )
  )
# No. of households with different types of lighting fuels IG
tza_lighting_IG5 <-tza_infras|> 
  left_join(lighting_code, by=c("lighting_fuel"="lighting_fuel"))|>
  dplyr::mutate(lighting_Label=ifelse(lighting_Label=="candle","others",
                                      lighting_Label),
                lighting_Label=ifelse(lighting_Label=="firewood","others",
                                      lighting_Label),
                lighting_Label=ifelse(lighting_Label=="torch","others",
                                      lighting_Label)
  )

tza_lighting_IG5_svy <- tza_lighting_IG5 |>
  as_survey_design(weights = hh_weights)

tza_lighting_IG5 <-tza_lighting_IG5_svy |> 
  group_by(incg_5,lighting_Label)|>
  summarise(num_households = n()
  )%>% 
  ungroup()|> 
  group_by(incg_5)|>
  mutate(share = num_households / sum(num_households,na.rm = TRUE))|> 
  ungroup()|> 
  dplyr::select(-num_households)



unique(tza_lighting_IG5$lighting_Label)

write_csv(
  tza_lighting_IG5,
  file.path(dir[["analysis"]],"hh_lighting_IG5_tza.csv")
)

# Count the number of individuals lighting fuel type UR-IG

# No. of households with different types of lighting fuels IG
tza_lighting_IG5UR <-tza_infras|> 
  left_join(lighting_code, by=c("lighting_fuel"="lighting_fuel"))|> 
  dplyr::mutate(lighting_Label=ifelse(lighting_Label=="candle","others",
                                      lighting_Label),
                lighting_Label=ifelse(lighting_Label=="firewood","others",
                                      lighting_Label),
                lighting_Label=ifelse(lighting_Label=="torch","others",
                                      lighting_Label)
  )

tza_lighting_IG5UR_svy <- tza_lighting_IG5UR |>
  as_survey_design(weights = hh_weights)

tza_lighting_IG5UR <- tza_lighting_IG5UR_svy |> 
  group_by(Urban,incg_5,lighting_Label)|>
  summarise(num_households = n()
  )%>% 
  ungroup()|> 
  group_by(Urban,incg_5)|>
  mutate(share = num_households / sum(num_households,na.rm = TRUE))|> 
  ungroup()|> 
  dplyr::select(-num_households)



write_csv(
  tza_lighting_IG5UR ,
  file.path(dir[["analysis"]],"hh_lighting_IG5UR_tza.csv")
)

# Electricity Types ----------------------------------------------------------
unique(tza_infras$electricity_typ)

electricity_code <-
  data.frame(
    "electricity_typ" = c(1:7,NA),
    "electricity_Label" = c(
      "tanesco",
      "community generator",
      "solar panels",
      "own generator",
      "car battery",
      "motorcycle",
      "battery",
      "others"
    )
  )
# No. of households with different types of electricity fuels IG
tza_electricity_IG5 <-tza_infras|> 
  left_join(electricity_code, by=c("electricity_typ"="electricity_typ"))|> 
  dplyr::mutate(electricity_Label=ifelse(electricity_Label=="tanesco","grid",
                                         electricity_Label),
                electricity_Label=ifelse(electricity_Label=="car battery",
                                         "others",
                                         electricity_Label),
                electricity_Label=ifelse(electricity_Label=="battery",
                                         "others",
                                         electricity_Label),
                electricity_Label=ifelse(electricity_Label=="community generator",
                                         "others",
                                         electricity_Label)
  )

tza_electricity_IG5_svy <- tza_electricity_IG5 |>
  as_survey_design(weights = hh_weights)

tza_electricity_IG5<- tza_electricity_IG5_svy |> 
  group_by(incg_5,electricity_Label)|>
  summarise(num_households = n()
  )%>% 
  ungroup()|> 
  group_by(incg_5)|>
  mutate(share = num_households / sum(num_households,na.rm = TRUE))|> 
  ungroup()|> 
  dplyr::select(-num_households)


unique(tza_electricity_IG5$electricity_Label)


write_csv(
  tza_electricity_IG5,
  file.path(dir[["analysis"]],"hh_electricity_IG5_tza.csv")
)

# Count the number of individuals electricity fuel type UR-IG

# No. of households with different types of electricity fuels IG
tza_electricity_IG5UR <-tza_infras|> 
  left_join(electricity_code, by=c("electricity_typ"="electricity_typ"))|> 
  dplyr::mutate(electricity_Label=ifelse(electricity_Label=="tanesco","grid",
                                         electricity_Label),
                electricity_Label=ifelse(electricity_Label=="car battery",
                                         "battery",
                                         electricity_Label),
                electricity_Label=ifelse(electricity_Label=="community generator",
                                         "others",
                                         electricity_Label)
  )

tza_electricity_IG5UR_svy <- tza_electricity_IG5UR |>
  as_survey_design(weights = hh_weights)

tza_electricity_IG5UR <- tza_electricity_IG5UR_svy |> 
  group_by(Urban,incg_5,electricity_Label)|>
  summarise(num_households = n()
  )%>% 
  ungroup()|> 
  group_by(Urban,incg_5)|>
  mutate(share = num_households / sum(num_households,na.rm = TRUE))|> 
  ungroup()|> 
  dplyr::select(-num_households)


unique(tza_electricity_IG5UR$electricity_Label)


write_csv(
  tza_electricity_IG5UR ,
  file.path(dir[["analysis"]],"hh_electricity_IG5UR_tza.csv")
)

# Drinking Water Types ----------------------------------------------------------
unique(tza_infras$drinking_water_typ)

water_code <-
  data.frame(
    "drinking_water_typ" = c(1:12),
    "drinking_water_Label" = c(
      "piped water",
      "tubewell/borehole",
      "protected dug well",
      "unprotected dugwell",
      "protected spring",
      "unprotected spring",
      "rainwater collection",
      "bottled water",
      "cart with small tank/drum",
      "tanker-truck",
      "surface water (river, dam, lake, pond, stream, canal, irrigation channels)",
      "others"
    )
  )
# No. of households with different types of drinking_water_typ IG
tza_dwater_IG5 <-tza_infras|> 
  left_join(water_code, by=c("drinking_water_typ"="drinking_water_typ"))|>
  dplyr::mutate(drinking_water_Label=ifelse(drinking_water_Label=="protected dug well",
                                            "tubewell/borehole",
                                            drinking_water_Label),
                drinking_water_Label=ifelse(drinking_water_Label=="unprotected dugwell",
                                            "tubewell/borehole",
                                            drinking_water_Label),
                drinking_water_Label=ifelse(drinking_water_Label=="protected spring",
                                            "others",
                                            drinking_water_Label),
                drinking_water_Label=ifelse(drinking_water_Label=="unprotected spring",
                                            "others",
                                            drinking_water_Label)
  )|> 
  dplyr::mutate(drinking_water_Label=ifelse(drinking_water_Label=="rainwater collection",
                                            "others",
                                            drinking_water_Label),
                drinking_water_Label=ifelse(drinking_water_Label=="cart with small tank/drum",
                                            "others",
                                            drinking_water_Label),
                drinking_water_Label=ifelse(drinking_water_Label=="tanker-truck",
                                            "others",
                                            drinking_water_Label),
                drinking_water_Label=ifelse(drinking_water_Label=="surface water (river, dam, lake, pond, stream, canal, irrigation channels)",
                                            "others",
                                            drinking_water_Label)
  )

tza_dwater_IG5_svy <- tza_dwater_IG5 |>
  as_survey_design(weights = hh_weights)

tza_dwater_IG5 <- tza_dwater_IG5_svy |> 
  group_by(incg_5,drinking_water_Label)|>
  summarise(num_households = n()
  )%>% 
  ungroup()|> 
  group_by(incg_5)|>
  mutate(share = num_households / sum(num_households,na.rm = TRUE))|> 
  ungroup()|> 
  dplyr::select(-num_households)


unique(tza_dwater_IG5$drinking_water_Label)


write_csv(
  tza_dwater_IG5,
  file.path(dir[["analysis"]],"hh_dwater_IG5_tza.csv")
)

# Count the number of individuals drinking_water_typ type UR-IG

# No. of households with different types of drinking_water_typ IG
tza_dwater_IG5UR <-tza_infras|> 
  left_join(water_code, by=c("drinking_water_typ"="drinking_water_typ"))|> 
  dplyr::mutate(drinking_water_Label=ifelse(drinking_water_Label=="protected dug well",
                                            "tubewell/borehole",
                                            drinking_water_Label),
                drinking_water_Label=ifelse(drinking_water_Label=="unprotected dugwell",
                                            "tubewell/borehole",
                                            drinking_water_Label),
                drinking_water_Label=ifelse(drinking_water_Label=="protected spring",
                                            "others",
                                            drinking_water_Label),
                drinking_water_Label=ifelse(drinking_water_Label=="unprotected spring",
                                            "others",
                                            drinking_water_Label)
  )|> 
  dplyr::mutate(drinking_water_Label=ifelse(drinking_water_Label=="rainwater collection",
                                            "others",
                                            drinking_water_Label),
                drinking_water_Label=ifelse(drinking_water_Label=="cart with small tank/drum",
                                            "others",
                                            drinking_water_Label),
                drinking_water_Label=ifelse(drinking_water_Label=="tanker-truck",
                                            "others",
                                            drinking_water_Label),
                drinking_water_Label=ifelse(drinking_water_Label=="surface water (river, dam, lake, pond, stream, canal, irrigation channels)",
                                            "others",
                                            drinking_water_Label)
  )

tza_dwater_IG5UR_svy <- tza_dwater_IG5UR |>
  as_survey_design(weights = hh_weights)

tza_dwater_IG5UR <- tza_dwater_IG5UR_svy |> 
  group_by(Urban,incg_5,drinking_water_Label)|>
  summarise(num_households = n()
  )%>% 
  ungroup()|> 
  group_by(Urban,incg_5)|>
  mutate(share = num_households / sum(num_households,na.rm = TRUE))|> 
  ungroup()|> 
  dplyr::select(-num_households)


unique(tza_dwater_IG5UR$drinking_water_Label)


write_csv(
  tza_dwater_IG5UR ,
  file.path(dir[["analysis"]],"hh_dwater_IG5UR_tza.csv")
)


# Other Water Types ----------------------------------------------------------
unique(tza_infras$other_water_typ)

water_code <-
  data.frame(
    "drinking_water_typ" = c(1:12),
    "drinking_water_Label" = c(
      "piped water",
      "tubewell/borehole",
      "protected dug well",
      "unprotected dugwell",
      "protected spring",
      "unprotected spring",
      "rainwater collection",
      "bottled water",
      "cart with small tank/drum",
      "tanker-truck",
      "surface water (river, dam, lake, pond, stream, canal, irrigation channels)",
      "others"
    )
  )
# No. of households with different types of other_water_typ IG
tza_owater_IG5 <-tza_infras|> 
  left_join(water_code, by=c("other_water_typ"="drinking_water_typ"))|> 
  dplyr::mutate(drinking_water_Label=ifelse(drinking_water_Label=="protected dug well",
                                            "tubewell/borehole",
                                            drinking_water_Label),
                drinking_water_Label=ifelse(drinking_water_Label=="unprotected dugwell",
                                            "tubewell/borehole",
                                            drinking_water_Label),
                drinking_water_Label=ifelse(drinking_water_Label=="protected spring",
                                            "others",
                                            drinking_water_Label),
                drinking_water_Label=ifelse(drinking_water_Label=="unprotected spring",
                                            "others",
                                            drinking_water_Label)
  )|> 
  dplyr::mutate(drinking_water_Label=ifelse(drinking_water_Label=="rainwater collection",
                                            "others",
                                            drinking_water_Label),
                drinking_water_Label=ifelse(drinking_water_Label=="cart with small tank/drum",
                                            "others",
                                            drinking_water_Label),
                drinking_water_Label=ifelse(drinking_water_Label=="tanker-truck",
                                            "others",
                                            drinking_water_Label),
                drinking_water_Label=ifelse(drinking_water_Label=="surface water (river, dam, lake, pond, stream, canal, irrigation channels)",
                                            "others",
                                            drinking_water_Label)
  )

tza_owater_IG5_svy <- tza_owater_IG5 |>
  as_survey_design(weights = hh_weights)

tza_owater_IG5 <- tza_owater_IG5_svy |> 
  group_by(incg_5,drinking_water_Label)|>
  summarise(num_households = n()
  )%>% 
  ungroup()|> 
  group_by(incg_5)|>
  mutate(share = num_households / sum(num_households,na.rm = TRUE))|> 
  ungroup()|> 
  dplyr::select(-num_households)


unique(tza_owater_IG5 $drinking_water_Label)


write_csv(
  tza_owater_IG5,
  file.path(dir[["analysis"]],"hh_owater_IG5_tza.csv")
)

# Count the number of individuals other_water_typ type UR-IG

# No. of households with different types of other_water_typ IG
tza_owater_IG5UR <-tza_infras|> 
  left_join(water_code, by=c("other_water_typ"="drinking_water_typ"))|> 
  dplyr::mutate(drinking_water_Label=ifelse(drinking_water_Label=="protected dug well",
                                            "tubewell/borehole",
                                            drinking_water_Label),
                drinking_water_Label=ifelse(drinking_water_Label=="unprotected dugwell",
                                            "tubewell/borehole",
                                            drinking_water_Label),
                drinking_water_Label=ifelse(drinking_water_Label=="protected spring",
                                            "others",
                                            drinking_water_Label),
                drinking_water_Label=ifelse(drinking_water_Label=="unprotected spring",
                                            "others",
                                            drinking_water_Label)
  )|> 
  dplyr::mutate(drinking_water_Label=ifelse(drinking_water_Label=="rainwater collection",
                                            "others",
                                            drinking_water_Label),
                drinking_water_Label=ifelse(drinking_water_Label=="cart with small tank/drum",
                                            "others",
                                            drinking_water_Label),
                drinking_water_Label=ifelse(drinking_water_Label=="tanker-truck",
                                            "others",
                                            drinking_water_Label),
                drinking_water_Label=ifelse(drinking_water_Label=="surface water (river, dam, lake, pond, stream, canal, irrigation channels)",
                                            "others",
                                            drinking_water_Label)
  )

tza_owater_IG5UR_svy <- tza_owater_IG5UR |>
  as_survey_design(weights = hh_weights)

tza_owater_IG5UR <- tza_owater_IG5UR_svy |> 
  group_by(Urban,incg_5,drinking_water_Label)|>
  summarise(num_households = n()
  )%>% 
  ungroup()|> 
  group_by(Urban,incg_5)|>
  mutate(share = num_households / sum(num_households,na.rm = TRUE))|> 
  ungroup()|> 
  dplyr::select(-num_households)


unique(tza_owater_IG5UR $drinking_water_Label)


write_csv(
  tza_owater_IG5UR ,
  file.path(dir[["analysis"]],"hh_owater_IG5UR_tza.csv")
)

# Toilet Types ----------------------------------------------------------
unique(tza_infras$toilet_typ)

toilet_code <-
  data.frame(
    "toilet_typ" = c(1:9),
    "toilet_Label" = c(
      "NO TOILET",
      "PIT LATRINE WITHOUT SLAB/OPEN",
      "PIT PIT LATRINE WITH SLAB (NOT WASHABLE)",
      "PIT LATRINE WITH SLAB (WASHABLE)",
      "vip",
      "POUR FLUSH",
      "FLUSH TOILET",
      "ecosan",
      "OTHERS"
    )
  )
# No. of households with different types of toilet_typ IG
tza_toilet_IG5 <-tza_infras|> 
  left_join(toilet_code, by=c("toilet_typ"="toilet_typ"))|>
  dplyr::mutate(toilet_Label=ifelse(toilet_Label=="PIT LATRINE WITHOUT SLAB/OPEN",
                                    "PIT LATRINE",
                                    toilet_Label),
                toilet_Label=ifelse(toilet_Label=="PIT PIT LATRINE WITH SLAB (NOT WASHABLE)",
                                    "PIT LATRINE",
                                    toilet_Label),
                toilet_Label=ifelse(toilet_Label=="PIT LATRINE WITH SLAB (WASHABLE)",
                                    "PIT LATRINE",
                                    toilet_Label),
                toilet_Label=ifelse(toilet_Label=="vip",
                                    "OTHERS",
                                    toilet_Label),
                toilet_Label=ifelse(toilet_Label=="POUR FLUSH",
                                    "OTHERS",
                                    toilet_Label),
                toilet_Label=ifelse(toilet_Label=="ecosan",
                                    "OTHERS",
                                    toilet_Label)
  )

tza_toilet_IG5_svy <- tza_toilet_IG5 |>
  as_survey_design(weights = hh_weights)

tza_toilet_IG5 <- tza_toilet_IG5_svy |> 
  group_by(incg_5,toilet_Label)|>
  summarise(num_households = n()
  )%>% 
  ungroup()|> 
  group_by(incg_5)|>
  mutate(share = num_households / sum(num_households,na.rm = TRUE))|> 
  ungroup()|> 
  dplyr::select(-num_households)


unique(tza_toilet_IG5$toilet_Label)


write_csv(
  tza_toilet_IG5,
  file.path(dir[["analysis"]],"hh_toilet_IG5_tza.csv")
)

# Count the number of individuals toilet_typ type UR-IG

# No. of households with different types of toilet_typ IG
tza_toilet_IG5UR <-tza_infras|> 
  left_join(toilet_code, by=c("toilet_typ"="toilet_typ"))|> 
  dplyr::mutate(toilet_Label=ifelse(toilet_Label=="PIT LATRINE WITHOUT SLAB/OPEN",
                                    "PIT LATRINE",
                                    toilet_Label),
                toilet_Label=ifelse(toilet_Label=="PIT PIT LATRINE WITH SLAB (NOT WASHABLE)",
                                    "PIT LATRINE",
                                    toilet_Label),
                toilet_Label=ifelse(toilet_Label=="PIT LATRINE WITH SLAB (WASHABLE)",
                                    "PIT LATRINE",
                                    toilet_Label),
                toilet_Label=ifelse(toilet_Label=="vip",
                                    "OTHERS",
                                    toilet_Label),
                toilet_Label=ifelse(toilet_Label=="POUR FLUSH",
                                    "OTHERS",
                                    toilet_Label),
                toilet_Label=ifelse(toilet_Label=="ecosan",
                                    "OTHERS",
                                    toilet_Label)
  )

tza_toilet_IG5UR_svy <- tza_toilet_IG5UR |>
  as_survey_design(weights = hh_weights)

tza_toilet_IG5UR <- tza_toilet_IG5UR_svy |> 
  group_by(Urban,incg_5,toilet_Label)|>
  summarise(num_households = n()
  )%>% 
  ungroup()|> 
  group_by(Urban,incg_5)|>
  mutate(share = num_households / sum(num_households,na.rm = TRUE))|> 
  ungroup()|> 
  dplyr::select(-num_households)


unique(tza_toilet_IG5$toilet_Label)


write_csv(
  tza_toilet_IG5UR ,
  file.path(dir[["analysis"]],"hh_toilet_IG5UR_tza.csv")
)

rm(list = ls(pattern = "_IG5"))
rm(list = ls(pattern = "_code"))
rm(list = ls(pattern = "_svy"))

# Exchange rate and CPI Prep --------------------------------------------------

exchange_rate_all <-
  read_csv(file.path(dir[["other"]], "exchange_rate_Data.csv"), na = "..")

exchange_rate <-  exchange_rate_all|>
  dplyr::select(
    Time,
    "Country Name",
    "Country Code",
    "Official exchange rate (LCU per US$, period average) [PA.NUS.FCRF]"
  )|>
  dplyr::rename(
    year = Time,
    country_name = "Country Name",
    country_code = "Country Code",
    exchange_rate = "Official exchange rate (LCU per US$, period average) [PA.NUS.FCRF]"
  )|>
  dplyr::filter(year == 2017 & country_name=="Tanzania")

# Get the exchange rate in US dollars

rate <- 1/exchange_rate$exchange_rate


# CPI data set Adjustment (Inflation/Deflation)

cpi <-
  read_excel(file.path(dir[["other"]],"Consumer_Price_Index_CPI_Inflation_Average.xlsx"))

cpi_0 <-  cpi|>
  dplyr::rename(country_name = "...1")|> 
  select(country_name, starts_with("2")) 


## Keep only Tanzania ----------------------------------------------
cpi_0 <-  cpi_0|>
  dplyr::filter(country_name == "Tanzania, United Rep. of")

Year_0<-2019

# Calculate inflation factor
cpi_1 <- cpi_0|>
  mutate_at(vars('2011':'2022'), function(x)
    x = as.numeric(x))|>
  mutate_at(vars('2011':'2022'), function(x)
    x = 1 + x / 100)|>
  rename_at(vars(starts_with("2")), list( ~ str_replace(., "^", "Year_")))|>
  mutate(inflation_factor =ifelse(Year_0 == 2019,
                                  (1/(Year_2018*Year_2019)),
                                  0))


inflation_factor <- cpi_1$inflation_factor[cpi_1$country_name ==
                                             "Tanzania, United Rep. of"]

# GTAP Concordance file --------------------------------------------------

gtap_match <-
  read_excel(file.path(dir[["other"]], "GTAP_matching_sectors.xlsx"), 
             sheet = "matching_edited", skip=1)




# Select Variables needed and delete sectors with no match

gtap_match <- gtap_match|>
  select("GTAP sector", "orig. code","unam. code")|>
  rename(itemcode = "unam. code",
         orig.code = "orig. code",
         GTAP = "GTAP sector")|> 
  #mutate(GTAP = ifelse(GTAP == "gas" | GTAP == "gdt", "gasgdt", GTAP))|> 
  filter(!(is.na(orig.code))) 

## Sector Checks  ---------------------------------------------------------------
# checking if items belong two sectors
# note original codes in tza have similar codes therefore we prefix with f and n
# therefore use item_code instead of original code

gtap_check <- count(gtap_match, itemcode)|>
  filter(n != 1)

if (nrow(gtap_check) != 0)
  (paste(
    "WARNING! Item-Codes existing with two different GTAP-categories in Excel-File"
  ))

rm(gtap_check)

## GTAP11  ---------------------------------------------------------------

gtap <-read_excel(file.path(dir[["other"]], "GTAP11.xlsx"))%>% 
  select(GTAP)|> 
  distinct()|> 
  filter(GTAP!="deleted")|> 
  mutate(GTAP = ifelse(GTAP == "gas" | GTAP == "gdt", "gasgdt", GTAP))


# Carbon Emissions  ----------------------------------

## International/Global Carbon Emissions ----------------------------------
emis_int <-
  read_excel(file.path(dir[["other"]], "hh-CO2-emissions-11-int.xlsx"))%>%
  rename(GTAP="...1",
         CO2_emi_t=TZA)|> 
  select(GTAP,CO2_emi_t) 



## National Carbon Emissions ----------------------------------
emis_nat <-
  read_excel(file.path(dir[["other"]], "hh-CO2-emissions-TZA.xlsx"))|>
  rename(GTAP="...1",
         CO2_emi_t_within=TZA)|> 
  select(GTAP,CO2_emi_t_within) 


## Electricity Sector Carbon Emissions ----------------------------------
emis_ely <-
  read_excel(file.path(dir[["other"]], "hh-CO2-emissions-ELY.xlsx"))|>
  rename(GTAP="...1",
         CO2_emi_t_ely=TZA)|> 
  select(GTAP,CO2_emi_t_ely) 



## Transport Sector Carbon Emissions ----------------------------------
emis_trs <-
  read_excel(file.path(dir[["other"]], "hh-CO2-emissions-fuel.xlsx"))|>
  rename(GTAP="...1",
         CO2_emi_t_trs=TZA)|> 
  select(GTAP,CO2_emi_t_trs) 



## Merge All Emissions ----------------------------------------------

carbon_emissions <- emis_int|>
  left_join(emis_nat, by = "GTAP", relationship = "one-to-one")%>%
  left_join(emis_ely, by = "GTAP", relationship = "one-to-one")%>%
  left_join(emis_trs, by = "GTAP", relationship = "one-to-one")

rm(emis_int,emis_nat,emis_ely,emis_trs)

## Save Analysis ---------------------------------------
write_csv(carbon_emissions,
          file.path(dir[["analysis"]],"carbon_emissions_tza.csv"))

# Carbon intensities ------------------------------------------------------
# CO2 are in tonnes per USD (CO2_t_per_dollar)

## International/Global Carbon intensities ----------------------------------
intensity_int <-
  read_excel(file.path(dir[["other"]], "hh-CO2-intensities-11-int.xlsx"))|>
  rename(GTAP="...1",
         CO2_t=TZA)|> 
  select(GTAP,CO2_t) 



## National Carbon intensities ----------------------------------
intensity_nat <-
  read_excel(file.path(dir[["other"]], "hh-CO2-intensities-TZA.xlsx"))|>
  rename(GTAP="...1",
         CO2_t_within=TZA)|> 
  select(GTAP,CO2_t_within) 



## Electricity Sector Carbon intensities ----------------------------------
intensity_ely <-
  read_excel(file.path(dir[["other"]], "hh-CO2-intensities-ELY.xlsx"))|>
  rename(GTAP="...1",
         CO2_t_ely=TZA)|> 
  select(GTAP,CO2_t_ely) 


## Transport Sector Carbon intensities ----------------------------------
intensity_trs <-
  read_excel(file.path(dir[["other"]], "hh-CO2-intensities-fuel.xlsx"))|>
  rename(GTAP="...1",
         CO2_t_trs=TZA)|> 
  select(GTAP,CO2_t_trs) 


## Merge All Intensities ----------------------------------------------

carbon_intensities <- intensity_int|>
  left_join(intensity_nat, by = "GTAP", relationship = "one-to-one")%>%
  left_join(intensity_ely, by = "GTAP", relationship = "one-to-one")%>%
  left_join(intensity_trs, by = "GTAP", relationship = "one-to-one")

rm(intensity_int,intensity_nat,intensity_ely,intensity_trs)

## Save Analysis ---------------------------------------
write_csv(carbon_intensities,
          file.path(dir[["analysis"]],"carbon_intensities_tza.csv"))

# Data Merge -------------------------------------------------------

## We load TZA raw data -------------------------------------------------------

tza_raw <- read_csv(file.path(dir[["tza_processed"]], "clean_tanzania_2020.csv"))

tza_info <- tza_raw|> 
  select(hh_id,hh_size,hh_weights, urban,adults)|> 
  left_join(Urban_New)|> 
  distinct()

## Selection of Variables 

tza_sub <- tza_raw|> 
  select(hh_id,hh_size,hh_weights, urban,adults,children,
         itemcode,expenditures)

# delete observations with no information
tza_sub <- tza_sub|>
  filter(!is.na(hh_id))|> 
  filter(!is.na(hh_weights))|>
  filter(!is.na(hh_size))|> 
  filter(!is.na(expenditures))




## Merge Household and GTAP concordance ------------------------------------

tza_merge <- tza_sub|> 
  left_join(gtap_match, by="itemcode")|> 
  relocate(hh_id, GTAP,itemcode, expenditures)|> 
  select(-orig.code)

## HH Income Groups ------------------------------

hh_bdn <- tza_merge|> 
  dplyr::group_by(hh_id)|>
  dplyr::mutate(hh_exp_0   = sum(expenditures,na.rm = T))|>
  dplyr::ungroup()|>
  dplyr::mutate(hh_exp_0_pc = hh_exp_0 / hh_size)|>
  dplyr::select(hh_id,
                hh_exp_0,
                hh_exp_0_pc,
                hh_weights,
                urban,
                hh_size)|>
  dplyr::filter(!duplicated(hh_id))|>
  dplyr::mutate(incg_5   = as.numeric(
    rattle::binning(
      hh_exp_0_pc,
      bins = 5,
      method = c("wtd.quantile"),
      labels = seq(1, 5, length.out = 5),
      weights = hh_weights
    )
  ))|>
  dplyr::mutate(incg_10  = as.numeric(
    rattle::binning(
      hh_exp_0_pc,
      bins = 10,
      method = c("wtd.quantile"),
      labels = seq(1, 10, length.out = 10),
      weights = hh_weights
    )
  ))

# All households have non-exp.  

exp_ana <- hh_bdn

# Income Groups for Urban and Rural --------------------------------

hh_bdn_r <- hh_bdn|>
  select(hh_id, hh_exp_0_pc, hh_weights, urban, hh_size)|>
  filter(urban == 1)|>
  mutate(incg_5_UR = as.numeric(
    binning(
      hh_exp_0_pc,
      bins = 5,
      method = c("wtd.quantile"),
      labels = seq(1, 5, length.out = 5),
      weights = hh_weights
    )
  ))


hh_bdn_u <- hh_bdn|>
  select(hh_id, hh_exp_0_pc, hh_weights, urban, hh_size)|>
  filter(urban == 2)|>
  mutate(incg_5_UR = as.numeric(
    binning(
      hh_exp_0_pc,
      bins = 5,
      method = c("wtd.quantile"),
      labels = seq(1, 5, length.out = 5),
      weights = hh_weights
    )
  ))

# Combine Urban and Rural income groups

hh_bdn_ur <- rbind(hh_bdn_r, hh_bdn_u)

# Select relevant variables for the next steps
hh_bdn <- hh_bdn|>
  select(
    hh_id,
    incg_5,
    incg_10,
    hh_exp_0_pc,
    hh_weights,
    urban,
    hh_size
  )|>
  arrange(hh_id)

hh_info <- hh_bdn

# adding shares for urban status for 5 IG
# p_c_weights is per capita weights

hh_bdn <- hh_bdn|>
  group_by(incg_5, urban)%>%
  mutate(p_c_weights = sum(hh_weights*hh_size, na.rm = T))%>%
  ungroup()%>%
  group_by(incg_5)%>%
  mutate(p_c_weights_IG = sum(hh_weights*hh_size, na.rm = T))%>%
  ungroup()%>%
  mutate(share_urban_5 = p_c_weights/p_c_weights_IG)%>%
  select(-p_c_weights, - p_c_weights_IG)%>%
  # addings share for urban status for 10 IG
  group_by(incg_10, urban)%>%
  mutate(p_c_weights = sum(hh_weights*hh_size, na.rm = T))%>%
  ungroup()%>%
  group_by(incg_10)%>%
  mutate(p_c_weights_IG = sum(hh_weights*hh_size, na.rm = T))%>%
  ungroup()%>%
  mutate(share_urban_10 = p_c_weights/p_c_weights_IG)%>%
  select(-p_c_weights, - p_c_weights_IG, -hh_weights, -hh_size)


# for Later checks

hh_bdn_0 <- hh_bdn|>
  select(hh_id, incg_5, incg_10)




# Dollar Equivalent  -------------------------------------------------
# mm=mean
# usd= US dollars

exp_ana <- exp_ana|>
  mutate(hh_exp_USD = hh_exp_0*rate,
         hh_exp_USD_pc = (hh_exp_0*rate)/hh_size)

mean_exp_USD <-
  weighted.mean(exp_ana$hh_exp_USD,exp_ana$hh_weights)




# outlier_upper Correction -------------------------------------

hh_corr <- tza_merge|>
  dplyr::left_join(hh_bdn_0, by = "hh_id")|>
  dplyr::select(hh_id,
                itemcode,
                expenditures,
                incg_5,
                hh_size,
                hh_weights)|>
  dplyr::mutate(hh_pop = hh_size * hh_weights)|>
  dplyr::group_by(itemcode)|>
  dplyr::summarise(
    median_item= wtd.quantile(expenditures, 
                              weights = hh_pop, 
                              probs = 0.5,
                              na.rm=T),
    upper_bound = wtd.quantile(expenditures, 
                               weights = hh_pop, 
                               probs = 0.99,
                               na.rm=T),
    lower_bound = wtd.quantile(expenditures, 
                               weights = hh_pop, 
                               probs = 0.01,
                               na.rm=T)
  )|>
  dplyr::ungroup()



hh_merge_1 <- tza_merge|>
  left_join(hh_bdn_0, by = "hh_id")%>%
  left_join(hh_corr, by = "itemcode")%>%
  mutate(outlier_upper = ifelse(expenditures > upper_bound, 1, 0),
         outlier_lower = ifelse(expenditures < lower_bound, 1, 0)
  )



# outlier_uppers/lower Checks ------------------------------------------

outlier_upper <- sum(hh_merge_1$outlier_upper)
outlier_upper <- sum(hh_merge_1$outlier_lower)

outl_ana <- hh_merge_1|>
  select(hh_id,
         incg_5,
         expenditures,
         median_item,
         upper_bound,
         lower_bound,
         GTAP,
         outlier_upper,
         outlier_lower)|>
  filter(outlier_upper == 1|outlier_lower ==1)

oa1<- count(outl_ana, incg_5, GTAP)




outl_ana <- outl_ana|>
  mutate(overshoot_percent = expenditures / upper_bound,
         downshoot_percent = expenditures / lower_bound)|>
  mutate(median_over_ninetynineth = median_item / upper_bound,
         median_over_onepth = median_item / lower_bound)|>
  mutate(ninetynineth = 1,
         onepth = 1)

#where the 99.9th percentile within one quintile spends zero,
#but one household spends something
outl_ana_1 <- outl_ana|>
  filter(overshoot_percent == Inf,
         downshoot_percent == Inf)

print(
  paste(
    "We find ",
    nrow(outl_ana_1),
    "cases, where the 99.9th
            percentile within one quintile spends zero, but
            one household spends something."
  )
)

outl_ana_2 <- outl_ana|>
  filter(overshoot_percent != Inf,
         downshoot_percent != Inf)|>
  select(incg_5,
         GTAP,
         overshoot_percent,
         downshoot_percent,
         ninetynineth,
         onepth,
         median_over_ninetynineth,
         median_over_onepth)|>
  group_by(incg_5, GTAP)|>
  summarise(
    overshoot_percent = mean(overshoot_percent,
                             na.rm=T),
    downshoot_percent = mean(downshoot_percent,
                             na.rm=T),
    ninetynineth = mean(ninetynineth,
                        na.rm=T),
    onepth = mean(onepth,
                  na.rm=T),
    median_over_ninetynineth = mean(median_over_ninetynineth,
                                    na.rm=T),
    median_over_onepth = mean(median_over_onepth,
                              na.rm=T)
  )|>
  ungroup()|>
  gather(key = "measure",
         value = "value",
         overshoot_percent:median_over_ninetynineth)


# We try to gain information on the extent of the outlier_lower/upper.
#How much bigger is the outlier_lower/upper than the actual median 99th percentile



## Replacement of outlier_lower/uppers ----------------------------------

hh_merge_0 <- tza_merge

# outlier_lower/uppers are replaced by the median of their Quintile

hh_merge_2 <- hh_merge_1|>
  select(-incg_5,-incg_10)|>
  mutate(expenditures = ifelse(expenditures > upper_bound,
                               median_item, expenditures),
         expenditures = ifelse(expenditures < lower_bound,
                               median_item, expenditures))|>
  select(-median_item,-upper_bound,-outlier_upper,-lower_bound,-outlier_lower)

hh_merge_0 <- hh_merge_2



## Sensitivity Check on outlier_upper Sensitive Binning ------------------------


hh_merge_2_bdn <- hh_merge_2|>
  group_by(hh_id)|>
  mutate(hh_exp_0   = sum(expenditures,na.rm=TRUE))|>
  ungroup()|>
  mutate(hh_exp_0_pc = hh_exp_0 / hh_size)|>
  select(hh_id,
         hh_exp_0,
         hh_exp_0_pc,
         hh_weights,
         urban,
         hh_size)|>
  filter(!duplicated(hh_id))|>
  mutate(incg_5   = as.numeric(
    binning(
      hh_exp_0_pc,
      bins = 5,
      method = c("wtd.quantile"),
      labels = seq(1, 5, length.out = 5),
      weights = hh_weights
    )
  ))|>
  mutate(incg_10  = as.numeric(
    binning(
      hh_exp_0_pc,
      bins = 10,
      method = c("wtd.quantile"),
      labels = seq(1, 10, length.out = 10),
      weights = hh_weights
    )
  ))


bdn_cor_check <- hh_merge_2_bdn|>
  select(hh_id,
         hh_size,
         hh_weights,
         incg_5)|>
  rename(incg_5_C = incg_5)|>
  left_join(hh_bdn_0, by = "hh_id")|>
  select(-incg_10)|>
  mutate(pop = hh_size * hh_weights)|>
  group_by(incg_5_C, incg_5)|>
  summarise(affected = sum(pop,na.rm=T))|> 
  ungroup()


# Final Income Groups -------------------------------------------

hh_bdn_1 <- hh_merge_2_bdn|>
  select(hh_id, incg_5, incg_10)

rm(hh_merge_2)

# Processing Household Merge ------------------------------------
# # including |is.na(GTAP)? 
# This should make sense as otherwise NAs would be dropped here

hh_merge <- hh_merge_0|>
  arrange(hh_id)%>%
  filter(!is.na(GTAP))%>% 
  group_by(hh_id)%>%
  mutate(hh_exp_check= sum(expenditures))%>%
  ungroup()

obs_4a <- nrow(hh_merge)


## Information on "Other"-Items-------------------------------


hh_other_1 <- hh_merge|>
  filter(GTAP == "other")%>%
  group_by(hh_id)%>%
  summarise(
    exp_other = sum(expenditures,na.rm=T)
  )|> 
  ungroup()

hh_other_2 <- hh_merge|>
  group_by(hh_id)%>%
  summarise(
    expenditures = sum(expenditures,na.rm=T)
  )%>% 
  ungroup()

hh_other_3 <- left_join(hh_other_2, hh_other_1, by = "hh_id")


hh_other_4 <- hh_merge|>
  select(hh_id, hh_size, hh_weights, urban)%>%
  filter(!duplicated(hh_id))%>%
  left_join(hh_bdn_1, by = "hh_id")%>%
  left_join(hh_other_3, by = "hh_id")

hh_other_4$exp_other[is.na(hh_other_4$exp_other)] <- 0

hh_other_4 <- hh_other_4|>
  mutate(share_other = exp_other/expenditures)


rm(hh_other_1, hh_other_2, hh_other_3, hh_other_4)


## Processing hh_merge 

hh_merge <- hh_merge|>
  filter(!is.na(GTAP))

obs.5a <- nrow(hh_merge)


# Energy/Food/Goods/Services Analysis -------------------------

# read matching categories


types_1 <-
  read_excel(file.path(dir[["other"]], "GTAP_matching_categories.xlsx"),
             sheet = "edited")%>%
  rename(itemcode="unam. code")|> 
  filter(category == "food" |
           category == "energy" |
           category == "service" | 
           category == "goods" | 
           category== "other")|> 
  select(category,itemcode,description)


# To include other comment and uncomment codes

hm_0 <- hh_merge|>
  select(hh_id, hh_size, hh_weights, itemcode, expenditures,
         hh_exp_check)%>%
  mutate(itemcode = as.character(itemcode))%>%
  left_join(types_1, by = "itemcode")%>%
  # To include other, the comment-out the code below 
  filter(category != "other")%>%
  group_by(hh_id, category)%>%
  summarise(hh_size= first(hh_size),
            hh_weights= first(hh_weights),
            expenditures= sum(expenditures, na.rm = TRUE),
            hh_exp_check = first(hh_exp_check))%>%
  ungroup()%>%
  mutate(share = expenditures/hh_exp_check)%>%
  select(hh_id, category, share)|>
  pivot_wider(names_from = category, values_from = share)%>%
  rename(share_energy = energy, share_food = food, 
         share_goods = goods, share_services = service
         #,share_other = other
  )|> 
  select(hh_id,share_food,share_energy,share_goods,
         share_services
         #,share_other
  ) 


# replace household with missing expenditure shares as zero

hm_0_1 <- hm_0

hm_0_1[is.na(hm_0_1)] <- 0 

# save if other is included

# write_csv(
#   hm_0_1,
#   file.path(dir[["analysis"]],"Expenditure_types_tza.csv")
# )

# Add binning 5 and 10

hm_0_1b <- hm_0_1|> 
  left_join(hh_bdn_0,by="hh_id")

# save if other is included

# write_csv(
#   hm_0_1b,
#   file.path(dir[["analysis"]],"Expenditure_types_incgrp_tza.csv")
# )

# Add binning 5 Urban and rural
hh_bdn_UR_1 <-hh_bdn_ur|> 
  select(hh_id,urban,incg_5_UR)

hm_0_1b_ur <- hm_0_1|> 
  left_join(hh_bdn_UR_1,by="hh_id")

# save if other is included
# write_csv(
#   hm_0_1b_ur,
#   file.path(dir[["analysis"]],"Expenditure_types_incgrp_UR_tza.csv")
# )

rm(hm_0_1,hm_0_1b,hh_bdn_UR_1,hm_0_1b_ur)

## Match Fuel Concordance  -------------------------------------
# (Electricity;LPG,Petrol,Biomass,Kerosene,Diesel; Gas, Coal, Firewood)

fuels <-
  read_excel(file.path(dir[["other"]], "GTAP_matching_categories.xlsx"),
             sheet = "edited")%>%
  select(category,"unam. code",description)|> 
  filter(category == "energy")|> 
  rename(itemcode="unam. code") |> 
  mutate(fuel=ifelse(itemcode=="n202","Electricity",NA_real_),
         fuel=ifelse(itemcode=="n203","Gas",fuel),
         fuel=ifelse(itemcode=="n201","Kerosene",fuel),
         fuel=ifelse(itemcode=="n207","Charcoal",fuel),
         fuel=ifelse(itemcode=="n205","Petrol_Diesel",fuel))|>
  select(fuel,itemcode)

## Calculate Exp.Shares energy items -----------------

exp_fuels <- hh_merge|>
  select(hh_id, hh_size, hh_weights, itemcode, expenditures,
         hh_exp_check)%>%
  mutate(itemcode = as.character(itemcode))%>%
  left_join(fuels)|>
  filter(!is.na(fuel))|>
  group_by(hh_id, fuel)|>
  summarise(
    expenditures = sum(expenditures,na.rm=T),
  )|>
  ungroup()|>
  mutate(
    exp = expenditures * inflation_factor * rate
  )|>
  pivot_wider(
    names_from = "fuel",
    values_from = c("exp"),
    names_prefix = "USD_",
    values_fill = 0
  )

exp_fuels <- distinct(tza_info, hh_id)|>
  left_join(exp_fuels)%>%
  mutate_at(vars(-hh_id), list(~ ifelse(is.na(.),0,.)))

write_csv(
  exp_fuels,
  file.path(dir[["analysis"]],"Expenditure_fuels_17USD_tza.csv")
)

## Sectoral intensities and additional exp. ---------------------
# in USD/tCO2
carbp=40.00

# hh_s_cfp = HH sectoral carbon footprint
hh_s_cfp <-
  left_join(hh_merge, gtap_match, by = c("GTAP","itemcode"))|>
  left_join(types_1)|>
  left_join(fuels)|>
  filter(GTAP != "deleted")|>
  filter(
    category != "deleted" &
      category != "in-kind" &
      category != "self-produced" &
      category != "other_binning" & 
      GTAP != "other"
  )|>
  mutate(exp_USD_2017 = expenditures * inflation_factor * rate)|>
  mutate(
    aggregate_category = ifelse(
      category == "food" |
        category == "goods" | category == "service",
      category,
      ifelse(
        is.na(category),
        "NA_1",
        ifelse(
          category == "energy" &
            (is.na(fuel) |
               fuel == "Biomass" | fuel == "Firewood"),
          "other_energy",
          ifelse(
            category == "energy" &
              (fuel == "Petrol_Diesel"),
            "transport_fuels",
            ifelse(
              category == "energy" & fuel == "Electricity",
              "Electricity",
              ifelse(
                category == "energy" &
                  (fuel == "Gas" |
                     fuel == "LPG" |
                     fuel == "Kerosene" |
                     fuel == "Coal"|
                     fuel == "Charcoal"),
                "cooking_fuels",
                "NA_2"
              )
            )
          )
        )
      )
    )
  )|>
  left_join(carbon_intensities, by = "GTAP")|>
  mutate(CO2_s_t = exp_USD_2017 * CO2_t,
         CO2_s_t_within = exp_USD_2017 * CO2_t_within)|>
  group_by(hh_id, aggregate_category)|>
  summarise(CO2_s_t= sum(CO2_s_t,na.rm = T),
            CO2_s_t_within= sum(CO2_t_within,na.rm = T))|>
  ungroup()|>
  mutate(exp_s_CO2= CO2_s_t*carbp,
         exp_s_CO2_within= CO2_s_t_within*carbp)|>
  select(-CO2_s_t,-CO2_s_t_within)|>
  pivot_wider(
    names_from = "aggregate_category",
    values_from = c("exp_s_CO2",
                    "exp_s_CO2_within"),
    values_fill = 0
  )|>
  rename(
    exp_s_Goods = exp_s_CO2_goods,
    exp_s_Services = exp_s_CO2_service,
    exp_s_Food = exp_s_CO2_food,
    exp_s_Electricity = exp_s_CO2_Electricity,
    exp_s_Goods_within = exp_s_CO2_within_goods,
    exp_s_Services_within = exp_s_CO2_within_service,
    exp_s_Food_within = exp_s_CO2_within_food,
    exp_s_Electricity_within = exp_s_CO2_within_Electricity
  )

write_csv(hh_s_cfp,
          file.path(dir[["analysis"]],"HH_Sectoral_Burden_tza.csv"))



## Merging gdt and gas for Household-Data ------------------------
gas_gdt     <- hh_merge|>
  filter(GTAP == "gas" | GTAP == "gdt")

row_gas_gdt <- nrow(gas_gdt)

gas_gdt     <- gas_gdt|>
  mutate(item_code = as.character(itemcode))|>
  mutate(hh_id = as.character(hh_id))|>
  group_by(hh_id)|>
  summarise(
    expenditures = sum(expenditures, na.rm = T),
    item_code= first(itemcode),
    adults= first(adults),
    children= first(children),
    hh_size= first(hh_size),
    hh_weights= first(hh_weights),
    urban= first(urban),
    GTAP= "gasgdt",
    hh_exp_check= first(hh_exp_check)
  )|>
  ungroup()


hh_merge <- hh_merge|>
  mutate(hh_id = as.character(hh_id))%>%
  mutate(item_code = as.character(itemcode))%>%
  mutate(GTAP = as.character(GTAP))



if ((row_gas_gdt - nrow(gas_gdt)) != 0) {
  hh_merge <- hh_merge |>
    filter(GTAP != "gas" & GTAP != "gdt") |> 
    bind_rows(gas_gdt) |> 
    arrange(hh_id)
} else {
  hh_merge <- hh_merge %>%
    mutate(GTAP = case_when(
      GTAP == "gas" ~ "gasgdt",
      GTAP == "gdt" ~ "gasgdt",
      TRUE ~ GTAP
    ))
}


# Grouping hh_id und GTAP together ------------------------------------------

hh_merge_T_1 <- hh_merge|>
  select(hh_id, adults, children, hh_size, hh_weights,
         urban, hh_exp_check
  )%>%
  filter(!duplicated(hh_id))

hh_merge_T_2 <- hh_merge|>
  select(hh_id, expenditures, GTAP)%>%
  group_by(hh_id, GTAP)%>%
  summarise(
    expenditures = sum(expenditures,na.rm=T)
  )%>%
  ungroup()

hh_merge <- left_join(hh_merge_T_1, hh_merge_T_2, by = "hh_id")


obs6 <- nrow(hh_merge)


# Deflate or inflate Expenditure -----------------------------

hh_merge_3 <- hh_merge|>
  mutate(exp_inflated = expenditures*inflation_factor)%>%
  mutate(exp_USD_17 = exp_inflated*rate)%>%
  group_by(hh_id)%>%
  mutate(hh_exp_USD = sum(exp_USD_17))%>%
  ungroup()%>%
  select(-exp_inflated)


hh_merge <- hh_merge_3



# Processing Household Merge -----------------------------

hh_final <- hh_merge

obs8 <- nrow(hh_final)

# Share of expenditures on electricity over all expenditures -----------

hh_merge_ely <- hh_merge|>
  filter(GTAP == "ele")%>%
  select(hh_id, expenditures)

hh_merge_ely_2 <- exp_ana|>
  mutate(hh_id = as.character(hh_id))%>%
  left_join(hh_merge_ely, by = "hh_id")

hh_merge_ely_2$expenditures[is.na(hh_merge_ely_2$expenditures)] <- 0

hh_merge_ely_2 <- hh_merge_ely_2|>
  mutate(share_ely = expenditures/hh_exp_0)%>%
  left_join(Urban_New, by = "urban")

write_csv(hh_merge_ely_2, 
          file.path(dir[["analysis"]],
                    "electricity_share_analysis.csv")
)

hh_mean_ely <- wtd.mean(hh_merge_ely_2$share_ely, 
                        weights = hh_merge_ely_2$hh_weights)



# We split the household up in order to map them together -------

hh_merge_ely_2_1 <- hh_merge_ely_2|>
  select(hh_id, incg_5, share_ely, Urban, hh_weights)

hh_merge_ely_2_2 <- hh_merge_ely_2|>
  select(hh_id, incg_5, share_ely, hh_weights)%>%
  mutate(Urban = "Country")

hh_merge_ely_2_3 <- hh_merge_ely_2|>
  select(hh_id, share_ely, Urban, hh_weights)%>%
  mutate(incg_5 = 0)

hh_merge_ely_2_4 <- hh_merge_ely_2|>
  select(hh_id, share_ely, hh_weights)%>%
  mutate(incg_5 = 0)%>%
  mutate(Urban = "Country")

hh_merge_ely_2_5 <-hh_merge_ely_2_1|>
  rbind(hh_merge_ely_2_2)%>%
  rbind(hh_merge_ely_2_3)%>%
  rbind(hh_merge_ely_2_4)%>%
  mutate(Urban = fct_reorder2(Urban, Urban, Urban))%>%
  arrange(hh_id, incg_5)

hh_merge_ely_2_6 <- hh_merge_ely_2_5|>
  group_by(incg_5, Urban)%>%
  summarise(
    share_ely_mean = weighted.mean(share_ely, weight = hh_weights,na.rm=T)
  )%>%
  ungroup()

hh_merge_ely_2_5 <- hh_merge_ely_2_5|>
  left_join(hh_merge_ely_2_6, by = c("incg_5", "Urban"))


# Joining Final HH.data  and carbon int. -------------------------------------------
# carbon price is US-$ per ton of CO2
carbp=40

## Calculate Co2 inci-----------------------------------------

# It is in tonnes per dollar

inci     <- left_join(hh_final, carbon_intensities,
                      by = "GTAP")
obs9 <- nrow(inci)

print(paste0("By merging the Household.final and c.int ", 
             obs8-obs9, " observations have been deleted."))


inci_1 <- inci|>
  select(-expenditures,-hh_exp_check)|>
  #filter(GTAP != "other")%>%
  mutate(CO2_t= CO2_t*exp_USD_17,
         CO2_within_t = CO2_t_within*exp_USD_17,
         CO2_ely_t= CO2_t_ely*exp_USD_17,
         CO2_trs_t= CO2_t_trs*exp_USD_17)

# burden_co2 is total CO2 price revenues per sector in USD
# from the GTAP expenditures per sector
inci.1 <- inci|>
  group_by(GTAP)|>
  summarise(
    sector_size = sum(exp_USD_17*hh_weights,na.rm=T)
  )%>%
  ungroup()%>%
  left_join(carbon_intensities, by ="GTAP")|> 
  mutate(burden_CO2= CO2_t*carbp,
         burden_CO2_within= CO2_t_within*carbp,
         burden_CO2_ely= CO2_t_ely*carbp,
         burden_CO2_trs= CO2_t_trs*carbp)

inci.2 <- inci.1|>
  mutate(CO2_percent= burden_CO2/sector_size,
         CO2_wi_percent= burden_CO2_within/sector_size,
         CO2_ely_percent= burden_CO2_ely/sector_size,
         CO2_trs_percent= burden_CO2_trs/sector_size
  )|>
  select(GTAP,CO2_percent,CO2_wi_percent,CO2_ely_percent,
         CO2_trs_percent)%>%
  rename('Case I' = CO2_percent,'Case II' = CO2_wi_percent,
         'Case III' = CO2_ely_percent,'Case IV' = CO2_trs_percent)


## One observation per household --------------------------------------


inci_2 <- inci_1|>
  group_by(hh_id)%>%
  summarise(
    exp_USD_17= sum(exp_USD_17,na.rm=T),
    hh_exp_USD= first(hh_exp_USD),
    adults= first(adults),
    children= first(children),
    hh_size= first(hh_size),
    hh_weights= first(hh_weights),
    urban= first(urban),
    CO2_t= sum(CO2_t,na.rm=T),
    CO2_within_t = sum(CO2_within_t,na.rm=T),
    CO2_ely_t= sum(CO2_ely_t,na.rm=T),
    CO2_trs_t= sum(CO2_trs_t,na.rm=T)
  )%>%
  ungroup()

obs10 <- nrow(inci_2)

print(paste0(obs9,
             " observations have been summarised to ", 
             obs10, " observations."))



# ANALYSING DATA --------------------------------------------------

## Merging inci with Income groups ------------------------------------------

hh_bdn_1 <- hh_bdn_1|> 
  mutate(hh_id = as.numeric(hh_id)) 

final_inci <- inci_2|>
  mutate(hh_id = as.numeric(hh_id))|>
  left_join(hh_bdn_1, by = "hh_id")|> 
  left_join(Urban_New, by = "urban") 


##  Analysis with 5 income groups ----------------------------------------------

inci_analy <- final_inci|>
  mutate(hh_exp_USD_pc = exp_USD_17/hh_size)|>
  # calculating the measures per household
  mutate(tCO2_hh= CO2_t,
         tCO2_within_hh= CO2_within_t,
         tCO2_ely_hh= CO2_ely_t,
         tCO2_trs_hh= CO2_trs_t
  )|>
  # local CO2-tax per household "absolute burden"
  # How much money would one person have to spend if a carbon tax would be imposed?
  mutate(exp_hh_CO2= tCO2_hh*carbp,
         exp_hh_CO2_within= tCO2_within_hh*carbp,
         exp_hh_CO2_ely= tCO2_ely_hh*carbp,
         exp_hh_CO2_trs= tCO2_trs_hh*carbp)|>
  # Burden: additional expenditures over expenditures (per capita)
  # relative burden
  mutate(burden_CO2_hh= exp_hh_CO2/exp_USD_17,
         burden_CO2_within_hh=exp_hh_CO2_within/exp_USD_17,
         burden_CO2_ely_hh= exp_hh_CO2_ely/exp_USD_17,
         burden_CO2_trs_hh= exp_hh_CO2_trs/exp_USD_17)

inci_analy <- inci_analy|>
  mutate(hh_exp_USD_pc = exp_USD_17/hh_size)|>
  # calculating the measures per capita
  mutate(tCO2_pc= CO2_t/(hh_size),
         tCO2_within_pc= CO2_within_t/(hh_size),
         tCO2_ely_pc= CO2_ely_t/(hh_size),
         tCO2_trs_pc= CO2_trs_t/(hh_size))|>
  # local CO2-tax per household "absolute burden"
  # How much money would one person have to spend if a carbon tax would be imposed?
  mutate(exp_pc_CO2= tCO2_pc*carbp,
         exp_pc_CO2_within= tCO2_within_pc*carbp,
         exp_pc_CO2_ely= tCO2_ely_pc*carbp,
         exp_pc_CO2_trs= tCO2_trs_pc*carbp)|>
  # Burden: additional expenditures over expenditures (per capita)
  # "relative burden"
  mutate(burden_CO2_pc= exp_pc_CO2/hh_exp_USD_pc,
         burden_CO2_within_pc=exp_pc_CO2_within/hh_exp_USD_pc,
         burden_CO2_ely_pc= exp_pc_CO2_ely/hh_exp_USD_pc,
         burden_CO2_trs_pc= exp_pc_CO2_trs/hh_exp_USD_pc)

write_csv(
  inci_analy,
  file.path(dir[["analysis"]],"inci_analysis_tza.csv")
)



rm(list = ls())

