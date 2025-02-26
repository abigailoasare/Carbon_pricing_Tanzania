# Authors: Abigail O. Asare and  Laura Schürer

# Date: 26/02/2025

# Load relevant Packages------------------------------------------
# library("cowplot")
library("dplyr")
library("data.table")
library("foreign")
library("ggsci")
library("Hmisc")
library("openxlsx")
library("rattle")
library("scales")
library("tidyverse")
library("weights")
library("readr")
library("readxl")
library("ggrepel")
library("colorspace")
library(survey)
library(ggforce)
library(ggmagnify)
library(ggfx)
library(ggpubr)
library("xtable")

# Currency is in TSH
# Year = 2019

# Setup -------------------------------------------------------------------
source("./00_setup.R")


# Read Incidence data set --------------------------------------------------

tza_final <-
  read_csv(file.path(dir[["analysis"]], "inci_analysis_tza.csv"))

tza_info <-
  read_csv(file.path(dir[["analysis"]], "hh_information_tza.csv")) |>
  mutate(Urban = ifelse(urban == 1, "Rural", "Urban"),
         hh_id = as.numeric(hh_id)) |>
  filter(!is.na(hh_id)) |>
  filter(!is.na(hh_weights)) |>
  filter(!is.na(hh_size)) |>
  select(
    hh_id,
    hh_edu,
    residence_status,
    wall_mat,
    roof_mat,
    floor_mat,
    rent_amt,
    rent_amt_est,
    toilet_typ,
    cooking_fuel,
    lighting_fuel,
    electricity_typ,
    drinking_water_typ,
    hh_i20,
    other_water_typ,
    water_amt_1,
    water_amt_2,
    access_ely_bd,
    access_ely_st,
    access_ely_grid,
    access_ely_solar,
    access_ely_other,
    access_sanit_st,
    access_water_st
  )

wt_cd<- read_csv(file.path(dir[["tza_processed"]], "Water_Code.csv")) |> 
  rename(wt_label=Label)
lg_cd<- read_csv(file.path(dir[["tza_processed"]], "Lighting_Code.csv"))|> 
  rename(lg_label=Label)
edu_cd<- read_csv(file.path(dir[["tza_processed"]],"Education_Code.csv"))|> 
  rename(edu_label=Label)
ck_cd<- read_csv(file.path(dir[["tza_processed"]],"cooking_Code.csv"))|> 
  rename(ck_label=Label)
ely_cd<- read_csv(file.path(dir[["tza_processed"]], "Electricity_Code.csv"))|> 
  rename(ely_label=Label)
toi_cd<- read_csv(file.path(dir[["tza_processed"]],"Toilet_Code.csv"))|> 
  rename(toilet_label=Label)

dw_cd<- read_csv(file.path(dir[["tza_processed"]], "Dwelling_Code.csv"))|> 
  rename(dw_label=Label)
wall_cd<- read_csv(file.path(dir[["tza_processed"]], "Wall_Code.csv"))|> 
  rename(wall_label=Label)
roof_cd<- read_csv(file.path(dir[["tza_processed"]], "Roof_Code.csv"))|> 
  rename(roof_label=Label)
floor_cd<- read_csv(file.path(dir[["tza_processed"]], "Floor_Code.csv"))|> 
  rename(floor_label=Label)

# Define the threshold
threshold <- 0.05

assets_hh <- tza_final |> 
  left_join(tza_info, by="hh_id") |> 
  left_join(wt_cd, by=c("drinking_water_typ"="drinking_water_typ")) |> 
  left_join(lg_cd, by=c("lighting_fuel"="lighting_fuel")) |> 
  left_join(edu_cd, by=c("hh_edu"="education")) |> 
  left_join(toi_cd, by=c("toilet_typ"="toilet_typ")) |> 
  left_join(ck_cd, by=c("cooking_fuel"="cooking_fuel")) |> 
  left_join(ely_cd, by=c("electricity_typ"="electricity_type")) |> 
  left_join(dw_cd, by=c("residence_status"="dwelling_code")) |> 
  left_join(wall_cd, by=c("wall_mat"="wall_mat")) |> 
  left_join(roof_cd, by=c("roof_mat"="roof_mat")) |> 
  left_join(floor_cd, by=c("floor_mat"="floor_mat"))

# List of household characteristics

label<-c("Urban","edu_label","dw_label","ely_label","lg_label" ,"ck_label",
         "wt_label","floor_label","roof_label","wall_label","toilet_label")        


# Filter households where burden_CO2_within_pc > threshold
high_burden_assets_hh <- assets_hh |> 
  filter(burden_CO2_within_pc > threshold) 

# Filter households where burden_CO2_within_pc <= threshold
low_burden_assets_hh <- assets_hh |> 
  filter(burden_CO2_within_pc <= threshold) 


# Appendix Table A.5 ---------------------------------------------------------
# Key Characteristics of Households Substantially Affected by Carbon Pricing

# Function to calculate weighted total households and weighted share
counts_shares_weighted <- function(df, label) {
  df |> 
    group_by(!!sym(label)) |> 
    summarise(
      category = first(!!sym(label)),
      total_hh = n(),
      total_weighted_hh = sum(hh_weights, na.rm = TRUE),  
      share_weighted = total_weighted_hh / sum(df$hh_weights, na.rm = TRUE), 
      .groups = "drop"
    )
}

# Compute weighted shares for high-burdened households
shares_high_burdened <- map_dfr(label, ~counts_shares_weighted(high_burden_assets_hh, .x), .id = "label")

# Compute weighted shares for low-burdened households
shares_low_burdened <- map_dfr(label, ~counts_shares_weighted(low_burden_assets_hh, .x), .id = "label")

# Compute weighted shares for the whole dataset
shares_total <- map_dfr(label, ~counts_shares_weighted(assets_hh, .x), .id = "label")

# Merge the results
comp_table <- left_join(
  shares_high_burdened, 
  shares_low_burdened, 
  shares_total,
  by = c("label", "category"),
  suffix = c("_high", "_low")
) |> 
  left_join(shares_total, by = c("label", "category"))

# Compute Relative Risk (RR)
comp_table <- comp_table |> 
  mutate(
    relative_risk = share_weighted_high / share_weighted_low
  )


# Keep relevant data 

comp_table_rd <- comp_table |>  
  dplyr::mutate(share_weighted= share_weighted*100,
                share_weighted_low=share_weighted_low*100,
                share_weighted_high=share_weighted_high*100
  ) |> 
  dplyr::select(label,category,total_hh,total_weighted_hh,share_weighted,
                total_hh_low,total_weighted_hh_low,share_weighted_low,
                total_hh_high,total_weighted_hh_high,share_weighted_high,
                relative_risk
  )


# export as latex
table_latex_1<-xtable(comp_table_rd)

print(table_latex_1, file =file.path(dir[["tables"]],"Appendix_Table_A5.tex")
)


rm(list = ls())  








