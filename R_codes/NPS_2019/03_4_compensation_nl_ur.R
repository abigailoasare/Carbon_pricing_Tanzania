# Authors: Abigail O. Asare and  Laura Schürer

# Date:26/02/2025

# Sys.getlocale()

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

# Currency is in TSH
# Year = 2019


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



# National Schemes Ananlysis ---------------------------------------

# Without Compensation ---------------------------------------------------

func_0 <- function(Incidence.X) {
  Incidence.Y <- Incidence.X |>
    dplyr::select(
      hh_id,
      hh_size,
      hh_weights,
      Urban,
      incg_5,
      hh_exp_USD_pc,
      starts_with("burden"),
      starts_with("exp_")
    ) |>
    mutate(
      tt_exp_CO2 = exp_pc_CO2 * hh_size * hh_weights,
      tt_exp_CO2_within = exp_pc_CO2_within*hh_size*hh_weights,
      population = hh_size * hh_weights
    )
  
  no_households<- sum(Incidence.Y$hh_weights)
  no_population<- sum(Incidence.Y$population)
  tt_exp_CO2_all<-sum(Incidence.Y$tt_exp_CO2)
  tt_exp_CO2_within_all <- sum(Incidence.Y$tt_exp_CO2_within)
  
  LST_CO2_per_hh<-tt_exp_CO2_all/no_households
  LST_CO2_pc <- tt_exp_CO2_all/no_population
  LST_CO2_within_per_hh<- tt_exp_CO2_within_all/no_households
  LST_CO2_within_pc <- tt_exp_CO2_within_all/no_population  
  
  Incidence.Y.1 <- Incidence.Y |>
    mutate(
      exp_pc_CO2_LST_hh = -(exp_pc_CO2 -(LST_CO2_per_hh/hh_size)),
      exp_pc_CO2_LST_pc = -(exp_pc_CO2 - LST_CO2_pc),
      exp_pc_CO2_within_LST_hh = -(exp_pc_CO2_within - (LST_CO2_within_per_hh/hh_size)),
      exp_pc_CO2_within_LST_pc = -(exp_pc_CO2_within - (LST_CO2_within_pc)),
      # Attention: Negative Values indicate positive budget change
      burden_CO2_pc_LST_hh= exp_pc_CO2_LST_hh/hh_exp_USD_pc,
      burden_CO2_pc_LST_pc= exp_pc_CO2_LST_pc/hh_exp_USD_pc,
      burden_CO2_within_pc_LST_hh = exp_pc_CO2_within_LST_hh/hh_exp_USD_pc,
      burden_CO2_within_pc_LST_pc = exp_pc_CO2_within_LST_pc/hh_exp_USD_pc,
      exp_pc_CO2= -exp_pc_CO2,
      exp_pc_CO2_within= -exp_pc_CO2_within,
      burden_CO2_pc= -burden_CO2_pc,
      burden_CO2_within_pc = -burden_CO2_within_pc
    ) |>
    dplyr::select(hh_id,
                  hh_weights,
                  Urban,
                  incg_5,
                  starts_with("burden_"),
                  starts_with("exp"))
  
  Incidence.Y.2 <- Incidence.Y.1 |>
    dplyr::group_by(Urban,incg_5) |>
    dplyr::summarise(
      exp_pc_CO2= wtd.quantile(
        exp_pc_CO2, 
        probs = 0.5, 
        weights = hh_weights),
      exp_pc_CO2_LST_pc= wtd.quantile(
        exp_pc_CO2_LST_pc, 
        probs = 0.5, 
        weights = hh_weights),
      exp_pc_CO2_LST_hh= wtd.quantile(
        exp_pc_CO2_LST_hh, 
        probs = 0.5, 
        weights = hh_weights),
      exp_pc_CO2_within= wtd.quantile(
        exp_pc_CO2_within, 
        probs = 0.5, 
        weights = hh_weights),
      exp_pc_CO2_within_LST_pc = wtd.quantile(
        exp_pc_CO2_within_LST_pc,
        probs = 0.5,
        weights = hh_weights
      ),
      exp_pc_CO2_within_LST_hh = wtd.quantile(
        exp_pc_CO2_within_LST_hh,
        probs = 0.5,
        weights = hh_weights
      ),
      
      
      burden_CO2_pc_no_LST = wtd.quantile(
        burden_CO2_pc, probs = 0.5, weights = hh_weights),
      burden_CO2_pc_LST_hh = wtd.quantile(
        burden_CO2_pc_LST_hh,  probs = 0.5, weights = hh_weights),
      burden_CO2_pc_LST_pc = wtd.quantile(
        burden_CO2_pc_LST_pc,  probs = 0.5, weights = hh_weights),
      
      burden_CO2_within_pc_no_LST = wtd.quantile(
        burden_CO2_within_pc,
        probs = 0.5,
        weights = hh_weights
      ),
      burden_CO2_within_pc_LST_hh = wtd.quantile(
        burden_CO2_within_pc_LST_hh,
        probs = 0.5,
        weights = hh_weights
      ),
      burden_CO2_within_pc_LST_pc = wtd.quantile(
        burden_CO2_within_pc_LST_pc,
        probs = 0.5,
        weights = hh_weights
      )
    ) |>
    ungroup() |>
    dplyr::select(everything())
  
  list_0 <-
    list("Full_df" = Incidence.Y.1, "Summarised_df" = Incidence.Y.2)
  
  return(list_0)
  
}

tza221 <- func_0(tza_final)$Full_df
tza222 <-func_0(tza_final)$Summarised_df

###  NCP Boxplot function --------------------------------------------
NCP_g <- tza221 |>
  dplyr::select(hh_id, incg_5,Urban,burden_CO2_within_pc, hh_weights) |> 
  rename(burden_CO2_pc_CP_hh=burden_CO2_within_pc) |> 
  mutate(comp_type="NCP")


# Compensation Schemes is for households not per person (not per-capita)----------

## Lump Sum Tranfer (LST) Function -----------------------------------------------------------

test<- tza_final |>
  dplyr::select(
    hh_id,
    hh_size,
    hh_weights,
    incg_5,
    hh_exp_USD_pc,
    starts_with("burden"),
    starts_with("exp_")
  ) |>
  mutate(
    tt_exp_CO2 = exp_pc_CO2 * hh_size * hh_weights,
    tt_exp_CO2_within = exp_pc_CO2_within*hh_size*hh_weights,
    population = hh_size * hh_weights
  )



no_hh<- sum(test$hh_weights)
no_poph<- sum(test$population)
tt_expenditures_CO2_all<-sum(test$tt_exp_CO2)
tt_expenditures_CO2_within_all <- sum(test$tt_exp_CO2_within)

LST_g_hh<-tt_expenditures_CO2_all/no_hh
LST_n_hh <- tt_expenditures_CO2_within_all/no_hh


LST_g_pc<-tt_expenditures_CO2_all/no_poph
LST_n_pc <- tt_expenditures_CO2_within_all/no_poph



func_0_lst <- function(Incidence.X) {
  Incidence.Y <- Incidence.X |>
    dplyr::select(
      hh_id,
      hh_size,
      hh_weights,
      Urban,
      incg_5,
      hh_exp_USD_pc,
      hh_exp_USD,
      starts_with("burden"),
      starts_with("exp_")
    ) |>
    mutate(
      tt_exp_CO2 = exp_pc_CO2 * hh_size * hh_weights,
      tt_exp_CO2_within = exp_pc_CO2_within*hh_size*hh_weights,
      population = hh_size * hh_weights
    )
  
  no_households<- sum(Incidence.Y$hh_weights)
  no_population<- sum(Incidence.Y$population)
  tt_exp_CO2_all<-sum(Incidence.Y$tt_exp_CO2)
  tt_exp_CO2_within_all <- sum(Incidence.Y$tt_exp_CO2_within)
  
  LST_CO2_per_hh<-tt_exp_CO2_all/no_households
  LST_CO2_pc <- tt_exp_CO2_all/no_population
  LST_CO2_within_per_hh<- tt_exp_CO2_within_all/no_households
  LST_CO2_within_pc <- tt_exp_CO2_within_all/no_population  
  
  Incidence.Y.1 <- Incidence.Y |>
    mutate(
      exp_pc_CO2_LST_hh = -(exp_pc_CO2 -(LST_CO2_per_hh/hh_size)),
      exp_pc_CO2_LST_pc = -(exp_pc_CO2 - LST_CO2_pc),
      exp_pc_CO2_within_LST_hh = -(exp_pc_CO2_within - (LST_CO2_within_per_hh/hh_size)),
      exp_pc_CO2_within_LST_pc = -(exp_pc_CO2_within - (LST_CO2_within_pc)),
      # Attention: Negative Values indicate positive budget change
      burden_CO2_pc_LST_hh= exp_pc_CO2_LST_hh/hh_exp_USD_pc,
      burden_CO2_pc_LST_pc= exp_pc_CO2_LST_pc/hh_exp_USD_pc,
      burden_CO2_within_pc_LST_hh = exp_pc_CO2_within_LST_hh/hh_exp_USD_pc,
      burden_CO2_within_pc_LST_pc = exp_pc_CO2_within_LST_pc/hh_exp_USD_pc,
      exp_pc_CO2= -exp_pc_CO2,
      exp_pc_CO2_within= -exp_pc_CO2_within,
      burden_CO2_pc= -burden_CO2_pc,
      burden_CO2_within_pc = -burden_CO2_within_pc
    ) |>
    dplyr::select(hh_id,
                  hh_weights,
                  Urban,
                  incg_5,
                  starts_with("burden_"),
                  starts_with("exp"))
  
  Incidence.Y.2 <- Incidence.Y.1 |>
    dplyr::group_by(Urban,incg_5) |>
    dplyr::summarise(
      exp_pc_CO2= wtd.quantile(
        exp_pc_CO2, 
        probs = 0.5, 
        weights = hh_weights),
      exp_pc_CO2_LST_pc= wtd.quantile(
        exp_pc_CO2_LST_pc, 
        probs = 0.5, 
        weights = hh_weights),
      exp_pc_CO2_LST_hh= wtd.quantile(
        exp_pc_CO2_LST_hh, 
        probs = 0.5, 
        weights = hh_weights),
      exp_pc_CO2_within= wtd.quantile(
        exp_pc_CO2_within, 
        probs = 0.5, 
        weights = hh_weights),
      exp_pc_CO2_within_LST_pc = wtd.quantile(
        exp_pc_CO2_within_LST_pc,
        probs = 0.5,
        weights = hh_weights
      ),
      exp_pc_CO2_within_LST_hh = wtd.quantile(
        exp_pc_CO2_within_LST_hh,
        probs = 0.5,
        weights = hh_weights
      ),
      
      
      burden_CO2_pc_no_LST = wtd.quantile(
        burden_CO2_pc, probs = 0.5, weights = hh_weights),
      burden_CO2_pc_LST_hh = wtd.quantile(
        burden_CO2_pc_LST_hh,  probs = 0.5, weights = hh_weights),
      burden_CO2_pc_LST_pc = wtd.quantile(
        burden_CO2_pc_LST_pc,  probs = 0.5, weights = hh_weights),
      
      burden_CO2_within_pc_no_LST = wtd.quantile(
        burden_CO2_within_pc,
        probs = 0.5,
        weights = hh_weights
      ),
      burden_CO2_within_pc_LST_hh = wtd.quantile(
        burden_CO2_within_pc_LST_hh,
        probs = 0.5,
        weights = hh_weights
      ),
      burden_CO2_within_pc_LST_pc = wtd.quantile(
        burden_CO2_within_pc_LST_pc,
        probs = 0.5,
        weights = hh_weights
      )
    ) |>
    ungroup() |>
    dplyr::select(everything())
  
  list_0 <-
    list("Full_df" = Incidence.Y.1, "Summarised_df" = Incidence.Y.2)
  
  return(list_0)
  
}

tza221_lst <- func_0_lst(tza_final)$Full_df
tza222_lst <-func_0_lst(tza_final)$Summarised_df

###  LST Boxplot function --------------------------------------------
LST_g <- tza221_lst |>
  dplyr::select(hh_id, incg_5,Urban,burden_CO2_within_pc_LST_hh, hh_weights) |> 
  rename(burden_CO2_pc_CP_hh=burden_CO2_within_pc_LST_hh) |> 
  mutate(comp_type="LST")


## Solar Electricity (EST) Transfer -------------------------------------------
tza_info <- tza_info |> 
  mutate(hh_id = as.numeric(hh_id)) 

tza_info_1<-tza_info |> 
  dplyr::select(hh_id, access_ely_grid)



test<- tza_final |>
  left_join(tza_info_1, by="hh_id", relationship="one-to-one")

no_ely<-test |> 
  filter(access_ely_grid==1) 


tza_info_1<-tza_info |>
  dplyr::select(hh_id, access_ely_grid)

test_noely<- tza_final |>
  left_join(tza_info_1, by="hh_id", relationship="one-to-one") |>
  filter(access_ely_grid==0)

test_noely<- test_noely |>
  dplyr::select(
    hh_id,
    hh_size,
    hh_weights,
    Urban,
    access_ely_grid,
    incg_5,
    hh_exp_USD_pc,
    starts_with("burden"),
    starts_with("exp_")
  ) |>
  mutate(
    population = hh_size * hh_weights
  )



no_hh_noely<- sum(test_noely$hh_weights)
no_poph_noely<- sum(test_noely$population)



solar_noely_g_hh<-tt_expenditures_CO2_all/no_hh_noely
solar_noely_n_hh <- tt_expenditures_CO2_within_all/no_hh_noely


solar_noely_g_pc<-tt_expenditures_CO2_all/no_poph_noely
solar_noely_n_pc <- tt_expenditures_CO2_within_all/no_poph_noely


func_0_est <- function(Incidence.X) {
  
  tza_info_1<-tza_info |> 
    dplyr::select(hh_id, access_ely_grid)
  
  Incidence.X<- Incidence.X |>
    left_join(tza_info_1, by="hh_id", relationship="one-to-one") 
  
  Incidence.Y <- Incidence.X |>
    dplyr::select(
      hh_id,
      hh_size,
      hh_weights,
      Urban,
      access_ely_grid,
      incg_5,
      hh_exp_USD_pc,
      starts_with("burden"),
      starts_with("exp_")
    ) |>
    mutate(
      tt_exp_CO2 = exp_pc_CO2 * hh_size * hh_weights,
      tt_exp_CO2_within = exp_pc_CO2_within*hh_size*hh_weights
    )
  
  test_noely<- tza_final |>
    left_join(tza_info_1, by="hh_id", relationship="one-to-one") |> 
    filter(access_ely_grid==0) 
  
  test_noely<- test_noely |>
    dplyr::select(
      hh_id,
      hh_size,
      hh_weights,
      Urban,
      access_ely_grid,
      incg_5,
      hh_exp_USD_pc,
      starts_with("burden"),
      starts_with("exp_")
    ) |>
    mutate(
      population = hh_size * hh_weights
    )
  
  
  
  no_hh_noely<- sum(test_noely$hh_weights)
  no_pop_noely<- sum(test_noely$population)
  
  tt_exp_CO2_all<-sum(Incidence.Y$tt_exp_CO2)
  tt_exp_CO2_within_all <- sum(Incidence.Y$tt_exp_CO2_within)
  
  EST_CO2_per_hh<-tt_exp_CO2_all/no_hh_noely
  EST_CO2_pc <- tt_exp_CO2_all/no_pop_noely
  EST_CO2_within_per_hh<- tt_exp_CO2_within_all/no_hh_noely
  EST_CO2_within_pc <- tt_exp_CO2_within_all/no_pop_noely 
  
  Incidence.Y.1 <- Incidence.Y |>
    mutate(
      exp_pc_CO2_EST_hh = ifelse(access_ely_grid==0,(-(exp_pc_CO2 -(EST_CO2_per_hh/hh_size))),
                                 0),
      exp_pc_CO2_EST_pc = ifelse(access_ely_grid==0,
                                 (-(exp_pc_CO2 - EST_CO2_pc)),
                                 0),
      exp_pc_CO2_within_EST_hh = ifelse(access_ely_grid==0,
                                        (-(exp_pc_CO2_within - (EST_CO2_within_per_hh/hh_size))),
                                        0),
      exp_pc_CO2_within_EST_pc = ifelse(access_ely_grid==0,
                                        (-(exp_pc_CO2_within - (EST_CO2_within_pc))),
                                        0),
      # Attention: Negative Values indicate positive budget change
      burden_CO2_pc_EST_hh= ifelse(access_ely_grid==0,
                                   (exp_pc_CO2_EST_hh/hh_exp_USD_pc),
                                   0),
      burden_CO2_pc_EST_pc= ifelse(access_ely_grid==0,
                                   (exp_pc_CO2_EST_pc/hh_exp_USD_pc),
                                   0),
      burden_CO2_within_pc_EST_hh = ifelse(access_ely_grid==0,
                                           (exp_pc_CO2_within_EST_hh/hh_exp_USD_pc),
                                           0),
      burden_CO2_within_pc_EST_pc =ifelse(access_ely_grid==0,
                                          (exp_pc_CO2_within_EST_pc/hh_exp_USD_pc),
                                          0),
      exp_pc_CO2= -exp_pc_CO2,
      exp_pc_CO2_within= -exp_pc_CO2_within,
      burden_CO2_pc= -burden_CO2_pc,
      burden_CO2_within_pc = -burden_CO2_within_pc
    ) |>
    dplyr::select(hh_id,
                  hh_weights,
                  Urban,
                  incg_5,
                  starts_with("burden_"),
                  starts_with("exp"))
  
  Incidence.Y.2 <- Incidence.Y.1 |>
    dplyr::group_by(Urban,incg_5) |>
    dplyr::summarise(
      exp_pc_CO2= wtd.quantile(
        exp_pc_CO2, 
        probs = 0.5, 
        weights = hh_weights),
      exp_pc_CO2_EST_pc= wtd.quantile(
        exp_pc_CO2_EST_pc, 
        probs = 0.5, 
        weights = hh_weights),
      exp_pc_CO2_EST_hh= wtd.quantile(
        exp_pc_CO2_EST_hh, 
        probs = 0.5, 
        weights = hh_weights),
      exp_pc_CO2_within= wtd.quantile(
        exp_pc_CO2_within, 
        probs = 0.5, 
        weights = hh_weights),
      exp_pc_CO2_within_EST_pc = wtd.quantile(
        exp_pc_CO2_within_EST_pc,
        probs = 0.5,
        weights = hh_weights
      ),
      exp_pc_CO2_within_EST_hh = wtd.quantile(
        exp_pc_CO2_within_EST_hh,
        probs = 0.5,
        weights = hh_weights
      ),
      
      
      burden_CO2_pc_no_LST = wtd.quantile(
        burden_CO2_pc, probs = 0.5, weights = hh_weights),
      burden_CO2_pc_EST_hh = wtd.quantile(
        burden_CO2_pc_EST_hh,  probs = 0.5, weights = hh_weights),
      burden_CO2_pc_EST_pc = wtd.quantile(
        burden_CO2_pc_EST_pc,  probs = 0.5, weights = hh_weights),
      
      burden_CO2_within_pc_no_LST = wtd.quantile(
        burden_CO2_within_pc,
        probs = 0.5,
        weights = hh_weights
      ),
      burden_CO2_within_pc_EST_hh = wtd.quantile(
        burden_CO2_within_pc_EST_hh,
        probs = 0.5,
        weights = hh_weights
      ),
      burden_CO2_within_pc_EST_pc = wtd.quantile(
        burden_CO2_within_pc_EST_pc,
        probs = 0.5,
        weights = hh_weights
      )
    ) |>
    ungroup() |>
    dplyr::select(everything())
  
  list_0 <-
    list("Full_df" = Incidence.Y.1, "Summarised_df" = Incidence.Y.2)
  
  return(list_0)
  
}


tza221_est <- func_0_est(tza_final)$Full_df
tza222_est <-func_0_est(tza_final)$Summarised_df



###  EST Boxplot function --------------------------------------------
EST_g <- tza221_est |>
  dplyr::select(hh_id, incg_5,Urban,burden_CO2_within_pc_EST_hh, hh_weights) |> 
  rename(burden_CO2_pc_CP_hh=burden_CO2_within_pc_EST_hh) |> 
  mutate(comp_type="EST")


## Targeted Transfer (TST) Function -----------------------------------------------------------

# 0.6 for adults
# 0.4 for children

tza_info_1<-tza_info |> 
  dplyr::select(hh_id, access_ely_grid)

test<- tza_final |>
  left_join(tza_info_1, by="hh_id", relationship="one-to-one")



test<- tza_final |>
  dplyr::select(
    hh_id,
    hh_size,
    hh_weights,
    Urban,
    adults,
    children,
    incg_5,
    hh_exp_USD_pc,
    starts_with("burden"),
    starts_with("exp_")
  ) |>
  mutate(
    tt_exp_CO2 = exp_pc_CO2 * hh_size * hh_weights,
    tt_exp_CO2_within = exp_pc_CO2_within*hh_size*hh_weights,
    population = hh_size * hh_weights
  )



no_hh<- sum(test$hh_weights)
no_poph<- sum(test$population)
tt_expenditures_CO2_all<-sum(test$tt_exp_CO2)
tt_expenditures_CO2_within_all <- sum(test$tt_exp_CO2_within)

lst_g_hh<-tt_expenditures_CO2_all/no_hh
lst_n_hh <- tt_expenditures_CO2_within_all/no_hh


lst_g_pc<-tt_expenditures_CO2_all/no_poph
lst_n_pc <- tt_expenditures_CO2_within_all/no_poph



qui_12<-test |>
  dplyr::select(
    hh_id,
    hh_size,
    hh_weights,
    Urban,
    adults,
    children,
    incg_5,
    hh_exp_USD_pc,
    starts_with("burden"),
    starts_with("exp_")
  ) |>
  filter(incg_5==1|incg_5==2) |> 
  mutate(
    population = hh_size * hh_weights
  )  


no_hh_qui_12<- sum(qui_12$hh_weights)
no_pop_qui_12<- sum(qui_12$population)

tg_qui_12_g_hh<-tt_expenditures_CO2_all/no_hh_qui_12
tg_qui_12_n_hh <- tt_expenditures_CO2_within_all/no_hh_qui_12

tg_qui_12_g_pc<-tt_expenditures_CO2_all/no_pop_qui_12
tg_qui_12_n_pc <- tt_expenditures_CO2_within_all/no_pop_qui_12


# National
qui_12$tg_adults_g_hh<-ifelse(qui_12$incg_5==1|
                                qui_12$incg_5==2 &
                                qui_12$adults>0,(0.6*tt_expenditures_CO2_all)/no_hh_qui_12,
                              0)
unique(qui_12$tg_adults_g_hh)

qui_12$tg_child_g_hh<-ifelse(qui_12$incg_5==1|qui_12$incg_5==2&qui_12$children>0,(0.4*tt_expenditures_CO2_all)/no_hh_qui_12,0)
unique(qui_12$tg_child_g_hh)

qui_12$tg_both_g_hh<-qui_12$tg_adults_g_hh+qui_12$tg_child_g_hh


# national
qui_12$tg_adults_n_hh<-ifelse(qui_12$incg_5==1|qui_12$incg_5==2&qui_12$adults>0,(0.6*tt_expenditures_CO2_within_all)/no_hh_qui_12,0)
unique(qui_12$tg_adults_n_hh)

qui_12$tg_child_n_hh<-ifelse(qui_12$incg_5==1|qui_12$incg_5==2&qui_12$children>0,(0.4*tt_expenditures_CO2_within_all)/no_hh_qui_12,0)
unique(qui_12$tg_child_n_hh)

qui_12$tg_both_n_hh<-qui_12$tg_adults_n_hh+qui_12$tg_child_n_hh

# per capita

# National
qui_12$tg_adults_g_pc<-ifelse(qui_12$incg_5==1|qui_12$incg_5==2&qui_12$adults>0,(0.6*tt_expenditures_CO2_all)/no_pop_qui_12,0)
unique(qui_12$tg_adults_g_pc)

qui_12$tg_child_g_pc<-ifelse(qui_12$incg_5==1|qui_12$incg_5==2&qui_12$children>0,(0.4*tt_expenditures_CO2_all)/no_pop_qui_12,0)

unique(qui_12$tg_child_g_pc)

qui_12$tg_both_g_pc<-qui_12$tg_adults_g_pc+qui_12$tg_child_g_pc


# national
qui_12$tg_adults_n_pc<-ifelse(qui_12$incg_5==1|qui_12$incg_5==2&qui_12$adults>0,(0.6*tt_expenditures_CO2_within_all)/no_pop_qui_12,0)
unique(qui_12$tg_adults_n_pc)

qui_12$tg_child_n_pc<-ifelse(qui_12$incg_5==1|qui_12$incg_5==2&qui_12$children>0,(0.4*tt_expenditures_CO2_within_all)/no_pop_qui_12,0)
unique(qui_12$tg_child_n_pc)

qui_12$tg_both_n_pc<-qui_12$tg_adults_n_pc+qui_12$tg_child_n_pc



### TST Transfer function
func_0_tg_q12 <- function(Incidence.X) {
  
  
  Incidence.X<- Incidence.X |>
    left_join(tza_info_1, by="hh_id", relationship="one-to-one") 
  
  Incidence.Y <- Incidence.X |>
    dplyr::select(
      hh_id,
      hh_size,
      hh_weights,
      Urban,
      adults,
      children,
      access_ely_grid,
      incg_5,
      hh_exp_USD_pc,
      starts_with("burden"),
      starts_with("exp_")
    ) |>
    mutate(
      tt_exp_CO2 = exp_pc_CO2 * hh_size * hh_weights,
      tt_exp_CO2_within = exp_pc_CO2_within*hh_size*hh_weights,
    )
  
  
  test_qui_12<- Incidence.X |>
    dplyr::select(
      hh_id,
      hh_size,
      hh_weights,
      Urban,
      adults,
      children,
      access_ely_grid,
      incg_5,
      hh_exp_USD_pc,
      starts_with("burden"),
      starts_with("exp_")
    ) |>
    filter(incg_5==1|incg_5==2) |> 
    mutate(
      population = hh_size * hh_weights
    ) 
  
  
  
  no_hh_qui_12<- sum(test_qui_12$hh_weights)
  no_pop_qui_12<- sum(test_qui_12$population)
  
  tt_exp_CO2_all<-sum(Incidence.Y$tt_exp_CO2)
  tt_exp_CO2_within_all <- sum(Incidence.Y$tt_exp_CO2_within)
  
  TST_CO2_per_hh<-tt_exp_CO2_all/no_hh_qui_12
  TST_CO2_pc <- tt_exp_CO2_all/no_pop_qui_12
  TST_CO2_within_per_hh<- tt_exp_CO2_within_all/no_hh_qui_12
  TST_CO2_within_pc <- tt_exp_CO2_within_all/no_pop_qui_12 
  
  Incidence.Y.1 <- Incidence.Y |>
    mutate(
      exp_pc_CO2_TST_ad = ifelse(incg_5==1|incg_5==2&adults>0,
                                 (-(exp_pc_CO2 -(0.6*(TST_CO2_per_hh/hh_size)))),
                                 0),
      exp_pc_CO2_TST_ch = ifelse(incg_5==1|incg_5==2&children>0,
                                 (-(exp_pc_CO2 -(0.4*(TST_CO2_per_hh/hh_size)))),
                                 0),
      exp_pc_CO2_TST_hh = exp_pc_CO2_TST_ad + exp_pc_CO2_TST_ch,
      exp_pc_CO2_TST_hh = ifelse(exp_pc_CO2_TST_hh==0,
                                 -exp_pc_CO2,
                                 exp_pc_CO2_TST_hh),
      exp_pc_CO2_TST_ad_pc = ifelse(incg_5==1|incg_5==2&adults>0,
                                    (-(exp_pc_CO2 - (0.6*TST_CO2_pc))),
                                    0),
      exp_pc_CO2_TST_ch_pc = ifelse(incg_5==1|incg_5==2&children>0,
                                    (-(exp_pc_CO2 - (0.4*TST_CO2_pc))), 
                                    0),
      exp_pc_CO2_TST_pc = exp_pc_CO2_TST_ad_pc + exp_pc_CO2_TST_ch_pc,
      exp_pc_CO2_TST_pc = ifelse(exp_pc_CO2_TST_pc==0,
                                 -exp_pc_CO2,
                                 exp_pc_CO2_TST_pc),
      exp_pc_CO2_within_TST_ad = ifelse(incg_5==1|incg_5==2&adults>0,
                                        (-(exp_pc_CO2_within - (0.6*(TST_CO2_within_per_hh/hh_size)))),
                                        0),
      exp_pc_CO2_within_TST_ch= ifelse(incg_5==1|incg_5==2&children>0,
                                       (-(exp_pc_CO2_within - (0.4*(TST_CO2_within_per_hh/hh_size)))),
                                       0),
      exp_pc_CO2_within_TST_hh = exp_pc_CO2_within_TST_ad + exp_pc_CO2_within_TST_ch,
      exp_pc_CO2_within_TST_hh = ifelse(exp_pc_CO2_within_TST_hh==0,
                                        -exp_pc_CO2_within,
                                        exp_pc_CO2_within_TST_hh),
      exp_pc_CO2_within_TST_ad_pc = ifelse(incg_5==1|incg_5==2&adults>0,
                                           (-(exp_pc_CO2_within - (0.6*TST_CO2_within_pc))),
                                           0),
      exp_pc_CO2_within_TST_ch_pc= ifelse(incg_5==1|incg_5==2&children>0,
                                          (-(exp_pc_CO2_within - (0.4*TST_CO2_within_pc))),
                                          0),
      exp_pc_CO2_within_TST_pc = exp_pc_CO2_within_TST_ad_pc + exp_pc_CO2_within_TST_ch_pc,
      exp_pc_CO2_within_TST_pc = ifelse(exp_pc_CO2_within_TST_pc==0,
                                        -exp_pc_CO2_within,
                                        exp_pc_CO2_within_TST_pc
      ),
      # Attention: Negative Values indicate positive budget change
      burden_CO2_pc_TST_hh= exp_pc_CO2_TST_hh/hh_exp_USD_pc,
      burden_CO2_pc_TST_pc=exp_pc_CO2_TST_pc/hh_exp_USD_pc,
      burden_CO2_within_pc_TST_hh =exp_pc_CO2_within_TST_hh/hh_exp_USD_pc,
      burden_CO2_within_pc_TST_pc =exp_pc_CO2_within_TST_pc/hh_exp_USD_pc,
      exp_pc_CO2= -exp_pc_CO2,
      exp_pc_CO2_within= -exp_pc_CO2_within,
      burden_CO2_pc= -burden_CO2_pc,
      burden_CO2_within_pc = -burden_CO2_within_pc
    ) |>
    dplyr::select(hh_id,
                  hh_weights,
                  Urban,
                  incg_5,
                  starts_with("burden_"),
                  starts_with("exp"))
  
  Incidence.Y.2 <- Incidence.Y.1 |>
    dplyr::group_by(Urban, incg_5) |>
    dplyr::summarise(
      exp_pc_CO2= wtd.quantile(
        exp_pc_CO2, 
        probs = 0.5, 
        weights = hh_weights),
      exp_pc_CO2_TST_pc= wtd.quantile(
        exp_pc_CO2_TST_pc, 
        probs = 0.5, 
        weights = hh_weights),
      exp_pc_CO2_TST_hh= wtd.quantile(
        exp_pc_CO2_TST_hh, 
        probs = 0.5, 
        weights = hh_weights),
      exp_pc_CO2_within= wtd.quantile(
        exp_pc_CO2_within, 
        probs = 0.5, 
        weights = hh_weights),
      exp_pc_CO2_within_TST_pc = wtd.quantile(
        exp_pc_CO2_within_TST_pc,
        probs = 0.5,
        weights = hh_weights
      ),
      exp_pc_CO2_within_TST_hh = wtd.quantile(
        exp_pc_CO2_within_TST_hh,
        probs = 0.5,
        weights = hh_weights
      ),
      
      
      burden_CO2_pc_no_TST = wtd.quantile(
        burden_CO2_pc, 
        probs = 0.5,
        weights = hh_weights
      ),
      burden_CO2_pc_TST_hh = wtd.quantile(
        burden_CO2_pc_TST_hh, 
        probs = 0.5, 
        weights = hh_weights
      ),
      burden_CO2_pc_TST_pc = wtd.quantile(
        burden_CO2_pc_TST_pc, 
        probs = 0.5,
        weights = hh_weights
      ),
      
      burden_CO2_within_pc_no_TST = wtd.quantile(
        burden_CO2_within_pc,
        probs = 0.5,
        weights = hh_weights
      ),
      burden_CO2_within_pc_TST_hh = wtd.quantile(
        burden_CO2_within_pc_TST_hh,
        probs = 0.5,
        weights = hh_weights
      ),
      burden_CO2_within_pc_TST_pc = wtd.quantile(
        burden_CO2_within_pc_TST_pc,
        probs = 0.5,
        weights = hh_weights
      )
    ) |>
    ungroup() |>
    dplyr::select(everything())
  
  list_0 <-
    list("Full_df" = Incidence.Y.1, "Summarised_df" = Incidence.Y.2)
  
  return(list_0)
  
}


tza221_tg_q12 <- func_0_tg_q12(tza_final)$Full_df
tza222_tg_q12 <-func_0_tg_q12(tza_final)$Summarised_df


###  TST Boxplot function --------------------------------------------
TST_g <- tza221_tg_q12 |>
  dplyr::select(hh_id, incg_5,Urban,burden_CO2_within_pc_TST_hh, hh_weights) |> 
  rename(burden_CO2_pc_CP_hh=burden_CO2_within_pc_TST_hh) |> 
  mutate(comp_type="TST")



## Solar Cooker (SCT) Transfer ------------------------------------

tza_info_1<-tza_info |> 
  dplyr::select(hh_id, cooking_fuel)|> 
  mutate(hhnocl_ckfuel=ifelse(cooking_fuel==3| cooking_fuel==4| cooking_fuel==7,0,1)) |> 
  dplyr::select(hh_id,hhnocl_ckfuel)

unique(tza_info_1$hhnocl_ckfuel)

test<- tza_final |>
  left_join(tza_info_1, by="hh_id", relationship="one-to-one")

no_nock<-test |> 
  filter(hhnocl_ckfuel==1) 

test<- tza_final |>
  dplyr::select(
    hh_id,
    hh_size,
    hh_weights,
    Urban,
    incg_5,
    hh_exp_USD_pc,
    starts_with("burden"),
    starts_with("exp_")
  ) |>
  mutate(
    tt_exp_CO2 = exp_pc_CO2 * hh_size * hh_weights,
    tt_exp_CO2_within = exp_pc_CO2_within*hh_size*hh_weights,
    population = hh_size * hh_weights
  )



no_hh<- sum(test$hh_weights)
no_poph<- sum(test$population)
tt_expenditures_CO2_all<-sum(test$tt_exp_CO2)
tt_expenditures_CO2_within_all <- sum(test$tt_exp_CO2_within)

LST_g_hh<-tt_expenditures_CO2_all/no_hh
LST_n_hh <- tt_expenditures_CO2_within_all/no_hh


LST_g_pc<-tt_expenditures_CO2_all/no_poph
LST_n_pc <- tt_expenditures_CO2_within_all/no_poph


tza_info_1<-tza_info_1 |>
  dplyr::select(hh_id, hhnocl_ckfuel)

test_nock<- tza_final |>
  left_join(tza_info_1, by="hh_id", relationship="one-to-one") |>
  filter(hhnocl_ckfuel==1)

test_nock<- test_nock |>
  dplyr::select(
    hh_id,
    hh_size,
    hh_weights,
    Urban,
    hhnocl_ckfuel,
    incg_5,
    hh_exp_USD_pc,
    starts_with("burden"),
    starts_with("exp_")
  ) |>
  filter(incg_5==1|incg_5==2) |> 
  mutate(
    population = hh_size * hh_weights
  )



no_hh_nock<- sum(test_nock$hh_weights)
no_poph_nock<- sum(test_nock$population)



solar_nock_g_hh<-tt_expenditures_CO2_all/no_hh_nock
solar_nock_n_hh <- tt_expenditures_CO2_within_all/no_hh_nock


solar_nock_g_pc<-tt_expenditures_CO2_all/no_poph_nock
solar_nock_n_pc <- tt_expenditures_CO2_within_all/no_poph_nock


### SCT Transfer Function 
func_0_nock <- function(Incidence.X) {
  
  tza_info_1<-tza_info |> 
    dplyr::select(hh_id, cooking_fuel)|> 
    mutate(hhnocl_ckfuel=ifelse(cooking_fuel==3| cooking_fuel==4| cooking_fuel==7,0,1)) |> 
    dplyr::select(hh_id,hhnocl_ckfuel)
  
  Incidence.X<- Incidence.X |>
    left_join(tza_info_1, by="hh_id", relationship="one-to-one") 
  
  Incidence.Y <- Incidence.X |>
    dplyr::select(
      hh_id,
      hh_size,
      hh_weights,
      Urban,
      hhnocl_ckfuel,
      incg_5,
      hh_exp_USD_pc,
      starts_with("burden"),
      starts_with("exp_")
    ) |>
    mutate(
      tt_exp_CO2 = exp_pc_CO2 * hh_size * hh_weights,
      tt_exp_CO2_within = exp_pc_CO2_within*hh_size*hh_weights,
    )
  
  test_nock<- tza_final |>
    left_join(tza_info_1, by="hh_id", relationship="one-to-one") |> 
    filter(hhnocl_ckfuel==1) 
  
  test_nock<- test_nock |>
    dplyr::select(
      hh_id,
      hh_size,
      hh_weights,
      Urban,
      hhnocl_ckfuel,
      incg_5,
      hh_exp_USD_pc,
      starts_with("burden"),
      starts_with("exp_")
    ) |>
    filter(incg_5==1|incg_5==2) |> 
    mutate(
      population = hh_size * hh_weights
    )
  
  
  
  no_hh_nock<- sum(test_nock$hh_weights)
  no_pop_nock<- sum(test_nock$population)
  
  tt_exp_CO2_all<-sum(Incidence.Y$tt_exp_CO2)
  tt_exp_CO2_within_all <- sum(Incidence.Y$tt_exp_CO2_within)
  
  SCT_CO2_per_hh<-tt_exp_CO2_all/no_hh_nock
  SCT_CO2_pc <- tt_exp_CO2_all/no_pop_nock
  SCT_CO2_within_per_hh<- tt_exp_CO2_within_all/no_hh_nock
  SCT_CO2_within_pc <- tt_exp_CO2_within_all/no_pop_nock 
  
  Incidence.Y.1 <- Incidence.Y |>
    mutate(
      exp_pc_CO2_SCT_hh = ifelse(incg_5==1|incg_5==2&hhnocl_ckfuel==1,
                                 (-(exp_pc_CO2 -(SCT_CO2_per_hh/hh_size))),
                                 -exp_pc_CO2),
      exp_pc_CO2_SCT_pc = ifelse(incg_5==1|incg_5==2&hhnocl_ckfuel==1,
                                 (-(exp_pc_CO2 - SCT_CO2_pc)),
                                 -exp_pc_CO2),
      exp_pc_CO2_within_SCT_hh = ifelse(incg_5==1|incg_5==2&hhnocl_ckfuel==1,
                                        (-(exp_pc_CO2_within - (SCT_CO2_within_per_hh/hh_size))),
                                        -exp_pc_CO2_within),
      exp_pc_CO2_within_SCT_pc = ifelse(incg_5==1|incg_5==2&hhnocl_ckfuel==1,
                                        (-(exp_pc_CO2_within - (SCT_CO2_within_pc))),
                                        -exp_pc_CO2_within),
      # Attention: Negative Values indicate positive budget change
      burden_CO2_pc_SCT_hh=exp_pc_CO2_SCT_hh/hh_exp_USD_pc,
      burden_CO2_pc_SCT_pc= exp_pc_CO2_SCT_pc/hh_exp_USD_pc,
      burden_CO2_within_pc_SCT_hh = exp_pc_CO2_within_SCT_hh/hh_exp_USD_pc,
      burden_CO2_within_pc_SCT_pc =exp_pc_CO2_within_SCT_pc/hh_exp_USD_pc,
      exp_pc_CO2= -exp_pc_CO2,
      exp_pc_CO2_within= -exp_pc_CO2_within,
      burden_CO2_pc= -burden_CO2_pc,
      burden_CO2_within_pc = -burden_CO2_within_pc
    ) |>
    dplyr::select(hh_id,
                  hh_weights,
                  Urban,
                  incg_5,
                  starts_with("burden_"),
                  starts_with("exp"))
  
  Incidence.Y.2 <- Incidence.Y.1 |>
    dplyr::group_by(Urban, incg_5) |>
    dplyr::summarise(
      exp_pc_CO2= wtd.quantile(
        exp_pc_CO2, 
        probs = 0.5, 
        weights = hh_weights),
      exp_pc_CO2_SCT_pc= wtd.quantile(
        exp_pc_CO2_SCT_pc, 
        probs = 0.5, 
        weights = hh_weights),
      exp_pc_CO2_SCT_hh= wtd.quantile(
        exp_pc_CO2_SCT_hh, 
        probs = 0.5, 
        weights = hh_weights),
      exp_pc_CO2_within= wtd.quantile(
        exp_pc_CO2_within, 
        probs = 0.5, 
        weights = hh_weights),
      exp_pc_CO2_within_SCT_pc = wtd.quantile(
        exp_pc_CO2_within_SCT_pc,
        probs = 0.5,
        weights = hh_weights
      ),
      exp_pc_CO2_within_SCT_hh = wtd.quantile(
        exp_pc_CO2_within_SCT_hh,
        probs = 0.5,
        weights = hh_weights
      ),
      
      
      burden_CO2_pc_no_LST = wtd.quantile(
        burden_CO2_pc, probs = 0.5, weights = hh_weights),
      burden_CO2_pc_SCT_hh = wtd.quantile(
        burden_CO2_pc_SCT_hh,  probs = 0.5, weights = hh_weights),
      burden_CO2_pc_SCT_pc = wtd.quantile(
        burden_CO2_pc_SCT_pc,  probs = 0.5, weights = hh_weights),
      
      burden_CO2_within_pc_no_LST = wtd.quantile(
        burden_CO2_within_pc,
        probs = 0.5,
        weights = hh_weights
      ),
      burden_CO2_within_pc_SCT_hh = wtd.quantile(
        burden_CO2_within_pc_SCT_hh,
        probs = 0.5,
        weights = hh_weights
      ),
      burden_CO2_within_pc_SCT_pc = wtd.quantile(
        burden_CO2_within_pc_SCT_pc,
        probs = 0.5,
        weights = hh_weights
      )
    ) |>
    ungroup() |>
    dplyr::select(everything())
  
  list_0 <-
    list("Full_df" = Incidence.Y.1, "Summarised_df" = Incidence.Y.2)
  
  return(list_0)
  
}


tza221_nock <- func_0_nock(tza_final)$Full_df
tza222_nock <-func_0_nock(tza_final)$Summarised_df

###  SCT Boxplot function --------------------------------------------
SCT_g <- tza221_nock |>
  dplyr::select(hh_id, incg_5,Urban,burden_CO2_within_pc_SCT_hh, hh_weights) |> 
  rename(burden_CO2_pc_CP_hh=burden_CO2_within_pc_SCT_hh) |> 
  mutate(comp_type="SCT")


# Merge all Compensations for Box plot---------------------------------------

all_com <- NCP_g |> 
  rbind(LST_g) |> 
  rbind(EST_g) |> 
  rbind(TST_g) |>
  rbind(SCT_g) |>
  arrange(hh_id, incg_5)

all_com_1 <- all_com |>
  dplyr::group_by(Urban,incg_5, comp_type)|>
  dplyr::summarise(
    mean = wtd.mean(burden_CO2_pc_CP_hh,
                    weight = hh_weights,na.rm=T)
  )|>
  ungroup()



all_com_2 <- all_com |>
  left_join(all_com_1, by = c("incg_5", "comp_type","Urban")) |> 
  mutate(Label=ifelse(comp_type=="NCP","Without Compensation",NA),
         Label=ifelse(comp_type=="LST","Lump-sum",Label),
         Label=ifelse(comp_type=="EST","Solar Light",Label),
         Label=ifelse(comp_type=="SCT","Solar Cooker",Label),
         Label=ifelse(comp_type=="TST","Targeted",Label)
  )

all_com_2$Label <- factor(
  all_com_2$Label,
  levels = c(
    "Without Compensation",
    "Lump-sum",
    "Targeted",
    "Solar Light",
    "Solar Cooker"
  )
)

# save data

all_com_test <- all_com_2 |> 
  dplyr::group_by(Urban, incg_5,Label) |> 
  dplyr::summarize(average_hh_budget=wtd.mean(burden_CO2_pc_CP_hh,
                                              weights=hh_weights,
                                              na.rm = T)
  ) |> 
  ungroup()

write.xlsx(all_com_test, 
           file.path(dir[["analysis"]], "hh_budget_change_nl_IGUR.xlsx")
)




# Merge Subset Compensations for Box plot---------------------------------------

## NLT Only ----------------------------------------------------------------


NLT <- NCP_g |> 
  rbind(LST_g) |> 
  rbind(TST_g) |> 
  arrange(hh_id, incg_5)

NLT_1 <- NLT |>
  group_by(incg_5, comp_type)|>
  summarise(
    mean = wtd.mean(burden_CO2_pc_CP_hh,
                    weight = hh_weights,na.rm=T)
  )|>
  ungroup()


NLT_2 <- NLT |>
  left_join(NLT_1, by = c("incg_5", "comp_type")) |> 
  mutate(Label=ifelse(comp_type=="NCP","Without Compensation",NA),
         Label=ifelse(comp_type=="LST","Lump-sum",Label),
         Label=ifelse(comp_type=="TST","Targeted",Label)
  )

NLT_2$Label <- factor(
  NLT_2$Label,
  levels = c(
    "Without Compensation",
    "Lump-sum",
    "Targeted"
  )
)

NLT_2_r <- NLT_2|> 
  filter(Urban=="Rural")

NLT_2_u <- NLT_2|> 
  filter(Urban=="Urban")

## ESST Only ----------------------------------------------------------------

ESST <- NCP_g |>
  rbind(EST_g) |> 
  rbind(SCT_g) |>
  arrange(hh_id, incg_5)

ESST_1 <- ESST |>
  group_by(incg_5, comp_type)|>
  summarise(
    mean = wtd.mean(burden_CO2_pc_CP_hh,
                    weight = hh_weights,na.rm=T)
  )|>
  ungroup()


ESST_2 <- ESST |>
  left_join(ESST_1, by = c("incg_5", "comp_type")) |> 
  mutate(
    Label=ifelse(comp_type=="NCP","Without Compensation",NA),
    Label=ifelse(comp_type=="EST","Solar Light",Label),
    Label=ifelse(comp_type=="SCT","Solar Cooker",Label)
  )

ESST_2$Label <- factor(
  ESST_2$Label,
  levels = c(
    "Without Compensation",
    "Solar Light",
    "Solar Cooker"
  )
)

ESST_2_r <- ESST_2|> 
  filter(Urban=="Rural")

ESST_2_u <- ESST_2|> 
  filter(Urban=="Urban")



## Calculate NLT Schemes for plot ----------------------------------------------

# rural
NLT_2_3_r <- NLT_2_r |>
  dplyr::group_by(incg_5, Label)|>
  dplyr::summarise(y5 = wtd.quantile(burden_CO2_pc_CP_hh, probs = 0.05, weights = hh_weights),
                   y25 = wtd.quantile(burden_CO2_pc_CP_hh, probs = 0.25, weights = hh_weights),
                   y50 = wtd.quantile(burden_CO2_pc_CP_hh, probs = 0.5, weights = hh_weights),
                   y75 = wtd.quantile(burden_CO2_pc_CP_hh, probs = 0.75, weights = hh_weights),
                   y95 = wtd.quantile(burden_CO2_pc_CP_hh, probs = 0.95, weights = hh_weights),
                   mean = wtd.mean(burden_CO2_pc_CP_hh, weights = hh_weights, na.rm = T))|>
  ungroup()

# urban
NLT_2_3_u <- NLT_2_u |>
  dplyr::group_by(incg_5, Label)|>
  dplyr::summarise(y5 = wtd.quantile(burden_CO2_pc_CP_hh, probs = 0.05, weights = hh_weights),
                   y25 = wtd.quantile(burden_CO2_pc_CP_hh, probs = 0.25, weights = hh_weights),
                   y50 = wtd.quantile(burden_CO2_pc_CP_hh, probs = 0.5, weights = hh_weights),
                   y75 = wtd.quantile(burden_CO2_pc_CP_hh, probs = 0.75, weights = hh_weights),
                   y95 = wtd.quantile(burden_CO2_pc_CP_hh, probs = 0.95, weights = hh_weights),
                   mean = wtd.mean(burden_CO2_pc_CP_hh, weights = hh_weights, na.rm = T))|>
  ungroup()


## Appendix Figure A.3a (Monetary-Urban) ---------------------------------------
plot_a <- ggplot(NLT_2_3_u) +
  theme_bw() +
  geom_boxplot(
    aes(
      ymin = y5,
      lower = y25,
      middle = y50,
      upper = y75,
      ymax = y95,
      x = factor(incg_5),
      fill = factor(Label)
    ),
    stat = "identity",
    position = position_dodge(0.7),
    outlier.shape = NA,
    width = 0.5,
    size = 0.2
  ) +
  stat_summary(
    aes(
      y = mean,
      group = interaction(incg_5,Label),
      x = factor(incg_5)
    ),
    fun = "mean",
    geom = "point",
    na.rm = T,
    position =  position_dodge(0.7),
    shape = 23,
    size = 2,
    fill = "white",
    stroke = 0.2
  ) +
  xlab("Expenditure quintiles") + 
  ylab("Household Budget Change") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limit = c(-0.05, 2)
  ) +
  scale_x_discrete(labels = c("1", "2", "3", "4", "5")) +
  scale_fill_manual(
    values = c(
      "Without Compensation" =  "red",
      "Lump-sum" =  "#882255",
      "Targeted" = "#D55E00"
    ),
    labels = c(
      "red"="Without Compensation",
      "#882255" = "Lump-sum",
      "#D55E00" = "Targeted"
    )
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme(
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.title  = element_text(size = 12),
    plot.title = element_text(size = 7, hjust=0.5,face = "bold"),
    legend.position = "bottom",
    strip.text = element_text(size = 12),
    strip.text.y = element_text(angle = 180),
    panel.grid.major = element_line(linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(linewidth = 0.2),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    plot.margin = unit(c(0.1, 0.1, 0, 0), "cm"),
    panel.border = element_rect(linewidth = 0.3)
  )+
  labs(colour = "",linetype = "", fill = "") +
  guides(fill = guide_legend(nrow = 1, order = 2)
  )

plot_a


# save plot
png(filename= file.path(dir[["figures"]], 
                        "Appendix_Figure_A3a.png"),
    width = 15.5, height = 15, unit = "cm", res = 400)
print(plot_a)
dev.off()


## Appendix Figure A.3b (Monetary-Rural) ---------------------------------------
plot_a <- ggplot(NLT_2_3_r) +
  theme_bw() +
  geom_boxplot(
    aes(
      ymin = y5,
      lower = y25,
      middle = y50,
      upper = y75,
      ymax = y95,
      x = factor(incg_5),
      fill = factor(Label)
    ),
    stat = "identity",
    position = position_dodge(0.7),
    outlier.shape = NA,
    width = 0.5,
    size = 0.2
  ) +
  stat_summary(
    aes(
      y = mean,
      group = interaction(incg_5,Label),
      x = factor(incg_5)
    ),
    fun = "mean",
    geom = "point",
    na.rm = T,
    position =  position_dodge(0.7),
    shape = 23,
    size = 2,
    fill = "white",
    stroke = 0.2
  ) +
  xlab("Expenditure quintiles") + 
  ylab("Household Budget Change") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limit = c(-0.05, 2)
  ) +
  scale_x_discrete(labels = c("1", "2", "3", "4", "5")) +
  scale_fill_manual(
    values = c(
      "Without Compensation" =  "red",
      "Lump-sum" =  "#882255",
      "Targeted" = "#D55E00"
    ),
    labels = c(
      "red"="Without Compensation",
      "#882255" = "Lump-sum",
      "#D55E00" = "Targeted"
    )
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme(
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.title  = element_text(size = 12),
    plot.title = element_text(size = 7, hjust=0.5,face = "bold"),
    legend.position = "bottom",
    strip.text = element_text(size = 12),
    strip.text.y = element_text(angle = 180),
    panel.grid.major = element_line(linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(linewidth = 0.2),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    plot.margin = unit(c(0.1, 0.1, 0, 0), "cm"),
    panel.border = element_rect(linewidth = 0.3)
  )+
  labs(colour = "",linetype = "", fill = "") +
  guides(fill = guide_legend(nrow = 1, order = 2)
  )

plot_a


# save plot
png(filename= file.path(dir[["figures"]], 
                        "Appendix_Figure_A3b.png"),
    width = 15.5, height = 15, unit = "cm", res = 400)
print(plot_a)
dev.off()



## Calculate ESST Schemes for plots --------------------------------------------

ESST_2_3_r <- ESST_2_r |>
  dplyr::group_by(incg_5, Label)|>
  dplyr::summarise(y5 = wtd.quantile(burden_CO2_pc_CP_hh, probs = 0.05, weights = hh_weights),
                   y25 = wtd.quantile(burden_CO2_pc_CP_hh, probs = 0.25, weights = hh_weights),
                   y50 = wtd.quantile(burden_CO2_pc_CP_hh, probs = 0.5, weights = hh_weights),
                   y75 = wtd.quantile(burden_CO2_pc_CP_hh, probs = 0.75, weights = hh_weights),
                   y95 = wtd.quantile(burden_CO2_pc_CP_hh, probs = 0.95, weights = hh_weights),
                   mean = wtd.mean(burden_CO2_pc_CP_hh, weights = hh_weights, na.rm = T))|>
  ungroup()


ESST_2_3_u <- ESST_2_u |>
  dplyr::group_by(incg_5, Label)|>
  dplyr::summarise(y5 = wtd.quantile(burden_CO2_pc_CP_hh, probs = 0.05, weights = hh_weights),
                   y25 = wtd.quantile(burden_CO2_pc_CP_hh, probs = 0.25, weights = hh_weights),
                   y50 = wtd.quantile(burden_CO2_pc_CP_hh, probs = 0.5, weights = hh_weights),
                   y75 = wtd.quantile(burden_CO2_pc_CP_hh, probs = 0.75, weights = hh_weights),
                   y95 = wtd.quantile(burden_CO2_pc_CP_hh, probs = 0.95, weights = hh_weights),
                   mean = wtd.mean(burden_CO2_pc_CP_hh, weights = hh_weights, na.rm = T))|>
  ungroup()


## Appendix Figure A.3c (Non-monetary-Urban) ---------------------------------------
plot_a <- ggplot(ESST_2_3_u) +
  theme_bw() +
  geom_boxplot(
    aes(
      ymin = y5,
      lower = y25,
      middle = y50,
      upper = y75,
      ymax = y95,
      x = factor(incg_5),
      fill = factor(Label)
    ),
    stat = "identity",
    position = position_dodge(0.7),
    outlier.shape = NA,
    width = 0.5,
    size = 0.2
  ) +
  stat_summary(
    aes(
      y = mean,
      group = interaction(incg_5,Label),
      x = factor(incg_5)
    ),
    fun = "mean",
    geom = "point",
    na.rm = T,
    position =  position_dodge(0.7),
    shape = 23,
    size = 2,
    fill = "white",
    stroke = 0.2
  ) +
  xlab("Expenditure quintiles") + 
  ylab("Household Budget Change") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limit = c(-0.05, 2)
                     #expand = c(-0.05, 0.1)
  ) +
  scale_x_discrete(labels = c("1", "2", "3", "4", "5")) +
  scale_fill_manual(
    values = c(
      "Without Compensation" =  "red",
      "Solar Light" = "#F0E442",
      "Solar Cooker" = "#0072B2"
    ),
    labels = c(
      "red"="Without Compensation",
      "#F0E442" = "Solar Light",
      "#0072B2" = "Solar Cooker"
    )
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme(
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.title  = element_text(size = 12),
    plot.title = element_text(size = 7, hjust=0.5,face = "bold"),
    legend.position = "bottom",
    strip.text = element_text(size = 12),
    strip.text.y = element_text(angle = 180),
    panel.grid.major = element_line(linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(linewidth = 0.2),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    plot.margin = unit(c(0.1, 0.1, 0, 0), "cm"),
    panel.border = element_rect(linewidth = 0.3)
  )+
  labs(colour = "",linetype = "", fill = "") +
  guides(fill = guide_legend(nrow = 1, order = 2)
  )

plot_a


# save plot
png(filename= file.path(dir[["figures"]], 
                        "Appendix_Figure_A3c.png"),
    width = 15.5, height = 15, unit = "cm", res = 400)
print(plot_a)
dev.off()




## Appendix Figure A.3d (Non-monetary-Rural) ---------------------------------------
plot_a <- ggplot(ESST_2_3_r) +
  theme_bw() +
  geom_boxplot(
    aes(
      ymin = y5,
      lower = y25,
      middle = y50,
      upper = y75,
      ymax = y95,
      x = factor(incg_5),
      fill = factor(Label)
    ),
    stat = "identity",
    position = position_dodge(0.7),
    outlier.shape = NA,
    width = 0.5,
    size = 0.2
  ) +
  stat_summary(
    aes(
      y = mean,
      group = interaction(incg_5,Label),
      x = factor(incg_5)
    ),
    fun = "mean",
    geom = "point",
    na.rm = T,
    position =  position_dodge(0.7),
    shape = 23,
    size = 2,
    fill = "white",
    stroke = 0.2
  ) +
  xlab("Expenditure quintiles") + 
  ylab("Household Budget Change") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limit = c(-0.05, 2)
  ) +
  scale_x_discrete(labels = c("1", "2", "3", "4", "5")) +
  scale_fill_manual(
    values = c(
      "Without Compensation" =  "red",
      "Solar Light" = "#F0E442",
      "Solar Cooker" = "#0072B2"
    ),
    labels = c(
      "red"="Without Compensation",
      "#F0E442" = "Solar Light",
      "#0072B2" = "Solar Cooker"
    )
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme(
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.title  = element_text(size = 12),
    plot.title = element_text(size = 7, hjust=0.5,face = "bold"),
    legend.position = "bottom",
    strip.text = element_text(size = 12),
    strip.text.y = element_text(angle = 180),
    panel.grid.major = element_line(linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(linewidth = 0.2),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    plot.margin = unit(c(0.1, 0.1, 0, 0), "cm"),
    panel.border = element_rect(linewidth = 0.3)
  )+
  labs(colour = "",linetype = "", fill = "") +
  guides(fill = guide_legend(nrow = 1, order = 2)
  )

plot_a


# save plot
png(filename= file.path(dir[["figures"]], 
                        "Appendix_Figure_A3d.png"),
    width = 15.5, height = 15, unit = "cm", res = 400)
print(plot_a)
dev.off()



rm(list = ls())


















