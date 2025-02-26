# Authors: Laura Schürer and Abigail O. Asare

# Date:26/02/2025

# Load relevant Packages------------------------------------------
library("data.table")
library("foreign")
library("haven")
library("tidyverse")
library("tidyr")
library("dplyr")
library("Hmisc")
library("expss")
library("ggplot2")
library("officer")
library("openxlsx")
library("rattle")
library("reshape2")
library("scales")
library("readr")

# Currency is in TSH
# Year = 2019_2020 

# Setup -------------------------------------------------------------------
source("./00_setup.R")

# Household Data ----------------------------------------------------------
## Household tracking -----------------------------------------------------

# HH General Info 
hh_a <- read_dta(file.path(dir[["tza"]], "HH_SEC_A.dta"))

# HH Demographics
hh_b <- read_dta(file.path(dir[["tza"]], "HH_SEC_B.dta"))

# HH Education
hh_c <- read_dta(file.path(dir[["tza"]], "HH_SEC_C.dta"))

# HH Occupation and income
hh_e <- read_dta(file.path(dir[["tza"]], "HH_SEC_E1.dta"))

# HH Dwelling characteristics (water, toilet etc,)
hh_i <- read_dta(file.path(dir[["tza"]], "HH_SEC_I.dta"))

#  HH Food Consumption
hh_j1 <- read_dta(file.path(dir[["tza"]], "HH_SEC_J1.dta"))


#  HH Non Food Consumption Expanded
hh_k <- read_dta(file.path(dir[["tza"]], "HH_SEC_K.dta"))

hh_l <- read_dta(file.path(dir[["tza"]], "HH_SEC_L.dta"))

## Generate codes ---------------------------------------

urban <-
  data.frame(
    "y5_rural" = c(1, 2, 3, 4),
    "urban" = c(1, 2, 1, 2),
    "Urban" = c("Rural", "Urban", "Rural", "Urban")
  )
urban_code <- urban |>
  select(urban, Urban) |>
  filter(!duplicated(urban))


education_code <-
  data.frame(
    "education" = c(seq(12, 25), 31, 32, 33, 34, 41, 42, 43, 44, 45, 1, 2),
    "Label" = c(
      "Primary DI",
      "Primary D2",
      "Primary D3",
      "Primary D4",
      "Primary D5",
      "Primary D6",
      "Primary D7",
      "Primary D8",
      "Primary osc",
      "Secondary F1",
      "Secondary F2",
      "Secondary F3",
      "Secondary F4",
      "Secondary O+COURSE",
      "Secondary F5",
      "Secondary F6",
      "Secondary A+COURSE",
      "diploma",
      "university U1",
      "university U2",
      "university U3",
      "university U4",
      "university U5&+",
      "pre-primary",
      "other"
    )
  )

# Generate codes

electricity_code <-
  data.frame(
    "electricity_type" = c(1:7),
    "Label" = c(
      "tanesco",
      "community generator",
      "solar panels",
      "own generator",
      "car battery",
      "motorcycle",
      "battery"
    )
  )


water_code <-
  data.frame(
    "drinking_water_typ" = c(1:12),
    "Label" = c(
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
      "Other, specify"
    )
  )

toilet_code <-
  data.frame(
    "toilet_typ" = c(1:9),
    "Label" = c(
      "NO TOILET",
      "PIT LATRINE WITHOUT SLAB/OPEN",
      "PIT PIT LATRINE WITH SLAB (NOT WASHABLE)",
      "PIT LATRINE WITH SLAB (WASHABLE)",
      "vip",
      "POUR FLUSH",
      "FLUSH TOILET",
      "ecosan",
      "Other, specify"
    )
  )


dwelling_code <-
  data.frame(
    "dwelling_code" = c(1:6),
    "Label" = c(
      "OWNER OCCUPIED EMPLOYER",
      "PROVIDED - SUBSIDIZED",
      "EMPLOYER PROVIDED - FREE",
      "rented",
      "free",
      "nomads"
    )
  )

cooking_code <-
  data.frame(
    "cooking_fuel" = c(seq(1:8)),
    "Label" = c(
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

lighting_code <-
  data.frame(
    "lighting_fuel" = c(seq(1:10)),
    "Label" = c(
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

wall_code <-
  data.frame(
    "wall_mat" = c(seq(1:7)),
    "Label" = c(
      "POLES (INCLUDING BAMBOO), BRANCHES, GRASS",
      "POLES AND MUD/MUD AND STONES",
      "MUD ONLY",
      "MUD BRICKS",
      "BAKED/BURNT BRICKS",
      "CONCRETE, CEMENT, STONES",
      "OTHER (SPECIFY)"
    )
  )

roof_code <-
  data.frame(
    "roof_mat" = c(seq(1:7)),
    "Label" = c(
      "GRASS, LEAVES, BAMBOO",
      "MUD AND GRASS",
      "CONCRETE, CEMENT",
      "METAL SHEETS (GCI)",
      "ASBESTOS SHEETS",
      "TILES",
      "OTHER (SPECIFY)"
    )
  )

floor_code <-
  data.frame(
    "floor_mat" = c(seq(1:3)),
    "Label" = c(
      "EARTH",
      "CONCRETE, CEMENT, TILES, TIMBER",
      "OTHER (SPECIFY)"
    )
  )


write_csv(water_code,file.path(dir[["tza_processed"]], "Water_Code.csv"))
write_csv(lighting_code,file.path(dir[["tza_processed"]], "Lighting_Code.csv"))
write_csv(education_code, file.path(dir[["tza_processed"]],"Education_Code.csv"))
write_csv(cooking_code, file.path(dir[["tza_processed"]],"cooking_Code.csv"))
write_csv(electricity_code,file.path(dir[["tza_processed"]], "Electricity_Code.csv"))
write_csv(urban_code, file.path(dir[["tza_processed"]],"Urban_Code.csv"))
write_csv(toilet_code, file.path(dir[["tza_processed"]],"Toilet_Code.csv"))
write_csv(dwelling_code,file.path(dir[["tza_processed"]], "Dwelling_Code.csv"))
write_csv(wall_code,file.path(dir[["tza_processed"]], "Wall_Code.csv"))
write_csv(roof_code,file.path(dir[["tza_processed"]], "Roof_Code.csv"))
write_csv(floor_code,file.path(dir[["tza_processed"]], "Floor_Code.csv"))




##  Household General Information ----------------------------------------------

hh_gen <- hh_a |>
  dplyr::select(sdd_hhid,sdd_weights, sdd_rural) |>
  dplyr::rename(hh_id = sdd_hhid,
                hh_weights = sdd_weights,
                urban = sdd_rural)

# Remove hyphens from hh_id

hh_gen$hh_id <- gsub("-", "", hh_gen$hh_id)



## HH Demographic  -----------------------------------------


hh_demo <- hh_b |>
  dplyr::select(sdd_hhid, sdd_indid, hh_b04) |>
  dplyr::rename(hh_id = sdd_hhid) |>
  group_by(hh_id) |>
  mutate(hh_size = n()) |>
  mutate(adults = ifelse(hh_b04 > 18, 1, 0)) |>
  mutate(children = ifelse(hh_b04 < 18, 1, 0)) |>
  summarise(
    hh_size = first(hh_size),
    children = sum(children),
    adults = sum(adults)
  ) |>
  ungroup()

hh_demo$hh_id <- gsub("-", "", hh_demo$hh_id)

## HH Education --------------------------------------------------------------


hh_edu <- hh_c |>
  dplyr::select(sdd_hhid, sdd_indid, hh_c07) |>
  dplyr::rename(hh_id = sdd_hhid, hh_edu = hh_c07)

hh_edu$hh_id <- gsub("-", "", hh_edu$hh_id)




# HH Income and expenditure (not joined) ----------------------------------------------------

hh_income <- hh_e |>
  dplyr::select(
    sdd_hhid,
    sdd_indid,
    hh_e30a,
    hh_e33,
    hh_e34,
    hh_e35,
    hh_e35_2,
    hh_e36,
    hh_e37a,
    hh_e38,
    hh_e46,
    hh_e49,
    hh_e50,
    hh_e51a,
    hh_e51b,
    hh_e52,
    hh_e53a,
    hh_e53b
  ) |>
  dplyr::rename(hh_id = sdd_hhid, indiv_id = sdd_indid)


hh_income$hh_id <- gsub("-", "", hh_income$hh_id)

hh_income_non <- hh_income |> 
  filter(hh_e35!=0)




# HH Dwelling Characteristics -------------------------------------------------

hh_char <- hh_i |>
  dplyr::select(
    sdd_hhid,
    hh_i01,
    hh_i03,
    hh_i04,
    hh_i08,
    hh_i09,
    hh_i10,
    hh_i12,
    hh_i16,
    hh_i17,
    hh_i18,
    hh_i19,
    hh_i20,
    hh_i25,
    hh_i26,
    hh_i36
  ) |>
  dplyr::rename(
    hh_id = sdd_hhid,
    residence_status = hh_i01,
    wall_mat=hh_i08,
    roof_mat=hh_i09,
    floor_mat=hh_i10,
    rent_amt = hh_i03,
    rent_amt_est = hh_i04,
    toilet_typ = hh_i12,
    cooking_fuel = hh_i16,
    lighting_fuel = hh_i17,
    electricity_typ = hh_i18,
    drinking_water_typ = hh_i19,
    other_water_typ = hh_i25,
    water_amt_1 = hh_i26,
    water_amt_2 = hh_i36
  )

hh_char <-hh_char |> 
  mutate(access_ely_bd=ifelse(is.na(electricity_typ),0,1),
         access_ely_st=NA,
         access_ely_st=ifelse(is.na(electricity_typ),0,access_ely_st),
         access_ely_st=ifelse(electricity_typ==1|electricity_typ==2|
                                electricity_typ==3|electricity_typ==4
                              ,1,access_ely_st),
         access_ely_st=ifelse(is.na(access_ely_st),0,access_ely_st)
  ) |> 
  mutate(access_ely_grid=NA,
         access_ely_grid=ifelse(is.na(electricity_typ),0,access_ely_grid),
         access_ely_grid=ifelse(electricity_typ==1,1,access_ely_grid),
         access_ely_grid=ifelse(is.na(access_ely_grid),0,access_ely_grid),
         access_ely_solar=NA,
         access_ely_solar=ifelse(is.na(electricity_typ),0,access_ely_solar),
         access_ely_solar=ifelse(electricity_typ==3,1,access_ely_solar),
         access_ely_solar=ifelse(is.na(access_ely_solar),0,access_ely_solar),
         access_ely_other=NA,
         access_ely_other=ifelse(is.na(electricity_typ),0,access_ely_other),
         access_ely_other=ifelse(electricity_typ!=1|electricity_typ!=3, 
                                 1,access_ely_other),
         access_ely_other=ifelse(is.na(access_ely_other),0,access_ely_other)
  ) |> 
  mutate(access_sanit_st=NA,
         access_sanit_st=ifelse(is.na(toilet_typ),0,1),
         access_sanit_st=ifelse(toilet_typ==1,0,access_sanit_st),
         #access_sanit_st=ifelse(hh_i12d==3,0,access_sanit_st),
         access_sanit_st=ifelse(is.na(access_sanit_st),0,access_sanit_st)
  ) |> 
  #Where is the main water source for drinking during the rainy season located?
  mutate(access_water_st=NA,
         access_water_st=ifelse(is.na(hh_i20),0,1),
         access_water_st=ifelse(hh_i20==3|hh_i20==4,0,access_sanit_st),
         access_water_st=ifelse(is.na(access_water_st),0,access_water_st)
  ) 


hh_char$hh_id <- gsub("-", "", hh_char$hh_id)

## Merge all HH Information -------------------------------------------

HH_info <- hh_gen |> 
  left_join(hh_demo,by="hh_id") |>
  left_join(hh_edu,by="hh_id") |>
  left_join(hh_char,by="hh_id") |> 
  distinct(hh_id, .keep_all = TRUE)






# Food Expenditure --------------------------------------------

# currency in TSH
# unit=ut
# quantity=qqt
# amount=amt

# Extract Food item labels   

item_labels <- attr(hh_j1$itemcode, "labels")

# Create a new dataframe with variable names, variable labels, and value labels
labels_food <- data.frame(
  code = unname(item_labels),
  food_item = names(item_labels)
)

# Export the new data frame as an Excel file

write.xlsx(labels_food,file.path(dir[["tza_processed"]], "labels_food_tza.xlsx"))

hh_food <- hh_j1 |>
  dplyr::select(
    sdd_hhid,
    itemcode,
    hh_j02_1,
    hh_j02_2,
    hh_j03_1,
    hh_j03_2,
    hh_j04,
    hh_j05_1,
    hh_j05_2
  ) |>
  dplyr::rename(
    hh_id = sdd_hhid,
    consume_7ut = hh_j02_1,
    consume_7qqt = hh_j02_2,
    purchase_7ut = hh_j03_1,
    purchase_7qqt = hh_j03_2,
    amount = hh_j04,
    ownprod_ut = hh_j05_1,
    ownprod_qqt = hh_j05_2
  )

hh_food$hh_id <- gsub("-", "", hh_food$hh_id)


# Replace missings with zero for futher analyses

hh_food[is.na(hh_food)] <- 0

## Select relevant variables And Calculate Yearly Expenditure ---------------
# Amount is the expenditure for the past 7 days
# 2019=365days

hhyr_food <- hh_food |> 
  select(hh_id,itemcode,amount) |> 
  mutate(expenditures=(amount/7)*365) |> 
  select(hh_id,itemcode,expenditures)

## Add prefix to itemcodes for later appending -----------------------
# some of the item codes are same for non-food codes
# Prefix is f = food

hhyr_food <- hhyr_food |>
  mutate(itemcode = paste("f", itemcode, sep = ""))



## Reshape to wide-format -----------------------------------------

hhyr_food <- hhyr_food |> 
  pivot_wider(names_from = itemcode, values_from = expenditures)







# Non_Food Expenditure (last week or 30 days) ---------------------

# Extract  item labels   

item_labels <- attr(hh_k$itemcode, "labels")

# Create a new dataframe with variable names, variable labels, and value labels
labels_nonfood_7_12 <- data.frame(
  code = unname(item_labels),
  nonfood_item = names(item_labels)
)

# Export the new data frame as an Excel file

write.xlsx(labels_nonfood_7_12 ,file.path(dir[["tza_processed"]], "labels_nonfood_7_12_tza.xlsx"))

hh_nonfood1 <- hh_k |>
  dplyr::select(
    sdd_hhid,
    itemcode,
    hh_k01,
    hh_k02
  ) |>
  dplyr::rename(
    hh_id = sdd_hhid,
    purchase = hh_k01,
    amount = hh_k02
  )

hh_nonfood1$hh_id <- gsub("-", "", hh_nonfood1$hh_id)


summary(hh_nonfood1)


# Replace missings with zero for further analyses

hh_nonfood1[is.na(hh_nonfood1)] <- 0

## Select relevant variables And Calculate Yearly Expenditure ---------------
# Amount is the expenditure for the past 7 days/ 30 days
# 2019=365days

hhyr_nonfood1 <- hh_nonfood1 |> 
  select(hh_id,itemcode,amount) |> 
  mutate(expenditures=ifelse(itemcode<=103,((amount/7)*365),amount),
         expenditures=ifelse(itemcode>=201,((amount/30)*365),expenditures))|> 
  select(hh_id,itemcode,expenditures)

# Add prefix to itemcodes for later appending -----------------------
# some of the item codes are same for non-food codes
# Prefix is n = nonfood

hhyr_nonfood1 <- hhyr_nonfood1 |>
  mutate(itemcode = paste("n", itemcode, sep = ""))

## Reshape to wide-format -----------------------------------------

hhyr_nonfood1 <- hhyr_nonfood1 |> 
  pivot_wider(names_from = itemcode, values_from = expenditures)


# Non_Food Expenditure (past 12 months) ----------------------------------------

item_labels <- attr(hh_l$itemcode, "labels")

# Create a new dataframe with variable names, variable labels, and value labels
labels_nonfood_12 <- data.frame(
  code = unname(item_labels),
  nonfood_item = names(item_labels)
)

# Export the new data frame as an Excel file

write.xlsx(labels_nonfood_12 ,file.path(dir[["tza_processed"]], "labels_nonfood_12_tza.xlsx"))


hh_nonfood2 <- hh_l |>
  dplyr::select(
    sdd_hhid,
    itemcode,
    hh_l01,
    hh_l02a,
    hh_l02b,
    hh_l03
  ) |>
  dplyr::rename(
    hh_id = sdd_hhid,
    purchase = hh_l01,
    amount = hh_l02a,
    amount_est=hh_l02b,
    cost=hh_l03
  )

hh_nonfood2$hh_id <- gsub("-", "", hh_nonfood2$hh_id)

# Replace missings with zero for further analyses

hh_nonfood2[is.na(hh_nonfood2)] <- 0

## Select relevant variables And Calculate Yearly Expenditure ---------------
# Amount is the expenditure for the past 12 months
# 2019=365days

hhyr_nonfood2 <- hh_nonfood2 |> 
  select(hh_id,itemcode,amount) |> 
  mutate(expenditures=amount)|> 
  select(hh_id,itemcode,expenditures)

# Add prefix to itemcodes for later appending -----------------------
# some of the item codes are same for non-food codes
# Prefix is n = nonfood

hhyr_nonfood2 <- hhyr_nonfood2 |>
  mutate(itemcode = paste("n", itemcode, sep = ""))

## Reshape to wide-format -----------------------------------------

hhyr_nonfood2 <- hhyr_nonfood2 |> 
  pivot_wider(names_from = itemcode, values_from = expenditures)




# I do not know about HH_SEC_m because it does not have any time component
# No religion, ethnicity in all the datasets


# Merge Expenditure data sets -------------------------------------------

# did not merge food_expanded and income


tza_expenditure <- hhyr_food |>
  left_join(hhyr_nonfood1, by = "hh_id")|>
  left_join(hhyr_nonfood2, by = "hh_id")

# Merge All data ---------------------------------------------


tza_clean <- HH_info |>
  left_join(tza_expenditure, by = "hh_id")


tza_clean <-tza_clean |> 
  pivot_longer(cols=31:142, names_to="itemcode", values_to="expenditures")



# Clean Merge data -------------------------------------------------------------

# keep only non-zero or non missing expenditures

tza_clean <-tza_clean |> 
  filter(expenditures != 0)


# Save final Clean data as csv -------------------------------------------------

write_csv(tza_clean,file.path(dir[["tza_processed"]],"clean_tanzania_2019.csv"))


rm(list = ls())



