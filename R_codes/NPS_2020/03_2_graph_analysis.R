# Authors: Abigail O. Asare and Laura Schürer 

# Date: 26/02/2025
# Authors: Laura Schürer and Abigail O. Asare

# Email: laura.schuerer@uol.de and abigail.asare@uol.de

# Date:07/02/2025

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
library(ggpubr)

# Currency is in TSH
# Year = 2019

# Setup -------------------------------------------------------------------
source("./00_setup.R")


# Read Incidence data set --------------------------------------------------

tza_final <-
  read_csv(file.path(dir[["analysis"]], "inci_analysis_tza.csv"))

tza_info <-
  read_csv(file.path(dir[["analysis"]], "hh_information_tza.csv")) %>% 
  mutate(Urban=ifelse(urban==1,"Rural","Urban")) %>% 
  filter(!is.na(hh_id)) %>% 
  filter(!is.na(hh_weights)) %>%
  filter(!is.na(hh_size)) 

# Appendix Figure A6a (Broad Categories)  --------------------------------------
# Engel-Curves of Energy Expenditures --Broad categories

tza_sub_f <- tza_final %>% 
  select(hh_id,hh_exp_USD,hh_exp_USD_pc)


types_tza <-  read_csv(file.path(dir[["analysis"]], "Expenditure_types_tza.csv"))

types_tza <- types_tza %>% 
  mutate(hh_id = as.numeric(hh_id)) 

tza_sub_f <- tza_sub_f %>% 
  mutate(hh_id = as.numeric(hh_id)) 

tza_info <- tza_info %>% 
  mutate(hh_id = as.numeric(hh_id)) 

types_tza <- types_tza %>% 
  left_join(tza_info, by="hh_id") %>% 
  left_join(tza_sub_f, by="hh_id")


types_tza_1 <- types_tza %>%
  select(hh_id, hh_weights, hh_size, hh_exp_USD,
         Urban, starts_with("share_"))%>%
  replace(is.na(.), 0) %>% 
  pivot_longer(!(hh_id:Urban), names_to = "Types", values_to = "share")%>%
  filter(Types!="share_other") %>% 
  mutate(incg_5   = as.numeric(binning(hh_exp_USD,
                                       bins=5  , 
                                       method = c("wtd.quantile"), 
                                       labels = seq(1,5,length.out = 5),
                                       weights = hh_weights)))%>%
  mutate(incg_100 = as.numeric(binning(hh_exp_USD,
                                       bins=100, 
                                       method = c("wtd.quantile"),  
                                       weights = hh_weights)))


plot_2a <- ggplot(types_tza_1) +
  geom_smooth(
    formula = as.formula (y ~ x),
    aes(
      x = incg_100,
      y = share,
      weight = hh_weights,
      colour = factor(Types),
      fill = factor(Types)
    ),
    size = 0.2,
    method = "loess",
    se = TRUE,
    fullrange = TRUE,
    span = 1
  ) +
  theme_bw() +
  theme(
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 7, hjust = 0.5, face = "bold"),
    legend.position = "bottom",
    axis.ticks = element_line(linewidth = 0.2),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    plot.margin = unit(c(0.1, 0.1, 0, 0), "cm"),
    panel.border = element_rect(size = 0.3)
  ) +
  xlab("Expenditure Centile") + 
  ylab("Expenditure Share") + 
  labs(colour = "", linetype = "") +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = c(0, 0)
  ) +
  scale_x_continuous(expand = c(0, 0), 
                     breaks = seq(0,100,25)
  ) +
  scale_colour_manual(
    name = "",
    labels = c("Energy", "Food", "Goods", "Services"),
    values = viridis_pal()(6)[-2]
  ) +
  scale_fill_manual(
    name = "",
    labels = c("Energy", "Food", "Goods", "Services"),
    values = viridis_pal()(6)[-2]
  ) +
  guides(colour = guide_legend(nrow = 1), fill = guide_legend(nrow = 1))+
  labs(fill = "", colour = "")


plot_2a

png(filename= file.path(dir[["figures"]], "Appendix_Figure_A6a.png"), 
    width = 15.5, height = 15, unit = "cm", res = 400)
print(plot_2a)
dev.off()




# Appendix Figure A.6b (Energy Types)  -----------------------------------------------------
# Engel-Curves of Energy Expenditures--- Energy types

tza_sub_f <- tza_final %>% 
  select(hh_id,hh_exp_USD,hh_exp_USD_pc)


fuels_tza <-  read_csv(file.path(dir[["analysis"]], 
                                 "Expenditure_fuels_17USD_tza.csv"))


fuels_tza <- fuels_tza %>% 
  left_join(tza_info, by="hh_id") 

fuels_tza <- fuels_tza %>% 
  mutate(hh_id = as.numeric(hh_id)) 


fuels_tza <- fuels_tza %>% 
  left_join(tza_sub_f, by="hh_id") %>% 
  filter(!is.na(hh_id)) %>% 
  filter(!is.na(hh_size)) 

# function
transform_0 <- function(x){
  y <- x %>%
    replace(is.na(.), 0) %>%  
    mutate(
      exp_Cooking     = USD_Kerosene + USD_Gas,
      exp_Transport   = USD_Petrol_Diesel,
      exp_Electricity = USD_Electricity,
      exp_Energy      = exp_Cooking + exp_Transport +
        exp_Electricity 
    )%>%
    mutate(incg_5   = as.numeric(binning(hh_exp_USD, 
                                         bins=5  , 
                                         method = c("wtd.quantile"),
                                         labels = seq(1,5,length.out = 5), 
                                         weights = hh_weights)))%>%
    mutate(incg_100 = as.numeric(binning(hh_exp_USD,
                                         bins=100, 
                                         method = c("wtd.quantile"),  
                                         weights = hh_weights)))
  
  y1 <- y %>%
    select(hh_id, hh_weights, hh_size, hh_exp_USD, incg_100,
           incg_5)
  
  y2 <- y %>%
    select(hh_id, starts_with("exp_"))%>%
    pivot_longer(!hh_id, names_to = "type", values_to ="exp_USD")
  
  y3 <- left_join(y2, y1, by = "hh_id")%>%
    dplyr::group_by(incg_100, type)%>%
    dplyr::summarise(mean_exp_USD    = weighted.mean(exp_USD, hh_weights),
                     mean_hh_inc_USD = weighted.mean(hh_exp_USD, hh_weights),
                     hh_weights      = sum(hh_weights))%>%
    ungroup()%>%
    mutate(share = mean_exp_USD/mean_hh_inc_USD)%>%
    filter(type != "exp_Energy")
  
  y4 <- left_join(y2, y1, by = "hh_id")%>%
    mutate(share = exp_USD/hh_exp_USD)%>%
    mutate(exp_pc_hh = hh_exp_USD/hh_size)
  
  y5 <- y %>%
    select(hh_id, hh_weights,hh_exp_USD, incg_5)%>%
    dplyr::group_by(incg_5)%>%
    dplyr::summarise(avg.income = wtd.mean(hh_exp_USD, hh_weights))%>%
    ungroup()
  
  out <- list('t1' = y3, 't2' = y4, 't3' = y5)
  
}

dec_2_tza <- transform_0(fuels_tza)$t1


plot_2b <- ggplot(dec_2_tza) +
  geom_smooth(
    formula = as.formula (y ~ x),
    aes(
      x = incg_100,
      y = share,
      weight = hh_weights,
      colour = factor(type),
      fill = factor(type)
    ),
    size = 0.3,
    method = "loess",
    se = TRUE,
    fullrange = TRUE,
    span = 0.75
  ) +
  theme_bw() +
  theme(
    axis.text.y = element_text(size = 12, vjust = 0.1),
    axis.text.x = element_text(size = 12, vjust = 0.1),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 7, hjust=0.5,face="bold"),
    legend.position = "bottom",
    strip.text = element_text(size = 12),
    strip.text.y = element_text(angle = 180),
    panel.grid.major = element_line(linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(linewidth = 0.2),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    plot.margin = unit(c(0.1, 0.1, 0, 0), "cm"),
    panel.border = element_rect(size = 0.3)
  ) +
  xlab("Expenditure Centile") + 
  ylab( "Expenditure Share") + 
  labs(colour = "", linetype = "") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = c(0, 0),
                     breaks = seq(0.0, 0.06, 0.02)
  ) +
  scale_x_continuous(expand = c(0, 0), 
                     breaks = seq(0, 100, 25)
  ) +
  coord_cartesian(ylim = c(0, 0.06), xlim = c(0, 100)) +
  scale_colour_npg(
    name = "",
    labels = c(
      "Cooking Fuels",
      "Electricity",
      "Transport Fuels"
    )
  ) +
  scale_fill_npg  (
    name = "",
    labels = c(
      "Cooking Fuels",
      "Electricity",
      "Transport Fuels"
    )
  ) 
guides(colour = guide_legend(nrow = 1), fill = guide_legend(nrow = 1))

plot_2b

png(filename= file.path(dir[["figures"]], "Appendix_Figure_A6b.png"), 
    width = 15.5, height = 15, unit = "cm", res = 400)
print(plot_2b)
dev.off()




rm(list = ls())













