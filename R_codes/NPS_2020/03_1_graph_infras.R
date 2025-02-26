# Authors: Abigail O. Asare and Laura Schürer 

# Date: 26/02/2025

# Load relevant Packages------------------------------------------
# library("cowplot")
library("dplyr")
library("data.table")
library("foreign")
library("ggpubr")
library("tidyverse")
library("readr")
library("readxl")
library("colorspace")
library(ggforce)
library(ggmagnify)
library(ggfx)

# Currency is in TSH
# Year = 2020

# Setup -------------------------------------------------------------------
source("./00_setup.R")


# Infrastructures Figures --------------------------------------------------

tza_infras <-
  read_csv(file.path(dir[["analysis"]], "hh_infrastructure_IG5UR_tza.csv")) %>% 
  pivot_longer(cols = c(access_sanit_st,access_ely_grid, access_water_st,access_ely_solar),
               names_to = "Type",values_to = "number") %>% 
  dplyr::select(Urban,incg_5,Type,number) %>% 
  mutate(infras_Label=ifelse(Type=="access_ely_grid","grid electricity",NA),
         infras_Label=ifelse(Type=="access_ely_solar","solar electricity",infras_Label),
         infras_Label=ifelse(Type=="access_water_st","water",infras_Label),
         infras_Label=ifelse(Type=="access_sanit_st","sanitation",infras_Label)
  )


# Reordering the levels of the infrastructure
tza_infras$infras_Label <- factor(tza_infras$infras_Label, 
                                  levels = c("grid electricity",
                                             "solar electricity",
                                             "water",
                                             "sanitation"))

tza_infras_r <-tza_infras %>% 
  filter(Urban=="Rural") %>% 
  na.omit()

tza_infras_u <-tza_infras %>% 
  filter(Urban=="Urban")%>% 
  na.omit()




## Appendix Figure A.5 -----------------------------------------

### Rural ------------------------------------
#aes(x = reorder(Type, -share)

plot_a <- ggplot(tza_infras_r, aes(x = incg_5, 
                                   y = number, 
                                   fill = infras_Label)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.5),
           width = 0.4,
           colour="black") + 
  scale_fill_manual(values = c( 
    "grid electricity" =  "#882255",
    "solar electricity"="#F0E442",
    "water"="#0072B2",
    "sanitation"="#D55E00")
  ) +
  theme_minimal()+
  scale_y_continuous(
    expand = c(0, 0),
    breaks = seq(0.0, 1, 0.25),
    labels = scales::percent_format(accuracy = 1)
  ) +
  theme(
    axis.text.y =  element_blank(),
    axis.text.x = element_text(size = 12),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 12, hjust=0.5,face = "bold"),
    legend.position = "bottom",
    strip.text = element_text(size = 12),
    strip.text.y = element_text(angle = 180),
    panel.grid.major = element_line(linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(linewidth = 0.2),
    legend.text = element_text(size = 12),
    legend.title = element_blank(),
    plot.margin = unit(c(0.1, 0.1, 0, 0), "cm")
  )+
  guides(fill = guide_legend(nrow = 1, order = 3)
  )+
  xlab("") +
  ylab("") +
  ggtitle("Rural")


plot_a



### Urban-------------------------------------------------

plot_b <- ggplot(tza_infras_u, aes(x = incg_5, y = number, fill =infras_Label)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.5),
           width = 0.4,
           colour="black") + 
  scale_fill_manual(values = c( 
    "grid electricity" =  "#882255",
    "solar electricity"="#F0E442",
    "water"="#0072B2",
    "sanitation"="#D55E00")
  ) +
  theme_minimal()+
  scale_y_continuous(
    expand = c(0, 0),
    breaks = seq(0.0,1, 0.25),
    labels = scales::percent_format(accuracy = 1)
  ) +
  theme(
    axis.text.y =  element_text(size = 13),
    axis.text.x = element_text(size = 12),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 12, hjust=0.5,face = "bold"),
    legend.position = "bottom",
    strip.text = element_text(size = 12),
    strip.text.y = element_text(angle = 180),
    panel.grid.major = element_line(linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_line(linewidth = 0.2),
    axis.ticks.x = element_line(linewidth = 0.2),
    legend.text = element_text(size = 12),
    legend.title = element_blank(),
    plot.margin = unit(c(0.1, 0.1, 0, 0), "cm")
  )+
  guides(fill = guide_legend(nrow = 1, order = 3)
  )+
  xlab("") +
  ylab("Infrastructure access (% of household)") +
  ggtitle("Urban")

plot_b

# Combine plots

combine <- ggarrange(plot_b,plot_a, legend="bottom",common.legend=T)

combine

# save plot
png(filename= file.path(dir[["figures"]], "Appendix_Figure_A5.png"), 
    width = 15.5, height = 15, unit = "cm", res = 400)
print(combine)
dev.off()




rm(list=ls())

