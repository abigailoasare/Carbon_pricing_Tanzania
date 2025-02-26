# Authors: Abigail O. Asare and Laura Schürer

# Date:26/02/2025

# Sys.getlocale()

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
# Year = 2019

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


## Figure 1 in Paper -----------------------------------------

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
png(filename= file.path(dir[["figures"]], "Figure_1.png"), 
    width = 15.5, height = 15, unit = "cm", res = 400)
print(combine)
dev.off()


# Appendix Figure A.1 (a) Lighting types --------------------------------------------------

tza_lighting <-
  read_csv(file.path(dir[["analysis"]], "hh_lighting_IG5UR_tza.csv")) %>% 
  mutate(lighting_Label=ifelse(lighting_Label=="electricity", 
                               "grid",
                               lighting_Label))


# Reordering the levels of the lighting_Label variable
#so that "others" comes last
tza_lighting$lighting_Label <- factor(tza_lighting$lighting_Label, 
                                      levels = c("grid",
                                                 "solar",
                                                 "lamp oil",
                                                 "others"))

tza_lighting_r <-tza_lighting %>% 
  select(Urban,incg_5,lighting_Label,share) %>% 
  filter(Urban=="Rural") %>% 
  na.omit()

tza_lighting_u <-tza_lighting %>% 
  select(Urban,incg_5,lighting_Label,share) %>% 
  filter(Urban=="Urban")%>% 
  na.omit()


### Rural -----------------------------------------



plot_lg_a <- ggplot(tza_lighting_r, aes(x = incg_5, 
                                        y = share, 
                                        fill = lighting_Label)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.5),
           width =0.4,
           colour="black") + 
  scale_fill_manual(values = c( 
    "grid" =  "#882255",
    "solar"="#F0E442",
    "lamp oil"="#0072B2",
    "others"="#D55E00")
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = seq(0.0, 1, 0.25),
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  theme_minimal()+
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
  guides(fill = guide_legend(nrow = 1, order = 1)
  )+
  xlab("") +
  ylab("") +
  ggtitle("Rural")


plot_lg_a



### Urban ----------------------------------------------

plot_lg_b <- ggplot(tza_lighting_u, aes(x = incg_5, 
                                        y = share, 
                                        fill = lighting_Label)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.5),
           width = 0.4,
           colour="black") + 
  scale_fill_manual(values = c( 
    "grid" =  "#882255",
    "solar"="#F0E442",
    "lamp oil"="#0072B2",
    "others"="#D55E00")
  ) +
  theme_minimal()+
  scale_y_continuous(
    expand = c(0, 0),
    breaks = seq(0.0,1, 0.25),
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  theme(
    axis.text.y =  element_text(size = 12),
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
  guides(fill = guide_legend(nrow = 1, order = 1)
  )+
  xlab("") +
  ylab("Lighting Type (% of household)") +
  ggtitle("Urban")

plot_lg_b



# Combine plots
legend_a <- get_legend(plot_lg_a)

combine <- ggarrange(plot_lg_b,plot_lg_a,
                     widths=c(2, 2),
                     legend="bottom",
                     legend.grob=legend_a)

combine

# save plot
png(filename= file.path(dir[["figures"]], "Appendix_Figure_A1a.png"), 
    width = 15.5, height = 15, unit = "cm", res = 400)
print(combine)
dev.off()



## Appendix Figure A.1 (b) Other Water usage type--------------------------------------------------

tza_owater <-
  read_csv(file.path(dir[["analysis"]], "hh_owater_IG5UR_tza.csv"))

unique(tza_owater$drinking_water_Label)

tza_owater$drinking_water_Label <- factor(tza_owater$drinking_water_Label, 
                                          levels = c("piped water",
                                                     "tubewell/borehole",
                                                     "others"))


tza_owater_r <-tza_owater %>% 
  select(Urban,incg_5,drinking_water_Label,share) %>% 
  filter(Urban=="Rural") %>% 
  na.omit()

tza_owater_u <-tza_owater %>% 
  select(Urban,incg_5,drinking_water_Label,share) %>% 
  filter(Urban=="Urban")%>% 
  na.omit()


## Rural -----------------------------------------


plot_ow_a <- ggplot(tza_owater_r, aes(x = incg_5, 
                                      y = share, 
                                      fill = drinking_water_Label)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.5),
           width = 0.4,
           colour="black") + 
  scale_fill_manual(values = c("piped water" = "#E69F00",
                               "tubewell/borehole"="#AA4499",
                               "others"="#999933"
  )
  ) +
  theme_minimal()+
  scale_y_continuous(
    expand = c(0, 0),
    breaks = seq(0.0, 1, 0.25),
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1)
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
  guides(fill = guide_legend(nrow = 1, order = 1)
  )+
  xlab("") +
  ylab("") +
  ggtitle("Rural")


plot_ow_a



## Urban -----------------------------------------------------

plot_ow_b <- ggplot(tza_owater_u, aes(x = incg_5, 
                                      y = share, 
                                      fill = drinking_water_Label)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.5),
           width = 0.4,
           colour="black") + 
  scale_fill_manual(values = c("piped water" = "#E69F00",
                               "tubewell/borehole"="#AA4499",
                               "others"="#999933"
  )
  ) +
  theme_minimal()+
  scale_y_continuous(
    expand = c(0, 0),
    breaks = seq(0.0,1, 0.25),
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  theme(
    axis.text.y =  element_text(size = 12),
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
  guides(fill = guide_legend(nrow = 1, order = 1)
  )+
  xlab("") +
  ylab("Other Water Type (% of household)") +
  ggtitle("Urban")

plot_ow_b

# Combine plots

combine <- ggarrange(plot_ow_b,plot_ow_a, legend="bottom", common.legend=T)

combine

# save plot
png(filename= file.path(dir[["figures"]], "Appendix_Figure_A1b.png"), 
    width = 15.5, height = 15, unit = "cm", res = 400)
print(combine)
dev.off()



# Appendix Figure A.1 (c) Cooking types  --------------------------------------------------

tza_cook <-
  read_csv(file.path(dir[["analysis"]], "hh_cooking_IG5UR_tza.csv")) 

unique(tza_cook$cooking_Label)

tza_cook_r <-tza_cook %>% 
  select(Urban,incg_5,cooking_Label,share) %>% 
  filter(Urban=="Rural") %>% 
  na.omit()

tza_cook_u <-tza_cook %>% 
  select(Urban,incg_5,cooking_Label,share) %>% 
  filter(Urban=="Urban")%>% 
  na.omit()



### Rural ---------------------------------------------------

plot_ck_a <- ggplot(tza_cook_r, aes(x = incg_5, 
                                    y = share, 
                                    fill = cooking_Label)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.5),
           width = 0.4,
           colour="black") + 
  theme_minimal()+
  scale_fill_manual(values = c("charcoal" = "#E69F00", 
                               "firewood" = "#56B4E9",
                               "gas" = "#009E73",
                               "electricity"="#882255",
                               "others"="#F0E442")
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = seq(0.0, 1, 0.25),
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1)
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


plot_ck_a



### Urban ----------------------------------------------

plot_ck_b <- ggplot(tza_cook_u, aes(x = incg_5, 
                                    y = share, 
                                    fill = cooking_Label)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.5),
           width = 0.4,
           colour="black") +
  scale_fill_manual(values = c("charcoal" = "#E69F00", 
                               "firewood" = "#56B4E9",
                               "gas" = "#009E73",
                               "electricity"="#882255",
                               "others"="#F0E442")
  ) +
  theme_minimal()+
  scale_y_continuous(
    expand = c(0, 0),
    breaks = seq(0.0,1.0, 0.25),
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  theme(
    axis.text.y =  element_text(size = 12),
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
  ylab("Cooking Fuel Type (% of household)") +
  ggtitle("Urban")

plot_ck_b



# Combine plots

# Combine plots
legend_a <- get_legend(plot_ck_b)

combine <- ggarrange(plot_ck_b,plot_ck_a, legend="bottom",legend.grob=legend_a)

combine

# save plot
png(filename= file.path(dir[["figures"]], "Appendix_Figure_A1c.png"), 
    width = 15.5, height = 15, unit = "cm", res = 400)
print(combine)
dev.off()



# Appendix Figure A.1 (d) Toilet type --------------------------------------------------

tza_toilet <-
  read_csv(file.path(dir[["analysis"]], "hh_toilet_IG5UR_tza.csv"))%>% 
  mutate(Label=ifelse(toilet_Label=="NO TOILET","no toilet",NA),
         Label=ifelse(toilet_Label=="FLUSH TOILET","flush toilet",Label),
         Label=ifelse(toilet_Label=="PIT LATRINE","pit latrine",Label),
         Label=ifelse(toilet_Label=="OTHERS","others",Label)
  )


unique(tza_toilet$toilet_Label)

tza_toilet$Label <- factor(
  tza_toilet$Label,
  levels = c("no toilet",
             "flush toilet",
             "pit latrine",
             "others")
)


tza_toilet_r <-tza_toilet %>% 
  select(Urban,incg_5,Label,share) %>% 
  filter(Urban=="Rural") %>% 
  na.omit()

tza_toilet_u <-tza_toilet %>% 
  select(Urban,incg_5,Label,share) %>% 
  filter(Urban=="Urban")%>% 
  na.omit()


## Rural -----------------------------------------


plot_tt_a <- ggplot(tza_toilet_r, aes(x = incg_5, 
                                      y = share, 
                                      fill = Label)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.5),
           width = 0.4,
           colour="black") + 
  scale_fill_manual(values = c(
    "no toilet"  = "#882255",
    "flush toilet" = "#E69F00",
    "pit latrine"="#999933",
    "others" = "#009E73"
  )
  )+
  theme_minimal()+
  scale_y_continuous(
    expand = c(0, 0),
    breaks = seq(0.0, 1, 0.25),
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1)
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
  guides(fill = guide_legend(nrow = 1, order = 1)
  )+
  xlab("") +
  ylab("") +
  ggtitle("Rural")


plot_tt_a


##  Urban -----------------------------------------------

plot_tt_b <- ggplot(tza_toilet_u, aes(x = incg_5, 
                                      y = share, 
                                      fill = Label)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.5),
           width = 0.4,
           colour="black") + 
  scale_fill_manual(values = c(
    "no toilet"  = "#882255",
    "flush toilet" = "#E69F00",
    "pit latrine"="#999933",
    "others" = "#009E73"
  )
  )+
  theme_minimal()+
  scale_y_continuous(
    expand = c(0, 0),
    breaks = seq(0.0,1, 0.25),
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  theme(
    axis.text.y =  element_text(size = 12),
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
  guides(fill = guide_legend(nrow = 1, order = 1)
  )+
  xlab("") +
  ylab("Sanitation Type (% of household)") +
  ggtitle("Urban")

plot_tt_b



# Combine plots

combine <- ggarrange(plot_tt_b,plot_tt_a, legend="bottom", common.legend = T)

combine

# save plot
png(filename= file.path(dir[["figures"]], "Appendix_Figure_A1d.png"), 
    width = 15.5, height = 15, unit = "cm", res = 400)
print(combine)
dev.off()



rm(list=ls())

