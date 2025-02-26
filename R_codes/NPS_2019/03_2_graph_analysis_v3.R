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


## Generate functions -----------------------------------------------------------------


fun_2 <- function(File) {
  File <- File |>
    select(
      hh_id,
      hh_size,
      hh_weights,
      Urban,
      incg_5,
      burden_CO2_pc,
      burden_CO2_within_pc,
      burden_CO2_ely_pc,
      burden_CO2_trs_pc
    )
  
  File_1 <- File |>
    dplyr::group_by(incg_5) |>
    dplyr::summarise(
      wtd.5_CO2 = wtd.quantile(burden_CO2_pc,
                               weight = hh_weights,
                               probs = 0.05),
      wtd.5_CO2_within = wtd.quantile(burden_CO2_within_pc,
                                      weight = hh_weights,
                                      probs = 0.05),
      wtd.5_CO2_ely = wtd.quantile(burden_CO2_ely_pc,
                                   weight = hh_weights,
                                   probs = 0.05),
      wtd.5_CO2_trs = wtd.quantile(burden_CO2_trs_pc,
                                   weight = hh_weights,
                                   probs = 0.05),
      wtd.25_CO2 = wtd.quantile(burden_CO2_pc,
                                weight = hh_weights,
                                probs = 0.25),
      wtd.25_CO2_within = wtd.quantile(burden_CO2_within_pc,
                                       weight = hh_weights,
                                       probs = 0.25),
      wtd.25_CO2_ely = wtd.quantile(burden_CO2_ely_pc,
                                    weight = hh_weights,
                                    probs = 0.25),
      wtd.25_CO2_trs = wtd.quantile(burden_CO2_trs_pc,
                                    weight = hh_weights,
                                    probs = 0.25),
      wtd.50_CO2 = wtd.quantile(burden_CO2_pc,
                                weight = hh_weights,
                                probs = 0.5),
      wtd.50_CO2_within = wtd.quantile(burden_CO2_within_pc,
                                       weight = hh_weights,
                                       probs = 0.5),
      wtd.50_CO2_ely = wtd.quantile(burden_CO2_ely_pc,
                                    weight = hh_weights,
                                    probs = 0.5),
      wtd.50_CO2_trs = wtd.quantile(burden_CO2_trs_pc,
                                    weight = hh_weights,
                                    probs = 0.5),
      wtd.75_CO2 = wtd.quantile(burden_CO2_pc,
                                weight = hh_weights,
                                probs = 0.75),
      wtd.75_CO2_within = wtd.quantile(burden_CO2_within_pc,
                                       weight = hh_weights,
                                       probs = 0.75),
      wtd.75_CO2_ely = wtd.quantile(burden_CO2_ely_pc,
                                    weight = hh_weights,
                                    probs = 0.75),
      wtd.75_CO2_trs = wtd.quantile(burden_CO2_trs_pc,
                                    weight = hh_weights,
                                    probs = 0.75),
      wtd.95_CO2 = wtd.quantile(burden_CO2_pc,
                                weight = hh_weights,
                                probs = 0.95),
      wtd.95_CO2_within = wtd.quantile(burden_CO2_within_pc,
                                       weight = hh_weights,
                                       probs = 0.95),
      wtd.95_CO2_ely = wtd.quantile(burden_CO2_ely_pc,
                                    weight = hh_weights,
                                    probs = 0.95),
      wtd.95_CO2_trs = wtd.quantile(burden_CO2_trs_pc,
                                    weight = hh_weights,
                                    probs = 0.95),
      mean_CO2 = wtd.mean(burden_CO2_pc, weights = hh_weights, na.rm = TRUE),
      mean_CO2_within = wtd.mean(burden_CO2_within_pc, weights = hh_weights, na.rm = TRUE),
      mean_CO2_ely = wtd.mean(burden_CO2_ely_pc, weights = hh_weights, na.rm = TRUE),
      mean_CO2_trs = wtd.mean(burden_CO2_trs_pc, weights = hh_weights, na.rm = TRUE)
    ) |>
    ungroup()
  
  
  
  File_1a <- File_1 |>
    select(incg_5,
           wtd.5_CO2,
           wtd.25_CO2,
           wtd.50_CO2,
           wtd.75_CO2,
           wtd.95_CO2,
           mean_CO2
    ) |>
    mutate(Type_0 = "CO2") |>
    rename(
      y5 = wtd.5_CO2,
      y25 = wtd.25_CO2,
      y50 = wtd.50_CO2,
      y75 = wtd.75_CO2,
      y95 = wtd.95_CO2,
      mean = mean_CO2
    )
  
  File_1b <- File_1 |>
    select(
      incg_5,
      wtd.5_CO2_within,
      wtd.25_CO2_within,
      wtd.50_CO2_within,
      wtd.75_CO2_within,
      wtd.95_CO2_within,
      mean_CO2_within
    ) |>
    mutate(Type_0 = "CO2_within") |>
    rename(
      y5 = wtd.5_CO2_within,
      y25 = wtd.25_CO2_within,
      y50 = wtd.50_CO2_within,
      y75 = wtd.75_CO2_within,
      y95 = wtd.95_CO2_within,
      mean = mean_CO2_within
    )
  
  File_1c <- File_1 |>
    select(incg_5,
           wtd.5_CO2_ely,
           wtd.25_CO2_ely,
           wtd.50_CO2_ely,
           wtd.75_CO2_ely,
           wtd.95_CO2_ely,
           mean_CO2_ely
    ) |>
    mutate(Type_0 = "ELY") |>
    rename(
      y5  =wtd.5_CO2_ely,
      y25 =wtd.25_CO2_ely, 
      y50 =wtd.50_CO2_ely, 
      y75 =wtd.75_CO2_ely,
      y95 =wtd.95_CO2_ely, 
      mean = mean_CO2_ely
    )
  
  File_1d <- File_1 |>
    select(incg_5,
           wtd.5_CO2_trs,
           wtd.25_CO2_trs, 
           wtd.50_CO2_trs, 
           wtd.75_CO2_trs,
           wtd.95_CO2_trs, 
           mean_CO2_trs
    ) |>
    mutate(Type_0 = "TRSP") |>
    rename(
      y5  =wtd.5_CO2_trs,
      y25 =wtd.25_CO2_trs, 
      y50 =wtd.50_CO2_trs, 
      y75 =wtd.75_CO2_trs,
      y95 =wtd.95_CO2_trs, 
      mean = mean_CO2_trs
    )
  
  File_2 <- bind_rows(File_1a, File_1b) |>
    bind_rows(File_1c) |>
    bind_rows(File_1d)
  
  File_final <- File_2
  return(File_final)
}


tza_2 <- fun_2(tza_final)
# save as excel

write.xlsx(tza_2, 
           file.path(dir[["analysis"]], "figure_3a_excel.xlsx")
)


fun_2.2 <- function(inci_x) {
  inci_y <- inci_x %>%
    select(
      hh_id,
      hh_size,
      hh_weights,
      incg_5,
      hh_exp_USD_pc,
      starts_with("burden"),
      starts_with("exp_")
    ) %>%
    mutate(
      tt_exp_CO2 = exp_pc_CO2 * hh_size * hh_weights,
      tt_exp_CO2_within = exp_pc_CO2_within*hh_size*hh_weights,
      population = hh_size * hh_weights
    )
  
  no_households<- sum(inci_y$hh_weights)
  no_population<- sum(inci_y$population)
  tt_exp_CO2_all<-sum(inci_y$tt_exp_CO2)
  tt_exp_CO2_within_all <- sum(inci_y$tt_exp_CO2_within)
  
  LST_CO2_per_hh<-tt_exp_CO2_all/no_households
  LST_CO2_pc <- tt_exp_CO2_all/no_population
  LST_CO2_within_per_hh<- tt_exp_CO2_within_all/no_households
  LST_CO2_within_pc <- tt_exp_CO2_within_all/no_population  
  
  inci_y.1 <- inci_y %>%
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
    ) %>%
    select(hh_id,
           hh_weights,
           incg_5,
           starts_with("burden_"),
           starts_with("exp"))
  
  inci_y.2 <- inci_y.1 %>%
    dplyr::group_by(incg_5) %>%
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
    ) %>%
    ungroup() %>%
    select(everything())
  
  list_0 <-
    list("Full_df" = inci_y.1, "Summarised_df" = inci_y.2)
  
  return(list_0)
  
}

tza_2.2.1 <- fun_2.2(tza_final)$Full_df




tza_2_box_int <- tza_2 |>
  dplyr::filter(Type_0=="CO2") |> 
  dplyr::mutate(
    incg_5 = factor(incg_5, levels = c(1, 2, 3, 4, 5)), 
    Policy = factor(Type_0, labels = c(
      "International Carbon Price"
    )) 
  )

tza_2_box_nat <- tza_2 |>
  dplyr::filter(Type_0=="CO2_within") |> 
  dplyr::mutate(
    incg_5 = factor(incg_5, levels = c(1, 2, 3, 4, 5)), 
    Policy = factor(Type_0, labels = c(
      "National Carbon Price"
    )) 
  )



tza_2_box_ely <- tza_2 |>
  dplyr::filter(Type_0=="ELY") |> 
  dplyr::mutate(
    incg_5 = factor(incg_5, levels = c(1, 2, 3, 4, 5)), 
    Policy = factor(Type_0, labels = c(
      "Electricity Carbon Price"
    )) 
  )

tza_2_box_tsp <- tza_2 |>
  dplyr::filter(Type_0=="TRSP") |> 
  dplyr::mutate(
    incg_5 = factor(incg_5, levels = c(1, 2, 3, 4, 5)), 
    Policy = factor(Type_0, labels = c(
      "Fuel Carbon Price"
    )) 
  )


## Figure 3a in paper -----------------------------------------

# Create boxplot1
plot_3a1 <- ggplot(tza_2_box_int) +
  geom_boxplot(
    aes(
      ymin = y5,
      lower = y25,
      middle = y50,
      upper = y75,
      ymax = y95,
      x = factor(incg_5),
      fill = Policy
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
      group = interaction(incg_5,Policy),
      x = factor(incg_5)
    ),
    fun = "mean",
    geom = "point",
    na.rm = T,
    position =  position_dodge(0.7),
    shape = 23,
    size = 3,
    fill = "white",
    stroke = 0.2
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 100), limits = c(0, 0.1)) +
  xlab("Expenditure Quintile") +
  ylab("Relative incidence (%)") +
  scale_fill_manual(values = c("#ED90A4")) +
  scale_color_manual(values = c("#ED90A4")) +
  theme_bw() +
  theme(
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    strip.text = element_text(size = 14, face = "bold"),
    panel.grid.major = element_line(linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(linewidth = 0.3)
  ) 

print(plot_3a1)



## Figure 3b in paper -----------------------------------------
plot_3a2 <- ggplot(tza_2_box_nat) +
  
  geom_boxplot(
    aes(
      ymin = y5,
      lower = y25,
      middle = y50,
      upper = y75,
      ymax = y95,
      x = factor(incg_5),
      fill = Policy
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
      group = interaction(incg_5,Policy),
      x = factor(incg_5)
    ),
    fun = "mean",
    geom = "point",
    na.rm = T,
    position =  position_dodge(0.7),
    shape = 23,
    size = 3,
    fill = "white",
    stroke = 0.2
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 100), limits = c(0, 0.1)) +
  xlab("Expenditure Quintile") +
  ylab("Relative incidence (%)") +
  scale_fill_manual(values = c( "#ABB150")) +
  scale_color_manual(values = c( "#ABB150")) +
  theme_bw() +
  theme(
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    strip.text = element_text(size = 14, face = "bold"),
    panel.grid.major = element_line(linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(linewidth = 0.3)
  ) 

print(plot_3a2)


## Figure 3c in paper -----------------------------------------
plot_3a3 <- ggplot(tza_2_box_ely) +
  geom_boxplot(
    aes(
      ymin = y5,
      lower = y25,
      middle = y50,
      upper = y75,
      ymax = y95,
      x = factor(incg_5),
      fill = Policy
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
      group = interaction(incg_5,Policy),
      x = factor(incg_5)
    ),
    fun = "mean",
    geom = "point",
    na.rm = T,
    position =  position_dodge(0.7),
    shape = 23,
    size = 3,
    fill = "white",
    stroke = 0.2
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 100), limits = c(0, 0.05)) +
  xlab("Expenditure Quintile") +
  ylab("Relative incidence (%)") +
  scale_fill_manual(values = c("#ACA2EC")) +
  scale_color_manual(values = c("#ACA2EC")) +
  theme_bw() +
  theme(
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    strip.text = element_text(size = 14, face = "bold"),
    panel.grid.major = element_line(linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(linewidth = 0.3)
  ) 

print(plot_3a3)



## Figure 3d in paper -----------------------------------------
plot_3a4 <- ggplot(tza_2_box_tsp) +
  
  geom_boxplot(
    aes(
      ymin = y5,
      lower = y25,
      middle = y50,
      upper = y75,
      ymax = y95,
      x = factor(incg_5),
      fill = Policy
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
      group = interaction(incg_5,Policy),
      x = factor(incg_5)
    ),
    fun = "mean",
    geom = "point",
    na.rm = T,
    position =  position_dodge(0.7),
    shape = 23,
    size = 3,
    fill = "white",
    stroke = 0.2
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 100), limits = c(0, 0.05)) +
  xlab("Expenditure Quintile") +
  ylab("Relative incidence (%)") +
  scale_fill_manual(values = c( "#00C1B2")) +
  scale_color_manual(values = c( "#00C1B2")) +
  theme_bw() +
  theme(
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    strip.text = element_text(size = 14, face = "bold"),
    panel.grid.major = element_line(linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(linewidth = 0.3)
  ) 

print(plot_3a4)




# save plot
png(filename= file.path(dir[["figures"]], "Figure_3a.png"), 
    width = 15.5, height = 15, unit = "cm", res = 400)
print(plot_3a1)
dev.off()

# save plot
png(filename= file.path(dir[["figures"]], "Figure_3b.png"), 
    width = 15.5, height = 15, unit = "cm", res = 400)
print(plot_3a2)
dev.off()

# save plot
png(filename= file.path(dir[["figures"]], "Figure_3c.png"), 
    width = 15.5, height = 15, unit = "cm", res = 400)
print(plot_3a3)
dev.off()

# save plot
png(filename= file.path(dir[["figures"]], "Figure_3d.png"), 
    width = 15.5, height = 15, unit = "cm", res = 400)
print(plot_3a4)
dev.off()



## Figure 4 in paper-Included Emissions -----------------------------------

emis_mt <-
  read_csv(file.path(dir[["analysis"]], "carbon_emissions_tza.csv")) 

agg_emi <- emis_mt %>%
  dplyr::summarise(CO2_t=sum(CO2_emi_t, na.rm=T),
                   CO2_t_within=sum(CO2_emi_t_within, na.rm=T),
                   CO2_t_ely=sum(CO2_emi_t_ely, na.rm=T),
                   CO2_t_trs=sum(CO2_emi_t_trs, na.rm=T)
  ) %>% 
  pivot_longer(
    cols=c("CO2_t", "CO2_t_within", "CO2_t_ely",
           "CO2_t_trs"),
    names_to = "Type",
    values_to = "total") %>%  
  mutate(share=total/total[1],
         Label=round(total,1),
         Label=ifelse(Type!="CO2_t",NA,Label),
         Label=ifelse(Type=="CO2_t", 
                      paste(Label,"Mt" , sep = " "),Label)
  ) %>% 
  dplyr::mutate(Type_name=c("International Carbon Price",
                            "National Carbon Price",
                            "Electricity Sector Carbon Price",
                            "Liquid Carbon Price"))


plot_3b <- ggplot(agg_emi, aes(x = reorder(Type, -share), y = share)) +
  geom_col(
    aes(fill = Type_name), colour = "black", width = 0.75, size = 0.2
  ) +
  geom_label_repel(
    aes(y = 0.95, group = Type_name, label = Label),
    size = 8,
    segment.linetype = 1,
    direction = "x",
    min.segment.length = 0,
    nudge_x = 1,
    label.size = 0.1,
    label.padding = 0.05,
    box.padding = 0,
    label.r = 0.01
  ) +
  theme_bw() +
  coord_cartesian(ylim = c(0, 1.02)) +
  scale_fill_manual(
    labels = c(
      "International Carbon Price",
      "National Carbon Price",
      "Fuel Carbon Price",
      "Electricity Sector Carbon Price"
    ),
    values = c("International Carbon Price" = "#ED90A4",
               "National Carbon Price" = "#ABB150",
               "Liquid Carbon Price" = "#ACA2EC",
               "Electricity Sector Carbon Price" = "#00C1B2"
    )
  )+ 
  ylab(bquote('Share of covered ' ~ CO[2] ~ 'Emissions')) + xlab("") +
  scale_y_continuous(labels = percent_format(), expand = c(0, 0)) +
  scale_x_discrete(labels = c("International",
                              "National",
                              "Fuel",
                              "Electricity"
  ))+
  theme(
    axis.text.y =  element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.title = element_blank(),
    plot.title = element_text(size = 7, hjust=0.5,face = "bold"),
    legend.position = "none",
    strip.text = element_text(size = 12),
    strip.text.y = element_text(angle = 180),
    panel.grid.major = element_line(linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_line(linewidth = 0.2),
    axis.ticks.x = element_blank(),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    plot.margin = unit(c(0.1, 0.1, 0, 0), "cm"),
    panel.border = element_rect(linewidth = 0.3),
    legend.background = element_rect(fill = "white", colour = "black")
  )

plot_3b



png(filename= file.path(dir[["figures"]], "Figure_4.png"), 
    width = 15.5, height = 15, unit = "cm", res = 400)
print(plot_3b)
dev.off()





#  Figure 5 in Paper ---------------------------------------------------

fun_1 <- function(File) {
  File <- File %>%
    select(hh_id,
           hh_weights,
           hh_size,
           Urban,
           incg_5,
           burden_CO2_pc,
           burden_CO2_within_pc,
           burden_CO2_ely_pc,
           burden_CO2_trs_pc)
  
  File_0 <- File %>%
    mutate(population = hh_weights * hh_size)
  
  population <- sum(File_0$population)
  
  File_1 <- File %>%
    select(-incg_5) %>%
    mutate(incg_5 = 0)
  
  File_2 <- File %>%
    select(-Urban) %>%
    mutate(Urban = "Country")
  
  File_3 <- File %>%
    select(-Urban,-incg_5) %>%
    mutate(incg_5 = 0) %>%
    mutate(Urban = "Country")
  
  File_4 <- File %>%
    rbind(File_1) %>%
    rbind(File_2) %>%
    rbind(File_3) %>%
    mutate(Urban = fct_reorder2(Urban, Urban, Urban)) %>%
    arrange(hh_id, incg_5)
  
  File_0 <- File_4 %>%
    mutate(population = (hh_size * hh_weights) / population) %>%
    dplyr::group_by(incg_5, Urban) %>%
    dplyr::summarise(population = sum(population) / 2) %>%
    ungroup() %>%
    select(population)
  
  Files <- list("File_4" = File_4, "File_0" = File_0)
  
  return(Files)
  
}


tza_1 <- (fun_1(tza_final))$File_4


cal_md <- function(x) {
  x <- x %>%
    group_by(incg_5) %>%
    mutate(cumsum_shares = cumsum(share)) %>%
    filter(cumsum_shares >= 0.5) %>%
    slice(which.min(cumsum_shares)) %>%
    ungroup() %>%
    rename(median = burden_CO2_within_pc) %>%
    select(incg_5, median)
}
cal_md_y <- function(x0, xmedian, adjust_0) {
  ggplot_build(ggplot(x0, aes(
    y = share,
    x = burden_CO2_within_pc,
    group = factor(incg_5)
  )) +
    geom_smooth(
      method = "loess",
      span = adjust_0,
      se = FALSE
    ))$data[[1]] %>%
    select(x, y, group) %>%
    left_join(xmedian, by = c("group" = "incg_5")) %>%
    mutate(help = median - x) %>%
    mutate(help_0 = ifelse(help < 0, help * -1, help)) %>%
    group_by(group) %>%
    filter(help_0 == min(help_0)) %>%
    ungroup() %>%
    rename(incg_5 = group,
           md_x = x,
           md_y = y) %>%
    select(-median,-help,-help_0) %>%
    select(md_x, md_y, incg_5)
}


adjust_0 <- 0.2

add_on <-
  expand.grid(
    incg_5 = c(1, 2, 3, 4, 5),
    Urban = c("Rural", "Urban", "Country"),
    burden_CO2_within_pc = c(seq(0, 0.1, 0.001))
  ) %>%
  mutate(hh_weights = 0)

# Round Values up, calculate households per bins
tza_10 <- tza_1 %>%
  filter(incg_5 != 0) %>%
  mutate(burden_CO2_within_pc = round(burden_CO2_within_pc, 3)) %>%
  filter(!is.na(burden_CO2_within_pc)) %>%
  bind_rows(add_on) %>%
  dplyr::group_by(incg_5, Urban, burden_CO2_within_pc) %>%
  dplyr::summarise(weights = sum(hh_weights)) %>%
  ungroup()
# Calculate total households
IG_weights <- tza_1 %>%
  filter(incg_5 != 0) %>%
  dplyr::group_by(incg_5, Urban) %>%
  dplyr::summarise(IG_weights = sum(hh_weights)) %>%
  ungroup()
# Calculate Shares
tza_10 <- left_join(tza_10, IG_weights) %>%
  mutate(share = weights / IG_weights)

# Select Subsamples
tza_11 <- tza_10 %>%
  filter(Urban == "Urban")
tza_12 <- tza_10 %>%
  filter(Urban == "Rural")
tza_13 <- tza_10 %>%
  filter(Urban == "Country")

#Calculate Median
md_X1 <- cal_md(tza_11)
md_X2 <- cal_md(tza_12)
md_X3 <- cal_md(tza_13)

# Calculate Median Y
tx1 <- cal_md_y(tza_11, md_X1, adjust_0)
tx2 <- cal_md_y(tza_12, md_X2, adjust_0)
tx3 <- cal_md_y(tza_13, md_X3, adjust_0)

tza_11.1 <- tza_11 %>%
  left_join(tx1, by = c("incg_5"))
tza_12.1 <- tza_12 %>%
  left_join(tx2, by = c("incg_5"))
tza_13.1 <- tza_13 %>%
  left_join(tx3, by = c("incg_5"))

max_median <- max(tza_13.1$md_x)
min_median <- min(tza_13.1$md_x)

## plot ------------------------------------------------------

plot_4 <- ggplot(tza_13.1,
                 aes(
                   group = factor(incg_5),
                   colour = factor(incg_5),
                   linetype = factor(incg_5)
                 )) +
  theme_bw() +
  theme(
    axis.text.y = element_text(size = 15, vjust = 0.1),
    axis.text.x = element_text(size = 15),
    axis.title = element_text(size = 12),
    plot.title = element_text(size=7,hjust = 0.5, face = "bold"),
    legend.position = "bottom",
    strip.text = element_text(size = 12),
    strip.text.y = element_text(angle = 180),
    panel.grid.major = element_line(linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(linewidth = 0.2),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    plot.margin = unit(c(0.1, 0.1, 0, 0), "cm"),
    panel.border = element_rect(linewidth = 0.3)
  ) +
  annotate(
    "rect",
    xmin = min_median,
    xmax = max_median,
    ymin = 0,
    ymax = 0.25,
    alpha = 0.5,
    fill = "grey"
  ) +
  annotate(
    "segment",
    x = min_median,
    xend = max_median,
    y = 0.24,
    yend = 0.24,
    arrow = arrow(
      ends = "both",
      angle = 90,
      length = unit (.05, "cm")
    ),
    size = 0.2
  ) +
  annotate(
    "text",
    x = (min_median + max_median)/ 2,
    y = 0.23,
    label = "paste(Delta, V)",
    parse = TRUE,
    size = 2
  ) +
  geom_smooth(
    aes(x = burden_CO2_within_pc, y = share),
    size = 0.5,
    method = "loess",
    n = 160,
    span = adjust_0,
    se = FALSE,
    fullrange = TRUE
  ) +
  geom_point(
    aes(
      x = md_x,
      y = md_y,
      group = factor(incg_5),
      fill = factor(incg_5)
    ),
    shape = 21,
    size = 1.3,
    stroke = 0.2,
    colour = "black"
  ) +
  xlab("Carbon Price Incidence") +
  ylab("Share of Households per Quintile") +
  labs(colour = "",
       linetype = "",
       fill = "") +
  scale_y_continuous(
    breaks = seq(0,0.25, 0.05),
    expand = c(0, 0),
    labels = scales::percent_format(accuracy = 0.1)
  ) +
  scale_x_continuous(
    expand = c(0, 0),
    breaks = seq(0, 0.08, 0.02),
    labels = scales::percent_format(accuracy = 1)
  )+
  coord_cartesian(xlim = c(0,0.085), ylim = c(0,0.25))+
  scale_colour_manual(values = qualitative_hcl(5, "Dark 3")) +
  scale_fill_manual(values = qualitative_hcl(5, "Dark 3")) +
  scale_linetype_manual(values = c("solid", "longdash", "dotdash", "solid",
                                   "solid")) +
  guides(
    fill = guide_legend("Expenditure Quintile"),
    colour = guide_legend("Expenditure Quintile"),
    linetype = guide_legend("Expenditure Quintile")
  )

plot_4

write.xlsx(tza_13.1, 
           file.path(dir[["analysis"]], "national_incidence_share.xlsx")
)




png(filename= file.path(dir[["figures"]], "Figure_5.png"), 
    width = 15.5, height = 15, unit = "cm", res = 400)
print(plot_4)
dev.off()



# Appendix Figure A.2a -------------------------------------------------------
#National Carbon Price Rural and Urban Areas
tza_1.4 <- rbind(tza_11.1, tza_12.1) %>%
  filter(incg_5 == 1 | incg_5 == 5)

write.xlsx(tza_1.4,file.path(dir[["analysis"]], "incidence_share_UR.xlsx"))

## plot ---------------------------------------------------------------
plot_5a <- ggplot(tza_1.4, aes(
  colour = factor(incg_5),
  linetype = factor(Urban))
) +
  theme_bw() +
  theme(
    axis.text.y = element_text(size = 12, vjust = 0.1),
    axis.text.x = element_text(size = 12),
    axis.title = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, size=6,face="bold"),
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
  geom_smooth(
    aes(x = burden_CO2_within_pc, y = share),
    size = 0.5,
    method = "loess",
    span = adjust_0,
    se = FALSE,
    n = 160,
    fullrange = TRUE
  ) +
  geom_point(
    aes(
      x = md_x,
      y = md_y,
      fill = factor(incg_5),
      alpha = factor(Urban)
    ),
    shape = 22,
    size = 1.3,
    stroke = 0.2,
    colour = "black"
  ) +
  xlab("Carbon Price Incidence") +
  ylab("Share of Households per Quintile") +
  labs(colour = "",
       linetype = "",
       fill = "") +
  scale_y_continuous(
    breaks = seq(0, 0.25, 0.05),
    expand = c(0, 0),
    labels = scales::percent_format(accuracy = 0.1)
  ) +
  scale_x_continuous(
    expand = c(0, 0),
    breaks = seq(0, 0.08, 0.02),
    labels = scales::percent_format(accuracy = 1)
  ) +
  coord_cartesian(xlim = c(0,0.085), ylim = c(0,0.25))+
  scale_colour_manual(values = qualitative_hcl(2, "Dark 3")) +
  scale_fill_manual(values = qualitative_hcl(2, "Dark 3")) +
  scale_linetype_manual(values = c("solid", "dotdash")) +
  scale_alpha_manual(values = c(1, 0.5)) +
  guides(
    fill = "none",
    colour = guide_legend("Expenditure Quintile:"),
    linetype = guide_legend("Location:"),
    alpha = "none"
  )

plot_5a

png(filename= file.path(dir[["figures"]], "Appendix_Figure_A2a.png"), 
    width = 15.5, height = 15, unit = "cm", res = 400)
print(plot_5a)
dev.off()


# Figure 2b in paper  -----------------------------------------------------
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


## plot -----------------------------------------------------------------------

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
                     breaks = seq(0.0, 0.04, 0.02)
  ) +
  scale_x_continuous(expand = c(0, 0), 
                     breaks = seq(0, 100, 25)
  ) +
  coord_cartesian(ylim = c(0, 0.045), xlim = c(0, 102)) +
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
  )+ 
  guides(colour = guide_legend(nrow = 1), fill = guide_legend(nrow = 1))

plot_2b

png(filename= file.path(dir[["figures"]], "Figure_2b.png"), 
    width = 15.5, height = 15, unit = "cm", res = 400)
print(plot_2b)
dev.off()



# Distribution Including Lump Sum Transfers Global --------------------------

cal_md <- function(x) {
  x <- x %>%
    group_by(incg_5, Type) %>%
    arrange(value) %>%
    mutate(cumsum_shares = cumsum(share)) %>%
    filter(cumsum_shares >= 0.5) %>%
    slice(which.min(cumsum_shares)) %>%
    ungroup() %>%
    rename(median = value) %>%
    select(incg_5, Type, median)
}


cal_md_y <- function(x0, xmedian, adjust_0) {
  ggplot_build(ggplot(x0, aes(
    y = share,
    x = value,
    group = interaction(factor(incg_5), Type)
  )) +
    geom_smooth(
      method = "loess",
      span = adjust_0,
      se = FALSE,
      n = 700
    ))$data[[1]] %>%
    select(x, y, group) %>%
    mutate(group = ifelse(group == 1, 1, 5)) %>%
    left_join(xmedian, by = c("group" = "incg_5")) %>%
    mutate(help = median - x) %>%
    mutate(help_0 = ifelse(help < 0, (help * -1), help)) %>%
    group_by(group) %>%
    filter(help_0 == min(help_0)) %>%
    ungroup() %>%
    rename(incg_5 = group,
           md_x = x,
           md_y = y) %>%
    select(-median,-help,-help_0) %>%
    select(md_x, md_y, incg_5)
}



add_on <-
  expand.grid(
    incg_5 = c(1, 2, 3, 4, 5),
    Urban = c("Urban", "Rural", "Country"),
    burden_CO2_pc = c(seq(-0.1, 0.1, 0.001)),
    burden_CO2_pc_LST_pc = c(seq(-0.1, 0.1, 0.001))
  ) %>%
  mutate(hh_weights = 0)

# Round Values up, calculate households per bins
inci_x0 <- tza_2.2.1 %>%
  select(hh_id,
         hh_weights,
         incg_5,
         burden_CO2_pc,
         burden_CO2_pc_LST_pc) %>%
  bind_rows(add_on) %>%
  pivot_longer(starts_with("burden"),
               names_to = "type",
               values_to = "value") %>%
  filter(incg_5 == 1 | incg_5 == 5) %>%
  filter(!is.na(value) &
           value != "-Inf" & value != "Inf" & value < 0.5 & value > -0.5) %>%
  mutate(
    Type = ifelse(
      type == "burden_CO2_pc",
      "Global Carbon Price",
      "Global Carbon Price and equal per capita transfer"
    ),
    value = round(value, 3)
  ) %>%
  dplyr::group_by(incg_5, Type, value) %>%
  dplyr::summarise(weights = sum(hh_weights)) %>%
  dplyr::ungroup()

# Calculate total households
IG_weights <- tza_2.2.1 %>%
  select(hh_id, incg_5, hh_weights) %>%
  dplyr::group_by(incg_5) %>%
  dplyr::summarise(IG_weights = sum(hh_weights)) %>%
  dplyr::ungroup()
# Calculate Shares
inci_x1 <- left_join(inci_x0, IG_weights) %>%
  mutate(share = weights / IG_weights) %>%
  filter(Type == "Global Carbon Price")
inci_x2 <- left_join(inci_x0, IG_weights) %>%
  mutate(share = weights / IG_weights) %>%
  filter(Type != "Global Carbon Price")

#Calculate Median
md_X1 <- cal_md(inci_x1) %>%
  dplyr::filter(Type == "Global Carbon Price")
md_X2 <- cal_md(inci_x2) %>%
  dplyr::filter(Type != "Global Carbon Price")

# Calculate Median Y
tx1 <- cal_md_y(inci_x1, md_X1, adjust_0)
tx2 <- cal_md_y(inci_x2, md_X2, adjust_0)

inci_x1.1 <- inci_x1 %>%
  left_join(tx1, by = c("incg_5"))
inci_x2.1 <- inci_x2 %>%
  left_join(tx2, by = "incg_5")
inci_x3.1 <- bind_rows(inci_x1.1, inci_x2.1) %>%
  mutate(
    Label = ifelse(
      incg_5 == 1 &
        Type == "Global Carbon Price",
      "Global Carbon Price (Expenditure Quintile 1)",
      ifelse(
        incg_5 == "5" &
          Type == "Global Carbon Price",
        "Global Carbon Price (Expenditure Quintile 5)",
        ifelse(
          incg_5 == "1" &
            Type == "Global Carbon Price and equal per capita transfer",
          "Global Carbon Price and equal per capita Transfer 
          (Expenditure Quintile 1)",
          "Global Carbon Price and equal per capita Transfer 
          (Expenditure Quintile 5)"
        )
      )
    )
  )


write.xlsx(inci_x3.1, 
           file.path(dir[["analysis"]], "LST_Global.xlsx")
)



# Distribution Including Lump Sum Transfers National--------------------------

cal_md <- function(x) {
  x <- x %>%
    group_by(incg_5, Type) %>%
    arrange(value) %>%
    mutate(cumsum_shares = cumsum(share)) %>%
    filter(cumsum_shares >= 0.5) %>%
    slice(which.min(cumsum_shares)) %>%
    ungroup() %>%
    rename(median = value) %>%
    select(incg_5, Type, median)
}


cal_md_y <- function(x0, xmedian, adjust_0) {
  ggplot_build(ggplot(x0, aes(
    y = share,
    x = value,
    group = interaction(factor(incg_5), Type)
  )) +
    geom_smooth(
      method = "loess",
      span = adjust_0,
      se = FALSE,
      n = 700
    ))$data[[1]] %>%
    select(x, y, group) %>%
    mutate(group = ifelse(group == 1, 1, 5)) %>%
    left_join(xmedian, by = c("group" = "incg_5")) %>%
    mutate(help = median - x) %>%
    mutate(help_0 = ifelse(help < 0, (help * -1), help)) %>%
    group_by(group) %>%
    filter(help_0 == min(help_0)) %>%
    ungroup() %>%
    rename(incg_5 = group,
           md_x = x,
           md_y = y) %>%
    select(-median,-help,-help_0) %>%
    select(md_x, md_y, incg_5)
}



add_on <-
  expand.grid(
    incg_5 = c(1, 2, 3, 4, 5),
    Urban = c("Urban", "Rural", "Country"),
    burden_CO2_within_pc = c(seq(-0.1, 0.1, 0.001)),
    burden_CO2_within_pc_LST_pc = c(seq(-0.1, 0.1, 0.001))
  ) %>%
  mutate(hh_weights = 0)

# Round Values up, calculate households per bins
inci_x0 <- tza_2.2.1 %>%
  select(hh_id,
         hh_weights,
         incg_5,
         burden_CO2_within_pc,
         burden_CO2_within_pc_LST_pc) %>%
  bind_rows(add_on) %>%
  pivot_longer(starts_with("burden"),
               names_to = "type",
               values_to = "value") %>%
  filter(incg_5 == 1 | incg_5 == 5) %>%
  filter(!is.na(value) &
           value != "-Inf" & value != "Inf" & value < 0.5 & value > -0.5) %>%
  mutate(
    Type = ifelse(
      type == "burden_CO2_within_pc",
      "National Carbon Price",
      "National Carbon Price and equal per capita transfer"
    ),
    value = round(value, 3)
  ) %>%
  dplyr::group_by(incg_5, Type, value) %>%
  dplyr::summarise(weights = sum(hh_weights)) %>%
  dplyr::ungroup()

# Calculate total households
IG_weights <- tza_2.2.1 %>%
  select(hh_id, incg_5, hh_weights) %>%
  dplyr::group_by(incg_5) %>%
  dplyr::summarise(IG_weights = sum(hh_weights)) %>%
  dplyr::ungroup()
# Calculate Shares
inci_x1 <- left_join(inci_x0, IG_weights) %>%
  mutate(share = weights / IG_weights) %>%
  filter(Type == "National Carbon Price")
inci_x2 <- left_join(inci_x0, IG_weights) %>%
  mutate(share = weights / IG_weights) %>%
  filter(Type != "National Carbon Price")

#Calculate Median
md_X1 <- cal_md(inci_x1) %>%
  dplyr::filter(Type == "National Carbon Price")
md_X2 <- cal_md(inci_x2) %>%
  dplyr::filter(Type != "National Carbon Price")

# Calculate Median Y
tx1 <- cal_md_y(inci_x1, md_X1, adjust_0)
tx2 <- cal_md_y(inci_x2, md_X2, adjust_0)

inci_x1.1 <- inci_x1 %>%
  left_join(tx1, by = c("incg_5"))
inci_x2.1 <- inci_x2 %>%
  left_join(tx2, by = "incg_5")
inci_x3.1 <- bind_rows(inci_x1.1, inci_x2.1) %>%
  mutate(
    Label = ifelse(
      incg_5 == 1 &
        Type == "National Carbon Price",
      "National Carbon Price (Expenditure Quintile 1)",
      ifelse(
        incg_5 == "5" &
          Type == "National Carbon Price",
        "National Carbon Price (Expenditure Quintile 5)",
        ifelse(
          incg_5 == "1" &
            Type == "National Carbon Price and equal per capita transfer",
          "National Carbon Price and equal per capita Transfer 
          (Expenditure Quintile 1)",
          "National Carbon Price and equal per capita Transfer 
          (Expenditure Quintile 5)"
        )
      )
    )
  )


write.xlsx(inci_x3.1, 
           file.path(dir[["analysis"]], "LST_national.xlsx")
)



# Figure 2a in paper  -----------------------------------------------------
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

png(filename= file.path(dir[["figures"]], "Figure_2a.png"), 
    width = 15.5, height = 15, unit = "cm", res = 400)
print(plot_2a)
dev.off()



# Other Graphs -----------------------------------------------------


tza_info <- read_csv(file.path(dir[["tza_processed"]], "clean_tanzania_2020.csv")) %>% 
  filter(!is.na(hh_id)) %>% 
  filter(!is.na(hh_weights)) %>%
  filter(!is.na(hh_size)) %>% 
  filter(!is.na(expenditures)) %>% 
  select(-itemcode,-expenditures) %>% 
  distinct(hh_id, .keep_all = T)



tza_et <-
  read_csv(file.path(dir[["analysis"]], "Expenditure_types_incgrp_tza.csv")) %>% 
  dplyr::group_by(incg_5) %>% 
  dplyr::summarize(share_food=sum(share_food)/n(),
                   share_energy=sum(share_energy)/n(),
                   share_goods=sum(share_goods)/n(),
                   share_services=sum(share_services)/n(),
                   share_other=sum(share_other)/n()
  ) %>% 
  dplyr::ungroup()%>% 
  pivot_longer(cols=c(starts_with("share_")),
               names_to = "type",
               values_to = "share") 



## Appendix Figures A2b ----------------------------------------------------


tza_et_UR <-
  read_csv(file.path(dir[["analysis"]], "Expenditure_types_incgrp_UR_tza.csv")) %>% 
  dplyr::group_by(urban, incg_5_UR) %>% 
  dplyr::summarize(share_food=sum(share_food,na.rm=TRUE)/n(),
                   share_energy=sum(share_energy,na.rm=TRUE)/n(),
                   share_goods=sum(share_goods,na.rm=TRUE)/n(),
                   share_services=sum(share_services,na.rm=TRUE)/n(),
                   share_other=sum(share_other,na.rm=TRUE)/n()
  ) %>% 
  dplyr::ungroup()%>%
  pivot_longer(cols=c(starts_with("share_")),
               names_to = "type",
               values_to = "share")



write.xlsx(tza_et_UR, 
           file.path(dir[["analysis"]], "Exp_typ_UR.xlsx")
)


### Rural plot ----------------------------------------------------------------
tza_et_r <-
  read_csv(file.path(dir[["analysis"]], "Expenditure_types_incgrp_UR_tza.csv")) %>% 
  dplyr::group_by(urban, incg_5_UR) %>% 
  dplyr::summarize(share_food=sum(share_food,na.rm=TRUE)/n(),
                   share_energy=sum(share_energy,na.rm=TRUE)/n(),
                   share_goods=sum(share_goods,na.rm=TRUE)/n(),
                   share_services=sum(share_services,na.rm=TRUE)/n(),
                   share_other=sum(share_other,na.rm=TRUE)/n()
  ) %>% 
  dplyr::ungroup()%>%
  pivot_longer(cols=c(starts_with("share_")),
               names_to = "type",
               values_to = "share")%>% 
  filter(urban==1)


plot_5br <- ggplot(tza_et_r,
                   aes(
                     x = factor(incg_5_UR),
                     y = share,
                     fill = reorder(type, share)
                   )) +
  geom_col(col="black") + 
  scale_colour_npg(
    name = "",
    labels = c(
      "Energy",
      "Other",
      "Services",
      "Goods",
      "Food"
    )
  ) +
  scale_fill_npg  (
    name = "",
    labels = c(
      "Energy",
      "Other",
      "Services",
      "Goods",
      "Food"
    )
  ) +
  theme_bw() + 
  scale_y_continuous(
    expand = c(0, 0),
    breaks = seq(0.0, 1.0, 0.2),
    labels = scales::percent_format(accuracy = 1)
  ) +
  theme(
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.title  = element_text(size = 12),
    plot.title  = element_text(size = 15, hjust = 0.5,face = "bold"),
    legend.position = "bottom",
    strip.text = element_text(size = 12),
    panel.grid.major = element_line(linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(linewidth = 0.2),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
    panel.border = element_rect(linewidth = 0.3)
  )+
  scale_x_discrete(labels = c("1", "2", "3", "4", "5")) +
  ylab("Share of total Expenditure (%)") +
  xlab("") +
  ggtitle("Rural") +
  guides(colour = guide_legend(nrow = 1), fill = guide_legend(nrow = 1))

plot_5br

### Urban Plot ------------------------------------

tza_et_u <-
  read_csv(file.path(dir[["analysis"]],
                     "Expenditure_types_incgrp_UR_tza.csv")) %>% 
  dplyr::group_by(urban, incg_5_UR) %>% 
  dplyr::summarize(share_food=sum(share_food,na.rm=TRUE)/n(),
                   share_energy=sum(share_energy,na.rm=TRUE)/n(),
                   share_goods=sum(share_goods,na.rm=TRUE)/n(),
                   share_services=sum(share_services,na.rm=TRUE)/n(),
                   share_other=sum(share_other,na.rm=TRUE)/n()
  ) %>% 
  dplyr::ungroup()%>%
  pivot_longer(cols=c(starts_with("share_")),
               names_to = "type",
               values_to = "share")%>% 
  filter(urban==2)



plot_5bu <- ggplot(tza_et_u,
                   aes(
                     x = factor(incg_5_UR),
                     y = share,
                     fill = reorder(type, share)
                   )) +
  geom_col(col="black") + 
  scale_colour_npg(
    name = "",
    labels = c(
      "Energy",
      "Other",
      "Services",
      "Goods",
      "Food"
    )
  ) +
  scale_fill_npg  (
    name = "",
    labels = c(
      "Energy",
      "Other",
      "Services",
      "Goods",
      "Food"
    )
  ) +
  theme_bw() + 
  scale_y_continuous(
    expand = c(0, 0),
    breaks = seq(0.0, 1.0, 0.2),
    labels = scales::percent_format(accuracy = 1)
  ) +
  theme(
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.title  = element_text(size = 12),
    plot.title  = element_text(size = 15, hjust=0.5,face="bold"),
    legend.position = "bottom",
    strip.text = element_text(size = 12),
    panel.grid.major = element_line(linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(linewidth = 0.2),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
    panel.border = element_rect(linewidth = 0.3)
  )+
  scale_x_discrete(labels = c("1", "2", "3", "4", "5")) +
  ylab("Share of total Expenditure (%)") +
  xlab("") +ggtitle("Urban") +
  guides(colour = guide_legend(nrow = 1), fill = guide_legend(nrow = 1))

plot_5bu


# Combine plots

combine <- ggarrange(plot_5bu,plot_5br, legend="bottom",common.legend=T)

combine


# save plot
png(filename= file.path(dir[["figures"]], "Appendix_Figure_A2b.png"), 
    width = 15.5, height = 15, unit = "cm", res = 400)
print(combine)
dev.off()


# absolute expenditure in USD for electricity and fuels over the quintiles


fuels_tza <-  read_csv(file.path(dir[["analysis"]],
                                 "Expenditure_fuels_17USD_tza.csv"))

fuels_tza <- fuels_tza %>% 
  mutate(hh_id = as.numeric(hh_id)) 

tza_sub_f <- tza_sub_f %>% 
  mutate(hh_id = as.numeric(hh_id)) 

tza_info <- tza_info %>% 
  mutate(hh_id = as.numeric(hh_id)) 


fuels_tza_a <- fuels_tza %>%
  left_join(tza_info, by="hh_id")

fuels_tza_a <- fuels_tza_a %>% 
  left_join(tza_sub_f, by="hh_id") %>% 
  filter(!is.na(hh_id)) %>% 
  filter(!is.na(hh_size)) %>% 
  mutate(incg_5 = as.numeric(binning(hh_exp_USD, 
                                     bins=5, 
                                     method = c("wtd.quantile"),
                                     labels = seq(1,5,length.out = 5), 
                                     weights = hh_weights)))%>% 
  dplyr::group_by(incg_5)%>%
  dplyr::summarize(Petrol_Diesel_usd_a=sum(USD_Petrol_Diesel, na.rm=T),
                   Electricityusd_a=sum(USD_Electricity, na.rm=T),
                   Gas_usd_a=sum(USD_Gas, na.rm=T),
                   Kerosene_usd_a=sum(USD_Kerosene, na.rm=T)
  ) %>% 
  ungroup()

write.xlsx(fuels_tza_a, 
           file.path(dir[["analysis"]],
                     "fuels_absolute_expenditure_usd_IG.xlsx")
)

rm(list = ls())













