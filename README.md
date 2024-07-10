# Incidence of Carbon Pricing in Tanzania: Using Revenues to Empower Low-Income Households with Renewable Energy 

  - This repository contains codes and supplementary data on "Incidence of Carbon Pricing in Tanzania:
    Using Revenues to Empower Low-Income Households with Renewable Energy" by Asare and Schüer

## Data_files folder:
  - GTAP_matching_categories.xlsx contains matching of item codes to the consumption categories (food, goods, energy, services).
  - GTAP_matching_sectors.xlsx contains matching each item code to GTAP sectors.
  - hh-CO2-emmisions*.xlsx and hh-CO2-intensities*.xlsx contains carbon intensities and emissions outputs from GAMS.
  - Where int-international; ELY- electricity; TZA-national and fuel-transport embedded carbon intensities or emissions.
 **Note:** _Due to confidentiality, we can not make available the micro and GTAP raw data_.
          _However the datasets can be download from respective data websites (see paper for links and further information)_

## R_Codes folder:
  - 01_selectcl_data.R is scripts for selecting relevant micro data and data cleaning
  - 02_analysis.R is the script for calculating the carbon incidence and related calculations. 
  - 03_1_infrastructure.R produces the infrastructure graph (Figure 1 and B.1 in paper).
  - 03_2_graph_analysis.R generates the Figure 2 and 5 in paper and Appendix tables.
  - 03_3_compensation_nl.R produces the compensation graph for national carborn price (Figure 6 in paper).
  - 03_4_compensation_nl-ur.R produces the compensation graph for global carborn price (Figure 7 in paper).
  - 03_5_compensation_gl.R produces the compensation graph for global carborn price (Figure B.2 in paper).

## Figures folder:
  - Figures contains all png figures in the paper.
    
## Email contact:
  - abigail.asare@uol.de
  - laura.schuerer@uol.de 
