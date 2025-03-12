# Using Carbon Pricing Revenues to Empower Low-Income Households with Renewable Energy: A Distributional Analysis for Tanzania

  - This repository contains code and supplementary data for the study "Using Carbon Pricing Revenues to Empower Low-Income Households with Renewable Energy: A Distributional Analysis for Tanzania" by Asare and Schürer.

## Data_files folder:
  - GTAP_matching_categories.xlsx: provides the classification of item codes into consumption categories, including food, goods, energy, and services.
  - GTAP_matching_sectors.xlsx: maps individual item codes to their corresponding GTAP sectors.
  - hh-CO2-emissions.xlsx* and hh-CO2-intensities.xlsx*: contain data on carbon intensities and CO₂ emissions.
    -- int refers to international data.
    -- TZA denotes national-level data. 
    -- ELY pertains to the electricity sector.
    -- fuel represents the fuel carbon price.
 **Note:** _Due to confidentiality restrictions, the microdata and GTAP raw data cannot be made publicly available_.
          _However, the datasets can be accessed from the following sources_:
- 2019 NPS Data: Available at [World Bank Microdata Library](https://microdata.worldbank.org/index.php/catalog/3885).
- 2020 NPS Data: Accessible at [World Bank Microdata Library](https://microdata.worldbank.org/index.php/catalog/3885/get-microdata)
  upon registration and submission of the required documentation.

## R_Codes folder:
This folder contains R scripts used for the analyses, figures and tables presented in both the main paper and the supplementary appendix.
### NPS 2019 Folder:
  - 00_setup.R: Script for seting up directories.
  - 01_selectcl_data.R: Script for selecting relevant microdata and performing data preprocessing.
  - 02_analysis.R: Script for computing carbon incidence and related metrics. 
  - 03_1_infras.R: Generates Figure 1 in the main paper and Appendix A.1.
  - 03_2_graph_analysis.R: Produces Figures 2, 3, 4, and 5 in the main paper, as well as Appendix Figure A.2.
  - 03_3_compensation_nl.R: Generates the compensation graph for the national carbon price (Figure 6 in the main paper) and Appendix Tables A.6 and A.7.
  - 03_4_compensation_nl_ur.R: Generates the compensation graph for the national carbon price, disaggregated by urban and rural areas (Appendix Figure A.3).
  - 03_5_compensation_gl.R: Produces the compensation graph for the global carbon price (Appendix Figure A.4).
  - 04_01_SS.R: Generates Appendix Tables A.2, A.3, A.4, A.8, and A.9.
  - 04_02_SS_hhchar.R: Generates Appendix Table A.5.
 ### NPS 2020 Folder:
  - 00_setup.R: Script for seting up directories.
  - 01_selectcl_data.R: Script for selecting relevant microdata and performing data preprocessing.
  - 02_analysis.R: Script for computing carbon incidence and related metrics.
  - 03_1_infras.R: Generates Appendix Figure A.5.
  - 03_2_graph_analysis.R: Produces Appendix Figures A.6, A.7, A.8, and A.9.
  - 04_01_SS.R: Generates Appendix Tables A.11, A.12, and A.13.
    
## Email contact:
  - abigail.asare@uol.de
  - laura.schuerer@uol.de 
