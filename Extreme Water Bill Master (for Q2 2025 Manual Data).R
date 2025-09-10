### Extreme Water Bill MASTER - Q2 2025 Update ### 
# Author: Rachel Skillman
# Date Created: 9/9/25
# Date Updated: 9/10/25

## SET-UP ######################################################################  
#### Set Working Directory ####

setwd("C:/Users/rskillman/OneDrive - Water Boards/Documents/Data Work/Master Data Files")

#### Load Libraries ####

#install.packages("")
library(readxl)
library(tidyverse)
library(sf) #for spatial data checks
library(readr)
library(scales) #for number/dollar format
library(tidycensus)
library(purrr)

## LOAD DATA ###################################################################
#### Load Water Rates master data file ####

wr <- read_excel("Manual Data/Q2 2025_08.28.25/WaterRate_ManualData_09.09.25.xlsx")

#### Load Inventory ####

inventory <- read_excel("C:/Users/rskillman/OneDrive - Water Boards/Documents/Data Work/Inventory/2025-Q2 Refresh_Inventory.xlsx", 
                        sheet = "Comprehensive Inventory 2025-Q2") 
cat("The inventory dataset queried on 08/28/2025 contains", nrow(inventory), "rows.\n")
#The inventory dataset queried on 08/28/2025 contains 3291 rows.


#### SABL+ Layer ####

sablplus <- st_read("C:/Users/rskillman/OneDrive - Water Boards/Documents/Data Work/SABL/SABL+/Manual Data/SABL+ Manual Data Q2 2025/0.5 Miles/SABL+_0.5_09.04.25.shp") %>% 
  st_transform("EPSG:3310") %>% #reproject to California Teale Albers 
  setNames(c("SABL_PWSID", "NAME", "BOUNDARY_TYPE", "FED_TYPE", "COUNTY", "POPULATION", "TINWSYS_IS_NUMBER", "SERVICE_CONNECTIONS", "REGULATING_AGENCY", "geometry")) %>%
  mutate(ws_area = st_area(geometry)) 
#st_crs(sablplus) #check CRS 
cat("The SABL+ dataset from 9/4/25 for the Q2 2025 Manual Data refresh contains", nrow(sablplus), "rows and", length(unique(sablplus$SABL_PWSID)), "unique PWSIDs.\n") 
#The SABL+ dataset from 9/4/25 contains 3291 rows and 3291 unique PWSIDs.


## CREATE EXTREME WATER BILL INDICATOR #########################################
#### Join inventory with water rates data ####

inventory_wr <- sablplus %>% 
  as.data.frame() %>% 
  dplyr::select(!geometry) %>%
  rename(PWSID = SABL_PWSID) %>%
  left_join(wr %>% 
              dplyr::select(PWSID, military_dummy, school_dummy, EAR23_STATUS, RATE_6HCF)) %>% 
  mutate(
    #RATE_6HCF = ifelse(NAME == "H & J WATER COMPANY", "$56.55", RATE_6HCF),
         RATE_NUM = ifelse(RATE_6HCF %in% c("Missing", "N/A"), NA, RATE_6HCF),
         RATE_NUM = as.numeric(gsub("[$]", "", RATE_NUM))) 

#### Calculate statewide average water rate ####

wr_mean <- round(mean(inventory_wr$RATE_NUM, na.rm = T), 2) #another question - for fairness should I round this?

ewb <- inventory_wr %>% 
  mutate(ewb = RATE_NUM/wr_mean,
         ewb = ifelse(RATE_6HCF == "Missing", "Missing", ewb),
         ewb = ifelse(RATE_6HCF == "N/A", "N/A", ewb))

EWB_CLEAN <- ewb %>% 
  dplyr::select(PWSID,
                NAME,
                `Monthly Total Drinking Water Charges for 6HCF` = RATE_6HCF,
                `Extreme Water Bill (Statewide Avg Water Rate $71.56)` = ewb)

#### Write Extreme Water Bill Data ####

#openxlsx::write.xlsx(EWB_CLEAN,"C:/Users/rskillman/OneDrive - Water Boards/Documents/Data Work/Master Data Files/Manual Data/Q2 2025_08.28.25/ExtremeWaterBill_ManualData_09.10.25.xlsx") 
