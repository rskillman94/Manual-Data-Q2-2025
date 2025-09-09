### Household Socioeconomic Burden MASTER - Q2 2025 Update ###
# Author: Rachel Skillman
# Date Created: 9/5/25
# Date Updated: 9/9/25

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
#### Load Poverty Prevalence master data file ####

poverty <- read_excel("Manual Data/Q2 2025_08.28.25/Poverty_ManualData_09.04.25.xlsx") 


#### Load Housing Burden master data file ####

hb <- read_excel("Manual Data/Q2 2025_08.28.25/HousingBurden_ManualData_09.04.25.xlsx") 

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


## CREATE HOUSEHOLD SOCIOECONOMIC BURDEN INDICATOR #############################
#### Join inventory with PPI and HBI ####

inventory_hsb <- inventory %>%
  dplyr::select(PWSID = `Water System Number`, Name = `Water System Name`) %>%
  
  # Join inventory with poverty and housing burden 
  left_join(poverty) %>%
  left_join(hb) %>%
  relocate(BOUNDARY_TYPE, .after = FED_TYPE) %>%
  
  mutate(
    # Round and format poverty and housing burden as 2-decimal character strings
    POVERTY_PREVALENCE_RD = ifelse(
      is.na(POVERTY_PREVALENCE), "N/A",
      formatC(round(POVERTY_PREVALENCE, 2), format = "f", digits = 2)
    ),
    HOUSING_BURDEN_RD = ifelse(
      is.na(HOUSING_BURDEN), "N/A",
      formatC(round(HOUSING_BURDEN, 2), format = "f", digits = 2)
    ),
    
    # thresholds based on numeric rounded values (keep numeric copies if needed)
    POVERTY_NUM = round(POVERTY_PREVALENCE, 2),
    HOUSING_NUM = round(HOUSING_BURDEN, 2),
    
    PPI_threshold = case_when(
      is.na(POVERTY_NUM)  ~ "N/A",
      POVERTY_NUM < 20    ~ "0",
      POVERTY_NUM <= 35   ~ "1",
      TRUE                ~ "2"
    ),
    PPI_score = case_when(
      PPI_threshold == "N/A" ~ NA_real_,
      PPI_threshold == "0"   ~ 0,
      PPI_threshold == "1"   ~ 0.25,
      PPI_threshold == "2"   ~ 1
    ),
    PPI_risk_level = case_when(
      PPI_threshold == "N/A" ~ "Unknown",
      PPI_threshold == "0"   ~ "None",
      PPI_threshold == "1"   ~ "Medium",
      PPI_threshold == "2"   ~ "High"
    ),
    HBI_threshold = case_when(
      is.na(HOUSING_NUM) ~ "N/A",
      HOUSING_NUM < 14   ~ "0",
      HOUSING_NUM <= 21  ~ "1",
      TRUE               ~ "2"
    ),
    HBI_score = case_when(
      HBI_threshold == "N/A" ~ NA_real_,
      HBI_threshold == "0"   ~ 0,
      HBI_threshold == "1"   ~ 0.25,
      HBI_threshold == "2"   ~ 1
    ),
    HBI_risk_level = case_when(
      HBI_threshold == "N/A" ~ "Unknown",
      HBI_threshold == "0"   ~ "None",
      HBI_threshold == "1"   ~ "Medium",
      HBI_threshold == "2"   ~ "High"
    ),
    
    # final HSB score, always 2 decimals and character
    HSB_score = ifelse(
      is.na(PPI_score) | is.na(HBI_score),
      "N/A",
      formatC((PPI_score + HBI_score)/2, format = "f", digits = 2)
    ),
    
    # convert individual scores to character and handle NA
    PPI_score = ifelse(is.na(PPI_score), "N/A", formatC(PPI_score, format = "f", digits = 2)),
    HBI_score = ifelse(is.na(HBI_score), "N/A", formatC(HBI_score, format = "f", digits = 2))
  )


colSums(is.na(inventory_hsb))

#### Clean HSB data ####
HSB_CLEAN <- inventory_hsb %>%
  dplyr::select(PWSID,
                Name = NAME,
                `Poverty Prevalence Percent` = POVERTY_PREVALENCE_RD,
                `Poverty Prevalence Threshold` = PPI_threshold,
                `Poverty Prevalence Score` = PPI_score,
                `Housing Burden Percent` = HOUSING_BURDEN_RD,
                `Housing Burden Threshold` = HBI_threshold,
                `Housing Burden Score` = HBI_score,
                `HSB Combined Score`  = HSB_score) %>%
  mutate(`HSB Threshold Score` = ifelse(`HSB Combined Score` == "N/A", "N/A", NA),
         `HSB Threshold Score` = ifelse(`HSB Combined Score` %in% c("0.000", "0.125"), "0", `HSB Threshold Score`),
         `HSB Threshold Score` = ifelse(`HSB Combined Score` %in% c("0.250", "0.500"), "0.5", `HSB Threshold Score`),
         `HSB Threshold Score` = ifelse(`HSB Combined Score` %in% c("0.625", "1.000"), "1", `HSB Threshold Score`))
#add the extra column and clean the file

#### Write Household Socioeconomic Burden Data ####
#openxlsx::write.xlsx(HSB_CLEAN, "Manual Data/Q2 2025_08.28.25/HSB_ManualData_09.05.25.xlsx")



