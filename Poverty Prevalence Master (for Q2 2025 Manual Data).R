### Poverty Prevalence MASTER - Q2 2025 Update ###
# Author: Rachel Skillman
# Date Created: 9/4/25
# Date Updated: 9/9/25

# Code history: this code was initially from "Poverty Percent.R"

## SET-UP ######################################################################  
#### Set Working Directory ####

setwd("C:/Users/rskillman/OneDrive - Water Boards/Documents/Data Work/Socioeconomic Indicators")

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
#### SABL+ Layer ####

sablplus <- st_read("C:/Users/rskillman/OneDrive - Water Boards/Documents/Data Work/SABL/SABL+/Manual Data/SABL+ Manual Data Q2 2025/0.5 Miles/SABL+_0.5_09.04.25.shp") %>% 
  st_transform("EPSG:3310") %>% #reproject to California Teale Albers 
  setNames(c("SABL_PWSID", "NAME", "BOUNDARY_TYPE", "FED_TYPE", "COUNTY", "POPULATION", "TINWSYS_IS_NUMBER", "SERVICE_CONNECTIONS", "REGULATING_AGENCY", "geometry")) %>%
  mutate(ws_area = st_area(geometry)) 
#st_crs(sablplus) #check CRS 
cat("The SABL+ dataset from 9/4/25 for the Q2 2025 Manual Data refresh contains", nrow(sablplus), "rows and", length(unique(sablplus$SABL_PWSID)), "unique PWSIDs.\n") 
#The SABL+ dataset from 9/4/25 contains 3291 rows and 3291 unique PWSIDs.

#### ACS Poverty Data ####

acs_poverty <- read_csv("ACS/ACSDT5Y2023.C17002_2025-03-11T140549/ACSDT5Y2023.C17002-Data.csv", skip = 1) %>% 
  dplyr::select(GEO_ID = 1,
                NAME = 2,
                acs_pov_pop = 3, #Estimate - Total - Population for whom poverty status is determined
                acs_pov_pop_moe = 4, #MOE - Population for whom poverty status is determined
                acs_under.5 = 5, #Estimate - Total - Population for whom poverty status is determined - ALL INDIVIDUALS WITH INCOME BELOW THE FOLLOWING POVERTY RATIOS -
                acs_under.5_moe = 6, 
                acs_.5to.99 = 7,
                acs_.5to.99_moe = 8,
                acs_1to1.24 = 9,
                acs_1to1.24_moe = 10,
                acs_1.25to1.49 = 11,
                acs_1.25to1.49_moe = 12, 
                acs_1.5to1.84 = 13,
                acs_1.5to1.84_moe = 14,
                acs_1.85to1.99 = 15,
                acs_1.85to1.99_moe = 16)

#### ACS Census Block Group Shapefile 2023 (TIGER/Line) ####

#(downloaded 1/29/25)
bg_23_proj <- st_read("C:/Users/rskillman/Downloads/tl_2023_06_bg/tl_2023_06_bg.shp") %>% 
  st_transform("EPSG:3310") #reproject to California Teale Albers
#st_crs(bg_23) #CRS: NAD 83

## CLEAN DATA ##################################################################
#### Clean Poverty data ####

poverty <- acs_poverty %>%
  mutate(across(c(5:16), as.numeric)) %>%  # Convert to numeric
  rowwise() %>%
  mutate(acs_pov200_pop = rowSums(across(c(5,7,9,11,13,15)), na.rm = TRUE),
         GEO_ID = GEO_ID %>% str_replace(".*US", ""),
         percent_below_200 = (acs_pov200_pop/acs_pov_pop)*100, #calculate the % of population assessed for poverty status that falls below 200% of poverty line
         percent_below_200 = ifelse(acs_pov_pop == 0, NA, percent_below_200)) %>% 
  rename(GEOID = 1)

## JOIN CLEAN DATA TO SHAPEFILES ###############################################
#### Join ACS Block Group data to Block Group shapefile ####

blockgroup23 <- bg_23_proj %>%
  left_join(poverty %>% 
              dplyr::select(GEOID,
                            percent_below_200), 
            by = c("GEOID")) %>%
  mutate(bg_area = st_area(geometry)) #calculate parcel area
# n = 25,607

## INTERSECT AREAS & SABL+ ##################################################### 
#### Intersect ACS Census Block Group shapefile with SABL+ ####
intersect_bg <- st_intersection(blockgroup23, sablplus) %>% 
  mutate(wsbg_intersect_area = st_area(geometry),
         ws_overlap_bg_percent = as.numeric(wsbg_intersect_area)/as.numeric(ws_area), #proportion of water system area taken up by each block group
         bg_overlap_ws_percent = as.numeric(wsbg_intersect_area)/as.numeric(bg_area)) #proportion of block group area taken up by each water system

# For reference, count the number of block groups that intersect each water system and how many of them have a known poverty
count_intersecting_bg_pov <- intersect_bg %>% 
  as.data.frame() %>% 
  dplyr::select(SABL_PWSID, ws_overlap_bg_percent, percent_below_200) %>%
  mutate(countws = 1, na_flag = ifelse(is.na(percent_below_200), 1, 0)) %>%
  group_by(SABL_PWSID) %>% 
  summarise(
    num_bg_intersecting = sum(countws, na.rm = TRUE),
    num_bg_na_pov = sum(na_flag, na.rm = TRUE) 
  ) %>% #3187 water systems, which matches inventory
  mutate(nopov = ifelse((num_bg_intersecting - num_bg_na_pov) == 0, 1, 0))

## CALCULATE BLOCK GROUP AREA-WEIGHTED POVERTY ###################
#### Calculate the % of WS covered by block groups with a known poverty ####

# Find the non NA area of block groups intersecting each WS ("known poverty area")
bg_wpov <- intersect_bg %>% 
  dplyr::select(SABL_PWSID, wsbg_intersect_area, percent_below_200) %>%
  group_by(SABL_PWSID) %>%
  filter(!is.na(percent_below_200)) %>% 
  summarise(nonNA_area = sum(wsbg_intersect_area, na.rm = T)) 

# Find the percentage of "known poverty area" made up by each non-NA intersecting block group
intersect_bg_known_pov <- intersect_bg %>% 
  left_join(bg_wpov %>% 
              as.data.frame() %>% 
              dplyr::select(!geometry)) %>%
  mutate(ws_overlap_nonNAbg_percent = as.numeric(wsbg_intersect_area)/as.numeric(nonNA_area),
         POVERTY_BG23_AreaWt = ws_overlap_nonNAbg_percent*percent_below_200) #if you keep some key things from this you can just join it and you don't have to do all this complicated other stuff


#### Calculate the Block Group Area-Weighted Poverty % for 2023 ####

POVERTY <- intersect_bg_known_pov %>%
  group_by(SABL_PWSID) %>%
  summarise(POVERTY_BG23_AreaWt_WS = sum(POVERTY_BG23_AreaWt, na.rm = TRUE)) %>%
  mutate(POVERTY_BG23_AreaWt_WS = 
           ifelse(SABL_PWSID %in% 
                    c(count_intersecting_bg_pov$SABL_PWSID[count_intersecting_bg_pov$nopov == 1]), 
                  NA, 
                  POVERTY_BG23_AreaWt_WS)) %>% 
  as.data.frame() %>%
  dplyr::select(!geometry) %>% 
  left_join(count_intersecting_bg_pov %>% dplyr::select(1:4))

POVERTY_CLEAN <- POVERTY %>%
  left_join(sablplus %>% as.data.frame() %>% dplyr::select(!geometry))%>%
  dplyr::select(PWSID = SABL_PWSID,
                NAME,
                COUNTY,
                REGULATING_AGENCY,
                FED_TYPE,
                SERVICE_CONNECTIONS,
                POPULATION,
                POVERTY_PREVALENCE = POVERTY_BG23_AreaWt_WS,
                BOUNDARY_TYPE) 

summary(POVERTY_CLEAN$POVERTY_PREVALENCE) #between 0-100

#### Write Poverty Prevalence Data ####

# openxlsx::write.xlsx(POVERTY_CLEAN,"C:/Users/rskillman/OneDrive - Water Boards/Documents/Data Work/Master Data Files/Poverty_SAFERDashboard_06.20.25.xlsx") 
# openxlsx::write.xlsx(POVERTY_CLEAN,"C:/Users/rskillman/OneDrive - Water Boards/Documents/Data Work/Master Data Files/Manual Data/Q2 2025_08.28.25/Poverty_ManualData_09.04.25.xlsx") 
