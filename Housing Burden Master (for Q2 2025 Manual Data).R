### Housing Burden MASTER - Q2 2025 Update ###
# Author: Rachel Skillman
# Date Created: 9/4/25
# Date Updated: 9/9/25

# Code history: this code was initially from "Housing Burden.R"

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


#### HUD CHAS Data ####

#2017-2021
hud <- read_csv("HUD/2017thru2021-140-csv/140/Table8.csv") #downloaded 1/27/25

# Find data dictionaries here: https://www.huduser.gov/portal/datasets/cp/CHAS/data_doc_chas.html
# Downloaded the data from here on 1/27/25: https://www.huduser.gov/portal/datasets/cp.html#data_2006-2021

#### TIGER/Line Census Tract Shapefile ####

#(downloaded 2023 - 1/28/25)
tract_23_proj <- st_read("C:/Users/rskillman/OneDrive - Water Boards/Documents/Data Work/ArcGIS Layer Formatting (Mehreen)/Census Tract/TIGERLine Data/tl_2023_06_tract.shp") %>% 
  st_transform("EPSG:3310") #reproject to California Teale Albers
#st_crs() #CRS: NAD 83
#can use 2023 shapefile because tract boundaries are unchanged between 2020-24


## CLEAN DATA ##################################################################
#### Clean Housing Burden data ####
# Filter to CA
# Keep variables: census tract - tract
# T8_est10 (Owner occupied - less than or equal to 30% of HAMFI - greater than 50% Cost burden) + T8_moe10
# T8_est23 (Owner occupied- greater than 30% but less than or equal to 50% of HAMFI - greater than 50%  Cost burden) + T8_moe23
# T8_est36 (Owner occupied - greater than 50% but less than or equal to 80% of HAMFI - greater than 50% Cost burden) + T8_moe36
# T8_est76 (Renter occupied - less than or equal to 30% of HAMFI - greater than 50%  Cost burden) + T8_moe76
# T8_est89 (Renter occupied - greater than 30% but less than or equal to 50% of HAMFI - greater than 50% Cost burden) + T8_moe89
# T8_est102 (Renter occupied - greater than 50% but less than or equal to 80% of HAMFI - greater than 50% Cost burden) + T8_moe102
# Sum all these up to get the # of households w/ household income < 80% HAMFI & cost burden > 50%
# Also need: total households --> % of people housing burdened
# T8_est1 (Total: Occupied housing units) + T8_moe1
hud_ca <- hud %>% filter(st == "06") %>% #8057
  dplyr::select(geoid, name,
                T8_est1, T8_moe1, #change names to be more intuitive for others checking my work
                T8_est10, T8_moe10, 
                T8_est23, T8_moe23, 
                T8_est36, T8_moe36, 
                T8_est76, T8_moe76, 
                T8_est89,  T8_moe89, 
                T8_est102, T8_moe102) %>% 
  rowwise() %>%
  mutate(geoid = geoid %>% str_replace(".*US", ""),
         `Census Tract` = as.numeric(geoid),
         total_sum = T8_est10 + T8_est23 + T8_est36 + T8_est76 + T8_est89 + T8_est102,
         prop_hb = total_sum / T8_est1,
         percent_hb = prop_hb*100,
         percent_hb = ifelse(is.na(percent_hb), NA, percent_hb)) %>% 
  rename(GEOID = 1)

## JOIN CLEAN DATA TO SHAPEFILES ###############################################
#### Join ACS Census Tract Housing Burden data to Tract shapefile ####

tract23 <- tract_23_proj %>%
  left_join(hud_ca %>% 
              dplyr::select(GEOID,
                            total_hh = T8_est1,
                            percent_hb), 
            by = c("GEOID")) %>%
  mutate(tract_area = st_area(geometry))
# n = 9,129

#31 tracts have 0% housing burdened and 99 tracts have no data (0 population)
# sum(is.na(tract23$percent_hb))
# sum(tract23$percent_hb == 0, na.rm = T)

## INTERSECT AREAS & SABL+ ##################################################### 
#### Intersect ACS Census Tract shapefile with SABL+ ####
intersect_tract <- st_intersection(tract23, sablplus) %>% 
  mutate(wstract_intersect_area = st_area(geometry),
         ws_overlap_tract_percent = as.numeric(wstract_intersect_area)/as.numeric(ws_area), #proportion of water system area taken up by each tract
         tract_overlap_ws_percent = as.numeric(wstract_intersect_area)/as.numeric(tract_area)) #proportion of tract area taken up by each water system

# For reference, count the number of tracts that intersect each water system 
count_intersecting_tract <- intersect_tract %>% 
  as.data.frame() %>% 
  mutate(countws = 1,
         na_flag = ifelse(is.na(percent_hb), 1, 0)) %>%
  group_by(SABL_PWSID) %>% 
  summarise(
    num_tract_intersecting = sum(countws, na.rm = TRUE),
    num_tract_na_hb = sum(na_flag, na.rm = TRUE)) %>%
  mutate(nohb = ifelse((num_tract_intersecting - num_tract_na_hb) == 0, 1, 0))

## CALCULATE CENSUS TRACT AREA-WEIGHTED HOUSING BURDEN #########################
#### Calculate the % of WS covered by tracts with a known HB ####

# Find the non NA area of census tracts intersecting each WS ("known Housing Burden area")
tract_whb <- intersect_tract %>% 
  dplyr::select(SABL_PWSID, wstract_intersect_area, percent_hb) %>%
  group_by(SABL_PWSID) %>%
  filter(!is.na(percent_hb)) %>% 
  summarise(nonNA_area = sum(wstract_intersect_area, na.rm = T)) 

# Find the percentage of "known Housing Burden area" made up by each non-NA intersecting tract
intersect_tract_known_hb <- intersect_tract %>% 
  left_join(tract_whb %>% 
              as.data.frame() %>% 
              dplyr::select(!geometry)) %>%
  mutate(ws_overlap_nonNAtract_percent = as.numeric(wstract_intersect_area)/as.numeric(nonNA_area),
         HOUSINGBURDEN23_AREAWT = ws_overlap_nonNAtract_percent*percent_hb) 

#### Calculate the Tract Area-Weighted Housing Burdened Percent for 2023 ####

HOUSINGBURDEN <- intersect_tract_known_hb %>%
  group_by(SABL_PWSID) %>%
  summarise(HOUSINGBURDEN23_AREAWT = sum(HOUSINGBURDEN23_AREAWT, na.rm = TRUE)) %>%
  #mutate(POVERTYPCT23_AREAWT = ifelse(POVERTYPCT23_AREAWT == 0, NA, POVERTYPCT23_AREAWT)) %>% 
  as.data.frame() %>%
  dplyr::select(!geometry) %>% 
  left_join(count_intersecting_tract) %>%
  mutate(HOUSINGBURDEN23_AREAWT = ifelse(nohb == 1, NA, HOUSINGBURDEN23_AREAWT))

HOUSINGBURDEN_CLEAN <- HOUSINGBURDEN %>%
  left_join(sablplus %>% as.data.frame() %>% dplyr::select(!geometry))%>%
  dplyr::select(PWSID = SABL_PWSID,
                NAME,
                COUNTY,
                REGULATING_AGENCY,
                FED_TYPE,
                SERVICE_CONNECTIONS,
                POPULATION,
                HOUSING_BURDEN = HOUSINGBURDEN23_AREAWT,
                BOUNDARY_TYPE) 

summary(HOUSINGBURDEN_CLEAN$HOUSING_BURDEN) #between 0-100

#### Write Housing Burden data ####
# openxlsx::write.xlsx(HOUSINGBURDEN_CLEAN,"C:/Users/rskillman/OneDrive - Water Boards/Documents/Data Work/Master Data Files/HousingBurden_SAFERDashboard_06.20.25.xlsx") 
# openxlsx::write.xlsx(HOUSINGBURDEN_CLEAN,"C:/Users/rskillman/OneDrive - Water Boards/Documents/Data Work/Master Data Files/Manual Data/Q2 2025_08.28.25/HousingBurden_ManualData_09.04.25.xlsx") 
