### Median Household Income MASTER - Q2 2025 Update ###
# Author: Rachel Skillman
# Date Created: 9/10/25
# Date Updated: 9/10/25

# Code history: this code was initially from "Median Household Income MASTER.R"


## SET-UP ######################################################################  
#### Set Working Directory ####

setwd("C:/Users/rskillman/Downloads") #worry about changing this later

#### Load Libraries ####

#install.packages("")
library(ncf)
library(raster)
library(readxl)
library(readr)
library(sf)
library(spdep)
library(tidyverse)
library(terra)
library(exactextractr)
library(fasterize)
library(tigris)
library(corrplot)
library(ggtext)
library(scales)

#### Open the Server Connection for SSMS Pulls ####

con <- dbConnect(odbc(),                 
                 Driver = "SQL Server",                 
                 Server = "reportmanager,1542",                 
                 Database = "ReportDB",                 
                 UID = Sys.getenv("CDM5_SQL_SERVER_USER"),
                 PWD = Sys.getenv("CDM5_SQL_SERVER_PASSWORD"),
                 TrustServerCertificate = "yes",
                 Port = 1542) #must be logged into SSMS and/or VPN


## QUERY SDWIS DATA ############################################################ 
#### Load TINWSYS data ####

tinwsys <- dbGetQuery(con, "SELECT *
                    FROM [ReportDB].[SDWIS].[TINWSYS]") %>%
  mutate(NUMBER0 = str_trim(NUMBER0)) # Remove any trailing or leading white space

cat("The SDWIS TINWSYS dataset queried", format(Sys.Date(), "%m/%d/%Y"), "contains", nrow(tinwsys), "water systems.\n")
#The SDWIS TINWSYS dataset queried 09/10/2025 contains 15962 water systems.


#### Load Historical Population SAFER data ####

histpop <- dbGetQuery(con, "SELECT *
                    FROM [ReportDB].[SAFER].[HISTORICAL_POPULATION]")

cat("The SAFER HISTORICAL_POPULATION dataset queried", format(Sys.Date(), "%m/%d/%Y"), "contains", nrow(tinwssaa), "records.\n")
#The SAFER HISTORICAL_POPULATION dataset queried 09/10/2025 contains 15354 records.


#### Calculate 8/28/25 Population ####

date_check <- as.Date("2025-08-28")

histpop_828 <- histpop %>%
  inner_join(tinwsys, by = "TINWSYS_IS_NUMBER") %>%
  filter(
    date_check >= START_DATE & date_check <= END_DATE,
    D_POPULATION_COUNT.x != D_POPULATION_COUNT.y) %>%
  transmute(TINWSYS_IS_NUMBER,
            historical_pop = D_POPULATION_COUNT.x,
            current_pop = D_POPULATION_COUNT.y,
            START_DATE,
            END_DATE,
            CREATION_DATETIME) %>%
  mutate(flag_pop_threshold = (historical_pop < 500 & current_pop >= 500) |
           (historical_pop >= 500 & current_pop < 500)) %>%
  left_join(tinwsys %>%
              dplyr::select(TINWSYS_IS_NUMBER, SABL_PWSID = NUMBER0)) 

histpop_828_flag <- histpop_828 %>% 
  inner_join(inventory %>% dplyr::select(SABL_PWSID = `Water System Number`, `Water System Name`))
cat("There are", sum(histpop_828_flag$flag_pop_threshold == T), "water systems in the inventory that had a population difference between 8/28 and the day of SABL+ creation that would affect their MHI (in other words, pop values on different sides of 500 threshold).\n")
#There are 0 water systems in the inventory that had a population difference between 8/28 and the day of SABL+ creation that would affect their MHI (in other words, pop values on different sides of 500 threshold).

## LOAD CENSUS DATA ############################################################ 
#### ACS Census Block Group MHI ####
#(Downloaded: 2023 - 1/16/25; 2022 - 7/1/24; 2021 - 7/1/24, 2020 - 7/1/24)
blockgroup_mhi <- (read_csv("C:/Users/rskillman/Downloads/ACS_5YR_2023_MHI_Data/ACSDT5Y2023.B19013-Data.csv",
                            col_types = cols(...5 = col_skip()), 
                            skip = 1) %>%
                     setNames(c("GEOID", "name", "MHI", "MOE")) %>% 
                     mutate(GEOID = GEOID %>% str_replace(".*US", ""),
                            MHI = gsub("\\+", "", MHI),
                            MHI = gsub("\\-", "", MHI),
                            MHI = as.numeric(gsub(",", "", MHI)),
                            MOE = as.numeric(ifelse(MOE == "***", "0", MOE)),
                            Year = 2023)) %>% #1580 missing, 755 high, 3 low
  rbind(read_csv("C:/Users/rskillman/Downloads/ACS_5YR_2022_MHI_Data/ACSDT5Y2022.B19013-Data.csv",
                 col_types = cols(...5 = col_skip()), 
                 skip = 1) %>% 
          setNames(c("GEOID", "name", "MHI", "MOE")) %>%
          mutate(GEOID = GEOID %>% str_replace(".*US", ""), 
                 MHI = gsub("\\+", "", MHI),
                 MHI = gsub("\\-", "", MHI),
                 MHI = as.numeric(gsub(",", "", MHI)),
                 MOE = as.numeric(ifelse(MOE == "***", "0", MOE)),
                 Year = 2022)) %>%
  rbind(read_csv("C:/Users/rskillman/Downloads/ACSDT5Y2021.B19013_2024-07-01T200611/ACSDT5Y2021.B19013-Data.csv",
                 col_types = cols(...5 = col_skip()), 
                 skip = 1) %>% 
          setNames(c("GEOID", "name", "MHI", "MOE")) %>%
          mutate(GEOID = GEOID %>% str_replace(".*US", ""), 
                 MHI = gsub("\\+", "", MHI),
                 MHI = gsub("\\-", "", MHI),
                 MHI = as.numeric(gsub(",", "", MHI)),
                 MOE = as.numeric(ifelse(MOE == "***", "0", MOE)),
                 Year = 2021)) %>%
  rbind(read_csv("C:/Users/rskillman/Downloads/ACSDT5Y2020.B19013_2024-07-01T191825/ACSDT5Y2020.B19013-Data.csv",
                 col_types = cols(...5 = col_skip()), 
                 skip = 1) %>% 
          setNames(c("GEOID", "name", "MHI", "MOE")) %>%
          mutate(GEOID = GEOID %>% str_replace(".*US", ""), 
                 MHI = gsub("\\+", "", MHI), 
                 MHI = gsub("\\-", "", MHI),
                 MHI = as.numeric(gsub(",", "", MHI)),
                 MOE = as.numeric(ifelse(MOE == "***", "0", MOE)),
                 Year = 2020))



#### ACS Census Block Group Population ###
#(downloaded 1/16/25)
acs_pop_23 <- read_csv("C:/Users/rskillman/Downloads/ACS_5YR_2023_POP_Data/ACSDT5Y2023.B01003-Data.csv",
                       col_types = cols(...5 = col_skip()),
                       skip = 1) %>%
  setNames(c("GEOID", "name", "pop_23", "MOE_23")) %>%
  mutate(GEOID = GEOID %>% str_replace(".*US", ""))
#### ACS Census Designated Place MHI ####
cdp_mhi <- read_csv("C:/Users/rskillman/Downloads/ACSDT5Y2023.B19013_2025-03-05T102303/ACSDT5Y2023.B19013-Data.csv",
                    col_types = cols(...5 = col_skip()),
                    skip = 1) %>% 
  setNames(c("GEOID", "name", "MHI", "MOE")) %>%
  mutate(Year = 2023) %>% 
  rbind(read_csv("C:/Users/rskillman/Downloads/ACSDT5Y2022.B19013_2025-03-05T083254/ACSDT5Y2022.B19013-Data.csv", 
                 col_types = cols(...5 = col_skip()),
                 skip = 1) %>% #contains even more data on income distribution used to calculate median
          setNames(c("GEOID", "name", "MHI", "MOE")) %>%
          mutate(Year = 2022)) %>%
  rbind(read_csv("C:/Users/rskillman/Downloads/ACSDT5Y2021.B19013_2025-03-05T083553/ACSDT5Y2021.B19013-Data.csv",
                 col_types = cols(...5 = col_skip()),
                 skip = 1) %>%
          setNames(c("GEOID", "name", "MHI", "MOE")) %>%
          mutate(Year = 2021)) %>% 
  rbind(read_csv("C:/Users/rskillman/Downloads/ACSDT5Y2020.B19013_2025-03-05T083818/ACSDT5Y2020.B19013-Data.csv",
                 col_types = cols(...5 = col_skip()),
                 skip = 1) %>%
          setNames(c("GEOID", "name", "MHI", "MOE")) %>%
          mutate(Year = 2020)) %>%
  mutate(GEOID = GEOID %>% str_replace(".*US", ""), 
         MHI = gsub("\\+", "", MHI), 
         MHI = gsub("\\-", "", MHI),
         MHI = as.numeric(gsub(",", "", MHI)),
         MOE = as.numeric(ifelse(MOE == "***", "0", MOE)))

#### ACS Census Tract MHI ####
tract_mhi <- (read_csv("ACSDT5Y2023.B19013_2025-03-04T192822/ACSDT5Y2023.B19013-Data.csv", 
                       skip = 1) %>% 
                dplyr::select(GEOID = 1, MHI = 3, MOE = 4) %>%
                mutate(GEOID = GEOID %>% str_replace(".*US", ""),
                       MHI = gsub("\\+", "", MHI),
                       MHI = gsub("\\-", "", MHI),
                       MHI = as.numeric(gsub(",", "", MHI)),
                       MOE = as.numeric(ifelse(MOE == "***", "0", MOE)),
                       Year = 2023)) %>% # x tracts have high bound MHI (250,000+), x have low bound MHI (2,500-) 
  rbind(read_csv("ACSDT5Y2022.B19013_2025-03-04T192218/ACSDT5Y2022.B19013-Data.csv", 
                 skip = 1) %>% 
          dplyr::select(GEOID = 1,MHI = 3, MOE = 4) %>%
          mutate(GEOID = GEOID %>% str_replace(".*US", ""),
                 MHI = gsub("\\+", "", MHI),
                 MHI = gsub("\\-", "", MHI),
                 MHI = as.numeric(gsub(",", "", MHI)),
                 MOE = as.numeric(ifelse(MOE == "***", "0", MOE)),
                 Year = 2022)) %>%
  rbind(read_csv("ACSDT5Y2021.B19013_2025-03-04T192301/ACSDT5Y2021.B19013-Data.csv", 
                 skip = 1) %>% 
          dplyr::select(GEOID = 1,MHI = 3, MOE = 4) %>%
          mutate(GEOID = GEOID %>% str_replace(".*US", ""),
                 MHI = gsub("\\+", "", MHI),
                 MHI = gsub("\\-", "", MHI),
                 MHI = as.numeric(gsub(",", "", MHI)),
                 MOE = as.numeric(ifelse(MOE == "***", "0", MOE)),
                 Year = 2021)) %>%
  rbind(read_csv("ACSDT5Y2020.B19013_2025-03-04T192335/ACSDT5Y2020.B19013-Data.csv", 
                 skip = 1) %>% 
          dplyr::select(GEOID = 1,MHI = 3, MOE = 4) %>%
          mutate(GEOID = GEOID %>% str_replace(".*US", ""),
                 MHI = gsub("\\+", "", MHI),
                 MHI = gsub("\\-", "", MHI),
                 MHI = as.numeric(gsub(",", "", MHI)),
                 MOE = as.numeric(ifelse(MOE == "***", "0", MOE)),
                 Year = 2020))

#### DFA Income Survey MHI ####

dfa_incsurv <- read_excel("~/MHI/DFA/Completed Median Household Income Surveys 1.29.25.xlsx") %>% 
  dplyr::select(SABL_PWSID = 2, name = 3, dfa_survey_year = 4, 
                dfa_mhi = 5, dfa_defaults = 6, dfa_dac_status = 7, 
                dw_ww = 8, dfa_funding_source = 9, 10) %>%
  filter(dfa_survey_year %in% c("2021-22", "2022-23", "2023-24", "2024-25"),
         !SABL_PWSID %in% c("N/A", "NA"),
         !is.na(SABL_PWSID)) %>% #remove the 11 systems with no PWSID (8 are WW, 1 is CW + WW, 2 DW)
  mutate(dfa_mhi = as.numeric(dfa_mhi), 
         PWSID = paste0("CA", SABL_PWSID)) 

# Add additional income survey (sent by Mehreen on x/x/2025)
dfa_mhi <- dfa_incsurv %>% 
  dplyr::select(PWSID, dfa_mhi, dfa_survey_year) %>%
  rbind(data.frame(PWSID = "CA4000653",
                   dfa_mhi = 50000,
                   dfa_survey_year = "2024-25",
                   stringsAsFactors = FALSE))

## LOAD GEOGRAPHIC DATA ######################################################## 
# All TIGER/Line downloads here: https://www.census.gov/cgi-bin/geo/shapefiles/index.php
#### ACS Census Block Group Shapefile ####
#(downloaded 2023 - 1/29/25, 2022 - ?)
bg_23_proj <- st_read("C:/Users/rskillman/Downloads/tl_2023_06_bg/tl_2023_06_bg.shp") %>% 
  st_transform("EPSG:3310") #reproject to California Teale Albers
#st_crs(bg_23) #CRS: NAD 83

bg_22_proj <- st_read("C:/Users/rskillman/Downloads/2022_Block_Group_Shapefiles/tl_2022_06_bg.shp") %>% 
  st_transform("EPSG:3310") #reproject to California Teale Albers
#st_crs(bg_22) #CRS: NAD 83

#### ACS Census Designated Place Shapefile ####
cdp_23_proj <- st_read("C:/Users/rskillman/Downloads/tl_2023_06_place/tl_2023_06_place.shp") %>% 
  st_transform("EPSG:3310") #reproject to California Teale Albers

#### ACS Census Tract Shapefile ####
tract_23_proj <- st_read("C:/Users/rskillman/Downloads/tl_2023_06_tract/tl_2023_06_tract.shp") %>% 
  st_transform("EPSG:3310") %>% #reproject to California Teale Albers
  mutate(tract_area = st_area(geometry))

#### SABL+ Layer ####

sablplus <- st_read("C:/Users/rskillman/OneDrive - Water Boards/Documents/Data Work/SABL/SABL+/Manual Data/SABL+ Manual Data Q2 2025/0.5 Miles/SABL+_0.5_09.04.25.shp") %>% 
  st_transform("EPSG:3310") %>% #reproject to California Teale Albers 
  setNames(c("SABL_PWSID", "NAME", "BOUNDARY_TYPE", "FED_TYPE", "COUNTY", "POPULATION", "TINWSYS_IS_NUMBER", "SERVICE_CONNECTIONS", "REGULATING_AGENCY", "geometry")) %>%
  mutate(ws_area = st_area(geometry)) %>% 
  
  # Replace different populations 
  left_join(histpop_828 %>%
              dplyr::select(historical_pop,
                            current_pop,
                            SABL_PWSID)) %>%
  mutate(POPULATION = ifelse(POPULATION != historical_pop & !is.na(historical_pop), historical_pop, POPULATION)) %>%
  dplyr::select(-c(historical_pop, current_pop))

#st_crs(sablplus) #check CRS 
cat("The SABL+ dataset from 9/4/25 for the Q2 2025 Manual Data refresh contains", nrow(sablplus), "rows and", length(unique(sablplus$SABL_PWSID)), "unique PWSIDs.\n") 
#The SABL+ dataset from 9/4/25 contains 3291 rows and 3291 unique PWSIDs.


## JOIN MHI DATA TO SHAPEFILE ##################################################
#### Join ACS Block Group MHI data to Block Group shapefile ####
# 2023
blockgroup23 <- bg_23_proj %>%
  left_join(blockgroup_mhi %>%
              filter(Year == 2023) %>%
              dplyr::select(!Year), 
            by = c("GEOID")) %>%
  mutate(bg_area = st_area(geometry)) #calculate parcel area
# n = 25,607


# 2022
blockgroup22 <- bg_22_proj %>%
  left_join(blockgroup_mhi %>%
              filter(Year == 2022) %>%
              dplyr::select(!Year), 
            by = c("GEOID")) %>%
  mutate(bg_area = st_area(geometry)) #calculate parcel area
# n = 25,607

#### Join ACS CDP MHI data to CDP shapefiles ####

# 2023
cdp23 <- cdp_23_proj %>%
  left_join(cdp_mhi %>%
              filter(Year == 2023) %>%
              dplyr::select(!Year), 
            by = c("GEOID")) %>%
  mutate(cdp_area = st_area(geometry)) #calculate parcel area
# n = 1,618

# Detect overlapping CDP areas
cdp_overlap <- st_intersection(cdp23 %>% dplyr::select(GEOID, name), 
                               cdp23 %>% dplyr::select(GEOID, name)) %>%
  filter(GEOID != GEOID.1)  #remove self-intersections (where a tract intersects itself)

# Identify which CDPs are involved in overlaps
overlapping_cdp <- cdp_overlap %>% as.data.frame() %>%
  group_by(GEOID, name) %>%
  summarise(
    NUM_OVERLAPPING_CDP = n_distinct(name.1),  # Count unique overlapping cdps
    NAME_OVERLAPPING_CDP = paste(unique(name.1), collapse = ", ")) %>%  # Concatenate names
  ungroup() %>% 
  mutate(cdp_self_overlap_flag = 1)

# Join back to the original CDP dataset to add the overlap flag
cdp23 <- cdp23 %>%
  left_join(overlapping_cdp %>% dplyr::select(!name), by = "GEOID") %>%
  mutate(cdp_self_overlap_flag = ifelse(is.na(cdp_self_overlap_flag), 0, 1))  # Set non-overlapping tracts to 0

#### Join ACS Census Tract MHI data to Census Tract shapefile ####

# 2023
tract23 <- tract_23_proj %>% 
  left_join(tract_mhi %>%
              filter(Year == 2022) %>%
              dplyr::select(!Year), by = c("GEOID")) %>%
  mutate(tract_area = st_area(geometry)) #calculate tract area
# n = 9,129

# 2022 
tract22 <- tract_23_proj %>% #for the sake of time, just assume tract boundaries don't change
  left_join(tract_mhi %>%
              filter(Year == 2022) %>%
              dplyr::select(!Year), 
            by = c("GEOID")) %>%
  mutate(tract_area = st_area(geometry)) #calculate parcel area
# n = 9,129

# 2021
tract21 <- tract_23_proj %>% #for the sake of time, just assume tract boundaries don't change
  left_join(tract_mhi %>%
              filter(Year == 2021) %>%
              dplyr::select(!Year), 
            by = c("GEOID")) %>%
  mutate(tract_area = st_area(geometry)) #calculate parcel area
# n = 9,129

# 2020 
tract20 <- tract_23_proj %>% #for the sake of time, just assume tract boundaries don't change
  left_join(tract_mhi %>%
              filter(Year == 2020) %>%
              dplyr::select(!Year), 
            by = c("GEOID")) %>%
  mutate(tract_area = st_area(geometry)) #calculate parcel area
# n = 9,129

## IMPUTE MISSING MHI DATA ##################################################### 
#### Adjust Block Groups with missing MOE in 2023 ####

# Get a list of systems that are candidates for MHI replacement (MOE missing because MHI either 250,000+ or 2,500-, or MHI & MOE missing)
adjustmhi_bg <- blockgroup_mhi %>% 
  filter(Year == 2023,
         MHI == 250000 & MOE == 0 | MHI == 2500 & MOE == 0 | is.na(MHI)) %>%
  dplyr::select(GEOID)  %>%
  mutate(flag_23 = 1) 
#n = 2338 = 758 + 1580 matches the number of highs + lows + NAs identified originally


# Identify the most recent non-NA MOE for each relevant block group
blockgroup_mhi_imputed <- blockgroup_mhi %>%
  left_join(adjustmhi_bg, by = "GEOID") %>%  # Keep only those missing MOE in 2023
  filter(flag_23 == 1) %>%  # Ensure only relevant block groups are checked
  mutate(MOE = ifelse(MOE == 0, NA, MOE)) %>% # Need to convert MHI back to NA so it can be combined with NA MHI imputation
  arrange(GEOID, desc(Year)) %>%  # Start from most recent year
  group_by(GEOID) %>%
  mutate(
    MHI_impute = if_else(Year == 2023, first(na.omit(MHI[-1]), default = NA_real_), NA_real_),
    MOE_impute = if_else(Year == 2023, first(na.omit(MOE[-1]), default = NA_real_), NA_real_),
    Year_impute = if_else(Year == 2023, first(Year[!is.na(MOE) & Year < 2023], default = NA_integer_), NA_integer_)
  ) %>%
  ungroup() %>%
  filter(Year == 2023) %>%
  drop_na(MOE_impute) %>%
  dplyr::select(GEOID,
                MHI_impute,
                MOE_impute,
                Year_impute)
#dplyr::select(GEOID, 7:9) #this may result in no MHI for some systems that otherwise would be 250000 every year?
#out of the 2338 candidates for replacement, only 1795 actually get new data

#### Adjust CDPs with missing MOE in 2023 ####

# Get a list of systems that are candidates for MHI replacement (MOE missing because MHI either 250,000+ or 2,500-, or MHI & MOE missing)
adjustmhi_cdp <- cdp_mhi %>% 
  filter(Year == 2023,
         MHI == 250000 & MOE == 0 | MHI == 2500 & MOE == 0 | is.na(MHI)) %>%
  dplyr::select(GEOID)  %>%
  mutate(flag_23 = 1) 
#n = 271 = x + x matches the number of highs + lows + NAs identified originally


# Identify the most recent non-NA MOE for each relevant block group
cdp_mhi_imputed <- cdp_mhi %>%
  left_join(adjustmhi_cdp, by = "GEOID") %>%  # Keep only those missing MOE in 2023
  filter(flag_23 == 1) %>%  # Ensure only relevant block groups are checked
  mutate(MOE = ifelse(MOE == 0, NA, MOE)) %>% # Need to convert MHI back to NA so it can be combined with NA MHI imputation
  arrange(GEOID, desc(Year)) %>%  # Start from most recent year
  group_by(GEOID) %>%
  mutate(
    MHI_impute = if_else(Year == 2023, first(na.omit(MHI[-1]), default = NA_real_), NA_real_),
    MOE_impute = if_else(Year == 2023, first(na.omit(MOE[-1]), default = NA_real_), NA_real_),
    Year_impute = if_else(Year == 2023, first(Year[!is.na(MOE) & Year < 2023], default = NA_integer_), NA_integer_)
  ) %>%
  ungroup() %>%
  filter(Year == 2023) %>%
  drop_na(MOE_impute) %>%
  dplyr::select(GEOID,
                MHI_impute,
                MOE_impute,
                Year_impute)
#dplyr::select(GEOID, 7:9) #this may result in no MHI for some systems that otherwise would be 250000 every year? Determine who this applies to? Also need to add the adjustment for MOE to the 250,000
#271 candidates, only 121 get replaced

#### Adjust Tracts with missing MOE in 2023 ####

# Get a list of systems that are candidates for MHI replacement (MOE missing because MHI either 250,000+ or 2,500-, which is never the case, or MHI & MOE missing)
adjustmhi_tract <- tract_mhi %>% 
  filter(Year == 2023,
         MHI == 250000 & MOE == 0 | MHI == 2500 & MOE == 0 | is.na(MHI)) %>%
  dplyr::select(GEOID)  %>%
  mutate(flag_23 = 1) 
#n = 266 = x + x matches the number of highs + lows + NAs identified originally

# Identify the most recent non-NA MOE for each relevant tract
tract_mhi_imputed <- tract_mhi %>%
  left_join(adjustmhi_tract, by = "GEOID") %>%  # Keep only those missing MOE in 2023
  filter(flag_23 == 1) %>%  # Ensure only relevant block groups are checked
  mutate(MOE = ifelse(MOE == 0, NA, MOE)) %>% # Need to convert MHI back to NA so it can be combined with NA MHI imputation
  arrange(GEOID, desc(Year)) %>%  # Start from most recent year
  group_by(GEOID) %>%
  mutate(
    MHI_impute = if_else(Year == 2023, first(na.omit(MHI[-1]), default = NA_real_), NA_real_),
    MOE_impute = if_else(Year == 2023, first(na.omit(MOE[-1]), default = NA_real_), NA_real_),
    Year_impute = if_else(Year == 2023, first(Year[!is.na(MOE) & Year < 2023], default = NA_integer_), NA_integer_)
  ) %>%
  ungroup() %>%
  filter(Year == 2023) %>%
  drop_na(MOE_impute) %>%
  dplyr::select(GEOID, 6:8) #this may result in no MHI for some systems that otherwise would be 250000 every year? Figure out who this occurs for if you're missing any at the end

## INTERSECT AREAS & SABL+ ##################################################### 
#### Intersect ACS Census Block Group shapefile with SABL+ ####
intersect_bg <- st_intersection(blockgroup23 %>% left_join(blockgroup_mhi_imputed), sablplus) %>% #I think initially this was just blockgroup_mhi?
  mutate(wsbg_intersect_area = st_area(geometry),
         ws_overlap_bg_percent = as.numeric(wsbg_intersect_area)/as.numeric(ws_area), #proportion of water system area taken up by each block group
         bg_overlap_ws_percent = as.numeric(wsbg_intersect_area)/as.numeric(bg_area), #proportion of block group area taken up by each water system
         MOE_adjusted = ifelse(POPULATION < 501 & MOE > 15000 | POPULATION < 501 & MOE == 0 & MHI == 250000, 15000, MOE),
         MOE_adjusted = ifelse(POPULATION > 500 & MOE > 7500 | POPULATION > 500 & MOE == 0 & MHI == 250000, 7500, MOE_adjusted),
         MOE_adjusted = ifelse(is.na(MOE), 0, MOE_adjusted),
         MHI_wMOE = MHI - MOE_adjusted)
sum(is.na(intersect_bg$MHI_wMOE)) #1155 SOMEHOW IT'S SO MUCH MORE - 2469 are missing now on 4/2/25 (maybe this is for affordability)


# With imputed MHI, intersect shapefile
intersect_bg_impute <- st_intersection(blockgroup23 %>% left_join(blockgroup_mhi_imputed), sablplus) %>% 
  mutate(MHI = ifelse(!is.na(MHI_impute), MHI_impute, MHI),
         MOE = ifelse(!is.na(MOE_impute), MOE_impute, MOE),
         wsbg_intersect_area = st_area(geometry),
         ws_overlap_bg_percent = as.numeric(wsbg_intersect_area)/as.numeric(ws_area), #proportion of water system area taken up by each block group
         bg_overlap_ws_percent = as.numeric(wsbg_intersect_area)/as.numeric(bg_area), #proportion of block group area taken up by each water system
         MOE_adjusted = ifelse(POPULATION < 501 & MOE > 15000 | POPULATION < 501 & MOE == 0 & MHI == 250000, 15000, MOE),
         MOE_adjusted = ifelse(POPULATION > 500 & MOE > 7500 | POPULATION > 500 & MOE == 0 & MHI == 250000, 7500, MOE_adjusted),
         MOE_adjusted = ifelse(is.na(MOE), 0, MOE_adjusted),
         MHI_wMOE = MHI - MOE_adjusted) 
sum(is.na(intersect_bg_impute$MHI_wMOE)) #234 (fixes 921) (533 - fixes 1936)

# For reference, count the number of block groups that intersect each water system and how many of them have a known MHI
count_intersecting_bg <- intersect_bg %>% 
  as.data.frame() %>% 
  dplyr::select(SABL_PWSID, ws_overlap_bg_percent, MHI_wMOE) %>%
  mutate(countws = 1, na_flag = ifelse(is.na(MHI_wMOE), 1, 0)) %>%
  group_by(SABL_PWSID) %>% 
  summarise(
    num_bg_intersecting = sum(countws, na.rm = TRUE),
    num_bg_na_MHI_wMOE = sum(na_flag, na.rm = TRUE) 
  ) #3041 water systems, which matches inventory

# For reference, count the number of block groups that intersect each water system and how many of them have a known MHI
count_intersecting_bg_impute <- intersect_bg_impute %>% 
  as.data.frame() %>% 
  dplyr::select(SABL_PWSID, ws_overlap_bg_percent, MHI_wMOE) %>%
  mutate(countws = 1, na_flag = ifelse(is.na(MHI_wMOE), 1, 0)) %>%
  group_by(SABL_PWSID) %>% 
  summarise(
    num_bg_intersecting = sum(countws, na.rm = TRUE),
    num_bg_na_MHI_wMOE_impute = sum(na_flag, na.rm = TRUE) 
  ) #3041 water systems, which matches inventory

#### Intersect ACS Census Designated Place shapefile with SABL+ ####
intersect_cdp <- st_intersection(cdp23, sablplus) %>% 
  mutate(wscdp_intersect_area = st_area(geometry),
         ws_overlap_cdp_percent = as.numeric(wscdp_intersect_area)/as.numeric(ws_area), #proportion of water system area taken up by each block group
         cdp_overlap_ws_percent = as.numeric(wscdp_intersect_area)/as.numeric(cdp_area), #proportion of block group area taken up by each water system
         MOE_adjusted = ifelse(POPULATION < 501 & MOE > 15000 | POPULATION < 501 & MOE == 0 & MHI == 250000, 15000, MOE),
         MOE_adjusted = ifelse(POPULATION > 500 & MOE > 7500 | POPULATION > 500 & MOE == 0 & MHI == 250000, 7500, MOE_adjusted),
         MOE_adjusted = ifelse(is.na(MOE), 0, MOE_adjusted),
         MHI_wMOE = MHI - MOE_adjusted) 
sum(is.na(intersect_cdp$MHI_wMOE)) #fixes 103 (207 NA --> 104)

# With imputed MHI, intersect shapefile
intersect_cdp_impute <- st_intersection(cdp23 %>% left_join(cdp_mhi_imputed), sablplus) %>% 
  mutate(MHI = ifelse(!is.na(MHI_impute), MHI_impute, MHI),
         MOE = ifelse(!is.na(MOE_impute), MOE_impute, MOE),
         wscdp_intersect_area = st_area(geometry),
         ws_overlap_cdp_percent = as.numeric(wscdp_intersect_area)/as.numeric(ws_area), #proportion of water system area taken up by each block group
         cdp_overlap_ws_percent = as.numeric(wscdp_intersect_area)/as.numeric(cdp_area), #proportion of block group area taken up by each water system
         MOE_adjusted = ifelse(POPULATION < 501 & MOE > 15000 | POPULATION < 501 & MOE == 0 & MHI == 250000, 15000, MOE),
         MOE_adjusted = ifelse(POPULATION > 500 & MOE > 7500 | POPULATION > 500 & MOE == 0 & MHI == 250000, 7500, MOE_adjusted),
         MOE_adjusted = ifelse(is.na(MOE), 0, MOE_adjusted),
         MHI_wMOE = MHI - MOE_adjusted) 
sum(is.na(intersect_cdp_impute$MHI_wMOE)) #fixes 103 (207 NA --> 104)


# For reference, count the number of CDPs that intersect each water system and how many of them have a known MHI
count_intersecting_cdp <- intersect_cdp %>% 
  as.data.frame() %>% 
  dplyr::select(SABL_PWSID, ws_overlap_cdp_percent, MHI_wMOE) %>%
  mutate(countws = 1, na_flag = ifelse(is.na(MHI_wMOE), 1, 0)) %>%
  group_by(SABL_PWSID) %>% 
  summarise(
    num_cdp_intersecting = sum(countws, na.rm = TRUE),
    num_cdp_na_MHI_wMOE = sum(na_flag, na.rm = TRUE) 
  ) #3041 water systems, which matches dfainventory

count_intersecting_cdp_impute <- intersect_cdp_impute %>% 
  as.data.frame() %>% 
  dplyr::select(SABL_PWSID, ws_overlap_cdp_percent, MHI_wMOE) %>%
  mutate(countws = 1, na_flag = ifelse(is.na(MHI_wMOE), 1, 0)) %>%
  group_by(SABL_PWSID) %>% 
  summarise(
    num_cdp_intersecting = sum(countws, na.rm = TRUE),
    num_cdp_na_MHI_wMOE_impute = sum(na_flag, na.rm = TRUE) 
  ) #3041 water systems, which matches dfainventory

#### Intersect ACS Census Tract shapefile with SABL+ ####

intersect_tract <- st_intersection(tract23, sablplus) %>% 
  mutate(wstract_intersect_area = st_area(geometry),
         ws_overlap_tract_percent = as.numeric(wstract_intersect_area)/as.numeric(ws_area), #proportion of water system area taken up by each block group
         tract_overlap_ws_percent = as.numeric(wstract_intersect_area)/as.numeric(tract_area), #proportion of block group area taken up by each water system
         MOE_adjusted = ifelse(POPULATION < 501 & MOE > 15000 | POPULATION < 501 & MOE == 0 & MHI == 250000, 15000, MOE),
         MOE_adjusted = ifelse(POPULATION > 500 & MOE > 7500 | POPULATION > 500 & MOE == 0 & MHI == 250000, 7500, MOE_adjusted),
         MOE_adjusted = ifelse(is.na(MOE), 0, MOE_adjusted),
         MHI_wMOE = MHI - MOE_adjusted) 
sum(is.na(intersect_tract$MHI_wMOE))

intersect_tract_impute <- st_intersection(tract23 %>% left_join(tract_mhi_imputed), sablplus) %>% 
  mutate(MHI = ifelse(!is.na(MHI_impute), MHI_impute, MHI),
         MOE = ifelse(!is.na(MOE_impute), MOE_impute, MOE),
         wstract_intersect_area = st_area(geometry),
         ws_overlap_tract_percent = as.numeric(wstract_intersect_area)/as.numeric(ws_area), #proportion of water system area taken up by each block group
         tract_overlap_ws_percent = as.numeric(wstract_intersect_area)/as.numeric(tract_area), #proportion of block group area taken up by each water system
         MOE_adjusted = ifelse(POPULATION < 501 & MOE > 15000 | POPULATION < 501 & MOE == 0 & MHI == 250000, 15000, MOE),
         MOE_adjusted = ifelse(POPULATION > 500 & MOE > 7500 | POPULATION > 500 & MOE == 0 & MHI == 250000, 7500, MOE_adjusted),
         MOE_adjusted = ifelse(is.na(MOE), 0, MOE_adjusted),
         MHI_wMOE = MHI - MOE_adjusted) 
sum(is.na(intersect_tract$MHI_wMOE)) #fixes 6 (143 NA --> 136)

# For reference, count the number of tracts that intersect each water system and how many of them have a known MHI
count_intersecting_tract <- intersect_tract %>% 
  as.data.frame() %>% 
  dplyr::select(SABL_PWSID, ws_overlap_tract_percent, MHI_wMOE) %>%
  mutate(countws = 1, na_flag = ifelse(is.na(MHI_wMOE), 1, 0)) %>%
  group_by(SABL_PWSID) %>% 
  summarise(
    num_tract_intersecting = sum(countws, na.rm = TRUE),
    num_tract_na_MHI_wMOE = sum(na_flag, na.rm = TRUE) 
  ) #2034 water systems, which matches dfainventory

count_intersecting_tract_impute <- intersect_tract_impute %>% 
  as.data.frame() %>% 
  dplyr::select(SABL_PWSID, ws_overlap_tract_percent, MHI_wMOE) %>%
  mutate(countws = 1, na_flag = ifelse(is.na(MHI_wMOE), 1, 0)) %>%
  group_by(SABL_PWSID) %>% 
  summarise(
    num_tract_intersecting = sum(countws, na.rm = TRUE),
    num_tract_na_MHI_wMOE_impute = sum(na_flag, na.rm = TRUE) 
  ) #2034 water systems, which matches dfainventory

## CALCULATE BLOCK GROUP AREA-WEIGHTED MHI #####################################
#### Calculate the % of WS covered by block groups with a known MHI ####

# Find the non NA area of block groups intersecting each WS ("known MHI area")
bg_wmhi <- intersect_bg %>% 
  dplyr::select(SABL_PWSID, wsbg_intersect_area, MHI_wMOE) %>%
  group_by(SABL_PWSID) %>%
  filter(!is.na(MHI_wMOE)) %>% 
  summarise(nonNA_area = sum(wsbg_intersect_area, na.rm = T)) 

# Find the percentage of "known MHI area" made up by each non-NA intersecting block group
intersect_bg_known_mhi <- intersect_bg %>% 
  left_join(bg_wmhi %>% 
              as.data.frame() %>% 
              dplyr::select(!geometry)) %>%
  mutate(ws_overlap_nonNAbg_percent = as.numeric(wsbg_intersect_area)/as.numeric(nonNA_area),
         MHI_BG23_AreaWt = ws_overlap_nonNAbg_percent*MHI_wMOE) #if you keep some key things from this you can just join it and you don't have to do all this complicated other stuff

# IMPUTATION - Find the non NA area of block groups intersecting each WS ("known MHI area")
bg_wmhi_imp <- intersect_bg_impute %>% 
  dplyr::select(SABL_PWSID, wsbg_intersect_area, MHI_wMOE) %>%
  group_by(SABL_PWSID) %>%
  filter(!is.na(MHI_wMOE)) %>% 
  summarise(nonNA_area = sum(wsbg_intersect_area, na.rm = T)) #1974 

# IMPUTATION Find the percentage of "known MHI area" made up by each non-NA intersecting block group
intersect_bg_known_mhi_impute <- intersect_bg_impute %>% 
  left_join(bg_wmhi_imp %>% 
              as.data.frame() %>% 
              dplyr::select(!geometry)) %>%
  mutate(ws_overlap_nonNAbg_percent = as.numeric(wsbg_intersect_area)/as.numeric(nonNA_area),
         MHI_BG23_AreaWt_impute = ws_overlap_nonNAbg_percent*MHI_wMOE) #if you keep some key things from this you can just join it and you don't have to do all this complicated other stuff


impute_flag_bg <- intersect_bg_known_mhi_impute %>% as.data.frame() %>% 
  dplyr::select(SABL_PWSID, GEOID, Year_impute) %>%
  filter(!is.na(Year_impute)) 

impute_flag_avg_bg <- intersect_bg_known_mhi_impute %>% 
  as.data.frame() %>% 
  dplyr::select(SABL_PWSID, GEOID, Year_impute) %>%
  filter(!is.na(Year_impute)) %>%
  group_by(SABL_PWSID) %>% 
  summarize(Avg_Year_impute_BG = mean(Year_impute, na.rm = TRUE), 
            Num_Imputed_BG = n(),  # Count of imputed block groups
            .groups = "drop")

#### Calculate the Block Group Area-Weighted MHI for 2023 ####

# First, only calculate the MHI using block group data without imputation
MHI_BLOCKGROUP <- intersect_bg_known_mhi %>%
  group_by(SABL_PWSID) %>%
  summarise(MHI_BG23_AreaWt_WS = sum(MHI_BG23_AreaWt, na.rm = TRUE)) %>%
  mutate(MHI_BG23_AreaWt_WS = ifelse(MHI_BG23_AreaWt_WS == 0, NA, MHI_BG23_AreaWt_WS)) %>% 
  as.data.frame() %>%
  dplyr::select(!geometry) %>% 
  left_join(count_intersecting_bg %>% dplyr::select(1:2))

# Now, impute 
MHI_BLOCKGROUP_IMPUTE <- intersect_bg_known_mhi_impute %>%
  group_by(SABL_PWSID) %>%
  summarise(MHI_BG23_AreaWt_WS_impute = sum(MHI_BG23_AreaWt_impute, na.rm = TRUE)) %>%
  mutate(MHI_BG23_AreaWt_WS_impute = ifelse(MHI_BG23_AreaWt_WS_impute == 0, NA, MHI_BG23_AreaWt_WS_impute)) %>% 
  as.data.frame() %>%
  dplyr::select(!geometry) %>% 
  left_join(count_intersecting_bg_impute %>% dplyr::select(1:2)) #CHANGE THIS - make the names different 


## CALCULATE CDP AREA-WEIGHTED MHI #############################################
#### Calculate the % of WS covered by CDPs with a known MHI ####

# Find the non NA area of CDPs intersecting each WS ("known MHI area")
cdp_wmhi <- intersect_cdp %>% 
  dplyr::select(SABL_PWSID, wscdp_intersect_area, MHI_wMOE) %>%
  group_by(SABL_PWSID) %>%
  filter(!is.na(MHI_wMOE)) %>% 
  summarise(nonNA_area = sum(wscdp_intersect_area, na.rm = T)) 

# Find the percentage of "known MHI area" made up by each non-NA intersecting CDPs
intersect_cdp_known_mhi <- intersect_cdp %>% 
  left_join(cdp_wmhi %>% 
              as.data.frame() %>% 
              dplyr::select(!geometry)) %>%
  mutate(ws_overlap_nonNAcdp_percent = as.numeric(wscdp_intersect_area)/as.numeric(nonNA_area),
         MHI_CDP23_AreaWt = ws_overlap_nonNAcdp_percent*MHI_wMOE) #if you keep some key things from this you can just join it and you don't have to do all this complicated other stuff

# IMPUTATION - Find the non NA area of CDPs intersecting each WS ("known MHI area")
cdp_wmhi_imp <- intersect_cdp_impute %>% 
  dplyr::select(SABL_PWSID, wscdp_intersect_area, MHI_wMOE) %>%
  group_by(SABL_PWSID) %>%
  filter(!is.na(MHI_wMOE)) %>% 
  summarise(nonNA_area = sum(wscdp_intersect_area, na.rm = T)) 

# IMPUTATION Find the percentage of "known MHI area" made up by each non-NA intersecting CDPs
intersect_cdp_known_mhi_impute <- intersect_cdp_impute %>% 
  left_join(cdp_wmhi_imp %>% 
              as.data.frame() %>% 
              dplyr::select(!geometry)) %>%
  mutate(ws_overlap_nonNAcdp_percent = as.numeric(wscdp_intersect_area)/as.numeric(nonNA_area),
         MHI_CDP23_AreaWt_impute = ws_overlap_nonNAcdp_percent*MHI_wMOE) #if you keep some key things from this you can just join it and you don't have to do all this complicated other stuff

impute_flag_cdp <- intersect_cdp_known_mhi_impute %>% as.data.frame() %>% 
  dplyr::select(SABL_PWSID, GEOID, Year_impute) %>%
  filter(!is.na(Year_impute)) 

impute_flag_avg_cdp <- intersect_cdp_known_mhi_impute %>% 
  as.data.frame() %>% 
  dplyr::select(SABL_PWSID, GEOID, Year_impute) %>%
  filter(!is.na(Year_impute)) %>%
  group_by(SABL_PWSID) %>% 
  summarize(Avg_Year_impute_CDP = mean(Year_impute, na.rm = TRUE), 
            Num_Imputed_CDP = n(),  # Count of imputed block groups
            .groups = "drop")

#### Calculate the Census Designated Place Area-Weighted MHI for 2023 ####

# First, only calculate the MHI using CDP data without imputation
MHI_CDP <- intersect_cdp_known_mhi %>%
  group_by(SABL_PWSID) %>%
  summarise(MHI_CDP23_AreaWt_WS = sum(MHI_CDP23_AreaWt, na.rm = TRUE)) %>%
  mutate(MHI_CDP23_AreaWt_WS = ifelse(MHI_CDP23_AreaWt_WS == 0, NA, MHI_CDP23_AreaWt_WS)) %>% 
  as.data.frame() %>%
  dplyr::select(!geometry) %>% 
  left_join(count_intersecting_cdp %>% dplyr::select(1:2))

# Now, impute 
MHI_CDP_IMPUTE <- intersect_cdp_known_mhi_impute %>%
  group_by(SABL_PWSID) %>%
  summarise(MHI_CDP23_AreaWt_WS_impute = sum(MHI_CDP23_AreaWt_impute, na.rm = TRUE)) %>%
  mutate(MHI_CDP23_AreaWt_WS_impute = ifelse(MHI_CDP23_AreaWt_WS_impute == 0, NA, MHI_CDP23_AreaWt_WS_impute)) %>% 
  as.data.frame() %>%
  dplyr::select(!geometry) %>% 
  left_join(count_intersecting_cdp_impute %>% dplyr::select(1:2)) 
sum(is.na(MHI_CDP_IMPUTE$MHI_CDP23_AreaWt_WS_impute)) #76
sum(is.na(MHI_CDP$MHI_CDP23_AreaWt_WS)) #157

## CALCULATE CENSUS TRACT AREA-WEIGHTED MHI ####################################
#### Calculate the % of WS covered by tracts with a known MHI ####

# Find the non NA area of tracts intersecting each WS ("known MHI area")
tract_wmhi <- intersect_tract %>% 
  dplyr::select(SABL_PWSID, wstract_intersect_area, MHI_wMOE) %>%
  group_by(SABL_PWSID) %>%
  filter(!is.na(MHI_wMOE)) %>% 
  summarise(nonNA_area = sum(wstract_intersect_area, na.rm = T)) 

# Find the percentage of "known MHI area" made up by each non-NA intersecting tract
intersect_tract_known_mhi <- intersect_tract %>% 
  left_join(tract_wmhi %>% 
              as.data.frame() %>% 
              dplyr::select(!geometry)) %>%
  mutate(ws_overlap_nonNAtract_percent = as.numeric(wstract_intersect_area)/as.numeric(nonNA_area),
         MHI_TRACT23_AreaWt = ws_overlap_nonNAtract_percent*MHI_wMOE) #if you keep some key things from this you can just join it and you don't have to do all this complicated other stuff

# IMPUTATION - Find the non NA area of tracts intersecting each WS ("known MHI area")
tract_wmhi_imp <- intersect_tract_impute %>% 
  dplyr::select(SABL_PWSID, wstract_intersect_area, MHI_wMOE) %>%
  group_by(SABL_PWSID) %>%
  filter(!is.na(MHI_wMOE)) %>% 
  summarise(nonNA_area = sum(wstract_intersect_area, na.rm = T)) #3009 

# IMPUTATION Find the percentage of "known MHI area" made up by each non-NA intersecting block group
intersect_tract_known_mhi_impute <- intersect_tract_impute %>% 
  left_join(tract_wmhi_imp %>% 
              as.data.frame() %>% 
              dplyr::select(!geometry)) %>%
  mutate(ws_overlap_nonNAtract_percent = as.numeric(wstract_intersect_area)/as.numeric(nonNA_area),
         MHI_TRACT23_AreaWt_impute = ws_overlap_nonNAtract_percent*MHI_wMOE) #if you keep some key things from this you can just join it and you don't have to do all this complicated other stuff

impute_flag_tract <- intersect_tract_known_mhi_impute %>% as.data.frame() %>% 
  dplyr::select(SABL_PWSID, GEOID, Year_impute) %>%
  filter(!is.na(Year_impute)) 

impute_flag_avg_tract <- intersect_tract_known_mhi_impute %>% 
  as.data.frame() %>% 
  dplyr::select(SABL_PWSID, GEOID, Year_impute) %>%
  filter(!is.na(Year_impute)) %>%
  group_by(SABL_PWSID) %>% 
  summarize(Avg_Year_impute_TRACT = mean(Year_impute, na.rm = TRUE), 
            Num_Imputed_TRACT = n(),  # Count of imputed block groups
            .groups = "drop")

#### Calculate the Tract Area-Weighted MHI for 2023 ####

# First, only calculate the MHI using tract data without imputation
MHI_TRACT <- intersect_tract_known_mhi %>%
  group_by(SABL_PWSID) %>%
  summarise(MHI_TRACT23_AreaWt_WS = sum(MHI_TRACT23_AreaWt, na.rm = TRUE)) %>%
  mutate(MHI_TRACT23_AreaWt_WS = ifelse(MHI_TRACT23_AreaWt_WS == 0, NA, MHI_TRACT23_AreaWt_WS)) %>% 
  as.data.frame() %>%
  dplyr::select(!geometry) %>% 
  left_join(count_intersecting_tract %>% dplyr::select(1:2)) %>% rename(PWSID = SABL_PWSID)

# Then calculate the MHI using tract data w/ imputation
MHI_TRACT_IMPUTE <- intersect_tract_known_mhi_impute %>%
  group_by(SABL_PWSID) %>%
  summarise(MHI_TRACT23_AreaWt_WS_impute = sum(MHI_TRACT23_AreaWt_impute, na.rm = TRUE)) %>%
  mutate(MHI_TRACT23_AreaWt_WS_impute = ifelse(MHI_TRACT23_AreaWt_WS_impute == 0, NA, MHI_TRACT23_AreaWt_WS_impute)) %>% 
  as.data.frame() %>%
  dplyr::select(!geometry) %>% 
  left_join(count_intersecting_tract_impute %>% dplyr::select(1:2)) %>% rename(PWSID = SABL_PWSID)

sum(is.na(MHI_TRACT_IMPUTE$MHI_TRACT23_AreaWt_WS)) #32
sum(is.na(MHI_TRACT$MHI_TRACT23_AreaWt_WS_impute)) #0 (use these #s to demonstrate how your method improves coverage)




## JOIN MHI DATA TOGETHER AND MAKE FINAL SELECTIONS ############################
#### Clean ACS data for specific data points that come from single area WS ####
bg_area_single <- intersect_bg_known_mhi_impute %>%
  group_by(SABL_PWSID) %>%
  summarise(MHI_BG23_AreaWt_WS_impute = sum(MHI_BG23_AreaWt_impute, na.rm = TRUE)) %>%
  mutate(MHI_BG23_AreaWt_WS_impute = ifelse(MHI_BG23_AreaWt_WS_impute == 0, NA, MHI_BG23_AreaWt_WS_impute)) %>%
  as.data.frame() %>%
  dplyr::select(!geometry) %>%
  left_join(count_intersecting_bg_impute %>% dplyr::select(1:2)) %>% 
  filter(num_bg_intersecting == 1) %>%
  left_join(intersect_bg_known_mhi_impute %>% as.data.frame() %>% dplyr::select(SABL_PWSID, bg_overlap_ws_percent)) %>% 
  rename(PWSID = SABL_PWSID) %>% 
  dplyr::select(PWSID, bg_overlap_ws_percent)

tract_area_single <- intersect_tract_known_mhi_impute %>%
  group_by(SABL_PWSID) %>%
  summarise(MHI_TRACT23_AreaWt_WS_impute = 
              sum(MHI_TRACT23_AreaWt_impute, na.rm = TRUE)) %>%
  mutate(MHI_TRACT23_AreaWt_WS_impute = 
           ifelse(MHI_TRACT23_AreaWt_WS_impute == 0, NA, MHI_TRACT23_AreaWt_WS_impute)) %>%
  as.data.frame() %>%
  dplyr::select(!geometry) %>%
  left_join(count_intersecting_tract_impute %>% dplyr::select(1:2)) %>% 
  filter(num_tract_intersecting == 1) %>%
  left_join(intersect_tract_known_mhi_impute %>% as.data.frame() %>% dplyr::select(SABL_PWSID, tract_overlap_ws_percent)) %>% 
  dplyr::select(PWSID = SABL_PWSID, tract_overlap_ws_percent)

cdp_area_single <- intersect_cdp_known_mhi_impute %>%
  group_by(SABL_PWSID) %>%
  summarise(MHI_CDP23_AreaWt_WS_impute = sum(MHI_CDP23_AreaWt_impute, na.rm = TRUE)) %>%
  mutate(MHI_CDP23_AreaWt_WS_impute = ifelse(MHI_CDP23_AreaWt_WS_impute == 0, NA, MHI_CDP23_AreaWt_WS_impute)) %>%
  as.data.frame() %>%
  dplyr::select(!geometry) %>%
  left_join(count_intersecting_cdp_impute %>% dplyr::select(1:2)) %>% filter(num_cdp_intersecting == 1) %>%
  left_join(intersect_cdp_known_mhi_impute %>% as.data.frame() %>% dplyr::select(SABL_PWSID, cdp_overlap_ws_percent)) %>% 
  dplyr::select(PWSID = SABL_PWSID, cdp_overlap_ws_percent)

#### Join data ####

inventory <- sablplus %>% 
  as.data.frame() %>%
  dplyr::select(SABL_PWSID, COUNTY, REGULATING_AGENCY, FED_TYPE, SERVICE_CONNECTIONS, POPULATION)
mhi <- inventory %>%
  rename(PWSID = SABL_PWSID) %>%
  left_join(dfa_mhi) %>%
  left_join(MHI_BLOCKGROUP_IMPUTE %>% 
              rename(PWSID = SABL_PWSID)) %>%
  left_join(MHI_CDP_IMPUTE %>% 
              rename(PWSID = SABL_PWSID)) %>%
  left_join(MHI_TRACT_IMPUTE) %>%
  mutate(MHI = dfa_mhi,
         MHI_SOURCE = ifelse(!is.na(MHI), paste0("DFA Survey ", dfa_survey_year), NA))

#### Find the area of best fit for each WS ####

mhi_bestfit <- mhi %>% 
  left_join(impute_flag_avg_bg %>% rename(PWSID = 1)) %>%
  left_join(impute_flag_avg_cdp %>% rename(PWSID = 1)) %>%
  left_join(impute_flag_avg_tract %>% rename(PWSID = 1)) %>%
  left_join(bg_area_single) %>%
  left_join(cdp_area_single) %>%
  left_join(tract_area_single)

# Function to assign scores and select best fit area
bestfit <- mhi_bestfit %>%
  mutate(
    # Create missing indicators
    Block_Group_Missing = ifelse(is.na(MHI_BG23_AreaWt_WS_impute), 1, 0),
    Tract_Missing = ifelse(is.na(MHI_TRACT23_AreaWt_WS_impute), 1, 0),
    CDP_Missing = ifelse(is.na(MHI_CDP23_AreaWt_WS_impute), 1, 0),
    
    # Continuous imputation penalty: More recent years (closer to 2022) get smaller penalties
    blockgroup_penalty = -((2024 - Avg_Year_impute_BG) * 0.5) - (Num_Imputed_BG * 0.2),
    tract_penalty = -((2024 - Avg_Year_impute_TRACT) * 0.5) - (Num_Imputed_TRACT * 0.2),
    place_penalty = -((2024 - Avg_Year_impute_CDP) * 0.5) - (Num_Imputed_CDP * 0.2),
    
    # Compute scores (only for non-missing data)
    blockgroup_score = ifelse(Block_Group_Missing == 1, -Inf,
                              2 + bg_overlap_ws_percent + blockgroup_penalty - (num_bg_intersecting * 0.1)),
    
    tract_score = ifelse(Tract_Missing == 1, -Inf,
                         2 + tract_overlap_ws_percent + tract_penalty - (num_tract_intersecting * 0.1)),
    
    place_score = ifelse(CDP_Missing == 1, -Inf,
                         2 + cdp_overlap_ws_percent + place_penalty - (num_cdp_intersecting * 0.1)),
    
    # Select best fit area, prioritizing non-missing MHI values
    best_fit_area = case_when(
      Block_Group_Missing == 0 ~ "BLOCKGROUP",
      Tract_Missing == 0 ~ "TRACT",
      CDP_Missing == 0 ~ "PLACE",
      TRUE ~ "MISSING" # Fallback in case all are missing
    ),
    
    # If multiple levels have MHI, use scores to determine the best
    best_fit_area = case_when(
      Block_Group_Missing == 0 & Tract_Missing == 0 & CDP_Missing == 0 ~ 
        case_when(blockgroup_score >= tract_score & blockgroup_score >= place_score ~ "BLOCKGROUP",
                  tract_score >= blockgroup_score & tract_score >= place_score ~ "TRACT",
                  TRUE ~ "PLACE"),
      Block_Group_Missing == 0 & Tract_Missing == 0 ~ 
        ifelse(blockgroup_score >= tract_score, "BLOCKGROUP", "TRACT"),
      Block_Group_Missing == 0 & CDP_Missing == 0 ~ 
        ifelse(blockgroup_score >= place_score, "BLOCKGROUP", "PLACE"),
      Tract_Missing == 0 & CDP_Missing == 0 ~ 
        ifelse(tract_score >= place_score, "TRACT", "PLACE"),
      TRUE ~ best_fit_area # Keep initial assignment
    )
  ) %>% dplyr::select(1,best_fit_area)


#### Select Final MHI ####
mhi_final <- inventory %>%
  rename(PWSID = SABL_PWSID) %>%
  left_join(bg_area_single) %>%
  left_join(cdp_area_single) %>%
  left_join(tract_area_single) %>% 
  left_join(dfa_mhi) %>%
  left_join(MHI_BLOCKGROUP_IMPUTE %>% rename(PWSID = SABL_PWSID)) %>%
  left_join(MHI_CDP_IMPUTE %>% rename(PWSID = SABL_PWSID)) %>%
  left_join(MHI_TRACT_IMPUTE) %>%
  left_join(bestfit) %>%
  
  # Start by assigning MHI from DFA survey if available
  mutate(
    MHI = ifelse(!is.na(dfa_mhi), dfa_mhi, NA_real_),
    MHI_SOURCE = ifelse(!is.na(dfa_mhi), paste0("DFA Survey ", dfa_survey_year), NA_character_)
  ) %>%
  
  # If MHI is still NA, assign based on best-fit source and threshold
  mutate(
    MHI = case_when(
      is.na(MHI) & best_fit_area == "BLOCKGROUP" & !is.na(MHI_BG23_AreaWt_WS_impute) & MHI_BG23_AreaWt_WS_impute < 77067.2 ~ MHI_BG23_AreaWt_WS_impute,
      is.na(MHI) & best_fit_area == "TRACT" & !is.na(MHI_TRACT23_AreaWt_WS_impute) & MHI_TRACT23_AreaWt_WS_impute < 77067.2 ~ MHI_TRACT23_AreaWt_WS_impute,
      is.na(MHI) & best_fit_area == "PLACE" & !is.na(MHI_CDP23_AreaWt_WS_impute) & MHI_CDP23_AreaWt_WS_impute < 77067.2 ~ MHI_CDP23_AreaWt_WS_impute,
      TRUE ~ MHI  # Preserve already assigned MHI
    )
  ) %>%
  
  # If MHI is still NA, fallback to the minimum of available estimates
  mutate(
    MHI = ifelse(
      is.na(MHI),
      pmin(MHI_BG23_AreaWt_WS_impute, MHI_CDP23_AreaWt_WS_impute, MHI_TRACT23_AreaWt_WS_impute, na.rm = TRUE),
      MHI
    )
  ) %>%
  
  # Now assign MHI_SOURCE only where it is still NA
  mutate(
    MHI_SOURCE = case_when(
      is.na(MHI_SOURCE) & MHI == MHI_BG23_AreaWt_WS_impute & num_bg_intersecting == 1 ~ "SINGLE BLOCK GROUP",
      is.na(MHI_SOURCE) & MHI == MHI_BG23_AreaWt_WS_impute & num_bg_intersecting > 1 ~ "BLOCK GROUP AREA WEIGHTED",
      is.na(MHI_SOURCE) & MHI == MHI_CDP23_AreaWt_WS_impute & num_cdp_intersecting == 1 ~ "SINGLE CDP",
      is.na(MHI_SOURCE) & MHI == MHI_CDP23_AreaWt_WS_impute & num_cdp_intersecting > 1 ~ "CDP AREA WEIGHTED",
      is.na(MHI_SOURCE) & MHI == MHI_TRACT23_AreaWt_WS_impute & num_tract_intersecting == 1 ~ "SINGLE CENSUS TRACT",
      is.na(MHI_SOURCE) & MHI == MHI_TRACT23_AreaWt_WS_impute & num_tract_intersecting > 1 ~ "CENSUS TRACT AREA WEIGHTED",
      TRUE ~ MHI_SOURCE
    )
  ) %>%
  mutate(MHI = ifelse(PWSID == "CA5010039", 153241, MHI),
         MHI_SOURCE = ifelse(PWSID == "CA5010039", "SINGLE CDP", MHI_SOURCE)) %>%
  
  # Assign DAC status based on thresholds
  mutate(
    `DAC STATUS` = case_when(
      MHI < (96334 * 0.6) ~ "SDAC",
      MHI < (96334 * 0.8) ~ "DAC",
      is.na(MHI) ~ NA_character_,
      TRUE ~ "Non-DAC"
    )
  )

MHI_CLEAN <- mhi_final %>%
  left_join(sablplus %>% as.data.frame() %>% dplyr::select(PWSID = SABL_PWSID, NAME, BOUNDARY_TYPE)) %>% 
  dplyr::select(PWSID,
                NAME,
                COUNTY,
                REGULATING_AGENCY,
                FED_TYPE,
                SERVICE_CONNECTIONS,
                POPULATION,
                MHI,
                MHI_SOURCE,
                `DAC STATUS`,
                BOUNDARY_TYPE) 

#Check if there are NAs in any column 
colSums(is.na(MHI_CLEAN))



#### Write MHI data ####
# openxlsx::write.xlsx(MHI_CLEAN,"C:/Users/rskillman/OneDrive - Water Boards/Documents/Data Work/Master Data Files/Manual Data/Q2 2025_08.28.25/MHI_ManualData_09.10.25.xlsx") 