### CREATING THE SABL+ LAYER - MANUAL DATA Q2 2025 UPDATE ###
# Author: Rachel Skillman
# Date Created: 9/4/25
# Date Updated: 9/5/25
## SET-UP ######################################################################
#### Set Working Directory ####

setwd("C:/Users/rskillman/OneDrive - Water Boards/Documents/Data Work/SABL")

#### Load Libraries ####

#install.packages("")
library(ncf)
library(raster)
library(readxl)
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
library(odbc)

#### Load SABL Layer ####

#(downloaded 9/3/25 - save into new file with date ("SABL_09.03.25") and file name "sabl090325")
sabl_proj <- st_read("SABL_09.03.25/sabl090325.shp") %>% 
  st_transform("EPSG:3310") #reproject to California Teale Albers 

cat("The SABL dataset downloaded", format(Sys.Date(), "%m/%d/%Y"), "contains", nrow(sabl_proj), "rows and", length(unique(sabl_proj$SABL_PWSID)), "unique PWSIDs.\n") 
#The SABL dataset downloaded 09/03/2025 contains 5366 rows and 5344 unique PWSIDs.

crs_info <- st_crs(sabl_proj)
cat("The SABL dataset projection is", crs_info$Name, "- EPSG:", crs_info$epsg, "\n") 
#The SABL dataset projection is NAD83 / California Albers - EPSG: 3310 

#### Load Relevant Inventory ####

# Manual Data Refresh Q2 2025 
inventory <- read_excel("C:/Users/rskillman/OneDrive - Water Boards/Documents/Data Work/Inventory/2025-Q2 Refresh_Inventory.xlsx", 
                        sheet = "Comprehensive Inventory 2025-Q2") 
cat("The inventory dataset queried on 08/28/2025 contains", nrow(inventory), "rows.\n")
#The inventory dataset queried on 08/29/2025 contains 3291 rows.

# mDWW pull for confirmation
# mdww <- read_excel("C:/Users/rskillman/OneDrive - Water Boards/Documents/Data Work/Inventory/mDWW_06.11.25.xlsx", 
#                    skip = 1)

#### Open the Server Connection for SSMS Pulls ####

con <- dbConnect(odbc(),                 
                 Driver = "SQL Server",                 
                 Server = "reportmanager,1542",                 
                 Database = "ReportDB",                 
                 UID = Sys.getenv("CDM5_SQL_SERVER_USER"),
                 PWD = Sys.getenv("CDM5_SQL_SERVER_PASSWORD"),
                 TrustServerCertificate = "yes",
                 Port = 1542) #must be logged into SSMS and/or VPN

#### Load SDWIS data ####

# Query System List from SDWIS TINWSYS
# tinwsys <- dbGetQuery(con, "SELECT *
#                     FROM [ReportDB].[SDWIS].[TINWSYS]") %>%
#   mutate(NUMBER0 = str_trim(NUMBER0)) # Remove any trailing or leading white space

tinwsys <- read_excel("C:/Users/rskillman/OneDrive - Water Boards/Documents/Data Work/SDWIS Saved Data/SDWIS_TINWSYS_09.04.25.xlsx")

#cat("The SDWIS TINWSYS dataset queried", format(Sys.Date(), "%m/%d/%Y"), "contains", nrow(tinwsys), "water systems.\n")
#The SDWIS TINWSYS dataset queried 09/04/2025 contains 15961 water systems.

#openxlsx::write.xlsx(tinwsys, "C:/Users/rskillman/OneDrive - Water Boards/Documents/Data Work/SDWIS Saved Data/SDWIS_TINWSYS_09.04.25.xlsx")



# Query Facility ID and Type from SDWIS TINWSF
# tinwsf <- dbGetQuery(con, "SELECT *
#                     FROM [ReportDB].[SDWIS].[TINWSF]")

tinwsf <- read_excel("C:/Users/rskillman/OneDrive - Water Boards/Documents/Data Work/SDWIS Saved Data/SDWIS_TINWSF_09.04.25.xlsx")

#cat("The SDWIS TINWSF dataset queried", format(Sys.Date(), "%m/%d/%Y"), "contains", nrow(tinwsf), "water systems.\n")
#The SDWIS TINWSF dataset queried 09/04/2025 contains 70361 water systems

#openxlsx::write.xlsx(tinwsf, "C:/Users/rskillman/OneDrive - Water Boards/Documents/Data Work/SDWIS Saved Data/SDWIS_TINWSF_09.04.25.xlsx")



# Query Lat Lon and Facility ID from SDWIS TINLOC 
tinloc <- dbGetQuery(con, "SELECT *
                    FROM [ReportDB].[SDWIS].[TINLOC]")

#tinloc <- read_excel("C:/Users/rskillman/OneDrive - Water Boards/Documents/Data Work/SDWIS Saved Data/SDWIS_TINLOC_09.04.25.xlsx")

cat("The SDWIS TINLOC dataset queried", format(Sys.Date(), "%m/%d/%Y"), "contains", nrow(tinloc), "water system lat lons.\n")
#The SDWIS TINLOC dataset queried 09/04/2025 contains 52373 water system lat lons.
#The SDWIS TINLOC dataset queried 09/10/2025 contains 52375 water system lat lons.

#openxlsx::write.xlsx(tinloc, "C:/Users/rskillman/OneDrive - Water Boards/Documents/Data Work/SDWIS Saved Data/SDWIS_TINLOC_09.04.25.xlsx")


# Query from SDWIS TINLGENT
tinlgent <- dbGetQuery(con, "SELECT *
                    FROM [ReportDB].[SDWIS].[TINLGENT]")

#tinlgent <- read_excel("C:/Users/rskillman/OneDrive - Water Boards/Documents/Data Work/SDWIS Saved Data/SDWIS_ TINLGENT_06.11.25.xlsx")

cat("The SDWIS TINLGENT dataset queried", format(Sys.Date(), "%m/%d/%Y"), "contains", nrow(tinlgent), "records.\n")
#The SDWIS TINLGENT dataset queried 09/04/2025 contains 59220 records.
#The SDWIS TINLGENT dataset queried 09/10/2025 contains 59257 records.

#openxlsx::write.xlsx(tinlgent, "C:/Users/rskillman/OneDrive - Water Boards/Documents/Data Work/SDWIS Saved Data/SDWIS_TINLGENT_06.11.25.xlsx")


# Query from SDWIS TINRAA
tinraa <- dbGetQuery(con, "SELECT *
                    FROM [ReportDB].[SDWIS].[TINRAA]")

#tinraa <- read_excel("C:/Users/rskillman/OneDrive - Water Boards/Documents/Data Work/SDWIS Saved Data/SDWIS_ TINRAA_06.11.25.xlsx")

cat("The SDWIS TINRAA dataset queried", format(Sys.Date(), "%m/%d/%Y"), "contains", nrow(tinraa), "records.\n")
#The SDWIS TINRAA dataset queried 09/04/2025 contains 75409 records.
#The SDWIS TINRAA dataset queried 09/10/2025 contains 75413 records.

#openxlsx::write.xlsx(tinraa, "C:/Users/rskillman/OneDrive - Water Boards/Documents/Data Work/SDWIS Saved Data/SDWIS_TINRAA_06.11.25.xlsx")

# Query water system statistics from SAFER HR2W Inventory table
# hr2w <- dbGetQuery(con, "SELECT *
#                     FROM [ReportDB].[SAFER].[RPT_HR2W_INVENTORY]")
# 
# hr2w <- read_excel("C:/Users/rskillman/OneDrive - Water Boards/Documents/Data Work/SDWIS Saved Data/SAFER_RPT_HR2W_INVENTORY_09.04.25.xlsx")
# 
# cat("The SAFER HR2W Inventory dataset queried", format(Sys.Date(), "%m/%d/%Y"), "contains", nrow(hr2w), "water systems.\n")
#The SAFER HR2W Inventory dataset queried 09/04/2025 contains 3211 water systems.

#openxlsx::write.xlsx(hr2w, "C:/Users/rskillman/OneDrive - Water Boards/Documents/Data Work/SDWIS Saved Data/SAFER_RPT_HR2W_INVENTORY_09.04.25.xlsx")


# Query population from SDWIS TINPOPSV
# tinpopsv <- dbGetQuery(con, "SELECT *
#                        FROM [ReportDB].[SDWIS].[TINPOPSV]")

tinpopsv <- read_excel("C:/Users/rskillman/OneDrive - Water Boards/Documents/Data Work/SDWIS Saved Data/SDWIS_TINPOPSV_09.04.25.xlsx")
 
#cat("The SDWIS TINPOPSV dataset queried", format(Sys.Date(), "%m/%d/%Y"), "contains", nrow(tinpopsv), "water system population records.\n")
#The SDWIS TINPOPSV dataset queried 09/04/2025 contains 30419 water system population records.

#openxlsx::write.xlsx(tinpopsv, "C:/Users/rskillman/OneDrive - Water Boards/Documents/Data Work/SDWIS Saved Data/SDWIS_TINPOPSV_09.04.25.xlsx")



# Query service connections from SDWIS TINSCC
# tinscc <- dbGetQuery(con, "SELECT *
#                        FROM [ReportDB].[SDWIS].[TINSCC]")

tinscc <- read_excel("C:/Users/rskillman/OneDrive - Water Boards/Documents/Data Work/SDWIS Saved Data/SDWIS_TINSCC_09.04.25.xlsx")

#cat("The SDWIS TINSCC dataset queried", format(Sys.Date(), "%m/%d/%Y"), "contains", nrow(tinscc), "water system service connection records.\n")
#The SDWIS TINSCC dataset queried 09/04/2025 contains 67449 water system service connection records.

#openxlsx::write.xlsx(tinscc, "C:/Users/rskillman/OneDrive - Water Boards/Documents/Data Work/SDWIS Saved Data/SDWIS_TINSCC_09.04.25.xlsx")



# Query Violations from SDWIS TMNVIOL
# tmnviol <- dbGetQuery(con, "SELECT *
#                     FROM [ReportDB].[SDWIS].[TMNVIOL]") 

#tmnviol <- read_excel("C:/Users/rskillman/OneDrive - Water Boards/Documents/Data Work/SDWIS Saved Data/SDWIS_TMNVIOL_09.04.25.xlsx")

#cat("The SDWIS TMNVIOL dataset queried", format(Sys.Date(), "%m/%d/%Y"), "contains", nrow(tmnviol), "records.\n")
#The SDWIS TMNVIOL dataset queried 06/10/2025 contains 91180 records.

#openxlsx::write.xlsx(tmnviol, "C:/Users/rskillman/OneDrive - Water Boards/Documents/Data Work/SDWIS Saved Data/SDWIS_TMNVIOL_09.04.25.xlsx")



# Query GRP Violations from SDWIS TMNVGRP
# tmnvgrp <- dbGetQuery(con, "SELECT *
#                     FROM [ReportDB].[SDWIS].[TMNVGRP]") 

#tmnvgrp <- read_excel("C:/Users/rskillman/OneDrive - Water Boards/Documents/Data Work/SDWIS Saved Data/SDWIS_TMNVGRP_09.04.25.xlsx")

#cat("The SDWIS TMNVGRP dataset queried", format(Sys.Date(), "%m/%d/%Y"), "contains", nrow(tmnvgrp), "records.\n")
#The SDWIS TMNVGRP dataset queried 06/10/2025 contains 343 records.

#openxlsx::write.xlsx(tmnvgrp, "C:/Users/rskillman/OneDrive - Water Boards/Documents/Data Work/SDWIS Saved Data/SDWIS_TMNVGRP_09.04.25.xlsx")



# Query analytes rom SDWIS TSAANLYT
# tsaanlyt <- dbGetQuery(con, "SELECT *
#                     FROM [ReportDB].[SDWIS].[TSAANLYT]") 

#tsaanlyt <- read_excel("C:/Users/rskillman/OneDrive - Water Boards/Documents/Data Work/SDWIS Saved Data/SDWIS_TSAANLYT_09.04.25.xlsx")

#cat("The SDWIS TSAANLYT dataset queried", format(Sys.Date(), "%m/%d/%Y"), "contains", nrow(tsaanlyt), "records.\n")
#The SDWIS TSAANLYT dataset queried 06/10/2025 contains 15944 records.

#openxlsx::write.xlsx(tsaanlyt, "C:/Users/rskillman/OneDrive - Water Boards/Documents/Data Work/SDWIS Saved Data/SDWIS_TSAANLYT_09.04.25.xlsx")



# Query from SDWIS TSAANGRP
# tsaangrp <- dbGetQuery(con, "SELECT *
#                     FROM [ReportDB].[SDWIS].[TSAANGRP]") 

#tsaangrp <- read_excel("C:/Users/rskillman/OneDrive - Water Boards/Documents/Data Work/SDWIS Saved Data/SDWIS_TSAANGRP_09.04.25.xlsx")

#cat("The SDWIS TSAANGRP dataset queried", format(Sys.Date(), "%m/%d/%Y"), "contains", nrow(tsaangrp), "records.\n")
#The SDWIS TSAANGRP dataset queried 06/10/2025 contains 7 records.

#openxlsx::write.xlsx(tsaangrp, "C:/Users/rskillman/OneDrive - Water Boards/Documents/Data Work/SDWIS Saved Data/SDWIS_TSAANGRP_09.04.25.xlsx")


# Query from SDWIS TMNVTYPE
# tmnvtype <- dbGetQuery(con, "SELECT *
#                     FROM [ReportDB].[SDWIS].[TMNVTYPE]") 

#tmnvtype <- read_excel("C:/Users/rskillman/OneDrive - Water Boards/Documents/Data Work/SDWIS Saved Data/SDWIS_TMNVTYPE_06.11.25.xlsx")

#cat("The SDWIS TMNVTYPE dataset queried", format(Sys.Date(), "%m/%d/%Y"), "contains", nrow(tmnvtype), "records.\n")
#The SDWIS TMNVTYPE dataset queried 06/11/2025 contains 145 records.

#openxlsx::write.xlsx(tmnvtype, "C:/Users/rskillman/OneDrive - Water Boards/Documents/Data Work/SDWIS Saved Data/SDWIS_TMNVTYPE_06.11.25.xlsx")


# Query from SDWIS TMNVIEAA
# tmnvieaa <- dbGetQuery(con, "SELECT *
#                     FROM [ReportDB].[SDWIS].[TMNVIEAA]") 

#tmnvieaa <- read_excel("C:/Users/rskillman/OneDrive - Water Boards/Documents/Data Work/SDWIS Saved Data/SDWIS_ TMNVIEAA_06.11.25.xlsx")

#cat("The SDWIS TMNVIEAA dataset queried", format(Sys.Date(), "%m/%d/%Y"), "contains", nrow(tmnvieaa), "records.\n")
#The SDWIS TMNVIEAA dataset queried 06/11/2025 contains 167552 records.

#openxlsx::write.xlsx(tmnvieaa, "C:/Users/rskillman/OneDrive - Water Boards/Documents/Data Work/SDWIS Saved Data/SDWIS_TMNVIEAA_06.11.25.xlsx")

# Query from SDWIS TENENACT
# tenenact <- dbGetQuery(con, "SELECT *
#                     FROM [ReportDB].[SDWIS].[TENENACT]") 

#tenenact <- read_excel("C:/Users/rskillman/OneDrive - Water Boards/Documents/Data Work/SDWIS Saved Data/SDWIS_ TENENACT_06.11.25.xlsx")

#cat("The SDWIS TENENACT dataset queried", format(Sys.Date(), "%m/%d/%Y"), "contains", nrow(tenenact), "records.\n")
#The SDWIS TENENACT dataset queried 06/11/2025 contains 116873 records.

#openxlsx::write.xlsx(tenenact, "C:/Users/rskillman/OneDrive - Water Boards/Documents/Data Work/SDWIS Saved Data/SDWIS_TENENACT_06.11.25.xlsx")



# Query from SDWIS TENACTYP
# tenactyp <- dbGetQuery(con, "SELECT *
#                     FROM [ReportDB].[SDWIS].[TENACTYP]") 

#tenactyp <- read_excel("C:/Users/rskillman/OneDrive - Water Boards/Documents/Data Work/SDWIS Saved Data/SDWIS_ TENACTYP_06.11.25.xlsx")

#cat("The SDWIS TENACTYP dataset queried", format(Sys.Date(), "%m/%d/%Y"), "contains", nrow(tenactyp), "records.\n")
#The SDWIS TENACTYP dataset queried 06/11/2025 contains 81 records.

#openxlsx::write.xlsx(tenactyp, "C:/Users/rskillman/OneDrive - Water Boards/Documents/Data Work/SDWIS Saved Data/SDWIS_TENACTYP_06.11.25.xlsx")


# Query from SDWIS TINWSSAA
# tinwssaa <- dbGetQuery(con, "SELECT *
#                     FROM [ReportDB].[SDWIS].[TINWSSAA]") 

#tinwssaa <- read_excel("C:/Users/rskillman/OneDrive - Water Boards/Documents/Data Work/SDWIS Saved Data/SDWIS_ TINWSSAA_06.11.25.xlsx")

#cat("The SDWIS TINWSSAA dataset queried", format(Sys.Date(), "%m/%d/%Y"), "contains", nrow(tinwssaa), "records.\n")
#The SDWIS TINWSSAA dataset queried 06/11/2025 contains 15324 records.

#openxlsx::write.xlsx(tinwssaa, "C:/Users/rskillman/OneDrive - Water Boards/Documents/Data Work/SDWIS Saved Data/SDWIS_TINWSSAA_06.11.25.xlsx")



# Query from SDWIS TINSAT
# tinsat <- dbGetQuery(con, "SELECT *
#                     FROM [ReportDB].[SDWIS].[TINSAT]") 

#tinsat <- read_excel("C:/Users/rskillman/OneDrive - Water Boards/Documents/Data Work/SDWIS Saved Data/SDWIS_ TINSAT_06.11.25.xlsx")

#cat("The SDWIS TINSAT dataset queried", format(Sys.Date(), "%m/%d/%Y"), "contains", nrow(tinsat), "records.\n")
#The SDWIS TINSAT dataset queried 06/11/2025 contains 32 records.

#openxlsx::write.xlsx(tinsat, "C:/Users/rskillman/OneDrive - Water Boards/Documents/Data Work/SDWIS Saved Data/SDWIS_TINSAT_06.11.25.xlsx")


## CLEAN SABL LAYER ############################################################
#### Check for duplicate PWSIDs and filter to explore ####
duplicated_pwsids <- sabl_proj %>% 
  left_join(duplicated_pwsids <- #Identify only the unique PWSIDs that are duplicated in SABL
              sabl_proj %>%
              mutate(dup = as.numeric(duplicated(SABL_PWSID))) %>% 
              filter(dup == 1) %>% 
              as.data.frame() %>% 
              dplyr::select(SABL_PWSID, dup)) %>%
  filter(dup == 1)  #duplicates are all because of jurisdictional v.s WSA - but you have to check!

cat("The SABL dataset contains", sum(duplicated_pwsids$BOUNDARY_T == "Jurisdictional"), "duplicate PWSIDs with Jurisdictional boundaries and", sum(duplicated_pwsids$BOUNDARY_T == "Water Service Area"), "duplicate PWSIDs with WSA boundaries.\n")
#The SABL dataset contains 22 duplicate PWSIDs with Jurisdictional boundaries and 22 duplicate PWSIDs with WSA boundaries.


#### Filter duplicate jurisdictional PWSIDs from SABL ####
sabl <- sabl_proj %>% 
  filter(!(BOUNDARY_T == "Jurisdictional" & SABL_PWSID %in% 
             c(duplicated_pwsids$SABL_PWSID)))

cat("The filtered SABL dataset now contains", nrow(sabl), "rows and", length(unique(sabl$SABL_PWSID)), "unique PWSIDs.\n") #The dataset contains 5196 rows and 5174 unique PWSIDs.
#TThe filtered SABL dataset now contains 5344 rows and 5344 unique PWSIDs.

## DETERMINE MISSING PWSIDS #################################################### 
#### Join SABL with inventory ####
sabl_inventory <- inventory %>% rename(SABL_PWSID = 1) %>%
  left_join(sabl %>%
              dplyr::select(SABL_PWSID, BOUNDARY_T)) #need to select only necessary data points - it will make it easier to join SABL+

cat("There are", sum(is.na(sabl_inventory$BOUNDARY_T)), "PWSIDs in the inventory of n =", nrow(inventory), "that do not have SABL boundaries.\n") #The dataset contains 5196 rows and 5174 unique PWSIDs.
#There are 62 PWSIDs in the inventory of n = 3291 that do not have SABL boundaries.

#### Get a list of PWSIDs to add to SABL+ ####
pwsids_add <- sabl_inventory %>%
  filter(is.na(BOUNDARY_T)) %>%
  pull(SABL_PWSID)

## CREATE MISSING PWSID LAYER
#### Match DS facilities with PWSIDS missing from SABL layer ####
facilities <- tinwsf %>% 
  rename(FACILITY_NAME = NAME) %>%
  inner_join(tinwsys %>%
               filter(NUMBER0 %in% c(pwsids_add)), 
             by = c("TINWSYS_IS_NUMBER")) %>% 
  filter(
    #TYPE_CODE == "DS",
    #FACILITY_NAME == "DISTRIBUTION SYSTEM",
    ST_ASGN_IDENT_CD == "DST") #one duplicate facility for CA4200833 (TYPE_CODE == "DS" but one is DISINFECTION BYPRODUCT so just filter by DST)
cat("There are", nrow(facilities), "systems (of", length(pwsids_add), "PWSIDs missing from SABL) with an associated distribution system in TINWSF.\n") 
#There are 62 systems (of 62 PWSIDs missing from SABL) with an associated distribution system in TINWSF.

#Note: every time is a little different depending on which systems are in the inventory
#and what TINWSYS, TINWSF, and TINLOC look like

#CA3600591 (CAMP OAKES), CA0103039 (MOHRLAND MUTUAL WATER SYSTEM) have inactive DS and are inactive in TINWSYS

#### Determine Lat and Lon for DS of PWSIDS missing from SABL ####

facilities_latlon <- facilities %>% 
  left_join(tinloc, by = c("TINWSF_IS_NUMBER")) 

cat("There are", sum(is.na(facilities_latlon$LATITUDE_MEASURE)), "distribution system facilities missing latitude and longitude in TINLOC.\n") 
#There are 1 distribution system facilities missing latitude and longitude in TINLOC.


#### For systems w/ DS is missing Lat/Lon, search for other facilities ####

facilities_no_ds <- tinwsf %>%
  rename(FACILITY_NAME = NAME) %>%
  inner_join(tinwsys %>%
               filter(NUMBER0 %in% c(facilities_latlon %>% filter(is.na(LATITUDE_MEASURE)) %>% pull(NUMBER0))),
             by = c("TINWSYS_IS_NUMBER"))  %>%
  left_join(tinloc, by = c("TINWSF_IS_NUMBER"))

#use this for lat lon of CA5800810
# facilities_no_ds_active_w_latlon <- facilities_no_ds %>%  
#   filter(ACTIVITY_STATUS_CD.x == "A",
#          !is.na(LATITUDE_MEASURE)) %>%
#   rename(LAT = LATITUDE_MEASURE, LON = LONGITUDE_MEASURE, SABL_PWSID = NUMBER0)

#use this for lat lon of CA0103039 (its Well (compared to Well (Standby) and 2 destroyed wells))
facilities_no_ds_w_latlon <- facilities_no_ds %>%
  filter(!is.na(LATITUDE_MEASURE),
         TINWSF_IS_NUMBER == 17102) %>%
  rename(LAT = LATITUDE_MEASURE, LON = LONGITUDE_MEASURE, SABL_PWSID = NUMBER0)


#### For systems still missing Lat and Lon, search for lat lon in mDWW ####
facilities_add_mDWW <- facilities_latlon %>%
  rename(LAT = LATITUDE_MEASURE, LON = LONGITUDE_MEASURE, SABL_PWSID = NUMBER0) 
# %>%
#   mutate(LAT = ifelse(SABL_PWSID == "CA0800529", 41.732408, LAT),
#          LON = ifelse(SABL_PWSID == "CA0800529", -123.978332, LON)) #ROCK CREEK MUTUAL WATER CO. (DS)


#### Join together all facilities with lat lon ####
latlon <- rbind(facilities_add_mDWW %>% filter(!SABL_PWSID %in% c("CA0103039")),
                facilities_no_ds_w_latlon)

cat("There are now", sum(is.na(latlon$LAT)), "water systems missing from SABL with missing latitude and longitude.\n") 
#There are now 0 water systems missing from SABL with missing latitude and longitude.

#### Get Service Connections from TINSCC ####

tinscc_sum <- tinscc %>%
  group_by(TINWSYS_IS_NUMBER) %>%
  summarise(CONNECT = sum(SVC_CONNECT_CNT, na.rm = TRUE))

#### Get Regulating Agency from TINLGENT and TINRAA ####

reg_agency <- tinlgent %>%
  inner_join(
    tinraa,
    by = c("TINLGENT_IS_NUMBER" = "TINLGENT_IS_NUMBER")) %>%
  filter((grepl("^DISTRICT", NAME) | grepl("^LPA", NAME)),   # LIKE 'DISTRICT%' or 'LPA%'
         ACTIVE_IND_CD == "A",                               # only active
         !is.na(ADDRESS_CITY_NAME),
         !is.na(ADDRESS_ZIP_CODE)) %>%
  group_by(TINWSYS_IS_NUMBER) %>%
  arrange(NAME, .by_group = TRUE) %>%
  mutate(RN = row_number()) %>%
  ungroup() %>%
  transmute(
    TINWSYS_IS_NUMBER,
    REGULATING_AGENCY = NAME,
    ACTIVE_IND_CD,
    REG_AGENCY_TYPE = TYPE_CODE,
    ADDRESS_CITY_NAME,
    ADDRESS_ZIP_CODE,
    RN) %>% 
  dplyr::select(1,2)

## CREATE SABL+ LAYER ##########################################################
#### Convert the lat lon into a point layer ####
ds_points <- st_as_sf(latlon %>%
                        mutate(BOUNDARY_T = "Artificial") %>%
                        dplyr::select(SABL_PWSID, BOUNDARY_T, LAT, LON), 
                      coords = c("LON", "LAT"), crs = 4326) %>% 
  st_transform(crs = 3310) #all lat and lon must be present

#### Create a buffer of 0.5 miles (804.67 meters) ####
ds_buffer_0.5 <- st_buffer(ds_points, dist = 804.67)
cat("Created a buffer of 0.5 miles (804.67 meters) around", nrow(ds_buffer_0.5), "PWSID points (Projection EPSG:3310).\n")
#Created a buffer of 0.5 miles (804.67 meters) around 62 PWSID points (Projection EPSG:3310).

#### Create a buffer of 1 mile (1609.34 meters) ####
ds_buffer_1 <- st_buffer(ds_points, dist = 1609.34)
cat("Created a buffer of 1 mile (1609.34 meters) around", nrow(ds_buffer_1), "PWSID points (Projection EPSG:3310).\n")

#### Join together the inventory and SABL combined with buffered Distribution System points ####
sablplus_0.5 <- inventory %>% 
  dplyr::select(SABL_PWSID = 1, WS_NAME = 2) %>%
  left_join(sabl %>%
              dplyr::select(SABL_PWSID, BOUNDARY_T) %>%
              rbind(ds_buffer_0.5)) %>%
  st_as_sf() %>%
  st_transform("EPSG:3310") %>% #reproject to California Teale Albers 
  left_join(tinwsys %>% rename(SABL_PWSID = NUMBER0) %>% 
              dplyr::select(SABL_PWSID, FED_TYPE = D_PWS_FED_TYPE_CD, COUNTY = D_PRIN_CNTY_SVD_NM, POPULATION = D_POPULATION_COUNT, TINWSYS_IS_NUMBER), 
            by = c("SABL_PWSID")) %>% 
  left_join(tinscc_sum) %>% 
  left_join(reg_agency) %>%
  # left_join(mdww %>% 
  #             mutate(SABL_PWSID = paste0("CA", `Water System No.`)) %>%
  #             dplyr::select(SABL_PWSID, REGULATING_AGENCY = `Regulating Agency`)) %>%
  # mutate(FED_TYPE = 
  #          case_when(
  #            FED_TYPE == "C" ~ "COMMUNITY",
  #            FED_TYPE == "NC" ~ "NON-COMMUNITY",
  #            FED_TYPE == "NP" ~ "NON-PUBLIC",
  #            FED_TYPE == "NTNC" ~ "NON-TRANSIENT NON-COMMUNITY",
  #            TRUE ~ FED_TYPE
  #          )) %>%
  dplyr::select(SABL_PWSID,
                NAME = WS_NAME,
                BOUNDARY_T,
                FED_TYPE,
                COUNTY,
                POPULATION, 
                TINWSYS_IS_NUMBER,
                CONNECT,
                REGULATING_AGENCY,
                geometry) %>%
    mutate(COUNTY = ifelse(SABL_PWSID == "CA4301410", "SANTA CLARA", COUNTY)) #shouldn't even be doing this but I am 
#mutate(ws_area = st_area(geometry)) 

# CURRENTLY DOESN'T REALLY WORK
# sablplus_1 <- inventory %>% rename(SABL_PWSID = 1) %>%
#   left_join(sabl %>%
#               dplyr::select(SABL_PWSID, BOUNDARY_T) %>%
#               rbind(ds_buffer_1)) %>%
#   st_as_sf() %>%
#   st_transform("EPSG:3310") %>% #reproject to California Teale Albers 
#   left_join(tinwsys %>% rename(SABL_PWSID = NUMBER0) %>% 
#               dplyr::select(SABL_PWSID, FED_TYPE = D_PWS_FED_TYPE_CD, COUNTY = D_PRIN_CNTY_SVD_NM, POPULATION = D_POPULATION_COUNT, TINWSYS_IS_NUMBER), 
#             by = c("SABL_PWSID")) %>% 
#   left_join(tinscc_sum) %>% 
#   # left_join(mdww %>% 
#   #             mutate(SABL_PWSID = paste0("CA", `Water System No.`)) %>%
#   #             dplyr::select(SABL_PWSID, REGULATING_AGENCY = `Regulating Agency`)) %>%
#   mutate(FED_TYPE = 
#            case_when(
#              FED_TYPE == "C" ~ "COMMUNITY",
#              FED_TYPE == "NC" ~ "NON-COMMUNITY",
#              FED_TYPE == "NP" ~ "NON-PUBLIC",
#              FED_TYPE == "NTNC" ~ "NON-TRANSIENT NON-COMMUNITY",
#              TRUE ~ FED_TYPE
#            )) %>%
#   dplyr::select(SABL_PWSID,
#                 NAME = `Water System Name`,
#                 BOUNDARY_T,
#                 FED_TYPE,
#                 COUNTY,
#                 POPULATION, 
#                 TINWSYS_IS_NUMBER,
#                 CONNECT,
#                 REGULATING_AGENCY,
#                 geometry) %>%
#   mutate(COUNTY = ifelse(SABL_PWSID == "CA4301410", "SANTA CLARA", COUNTY)) #shouldn't even be doing this but I am 
# 
# cat("There are", sum(is.na(sablplus_1$BOUNDARY_T)), "PWSIDs in the inventory of n =", nrow(inventory), "that do not have SABL boundaries.\n") #The dataset contains 5196 rows and 5174 unique PWSIDs.
# setNames(c("SABL_PWSID", "NAME", "BOUNDARY_TYPE", "FED_TYPE", "COUNTY", "POPULATION", "TINWSYS_IS_NUMBER", "SERVICE_CONNECTIONS", "REGULATING_AGENCY", "geometry"))

#### Write the SABL+ layer to a shapefile ####
st_write(sablplus_0.5, "SABL+/Manual Data/SABL+ Manual Data Q2 2025/0.5 Miles/SABL+_0.5_09.04.25.shp")
st_write(sablplus_1, "SABL+/Manual Data/SABL+ Manual Data Q1 2025/1 Mile/SABL+_1_06.10.25.shp")

# %>% left_join(mdww %>% 
#               mutate(SABL_PWSID = paste0("CA", `Water System No.`)) %>%
#               select(SABL_PWSID, 
#                      Population, `Service Connections`)) 
#amazing they perfectly match (between SDWIS and mDWW)
