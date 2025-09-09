### eAR Water Rates MASTER - Q2 2025 Update ###
# Author: Rachel Skillman
# Date Created: 9/9/25
# Date Updated: 9/9/25

# Code history: this code was initially from: "Clean eAR Water Rates Data for Affordability Assessment.R"

## SET-UP ######################################################################  
#### Set Working Directory ####

setwd("C:/Users/rskillman/OneDrive - Water Boards/Documents/Data Work/eAR")

#### Load Libraries ####

#install.packages("")
library(tidyverse) #data management
library(odbc) #object database connection
library(readr) #read .csv files
library(readxl) #read .xlsx files
library(sf) #spatial data manipulation

#### Open the Server Connection for SSMS Pulls ####

con <- dbConnect(odbc(),                 
                 Driver = "SQL Server",                 
                 Server = "reportmanager,1542",                 
                 Database = "ReportDB",                 
                 UID = Sys.getenv("CDM5_SQL_SERVER_USER"),
                 PWD = Sys.getenv("CDM5_SQL_SERVER_PASSWORD"),
                 TrustServerCertificate = "yes",
                 Port = 1542) #must be logged into SSMS and/or VPN

## LOAD DATA ###################################################################
#### Load 2023 eAR 8a + 8b Data ####

# publicly available download (downloaded 3/27/25)
# Section 8 is downloaded as a .txt file and needs to be split in notepad before being imported to excel

ear23_8a_1 <- read_excel("2023/split_files/eAR2023_Section8_Part1.xlsx") %>%
  filter(SectionName == "08a Customer Charges") #don't need to set names because it's the first file and colnames are already present

ear23_8a_2 <- read_excel("2023/split_files/eAR2023_Section8_Part2.xlsx", 
                         col_names = FALSE) %>%
  setNames(c("WaterSystemID", "WaterSystemName", "WaterSystemClassification", 
             "Survey", "SectionName", "SurveyQuestionId", 
             "QuestionAbbreviation", "Question" , "Answer")) %>%
  filter(SectionName == "08a Customer Charges")

ear23_8a_3 <- read_excel("2023/split_files/eAR2023_Section8_Part3.xlsx", 
                         col_names = FALSE) %>%
  setNames(c("WaterSystemID", "WaterSystemName", "WaterSystemClassification", 
             "Survey", "SectionName", "SurveyQuestionId", 
             "QuestionAbbreviation", "Question" , "Answer")) %>%
  filter(SectionName == "08a Customer Charges")

ear23_8a_4 <- read_excel("2023/split_files/eAR2023_Section8_Part4.xlsx", 
                         col_names = FALSE) %>%
  setNames(c("WaterSystemID", "WaterSystemName", "WaterSystemClassification", 
             "Survey", "SectionName", "SurveyQuestionId", 
             "QuestionAbbreviation", "Question" , "Answer")) %>%
  filter(SectionName == "08a Customer Charges")

# Stack section 8A data together
ear23_8a <- rbind(ear23_8a_1, ear23_8a_2, ear23_8a_3, ear23_8a_4)

ear23_8b_1 <- read_excel("2023/split_files/eAR2023_Section8_Part1.xlsx") %>%
  filter(SectionName == "08b Income")  #don't need to set names because it's the first file and colnames are already present

ear23_8b_2 <- read_excel("2023/split_files/eAR2023_Section8_Part2.xlsx", 
                         col_names = FALSE) %>%
  setNames(c("WaterSystemID", "WaterSystemName", "WaterSystemClassification", 
             "Survey", "SectionName", "SurveyQuestionId", 
             "QuestionAbbreviation", "Question" , "Answer")) %>%
  filter(SectionName == "08b Income")

ear23_8b_3 <- read_excel("C:/Users/rskillman/Downloads/split_files/eAR2023_Section8_Part3.xlsx", 
                         col_names = FALSE) %>%
  setNames(c("WaterSystemID", "WaterSystemName", "WaterSystemClassification", 
             "Survey", "SectionName", "SurveyQuestionId", 
             "QuestionAbbreviation", "Question" , "Answer")) %>%
  filter(SectionName == "08b Income")

ear23_8b_4 <- read_excel("C:/Users/rskillman/Downloads/split_files/eAR2023_Section8_Part4.xlsx", 
                         col_names = FALSE) %>%
  setNames(c("WaterSystemID", "WaterSystemName", "WaterSystemClassification", 
             "Survey", "SectionName", "SurveyQuestionId", 
             "QuestionAbbreviation", "Question" , "Answer")) %>%
  filter(SectionName == "08b Income")

# Stack section 8B data together
ear23_8b <- rbind(ear23_8b_1, ear23_8b_2, ear23_8b_3, ear23_8b_4)


#### Load 2023 eAR Data from DW ####

ear23_dw <- read_excel("C:/Users/rskillman/Downloads/eAR2023Section8QuestionsForRA_DW_03.28.25.xlsx") #she pre-selected only the questions needed
# Needed for status even though public data is used to calculate the rate

#### Load Supplemental Data ####

military <- read_csv("C:/Users/rskillman/Downloads/MilitaryPWSList 1.csv") %>% 
  mutate(military = 1) %>% rename(PWSID = WSID) 
# Needed to censor military WS rates --> "N/A"

#### Load Relevant Inventory ####

inventory <- read_excel("C:/Users/rskillman/OneDrive - Water Boards/Documents/Data Work/Inventory/2025-Q2 Refresh_Inventory.xlsx", 
                        sheet = "Comprehensive Inventory 2025-Q2") 
cat("The inventory dataset queried on 08/28/2025 contains", nrow(inventory), "rows.\n")
#The inventory dataset queried on 08/28/2025 contains 3291 rows.


#### Load SDWIS data ####

# Query System List from SDWIS TINWSYS
tinwsys <- dbGetQuery(con, "SELECT *
                    FROM [ReportDB].[SDWIS].[TINWSYS]") %>%
  mutate(NUMBER0 = str_trim(NUMBER0)) # Remove any trailing or leading white space

cat("The SDWIS TINWSYS dataset queried", format(Sys.Date(), "%m/%d/%Y"), "contains", nrow(tinwsys), "water systems.\n")
#The SDWIS TINWSYS dataset queried 09/09/2025 contains 15962 water systems.


# Query Service Areas from SDWIS TINWSSAA
tinwssaa <- dbGetQuery(con, "SELECT *
                    FROM [ReportDB].[SDWIS].[TINWSSAA]")

cat("The SDWIS TINWSSAA dataset queried", format(Sys.Date(), "%m/%d/%Y"), "contains", nrow(tinwssaa), "service area records.\n")
#The SDWIS TINWSSAA dataset queried 09/09/2025 contains 15354 service area records.

#### SABL+ Layer ####

sablplus <- st_read("C:/Users/rskillman/OneDrive - Water Boards/Documents/Data Work/SABL/SABL+/Manual Data/SABL+ Manual Data Q2 2025/0.5 Miles/SABL+_0.5_09.04.25.shp") %>% 
  st_transform("EPSG:3310") %>% #reproject to California Teale Albers 
  setNames(c("SABL_PWSID", "NAME", "BOUNDARY_TYPE", "FED_TYPE", "COUNTY", "POPULATION", "TINWSYS_IS_NUMBER", "SERVICE_CONNECTIONS", "REGULATING_AGENCY", "geometry")) %>%
  mutate(ws_area = st_area(geometry)) 
#st_crs(sablplus) #check CRS 
cat("The SABL+ dataset from 9/4/25 for the Q2 2025 Manual Data refresh contains", nrow(sablplus), "rows and", length(unique(sablplus$SABL_PWSID)), "unique PWSIDs.\n") 
#The SABL+ dataset from 9/4/25 contains 3291 rows and 3291 unique PWSIDs.

## CLEAN AND JOIN DATA #########################################################
#### Create school list ####

schools <- tinwssaa %>% 
  
  # Inner join TINWSSAA with TINWSYS
  inner_join(tinwsys, by = "TINWSYS_IS_NUMBER") %>%
  
  # Filter to service areas of active NTNC WS that are not state-owned 
  filter(
    ACTIVITY_STATUS_CD == "A",
    D_PWS_FED_TYPE_CD == "NTNC",
    OWNER_TYPE_CODE != "S"     # not state gov
  ) %>%
  
  # Group by WS
  group_by(TINWSYS_IS_NUMBER, NUMBER0) %>%
  
  # Collapse primary service indicator and service area code into one variable
  summarise(agg_string = paste0(PRIMARY_IND_CD, TINSAT_NAME_CODE, collapse = ","),
            .groups = "drop") %>%
  
  # Filter to primary service area (Y) of school (SC) and non-primary service area (N) of K-12 (KT)
  filter(grepl("YSC", agg_string), #primary service area (Y) of school (SC)
         grepl("NKT", agg_string)) %>% #non-primary service area (N) of K-12 (KT)
  
  dplyr::select(SABL_PWSID = NUMBER0) %>%
  mutate(school = 1)

#### Clean 2023 eAR DW data for water rates ####

ear23_hasrate <- ear23_dw %>% 
  filter(SurveyQuestionId == 45998) %>%
  dplyr::select(WaterSystemID = WSID, 
                WRHasRate = AnswerText,
                StatusName)

ear23_hcf <- ear23_dw %>% 
  filter(SurveyQuestionId == 46701) %>%
  dplyr::select(WaterSystemID = WSID, 
                WR6HCFTotalWBill = AnswerText,
                StatusName)

ear23_hcf_alt <- ear23_dw %>% 
  filter(SurveyQuestionId == 46702) %>%
  dplyr::select(WaterSystemID = WSID, 
                WR6HCFAlternative = AnswerText,
                StatusName)

ear23_hcf_altamt <- ear23_dw %>% 
  filter(SurveyQuestionId == 46703) %>%
  dplyr::select(WaterSystemID = WSID, 
                WR6HCFAltAmt = AnswerText,
                StatusName)

ear23_hcf_altamt_cmt <- ear23_dw %>% 
  filter(SurveyQuestionId == 46704) %>%
  dplyr::select(WaterSystemID = WSID, 
                WR6HCFComment = AnswerText,
                StatusName)

cat("The 2023 eAR (sent by DW 3/28/25) has", nrow(ear23_hasrate), "responses for Charging Water Rates from section 8a, and from Section 8b the 2022 eAR has", 
    nrow(ear23_hcf), "responses for 6HCF Charge Amount,",
    nrow(ear23_hcf_alt), "responses for Alternative Water Rate,",
    nrow(ear23_hcf_altamt), "responses for Alternative Water Rate Amount,",
    nrow(ear23_hcf_altamt_cmt), "and responses for Alternative Water Rate Comment.\n") #The dataset contains 3184 rows and 3184 unique PWSIDs.
#The 2023 eAR has 6636 responses for Charging Water Rates from section 8a, 
#and from Section 8b the 2023 eAR has 6040 responses for 6HCF Charge Amount, 
#6040 responses for Alternative Water Rate, 6040 responses for Alternative Water Rate Amount, 
#6040 and responses for Alternative Water Rate Comment.


# Find duplicate entries
cat("The 2023 eAR(sent by DW 3/28/25) has", sum(duplicated(ear23_hasrate$WaterSystemID)), "duplicate survey responses for Charging Water Rates from section 8a, and from Section 8b the 2022 eAR has", 
    sum(duplicated(ear23_hcf$WaterSystemID)), "duplicate responses for 6HCF Charge Amount,",
    sum(duplicated(ear23_hcf_alt$WaterSystemID)), "duplicate responses for Alternative Water Rate,",
    sum(duplicated(ear23_hcf_altamt$WaterSystemID)), "duplicate responses for Alternative Water Rate Amount,",
    sum(duplicated(ear23_hcf_altamt_cmt$WaterSystemID)), "and duplicate responses for Alternative Water Rate Comment.\n") #The dataset contains 3184 rows and 3184 unique PWSIDs.
#The 2023 eAR(sent by DW 3/28/25) has 1 duplicate survey responses for Charging Water Rates from section 8a, 
#and from Section 8b the 2023 eAR has 0 duplicate responses for 6HCF Charge Amount, 
#0 duplicate responses for Alternative Water Rate, 0 duplicate responses for Alternative Water Rate Amount, 
#0 and duplicate responses for Alternative Water Rate Comment.

#dup <- ear23_dw_hasrate %>% mutate(dup = duplicated(WaterSystemID)) #both are missing WR so it's fine
#If rows are identical
ear23_hasrate <- ear23_hasrate[-which(duplicated(ear23_hasrate)), ] #6635

#### Join 2023 eAR DW data for water rates ####

ear23_join <- ear23_hasrate %>% 
  left_join(ear23_hcf) %>%
  left_join(ear23_hcf_alt) %>%
  left_join(ear23_hcf_altamt) %>%
  left_join(ear23_hcf_altamt_cmt) #6635

ear23_complete <- ear23_join %>%
  filter(StatusName %in% c("Completed", "Submitted")) #104 submitted, 6393 completed = 6497

cat("The 2023 eAR(sent by DW 3/28/25) has", nrow(ear23_complete), "valid surveys, including", sum(ear23_complete$StatusName == "Completed"), "completed and", sum(ear23_complete$StatusName == "Submitted"), "submitted.\n")
#The 2023 eAR(sent by DW 3/28/25) has 6497 valid surveys, including 6393 completed and 104 submitted.

#### Clean 2023 public eAR data for water rates ####

#make the names more clear/different
ear_hasrate_23 <- ear23_8a %>% filter(QuestionAbbreviation == "WRHasRate") %>%
  dplyr::select(1, WRHasRate = Answer)
ear_hcf_23 <- ear23_8b %>% filter(QuestionAbbreviation == "WR6HCFTotalWBill") %>%
  dplyr::select(1, WR6HCFTotalWBill = Answer)
ear_hcf_alt_23 <- ear23_8b %>% filter(QuestionAbbreviation == "WR6HCFAlternative") %>%
  dplyr::select(1, WR6HCFAlternative = Answer)
ear_hcf_altamt_23 <- ear23_8b %>% filter(QuestionAbbreviation == "WR6HCFAltAmt") %>%
  dplyr::select(1, WR6HCFAltAmt = Answer)
ear_hcf_cmt_23 <- ear23_8b %>% filter(QuestionAbbreviation == "WR6HCFComment") %>%
  dplyr::select(1, WR6HCFComment = Answer)

cat("The 2023 eAR has", nrow(ear_hasrate_23), "responses for Charging Water Rates from section 8a, and from Section 8b the 2023 eAR has", 
    nrow(ear_hcf_23), "responses for 6HCF Charge Amount,",
    nrow(ear_hcf_alt_23), "responses for Alternative Water Rate,",
    nrow(ear_hcf_altamt_23), "responses for Alternative Water Rate Amount,",
    nrow(ear_hcf_cmt_23), "and responses for Alternative Water Rate Comment.\n") #The dataset contains 3184 rows and 3184 unique PWSIDs.
#The 2023 eAR has 6395 responses for Charging Water Rates from section 8a, 
#and from Section 8b the 2023 eAR has 5793 responses for 6HCF Charge Amount, 
#5793 responses for Alternative Water Rate, 5793 responses for Alternative Water Rate Amount, 
#5793 and responses for Alternative Water Rate Comment.


# Find duplicate entries
cat("The 2023 eAR has", sum(duplicated(ear_hasrate_23$WaterSystemID)), "duplicate survey responses for Charging Water Rates from section 8a, and from Section 8b the 2023 eAR has", 
    sum(duplicated(ear_hcf_23$WaterSystemID)), "duplicate responses for 6HCF Charge Amount,",
    sum(duplicated(ear_hcf_alt_23$WaterSystemID)), "duplicate responses for Alternative Water Rate,",
    sum(duplicated(ear_hcf_altamt_23$WaterSystemID)), "duplicate responses for Alternative Water Rate Amount, and",
    sum(duplicated(ear_hcf_cmt_23$WaterSystemID)), "duplicate responses for Alternative Water Rate Comment.\n") #The dataset contains 3184 rows and 3184 unique PWSIDs.
#The 2023 eAR has 1 duplicate survey responses for Charging Water Rates from section 8a, 
#and from Section 8b the 2023 eAR has 0 duplicate responses for 6HCF Charge Amount, 
#0 duplicate responses for Alternative Water Rate, 0 duplicate responses for Alternative Water Rate Amount, and 
#0 duplicate responses for Alternative Water Rate Comment.

#dup <- ear_hasrate_23 %>% mutate(dup = duplicated(WaterSystemID))
#If rows are identical
ear_hasrate_23 <- ear_hasrate_23[-which(duplicated(ear_hasrate_23)), ]

#### Join 2023 public eAR data for water rates ####

ear23_join_public <- ear_hasrate_23 %>% 
  mutate(has_wr = 1) %>% #has_wr just means I saw you in the survey
  full_join(ear_hcf_23) %>% 
  full_join(ear_hcf_alt_23) %>%
  full_join(ear_hcf_altamt_23) %>%
  full_join(ear_hcf_cmt_23)  #there are 4 additional systems in public eAR but they would all get missing anyway

cat("The 2023 publicly downloaded eAR has", nrow(ear23_join_public), "valid surveys.\n")
#The 2023 publicly downloaded eAR has 6398 valid surveys.

#### Compare public eAR data with DW data ####

join <- ear23_join %>%
  full_join(ear23_join_public, by = c("WaterSystemID")) 
table((join %>% filter(is.na(has_wr)))$StatusName)

#Water Systems missing from public data if their eAR status is "In process", "Need revision", and "Submitted"
#One weird system marked as complete in eAR is not in public data (CA1510029)
#All WS in public data are in DW data - use ear23_complete 
# join_complete <- ear23_complete %>%
#   full_join(ear23_join_public, by = c("WaterSystemID"))

## FINALIZE WR DATA ############################################################
#### Finalize 2023 eAR data for water rates ####

water_rate_23 <- sablplus %>%
  left_join(ear23_complete %>% rename(SABL_PWSID = WaterSystemID)) %>%
  left_join(military %>% rename(SABL_PWSID = PWSID)) %>%
  left_join(schools, by = c("SABL_PWSID")) %>%
  mutate(military_dummy = ifelse(is.na(military), 0, military), 
         school_dummy = ifelse(is.na(school), 0, school), 
         
         #these variables are not necessary but useful for checking 
         rate_num = as.numeric(WR6HCFTotalWBill), #get the rate as a number
         altamt_num = as.numeric(WR6HCFAltAmt), #get the alternative rate as a number
         rate_flag = ifelse(rate_num <= 500 & rate_num >= 5, 1, 0), #flag if there is a rate provided between 5 and 500 (0 if numeric and outside this range, 1 if numeric and inside and NA if no number is provided)
         altamt_flag = ifelse(altamt_num <= 500 & altamt_num >= 5, 1, 0), #flag if there is a alternative provided between 5 and 500 (0 if numeric and outside this range, 1 if numeric and inside and NA if no number is provided)
         
         #Set Rate as N/A for military WS, school WS and systems that don't charge for water
         Rate = ifelse(military_dummy == 1 | school_dummy == 1 | WRHasRate == "No", "N/A", NA),
         
         #Set Rate as N/A for Non-Community WS 
         #Rate = ifelse(FED_TYPE != "COMMUNITY", "N/A", NA),
         
         # CASE: VALID ALTERNATIVE AMOUNT AND INVALID 6 HCF AMOUNT (CHECK YES ALTERNATIV AMOUNT BOX (or not in 3 4? cases))
         # Set Rate as alternative amount if WS says there is an alternative amount and alt amt is valid (this also covers the 3 4? instances where they did not check alt amt box but their 6 HCF rate was invalid and their alt rate is valid)
         Rate = ifelse(altamt_flag == 1 & rate_flag == 0 & is.na(Rate), 
                       paste0("$", formatC(as.numeric(altamt_num), format = "f", digits = 2)), Rate), #if WS says they charge alternative amount, and the number is w/i acceptable range 5,500 --> take the alternative amount as the rate - OVERRIDE THE 6 HCF AMT (do not need (!is.na(Rate)))
         
         # CASE: VALID 6 HCF AMOUNT AND NO ALTERNATIVE AMOUNT PROVIDED
         # Set Rate as 6 HCF amount if WS does charge for water and rate is valid 
         Rate = ifelse(WRHasRate == "Yes" & rate_flag == 1 & is.na(Rate), 
                       paste0("$", formatC(as.numeric(rate_num), format = "f", digits = 2)), Rate), #if WS says they charge, and the number is w/i acceptable range 5,500 --> take the 6 HCF amount as the rate
         
         # Set Rate to Missing if rate is still missing        
         Rate = ifelse(is.na(Rate), "Missing", Rate)) %>%  #if WS says they charge, but there is no rate or alternative rate provided w/i acceptable range 5,500 --> Missing
  
  left_join(ear23_join %>% dplyr::select(SABL_PWSID = WaterSystemID, EAR23_STATUS = StatusName)) %>%
  mutate(EAR23_STATUS = ifelse(is.na(EAR23_STATUS), "No eAR", EAR23_STATUS),
         
         # Fix incorrect water rate for H & J
         altamt_num = ifelse(SABL_PWSID == "CA3700073", 56.55, altamt_num),
         Rate = ifelse(SABL_PWSID == "CA3700073", "$56.55", Rate)) 


#### Clean Water Rates data ####

WR_CLEAN <- water_rate_23 %>% 
  as.data.frame() %>%
  dplyr::select(PWSID = SABL_PWSID,
                NAME,
                COUNTY,
                REGULATING_AGENCY,
                FED_TYPE,
                SERVICE_CONNECTIONS,
                POPULATION,
                military_dummy,
                school_dummy,
                EAR23_STATUS,
                RATE_6HCF = Rate,
                BOUNDARY_TYPE) 

#### QA/QC ####

#Ensure all military WS are N/A
mil_check <- water_rate_23 %>% filter(military_dummy == 1)
table(mil_check$Rate)

#Ensure all non-community WS are N/A
noncws_check <- water_rate_23 %>% filter(FED_TYPE != "COMMUNITY", military_dummy == 0)
table(noncws_check$Rate)

#Ensure all (C)WS that do not charge for water are N/A
nocharge_check1 <- water_rate_23 %>% filter(WRHasRate == "No")
table(nocharge_check1$Rate)

#Ensure all CWS that charge for water and do not have a valid rate are Missing
missing_check <- water_rate_23 %>% filter(FED_TYPE == "C", WRHasRate == "Yes", altamt_flag == 0, rate_flag == 0)
table(missing_check$Rate)

#Ensure all CWS that have a valid water rate are NOT Missing or N/A
cws_check <- water_rate_23 %>% filter(FED_TYPE == "C", WRHasRate == "Yes", military_dummy == 0, altamt_flag == 1 | rate_flag == 1)
sum(cws_check$Rate == "Missing")
sum(cws_check$Rate == "N/A")

#Ensure valid rates are between $5 and $500
summary(water_rate_23$rate_num[water_rate_23$rate_flag == 1])
summary(water_rate_23$altamt_num[water_rate_23$altamt_flag == 1])

#Pull out a list of systems that did not check alternative amount box but did provide an alternative amount
altamt_error <- water_rate_23 %>% filter(is.na(WR6HCFAlternative), !is.na(altamt_num))

#### Write Water Rates Data #### 

# openxlsx::write.xlsx(WR_CLEAN,"C:/Users/rskillman/OneDrive - Water Boards/Documents/Data Work/Master Data Files/Manual Data/Q2 2025_08.28.25/WaterRate_ManualData_09.09.25.xlsx") 
