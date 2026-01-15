# Load libraries
library(readr) 
library(dplyr)
library(lubridate)
library(stringr)

# Load the raw data files
SRS_N_C_raw <- read_csv("data/raw/LT_ND_Grahl_004.csv") %>%
  # mutate values in the Type column from "Above" and "Below"
  # to Aboveground and Belowground using case_when
  mutate(Type = case_when(
    Type == "Above" ~ "Aboveground",
    Type == "Below" ~ "Belowground",
    TRUE ~ Type
  ))

SRS_P_raw <- read_csv("data/raw/LT_ND_Grahl_005.csv") %>%
  # mutate values in the Type column from "Above" and "Below"
  # to Aboveground and Belowground using case_when
  mutate(Type = case_when(
    Type == "Above" ~ "Aboveground",
    Type == "Below" ~ "Belowground",
    TRUE ~ Type
  ))

TS_P_raw <- read_csv("data/raw/LT_ND_Rubio_004.csv")
TS_N_C_raw <- read_csv("data/raw/LT_ND_Rubio_005.csv")

# Harmonize the column names for the datasets by changing "TypeTP" to "Type" in TS_P_raw data
TS_P_renamed <- TS_P_raw %>%
  rename(Type = TypeTP)

# Antijoin the two SRS datasets by SiteName, Date, and Type to ensure all records can join
anti_srs_sawgrass_1 <-
  anti_join(SRS_N_C_raw,
            SRS_P_raw,
            by = c("SITENAME", "Date", "Type")) %>%
  select(SITENAME, Date, Type)

anti_srs_sawgrass_2 <-
  anti_join(SRS_P_raw,
            SRS_N_C_raw,
            by = c("SITENAME", "Date", "Type")) %>%
  select(SITENAME, Date, Type)

# Antijoin the two TS datasets by SiteName, Date, and Type to ensure all records can join
anti_ts_sawgrass_1 <-
  anti_join(TS_N_C_raw,
            TS_P_renamed,
            by = c("SITENAME", "Date", "Type")) %>%
  select(SITENAME, Date, Type)

anti_ts_sawgrass_2 <- 
  anti_join(TS_P_renamed,
            TS_N_C_raw,
            by = c("SITENAME", "Date", "Type")) %>%
  select(SITENAME, Date, Type)


# The TS sites appear to fail joining because the 2004 data have different months (March vs April) at TS4 and TS5,
# and some TS site records fail to join because the CN records list something other than the 1st of the month

# create year and month columns for the TS FCE datasets to help identify cases
# that have differing dates
TS_P_temporal <- TS_P_renamed %>%
  mutate(Year = lubridate::year(Date),
         Month = lubridate::month(Date))

TS_N_C_temporal <- TS_N_C_raw %>%
  mutate(Year = lubridate::year(Date),
         Month = lubridate::month(Date)) #%>%
# Drop RecordNum column
#select(-RecordNum)

# Try to antijoin the TS datasets by SiteName, Year, Month, and Type to ensure all records can join
anti_ts_sawgrass_3 <-
  anti_join(TS_P_temporal,
            TS_N_C_temporal,
            by = c("SITENAME", "Year", "Month", "Type")) %>%
  select(SITENAME, Year, Month, Type)

# Filter TS_P data so that it only contains records from 2004
TS_P_temporal_2004 <- TS_P_temporal %>%
  filter(Year == 2004) %>%
  select(-RecordNum)


# Filter TS_N_C data so that it only contains records from 2004
TS_N_C_temporal_2004 <- TS_N_C_temporal %>%
  filter(Year == 2004) 

# Correct the month for TS/Ph4 and TS/Ph5 in TS_P_temporal_2004 to 4 if year is 2004
TS_P_temporal_corrected <- TS_P_temporal %>%
  mutate(Month = case_when(
    SITENAME == "TS/Ph4" & Year == 2004 ~ 4,
    SITENAME == "TS/Ph5" & Year == 2004 ~ 4,
    TRUE ~ Month
  )) %>%
  # Update Dates
  mutate(Date = as.Date(paste(Year, Month, "01", sep = "-"))) %>%
  arrange(RecordNum) %>%
  # Drop RecordNum, Month, and Year columns
  select(-c(RecordNum,
            Month,
            Year))

TS_N_C_temporal_corrected <- TS_N_C_temporal %>%
  # Correct day values in Date to 01
  mutate(Date = as.Date(paste(Year, Month, "01", sep = "-"))) %>%
  arrange(RecordNum) %>%
  # Drop RecordNum, Month, and Year columns
  select(-c(RecordNum,
            Month,
            Year))

anti_ts_sawgrass_4 <-
  anti_join(TS_P_temporal_corrected,
            TS_N_C_temporal_corrected,
            by = c("SITENAME", "Date", "Type")) %>%
  select(SITENAME, Date, Type)