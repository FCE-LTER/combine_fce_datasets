# Load libraries
library(tidyverse) 
library(EDIutils)
library(plotly)

# If the data directory does not exist, create it and include raw, intermediate,
# and final subdirectories

if (!dir.exists("data")) {
  dir.create("data")
  dir.create("data/raw")
  dir.create("data/intermediate")
  dir.create("data/final")
}

scope <- "knb-lter-fce"
FCE1071_identifier <- "1071"
revision <- list_data_package_revisions(scope, FCE1071_identifier, filter = "newest")
packageId <- paste(scope, FCE1071_identifier, revision, sep = ".")


# Get list of data files in the last revision of your package in EDI
res <- read_data_entity_names(packageId)

res

# Download desired file in raw bytes. Use the entityName and entityID as keys.
entityName <- "LT_ND_Grahl_005.csv"

entityId <- res$entityId[res$entityName == entityName]

# Load last revision of published data file from EDI repository
raw <- read_data_entity(packageId, entityId)

# Parse published raw file with a .csv reader
FCE1071_data <- readr::read_csv(file = raw)

# Repeat for FCE1070
FCE1070_identifier <- "1070"
revision <- list_data_package_revisions(scope, FCE1070_identifier, filter = "newest")
packageId <- paste(scope, FCE1070_identifier, revision, sep = ".")

# Get list of data files in the last revision of your package in EDI
res <- read_data_entity_names(packageId)

# Download desired file in raw bytes. Use the entityName and entityID as keys.
entityName <- "LT_ND_Grahl_004.csv"
entityId <- res$entityId[res$entityName == entityName]

# Load last revision of published data file from EDI repository
raw <- read_data_entity(packageId, entityId)

# Parse published raw file with a .csv reader
FCE1070_data <- readr::read_csv(file = raw)


# Repeat for FCE1082
FCE1082_identifier <- "1082"
revision <- list_data_package_revisions(scope, FCE1082_identifier, filter = "newest")
packageId <- paste(scope, FCE1082_identifier, revision, sep = ".")

# Get list of data files in the last revision of your package in EDI
res <- read_data_entity_names(packageId)

# Download desired file in raw bytes. Use the entityName and entityID as keys.
entityName <- "LT_ND_Rubio_004.csv"
entityId <- res$entityId[res$entityName == entityName]

# Load last revision of published data file from EDI repository
raw <- read_data_entity(packageId, entityId)

# Parse published raw file with a .csv reader
FCE1082_data <- readr::read_csv(file = raw)

# Repeat for FCE1083
FCE1083_identifier <- "1083"
revision <- list_data_package_revisions(scope, FCE1083_identifier, filter = "newest")
packageId <- paste(scope, FCE1083_identifier, revision, sep = ".")

res <- read_data_entity_names(packageId)

entityName <- "LT_ND_Rubio_005.csv"
entityId <- res$entityId[res$entityName == entityName]

raw <- read_data_entity(packageId, entityId)

FCE1083_data <- readr::read_csv(file = raw)

# Save the raw data files to the raw directory, using entityName as the filename
write_csv(FCE1070_data, "data/raw/LT_ND_Grahl_004.csv")
write_csv(FCE1071_data, "data/raw/LT_ND_Grahl_005.csv")
write_csv(FCE1082_data, "data/raw/LT_ND_Rubio_004.csv")
write_csv(FCE1083_data, "data/raw/LT_ND_Rubio_005.csv")



# Harmonize the column names for the datasets by changing "TypeTP" to "Type" in FCE1082_data
FCE1082_data <- FCE1082_data %>%
  rename(Type = TypeTP)


# Antijoin the two SRS datasets by SiteName, Date, and Type to ensure all records can join
anti_srs_sawgrass_1 <-
  anti_join(FCE1071_data,
            FCE1070_data,
            by = c("SITENAME", "Date", "Type")) %>%
  select(SITENAME, Date, Type)

anti_srs_sawgrass_2 <-
  anti_join(FCE1070_data,
            FCE1071_data,
            by = c("SITENAME", "Date", "Type")) %>%
  select(SITENAME, Date, Type)

# Antijoin the two TS datasets by SiteName, Date, and Type to ensure all records can join
anti_ts_sawgrass_1 <-
  anti_join(FCE1082_data,
            FCE1083_data,
            by = c("SITENAME", "Date", "Type")) %>%
  select(SITENAME, Date, Type)

anti_ts_sawgrass_2 <- 
  anti_join(FCE1083_data,
            FCE1082_data,
            by = c("SITENAME", "Date", "Type")) %>%
  select(SITENAME, Date, Type)


# The TS sites appear to fail joining because the 2004 data have different months (March vs April) at TS4 and TS5,
# and some TS site records fail to join because the CN records list something other than the 1st of the month

# create year and month columns for all the FCE datasets
FCE1070_data <- FCE1070_data %>%
  mutate(Year = lubridate::year(Date),
         Month = lubridate::month(Date))

FCE1071_data <- FCE1071_data %>%
  mutate(Year = lubridate::year(Date),
         Month = lubridate::month(Date))


FCE1082_data <- FCE1082_data %>%
  mutate(Year = lubridate::year(Date),
         Month = lubridate::month(Date))

FCE1083_data <- FCE1083_data %>%
  mutate(Year = lubridate::year(Date),
         Month = lubridate::month(Date))
  
# Try to antijoin the TS datasets by SiteName, Year, Month, and Type to ensure all records can join
anti_ts_sawgrass_3 <-
  anti_join(FCE1082_data,
            FCE1083_data,
            by = c("SITENAME", "Year", "Month", "Type")) %>%
  select(SITENAME, Year, Month, Type)

# Filter FCE1282 data so that it only contains records from 2004
FCE1082_data_2004 <- FCE1082_data %>%
  filter(Year == 2004)

# Filter FCE1083 data so that it only contains records from 2004
FCE1083_data_2004 <- FCE1083_data %>%
  filter(Year == 2004)

# Correct the month for TS/Ph4 and TS/Ph5 in FCE1082_data_2004 to 4 if year is 2004
FCE1082_data_corrected <- FCE1082_data %>%
  mutate(Month = case_when(
    SITENAME == "TS/Ph4" & Year == 2004 ~ 4,
    SITENAME == "TS/Ph5" & Year == 2004 ~ 4,
    TRUE ~ Month
  )) %>%
  # Drop RecordNum column
  select(-RecordNum)

anti_ts_sawgrass_4 <-
  anti_join(FCE1082_data_corrected,
            FCE1083_data,
            by = c("SITENAME", "Year", "Month", "Type")) %>%
  select(SITENAME, Year, Month, Type)

# Rowbind the TP data from SRS and TS
sawgras_tp_all_sites <- rbind(FCE1071_data,
                              FCE1082_data_corrected) %>%
  #change -9999 values to NA
  mutate(`ugP/g` = ifelse(`ugP/g` == -9999, NA, `ugP/g`))
# Average the TN values by site, date, and type
sawgras_tp_all_sites <- sawgras_tp_all_sites %>%
  group_by(SITENAME, Date, Type) %>%
  summarise(`ugP/g` = mean(`ugP/g`, na.rm = TRUE), .groups = 'drop')

# Remove the RecordNum column from FCE1083_data
FCE1083_data <- FCE1083_data %>%
  select(-RecordNum)

# Rowbind the TN data from SRS and TS
sawgrass_tn_all_sites <- rbind(FCE1070_data,
                              FCE1083_data) %>%
  #change -9999 values to NA
  mutate(`mgN/g` = ifelse(`mgN/g` == -9999, NA, `mgN/g`)) %>%
  # Average the TN values by site, date, and type
  group_by(SITENAME, Date, Type) %>%
  summarise(`mgN/g` = mean(`mgN/g`, na.rm = TRUE), .groups = 'drop')

# Create plot for TP at each site
sawgrass_tp_plot <- sawgras_tp_all_sites %>%
  ggplot(aes(x = Date, y = `ugP/g`, color = Type)) +
  geom_line() +
  labs(title = "Sawgrass TP Across Transects",
       x = "Date",
       y = "TP (ug/g)") +
  theme_minimal()

# Create plot for TP at each site, and group by site
sawgrass_tp_plot_grouped <- sawgras_tp_all_sites %>%
  ggplot(aes(x = Date, y = `ugP/g`, color = SITENAME, fill = Type)) +
  geom_line() +
  labs(title = "Sawgrass TP Across Transects (Grouped by Site and Type)",
       x = "Date",
       y = "TP (ug/g)") +
  theme_minimal()


interactive_tp_plot <- ggplotly(sawgrass_tp_plot_grouped)

# Create plot for TN at each site, and group by site
sawgrass_tn_plot_grouped <- sawgrass_tn_all_sites %>%
  ggplot(aes(x = Date, y = `mgN/g`, color = SITENAME, fill = Type)) +
  geom_line() +
  labs(title = "Sawgrass TN Across Transects (Grouped by Site and Type)",
       x = "Date",
       y = "TN (mg/g)") +
  theme_minimal()

interactive_tn_plot <- ggplotly(sawgrass_tn_plot_grouped)

