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
FCE1071_data <- readr::read_csv(file = raw,
                                col_types = cols(.default = col_character()))

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

# Save the raw data files to the raw directory, using entityName as the file name
# Use write.table with eol = "\n" to maintain same line endings in files.
# Keeping the line endings the same as in the published data will allow
# checksums to be used for file verification against what was published in EDI. 

write.table(FCE1070_data,
            eol = "\n",
            quote = F,
            sep = ",",
            row.names = F,
            "data/raw/LT_ND_Grahl_004.csv")

# Write data to file
write.table(FCE1071_data,
            eol = "\n",
            quote = F,
            sep = ",",
            row.names = F,
            "data/raw/LT_ND_Grahl_005.csv")

write.table(FCE1082_data,
            eol = "\n",
            quote = F,
            sep = ",",
            row.names = F,
            "data/raw/LT_ND_Rubio_004.csv")

write.table(FCE1083_data,
            eol = "\n",
            quote = F,
            sep = ",",
            row.names = F,
            "data/raw/LT_ND_Rubio_005.csv")
