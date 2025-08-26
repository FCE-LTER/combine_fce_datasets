# Load libraries
library(tidyverse) 
library(EDIutils)
library(tools)

scope <- "knb-lter-fce"
FCE1071_identifier <- "1071"
revision <- list_data_package_revisions(scope, FCE1071_identifier, filter = "newest")
packageId <- paste(scope, FCE1071_identifier, revision, sep = ".")

# Get list of data files in the last revision of your package in EDI
res <- read_data_entity_names(packageId)

res

entityId <- res$entityId

# Note: checksum via EDIutils function is SHA-1, not md5.
edi_chcksum <- read_data_entity_checksum(packageId,
                                         entityId,
                                         env = "production")

# Get SHA-1 checksum of LT_ND_Grahl_005.csv in data/raw
sha_1_local_1071 <- as.character(openssl::sha1(file("data/raw/LT_ND_Grahl_005.csv"))
)


# Check if checksum has changed
sha_1_local_1071 == edi_chcksum
