# Load libraries
library(readr)
library(EDIutils)
library(dplyr)
library(purrr)
# library(keyring) load if adding or loading an API Key outside of GitHub Actions

# Load API key from GitHub Secrets
login(key = Sys.getenv("API_KEY_READ_FROM_EDI"))

# See below if running script locally, rather than in GitHub Actions, 

#### Create API key and load it into system environment ####

# Note: See the following link to learn about EDI's
# Identity and Access Manager and how to create an EDI API key:
# https://edirepository.org/resources/iam

# If you created an API key, you can save it on your local computer's credential
# storage by running the commented out code below. You should only need to run these
# lines once per user/per computer as long as the API key is current
#
## First create a keyring, you will be prompted to create a password for the keyring.
## Save the keyring password somewhere safe.
#keyring_create(keyring = "my_keyring")
#
## Add the API Key. You will be prompted to enter the API as a password
# key_set(service = "edi",
#        username = "edi_im_read_api_key",
#        keyring = "my_keyring")
#
## Login with the API key
# login(key = key_get(service = "edi",
#                     username = "edi_im_read_api_key",
#                     keyring = "my_keyring"))
#
## Relock the key
#keyring_lock("my_keyring")

# Read in local list of packages of interest
fce_package_list <- read_csv("data/raw/package_list.csv")

# Get current list of packageids in EDI 
query <- 'q=scope:knb-lter-fce&fl=packageid,title,doi'

all_res <- search_data_packages(query)

all_res

all_res_with_main_id <- all_res %>%
  # Mutate package column by extracting everything before the last "." in
  # packageid
  mutate(package = sub("\\.[^.]*$", "", packageid))

# Get list of packages fce_package_list that have been updated in EDI
packages_updated <- fce_package_list %>%
  filter(!(current_local_rev_full_package_id %in% all_res$packageid)) %>%
  inner_join(all_res_with_main_id,
             by ="package") %>%
  rename(new_rev_package_id = packageid)



# For each row in packages_updated, get the associated data entityID and entityName 
results_df <- map_dfr(packages_updated$new_rev_package_id, function(pkg_id) {
  read_data_entity_names(pkg_id) %>%
    mutate(full_package_id = pkg_id)   # keep track of source package
})

# For each updated package, get the entity names
results_list <- lapply(packages_updated$new_rev_package_id, read_data_entity_names)

results_list_df <- as.data.frame(results_list)

entity_names_df <- map_dfr(packages_updated$new_rev_package_id, function(entity_names) {
  read_data_entity_names(entity_names) %>%
    mutate(new_rev_package_id = entity_names)   # add the package id
})


# Loop through each package + entityName and read the raw data
raw_data_list <- pmap(
  list(entity_names_df$new_rev_package_id, entity_names_df$entityId),
  function(pkg_id, entity) {
    read_data_entity(pkg_id, entity)
  }
)


# Name the list by package + entityName so you can track them
names(raw_data_list) <- paste(entity_names_df$entityName, sep = "_") %>%
  #Remove file extension  in name if present for consistency. We will add it back later
  sub(pattern = "(.*)\\..*$", replacement = "\\1", .)

#Read each raw data element into a data frame
raw_data_frames <- map(raw_data_list, ~ read_csv(file = .x, col_types = cols(.default = col_character())))


# Loop through list and write each data frame to disk
for (i in seq_along(raw_data_frames)) {
  # Create a clean filename from the list name
  filename <- paste0("data/raw/", names(raw_data_frames)[i], ".csv")
  
  # Write to disk
  write.table(
    raw_data_frames[[i]],
    file = filename,
    eol = "\n",
    quote = F,
    sep = ",",
    row.names = F
    )
}

# Update the fce_package_list file with the packageid from all_res_with_main_id
fce_package_list_updated <- fce_package_list %>%
  left_join(all_res_with_main_id,
            by = c("package" = "package")) %>%
  mutate(current_local_rev_full_package_id = packageid,
         current_local_doi = doi) %>%
  select(package,
         current_local_rev_full_package_id,
         current_local_doi)


# Get the updated citations, and output them into a tibble.
fce_citations_df <- map_dfr(
  fce_package_list_updated$current_local_rev_full_package_id,
  ~ tibble(
    local_fce_citation = read_data_package_citation(.x),
    current_local_rev_full_package_id = .x
  )
)


# Write fce_package_list_updated to file,
# replacing last version of package_list.csv
write_csv(fce_package_list_updated,
          "data/raw/package_list.csv")

# Write the updated citations to file
write_csv(fce_citations_df,
          "data/raw/fce_local_citations.csv")
