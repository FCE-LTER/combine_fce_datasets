# Load libraries
library(readr)
library(EDIutils)
library(tools)
library(dplyr)
library(purrr)

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
  filter(!(full_package_id %in% all_res$packageid))



# For each row in packages_updated, get the associated data entityID and entityName 
results_df <- map_dfr(packages_updated$full_package_id, function(pkg_id) {
  read_data_entity_names(pkg_id) %>%
    mutate(full_package_id = pkg_id)   # keep track of source package
})

# For each updated package, get the entity names
results_list <- lapply(packages_updated$full_package_id, read_data_entity_names)

results_list_df <- as.data.frame(results_list)

entity_names_df <- map_dfr(fce_package_list$full_package_id, function(entity_names) {
  read_data_entity_names(entity_names) %>%
    mutate(full_package_id = entity_names)   # add the package id
}) %>%
  # filter full_package_id against packages_updated
  filter(full_package_id %in% packages_updated$full_package_id)


# Loop through each package + entityName and read the raw data
raw_data_list <- pmap(
  list(entity_names_df$full_package_id, entity_names_df$entityId),
  function(pkg_id, entity) {
    read_data_entity(pkg_id, entity)
  }
)


# Name the list by package + entityName so you can track them
names(raw_data_list) <- paste(entity_names_df$entityName, sep = "_") %>%
  #Remove ".csv" if present for consistency. We will add it back later
  file_path_sans_ext()

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
  mutate(full_package_id = packageid) %>%
  select(package,
         full_package_id)

# Write fce_package_list_updated to file, replacing package_list.csv
write_csv(fce_package_list_updated,
          "data/raw/package_list.csv")

