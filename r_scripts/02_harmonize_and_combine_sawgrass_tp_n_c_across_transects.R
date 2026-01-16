# Load libraries
library(readr) 
library(dplyr)
library(plotly)
library(lubridate)
library(stringr)
library(viridis)


##### Load and harmonize the raw data files #####

# not yet working with biomasss data
# SRS_saw_biomass <- read_csv("data/raw/LT_PP_Grahl_001.csv") 

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

# Harmonize the column names for the TS datasets by changing "TypeTP" to "Type" in TS_P_raw data
TS_P_renamed <- TS_P_raw %>%
  rename(Type = TypeTP)


# The TS sites CN and TP data will fail joining because the 2004 data have different months (March vs April) at TS4 and TS5,
# and some TS site records fail to join because the CN records list something other than the 1st of the month

# create year and month columns for the TS FCE datasets
TS_P_temporal <- TS_P_renamed %>%
  mutate(Year = lubridate::year(Date),
         Month = lubridate::month(Date))

TS_N_C_temporal <- TS_N_C_raw %>%
  mutate(Year = lubridate::year(Date),
         Month = lubridate::month(Date))

# Correct the month for TS/Ph4 and TS/Ph5 in TS_P_temporal_2004 to 4 if year is 2004
TS_P_temporal_corrected <- TS_P_temporal %>%
  mutate(Month = case_when(
    SITENAME == "TS/Ph4" & Year == 2004 ~ 4,
    SITENAME == "TS/Ph5" & Year == 2004 ~ 4,
    TRUE ~ Month
  )) %>%
  # Update Dates to follow the convention using 1st day of each month
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
  
# Rowbind the TP data from SRS and TS
sawgrass_tp_all_sites <- rbind(SRS_P_raw,
                               TS_P_temporal_corrected) %>%
  #change -9999 values to NA
  mutate(`ugP/g` = ifelse(`ugP/g` == -9999, NA, `ugP/g`))

# Rowbind the TN and TC data from SRS and TS
sawgrass_n_c_all_sites <- rbind(SRS_N_C_raw,
                                TS_N_C_temporal_corrected) %>%
  #change -9999 values to NA
  mutate(`mgN/g` = ifelse(`mgN/g` == -9999, NA, `mgN/g`),
         `mgC/g` = ifelse(`mgC/g` == -9999, NA, `mgN/g`))


# Note, cbind used after verifying sample row order aligned correctly
srs_all_nutrients <- cbind(SRS_P_raw,
                           SRS_N_C_raw %>%
                             select(-c(SITENAME,
                                       Date,
                                       Type))
                           )

ts_all_nutrients <- cbind(TS_P_temporal_corrected,
                          TS_N_C_temporal_corrected %>%
                            select(-c(SITENAME,
                                      Date,
                                      Type)))


all_sites_all_nutrients <- rbind(srs_all_nutrients,
                                 ts_all_nutrients)

# Average the TP values by site, date, and type
sawgrass_tp_all_sites_avg <- sawgrass_tp_all_sites %>%
  group_by(SITENAME, Date, Type) %>%
  summarise(`mean_ugP/g` = mean(`ugP/g`, na.rm = TRUE), .groups = 'drop') %>%
  mutate(`mean_ugP/g` = round(`mean_ugP/g`, 3))

# Assign habitat types to sites
habitats <- sawgrass_tp_all_sites %>%
  distinct(SITENAME) %>%
  mutate(Habitat = case_when(SITENAME %in% c("SRS1b",
                                             "SRS1c",
                                             "SRS1d",
                                             "SRS2",
                                             "SRS3",
                                             "TS/Ph1b",
                                             "TS/Ph2",
                                             "TS/Ph3")
                             ~ "Marsh",
                             SITENAME %in% c("TS/Ph4",
                                             "TS/Ph5",
                                             "TS/Ph6b")
                             ~ "Ecotone"),
         Transect = case_when(str_detect(SITENAME, "SRS")
                              ~ "SRS",
                              str_detect(SITENAME, "TS")
                              ~ "TS")) %>%
  mutate(Transect_Habitat = paste(Transect,
                                  Habitat,
                                  sep = "_"))

sawgrass_tp_by_habitat <- sawgrass_tp_all_sites %>%
  left_join(.,
            habitats,
            by = "SITENAME") %>%
  group_by(Transect_Habitat, Date, Type) %>%
  summarise(`mean_ugP/g` = mean(`ugP/g`, na.rm = TRUE), .groups = 'drop') %>%
  mutate(Transect_Habitat_Type = paste(Transect_Habitat,
                                       Type,
                                       sep = "_"),
         `mean_ugP/g` = round(`mean_ugP/g`, 3))


  

sawgrass_tn_all_sites_avg <- sawgrass_n_c_all_sites %>%
  # Average the TN values by site, date, and type
  group_by(SITENAME, Date, Type) %>%
  summarise(`mean_mgN/g` = mean(`mgN/g`, na.rm = TRUE), .groups = 'drop') %>%
  mutate(`mean_mgN/g` = round(`mean_mgN/g`, 2))

sawgrass_tn_by_habitat <- sawgrass_n_c_all_sites %>%
  left_join(.,
            habitats,
            by = "SITENAME") %>%
  group_by(Transect_Habitat, Date, Type) %>%
  summarise(`mean_mgN/g` = mean(`mgN/g`, na.rm = TRUE), .groups = 'drop') %>%
  mutate(Transect_Habitat_Type = paste(Transect_Habitat,
                                       Type,
                                       sep = "_"),
         `mean_mgN/g` = round(`mean_mgN/g`, 3))


sawgrass_tc_all_sites_avg <- sawgrass_n_c_all_sites %>%
  # Average the TN values by site, date, and type
  group_by(SITENAME, Date, Type) %>%
  summarise(`mean_mgC/g` = mean(`mgC/g`, na.rm = TRUE), .groups = 'drop') %>%
  mutate(`mean_mgC/g` = round(`mean_mgC/g`, 2))

sawgrass_tc_by_habitat <- sawgrass_n_c_all_sites %>%
  left_join(.,
            habitats,
            by = "SITENAME") %>%
  group_by(Transect_Habitat, Date, Type) %>%
  summarise(`mean_mgC/g` = mean(`mgC/g`, na.rm = TRUE), .groups = 'drop') %>%
  mutate(Transect_Habitat_Type = paste(Transect_Habitat,
                                       Type,
                                       sep = "_"),
         `mean_mgC/g` = round(`mean_mgC/g`, 2))

sawgrass_all_sites_nutrient_avg_output <- sawgrass_tc_all_sites_avg %>%
  inner_join(.,
             sawgrass_tn_all_sites_avg,
             by = c("SITENAME",
                    "Date",
                    "Type")) %>%
  left_join(.,
            sawgrass_tp_all_sites_avg,
            habitats,
            by = c("SITENAME",
                   "Date",
                   "Type"))


sawgrass_all_nutrients_by_habitat_output <- sawgrass_tc_by_habitat %>%
  inner_join(.,
             sawgrass_tn_by_habitat,
             by = c("Transect_Habitat",
                    "Transect_Habitat_Type",
                    "Date",
                    "Type")) %>%
  inner_join(.,
             sawgrass_tp_by_habitat,
             by = c("Transect_Habitat",
                    "Transect_Habitat_Type",
                    "Date",
                    "Type")) %>%  
  select(Transect_Habitat,
         Date,
         Type,
         Transect_Habitat_Type,
         `mean_mgC/g`,
         `mean_mgN/g`,
         `mean_ugP/g`
         )

##### Output combined data files ##### 
write_csv(all_sites_all_nutrients, "data/final/sawgrass_nutrients_all_sites_and_replicates.csv") 

write_csv(sawgrass_all_sites_nutrient_avg_output, "data/final/sawgrass_mean_nutrients_by_site.csv") 

write_csv(sawgrass_all_nutrients_by_habitat_output, "data/final/sawgrass_mean_nutrients_by_transect_habitat.csv") 



#### Create plots ####

# Create plot for TP at each site
sawgrass_tp_plot <- sawgrass_tp_all_sites_avg %>%
  ggplot(aes(x = Date, y = `mean_ugP/g`, color = Type)) +
  geom_line() +
  labs(title = "Sawgrass TP Across Transects",
       x = "Date",
       y = "TP (ug/g)") +
  theme_minimal()

# Create plot for TP at each site, and group by site
# This is too busy, so we will instead try to plot by transect and habitat
# sawgrass_tp_site_grouped <- sawgrass_tp_all_sites_avg %>%
#   ggplot(aes(x = Date, y = `mean_ugP/g`, color = SITENAME, fill = Type)) +
#   geom_line() +
#   scale_color_viridis(discrete = TRUE, option = "D") +
#   labs(title = "Mean Sawgrass TP Across Transects (Grouped by Site and Type)",
#        x = "Date",
#        y = "TP (ug/g)") +
#   theme_minimal()
# 
# interactive_tp_plot <- ggplotly(sawgrass_tp_site_grouped)

# Plot by habitat, grouping by Transect, Habitat, and Type
sawgrass_tp_habitat_grouped <- sawgrass_tp_by_habitat %>%
  ggplot(aes(x = Date, y = `mean_ugP/g`, color = Transect_Habitat_Type)) +
  geom_line() +
  scale_color_viridis(discrete = TRUE, option = "D") +
  labs(title = "Mean Aboveground and Belowground Sawgrass TP Across Transect Habitats",
       x = "Date",
       y = "TP (ug/g)") +
  theme_minimal()

interactive_tp_by_habitat_plot <- ggplotly(sawgrass_tp_habitat_grouped)

# Plot TN grouping by Transect, Habitat, and Type
sawgrass_tn_habitat_grouped <- sawgrass_tn_by_habitat %>%
  ggplot(aes(x = Date, y = `mean_mgN/g`, color = Transect_Habitat_Type)) +
  geom_line() +
  scale_color_viridis(discrete = TRUE, option = "D") +
  labs(title = "Mean Sawgrass Aboveground and Belowground TN Across Transect Habitats",
       x = "Date",
       y = "TN (mg/g)") +
  theme_minimal()

interactive_tn_by_habitat_plot <- ggplotly(sawgrass_tn_habitat_grouped)

# Plot TC grouping by Transect, Habitat, and Type
sawgrass_tn_habitat_grouped <- sawgrass_tc_by_habitat %>%
  ggplot(aes(x = Date, y = `mean_mgC/g`, color = Transect_Habitat_Type)) +
  geom_line() +
  scale_color_viridis(discrete = TRUE, option = "D") +
  labs(title = "Mean Sawgrass Aboveground and Belowground TN Across Transect Habitats",
       x = "Date",
       y = "TC (mg/g)") +
  theme_minimal()

interactive_tc_by_habitat_plot <- ggplotly(sawgrass_tn_habitat_grouped)


plots_arranged <- subplot(interactive_tp_by_habitat_plot,
                          style(interactive_tn_by_habitat_plot,
                                showlegend = FALSE),
                          style(interactive_tc_by_habitat_plot,
                                showlegend = FALSE),
                          shareX = TRUE,
                          titleY = TRUE,
                          nrows =3) %>%
  layout(title = 'Mean Aboveground and Belowground Sawgrass Nutrients Across Transect Habitats')

# Create "plots" folder if it doesn't already exist
if (!dir.exists("plots")) {
  dir.create("plots")
}


# Write interactive plots to /plots folder as html files
htmlwidgets::saveWidget(interactive_tc_by_habitat_plot, "plots/sawgrass_tc_across_transects.html")
htmlwidgets::saveWidget(interactive_tn_by_habitat_plot, "plots/sawgrass_tn_across_transects.html")
htmlwidgets::saveWidget(interactive_tp_by_habitat_plot, "plots/sawgrass_tp_across_transects.html")
htmlwidgets::saveWidget(plots_arranged, "plots/sawgrass_nutrients_across_transects.html")

