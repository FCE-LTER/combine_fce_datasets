# Load libraries
library(readr) 
library(dplyr)
library(plotly)

# Load the raw data files
SRS_saw_biomass <- read_csv("data/raw/LT_PP_Grahl_001.csv")
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

# create year and month columns for all the FCE datasets
SRS_N_C_temporal <- SRS_N_C_raw %>%
  mutate(Year = lubridate::year(Date),
         Month = lubridate::month(Date))

SRS_P_temporal <- SRS_P_raw %>%
  mutate(Year = lubridate::year(Date),
         Month = lubridate::month(Date))


TS_P_temporal <- TS_P_renamed %>%
  mutate(Year = lubridate::year(Date),
         Month = lubridate::month(Date))

TS_N_C_temporal <- TS_N_C_raw %>%
  mutate(Year = lubridate::year(Date),
         Month = lubridate::month(Date)) %>%
  # Drop RecordNum column
  select(-RecordNum)

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
  # Drop RecordNum column
  select(-RecordNum)

anti_ts_sawgrass_4 <-
  anti_join(TS_P_temporal_corrected,
            TS_N_C_temporal,
            by = c("SITENAME", "Year", "Month", "Type")) %>%
  select(SITENAME, Year, Month, Type)

# Rowbind the TP data from SRS and TS
sawgras_tp_all_sites <- rbind(SRS_P_temporal,
                              TS_P_temporal_corrected) %>%
  #change -9999 values to NA
  mutate(`ugP/g` = ifelse(`ugP/g` == -9999, NA, `ugP/g`))
# Average the TN values by site, date, and type
sawgras_tp_all_sites <- sawgras_tp_all_sites %>%
  group_by(SITENAME, Date, Type) %>%
  summarise(`ugP/g` = mean(`ugP/g`, na.rm = TRUE), .groups = 'drop')

# Rowbind the N and C data from SRS and TS
sawgrass_n_c_all_sites <- rbind(SRS_N_C_temporal,
                               TS_N_C_temporal) %>%
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

# Create plot for N at each site, and group by site
sawgrass_n_plot_grouped <- sawgrass_n_c_all_sites %>%
  ggplot(aes(x = Date, y = `mgN/g`, color = SITENAME, fill = Type)) +
  geom_line() +
  labs(title = "Sawgrass N Across Transects (Grouped by Site and Type)",
       x = "Date",
       y = "N (mg/g)") +
  theme_minimal()

interactive_n_plot <- ggplotly(sawgrass_n_plot_grouped)

# Create faceted plot for N and C at each site, and group by site
sawgrass_n_c_plot_grouped <- sawgrass_n_c_all_sites %>%
  ggplot(aes(x = Date, y = `mgN/g`, color = SITENAME, fill = Type)) +
  geom_line() +
  labs(title = "Sawgrass N Across Transects (Grouped by Site and Type)",
       x = "Date",
       y = "N (mg/g)") +
  #facet_wrap(~Type) +
  theme_minimal()

interactive_n_c_plot <- ggplotly(sawgrass_n_c_plot_grouped)

# Create "plots" folder if it doesn't already exist
if (!dir.exists("plots")) {
  dir.create("plots")
}


# Write interactive plots to /plots folder as html files
htmlwidgets::saveWidget(interactive_tp_plot, "plots/sawgrass_tp_across_transects.html")
htmlwidgets::saveWidget(interactive_n_plot, "plots/sawgrass_n_across_transects.html")
