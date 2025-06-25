# ==========================================================================================================================================
# Script Name: Ento Analysis and Plots
# Author: Eniola Bamgboye, ebamgboye@luc.edu
# Edited: Grace Legris (gracebea@gmail.com), 11/20/24
# Purpose: Conduct analyses of data in Ibadan, compare wet/dry season data, create figures for manuscript
# ==========================================================================================================================================

# clear current workspace
rm(list=ls())

source("load_path.R")

## =========================================================================================================================================
### Data Prep
## =========================================================================================================================================

## -----------------------------------------------------------------------------------------------------------------------------------------
### Read in Excel files
## -----------------------------------------------------------------------------------------------------------------------------------------

# list all Excel files in the specified directory
files <- list.files(
  path = EntoDat,
  pattern = ".xlsx",
  full.names = TRUE,
  recursive = FALSE # do not search subdirectories
)

# read all Excel files into a list of data frames
excel_dfs <- sapply(files, readxl::read_xlsx, simplify = FALSE)

# display the names of the data frames in the list
names(excel_dfs)

# combine the fourth, fifth, and sixth data frames into one (all CDC light trap data)
cdc <- rbind(excel_dfs[[5]], excel_dfs[[6]], excel_dfs[[7]])

# filter for only Ibadan data (remove Kano data)
cdc <- cdc %>%
  dplyr::filter(City == "Ibadan")

# save dataset
write_xlsx(cdc, file.path(EntoDat, "cdc_ibadan_dry.xlsx"))

cdc <- read_excel(file.path(EntoDat, "cdc_ibadan_dry.xlsx"))

## =========================================================================================================================================
### Indoor Transmission: Ibadan
## =========================================================================================================================================

# filter the dataset for indoor CDC collections and summarize by group
indoor_cdc <- cdc %>%
  dplyr::filter(Location == "Indoor") %>% 
  group_by(`Settlement Classification`, `Time of Collection`) %>%
  summarise(total_mosquitoes = sum(`Total Anopheles`, na.rm = TRUE)) %>% # calculate total mosquitoes
  ungroup()

# create a plot for hourly indoor biting of anopheles mosquitoes (Ibadan only)
indoor_anopheles_plot <- ggplot(data = indoor_cdc, aes(
  x = `Time of Collection`, 
  y = total_mosquitoes, 
  group = `Settlement Classification`, 
  colour = `Settlement Classification`)) +
  scale_x_discrete(limits = c("6-7pm", "7-8pm", "8-9pm", "9-10pm", "10-11pm", "11-12am", "12-1am", "1-2am", "2-3am", "3-4am", "4-5am", "5-6am")) + 
  geom_point() + 
  labs(
    y = "Total Number of Anopheles \nMosquitos Caught per Hour",
    x = "Time of Collection"
  ) +
  geom_line() + 
  ggtitle("Hourly Indoor Biting") + 
  geom_point(size = 3.0) + 
  theme(plot.title = element_text(size = 12)) + 
  theme_manuscript() + 
  theme(
    legend.position = c(.27, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    axis.text.x = element_text(size = 10),
    legend.key.size = unit(0.8, "lines"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    legend.background = element_rect(color = "black", size = 0.5)
  )
indoor_anopheles_plot

# save the indoor plot as a PDF file
ggsave(file.path(NewFigDir, '_indoor_cdc_ibadan.pdf'), plot = indoor_anopheles_plot, width = 8, height = 6)

## =========================================================================================================================================
### Outdoor Transmission: Ibadan
## =========================================================================================================================================

# filter the dataset for outdoor CDC collections and summarize by group
outdoor_cdc <- cdc %>%
  dplyr::filter(Location == "Outdoor") %>%
  group_by(`Settlement Classification`, `Time of Collection`) %>%
  summarise(total_mosquitoes = sum(`Total Anopheles`)) %>% # calculate total mosquitoes
  ungroup()

# create a plot for hourly outdoor biting of anopheles mosquitoes
outdoor_anopheles_plot <- ggplot(data = outdoor_cdc, aes(
  x = `Time of Collection`, 
  y = total_mosquitoes, 
  group = `Settlement Classification`, 
  colour = `Settlement Classification`
)) + 
  scale_x_discrete(limits = c("6-7pm", "7-8pm", "8-9pm", "9-10pm", "10-11pm", "11-12am", "12-1am", "1-2am", "2-3am", "3-4am", "4-5am", "5-6am")) + 
  geom_point() + 
  labs(
    y = "Total Number of Anopheles \nMosquitos Caught per Hour",
    x = "Time of Collection"
  ) + 
  geom_line() + 
  ggtitle("Hourly Outdoor Biting") + 
  geom_point(size = 3.0) + 
  theme(plot.title = element_text(size = 12)) + 
  theme_manuscript() + 
  theme(
    legend.position = c(.27, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    axis.text.x = element_text(size = 10),
    legend.key.size = unit(0.8, "lines"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    legend.background = element_rect(color = "black", size = 0.5)
  )
outdoor_anopheles_plot

# save the outdoor plot as a PDF file
ggsave(file.path(NewFigDir, '_outdoor_cdc_ibadan.pdf'), plot = outdoor_anopheles_plot, width = 8, height = 6)

# remove x axis label from indoor biting plot
indoor_anopheles_plot <- indoor_anopheles_plot + theme(axis.title.x = element_blank())

# arrange indoor biting and outdoor biting plots into a grid
hourly_biting_plots <- grid.arrange(indoor_anopheles_plot, outdoor_anopheles_plot, nrow = 2, ncol = 1)

# save combined plots as .pdf
ggsave(file.path(NewFigDir, '_bitingrate_cdc_ibadan.pdf'), plot = hourly_biting_plots, width = 8, height = 6)

## =========================================================================================================================================
### FIGURE 1 - MAPS (Wards Sampled and PSC Households Visited)
## =========================================================================================================================================

## -----------------------------------------------------------------------------------------------------------------------------------------
### Ibadan Wards Sampled
## -----------------------------------------------------------------------------------------------------------------------------------------

# read the shapefile for Ibadan metro area and correct ward name if necessary
ibadan.shp <- st_read(file.path(shapepath, "ibadan_metro_ward_fiveLGAs", "Ibadan_metro_fiveLGAs.shp")) %>%
  mutate(WardName = ifelse(WardName == "Oranyan" & LGACode == "31007", "Oranyan_7", WardName))

# assign variable based on whether wards were sampled or not (will be used to color map)
ibadan.shp$ward_color <- ifelse(ibadan.shp$WardName %in% c("Agugu", "Olopomewa", "Challenge"), 
                                "Sampled", "Unsampled")

# extract ward names and save them to a CSV
ib_w <- ibadan.shp$WardName
#write.csv(ib_w, file.path(NuDir, "ib_wards.csv"), row.names = FALSE)

# plot: ibadan metro area showing selected wards
wards_ibadan_plot <- ggplot(ibadan.shp) +
  geom_sf(aes(fill = ward_color)) +
  scale_fill_manual(
    values = c("Sampled" = "#6699CC", "Unsampled" = "#F1F1F1"),
    na.value = "transparent"
  ) +
  # geom_text_repel(
  #   data = ibadan.shp,
  #   aes(label = WardName, geometry = geometry), color = "black",
  #   stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1
  # ) +
  map_theme() +
  labs(
    title = "Wards Selected for Entomological Survey",
    fill = NULL
  ) +
  coord_sf() +
  theme(legend.title = element_blank())

wards_ibadan_plot

# save plot
ggsave(paste0(ResultDir, "/", Sys.Date(), "_wards_sampled_ibadan.pdf"), wards_ibadan_plot, width = 8, height = 6)

## -----------------------------------------------------------------------------------------------------------------------------------------
### PSC: Ibadan
## -----------------------------------------------------------------------------------------------------------------------------------------

# combine relevant dataframes for Pyrethrum Spray Catches (PSC)
psc <- rbind(excel_dfs[[8]], excel_dfs[[9]], excel_dfs[[10]])

# filter PSC data for Oyo state and aggregate by settlement classification and month
psc_grouped <- psc %>%
  dplyr::filter(State == "Oyo") %>%
  group_by(`Settlement Classification`, Month) %>%
  summarise(total_mosquitoes = sum(`An. Gambiae`, na.rm = TRUE)) %>%
  ungroup()

# plot: locations of PSC data collection in Ibadan
psc_locations_plot <- ggplot(ibadan.shp) +
  geom_sf(fill = "#F1F1F1") +
  geom_point(
    data = dplyr::filter(psc, State == "Oyo"), 
    mapping = aes(x = Longitude, y = Latitude), 
    shape = 21,
    fill = "red",
    color = "black",
    size = 1.5,
    stroke = 0.2
  ) +
  # geom_text_repel(
  #   data = ibadan.shp,
  #   aes(label = WardName, geometry = geometry), color = "black",
  #   stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1
  # ) +
  map_theme() +
  labs(
    title = "Households Visited for PSC Mosquito Collection"
  ) +
  coord_sf() + 
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )
psc_locations_plot

# save the plot
ggsave(paste0(NewFigDir, "/", Sys.Date(), "_locations_psc_ibadan.pdf"), psc_locations_plot, width = 8, height = 6)

## -----------------------------------------------------------------------------------------------------------------------------------------
### Combine Ward Plots into Grid
## -----------------------------------------------------------------------------------------------------------------------------------------

# arrange wards selected and PSC households visited plots into a grid
ward_maps <- grid.arrange(wards_ibadan_plot, psc_locations_plot, nrow = 1, ncol = 2)

# save combined plots as .pdf
ggsave(filename = paste0(ResultDir, "/", Sys.Date(), '_ward_maps.pdf'), plot = ward_maps, width = 12, height = 8)


## =========================================================================================================================================
### RELATIVE ABUNDANCE OF SPECIES ANALYSIS
# 1) Compile a species inventory from all datasets. 
# 2) Calculate the proportion of each species relative to the total mosquitoes collected. 
# 3) Compare species composition and relative abundance across seasons (wet vs. dry) and collection methods. 
## =========================================================================================================================================

## -----------------------------------------------------------------------------------------------------------------------------------------
### Data Cleaning/Prep
## -----------------------------------------------------------------------------------------------------------------------------------------

# change names of cdc and psc dfs to specify they are dry season
cdc_dry <- cdc %>%
  rename(settlement_type = `Settlement Classification`)
psc_dry <- psc %>%
  rename(settlement_type = `Settlement Classification`)

# load in wet season data and name dfs appropriately
cdc_wet <- readxl::read_xlsx(file.path(WetData, "cdc_wet_no_env_time.xlsx"))
psc_wet <- readxl::read_xlsx(file.path(WetData, "WET_SEASON_ENTO_COLLECTION_PSC_-_all_versions_-_labels_-_2024-08-12-21-20-23.xlsx"))

# rename variables in cdc_wet for consistency
cdc_wet_counts <- cdc_wet %>%
  mutate(method = "CDC") %>%
  mutate(Other = NA) %>%
  mutate(season = "wet") %>%
  rename(Anopheles = total_anopheles, An.gambiae = total_gambiae, An.funestus = total_funestus, Culicine = total_culicine,
         household_code = household_code_number, location = cdc_location, ) %>%
  dplyr::select(date, city, ward_name, settlement_type, household_code, method, season, location, Anopheles, An.gambiae, An.funestus, Culicine, Other)

# select relevant columns of psc_wet and rename for consistency with cdc_wet column names, filter out Kano data
psc_wet_counts <- psc_wet %>%
  mutate(method = "PSC") %>%
  mutate(location = NA) %>%
  mutate(Culicine = NA) %>%
  mutate(season = "wet") %>%
  rename(date = Date, city = City, ward_name = `Ward Name`, settlement_type = `Settlement Type`, 
         household_code = `Household Code/Number`, Anopheles = `Total Number of Anopheles`, 
         An.gambiae = `Total Number of Anopheles Gambiae`, An.funestus = `Total Number of Anopheles Funestus`, Other = `Total Number of Other Species`,
         fed_An.gambiae = `Number of Fed Gambiae`, unfed_An.gambiae = `Number of Unfed Gambiae`,
         fed_An.funestus = `Number of Fed Funestus`, unfed_An.funestus = `Number of Unfed Funestus`,
         fed_Other = `Number of Fed specie(Other)`, unfed_Other = `Number of Unfed specie(Other)`) %>%
  select(date, city, ward_name, settlement_type, household_code, method, season, location, 
         Anopheles, An.gambiae, An.funestus, Culicine, Other, fed_An.gambiae, unfed_An.gambiae,
         fed_An.funestus, unfed_An.funestus, fed_Other, unfed_Other) %>%
  dplyr::filter(psc_wet$City != "Kano") %>%
  mutate(date = as.Date(date)) # remove time from the date

# data cleaning for cdc_dry data
cdc_dry <- cdc_dry %>%
  mutate(`Type of Anopheles_1` = dplyr::recode(`Type of Anopheles_1`, 
                                               "Gambiens" = "An.gambiae", 
                                               "An. gambiense" = "An.gambiae", 
                                               "An. gambiae" = "An.gambiae", 
                                               "An. funestus" = "An.funestus"))

# new columns for An.gambiae and An.funestus
cdc_dry_counts <- cdc_dry %>%
  mutate(
    An.gambiae = ifelse(grepl("An.gambiae", `Type of Anopheles_1`), `Total Anopheles`, 0),
    An.funestus = ifelse(grepl("An.funestus", `Type of Anopheles_1`), `Total Anopheles`, 0)
  ) %>%
  mutate(Other = NA) %>%
  mutate(season = "dry") %>%
  rename(city = City, ward_name = `Ward Name`, day = Day, month = Month, year = Year, method = Method, location = Location,
         Anopheles = `Total Anopheles`, Culicine = `Total Culicine`, household_code = `Household Code`) %>%
  mutate(date = as.POSIXct(paste(year, month, day), format="%Y %B %d")) %>%
  dplyr::select(date, city, ward_name, settlement_type, household_code, method, season, location, Anopheles, An.gambiae, An.funestus, Culicine, Other)

# data cleaning for psc dry data
psc_dry_counts <- psc_dry %>%
  dplyr::filter(State == "Oyo") %>% 
  rename(city = City, ward_name = Ward, day = Day, month = Month, year = Year, method = Method, household_code = `Household Code`,
         An.gambiae = `An. Gambiae`, An.funestus = `An.Funestus`, Other = Others_1, fed_An.gambiae = `Number Fed Gambiae`, 
         unfed_An.gambiae = `Number Unfed Gambiae`, fed_An.funestus = `Number Fed. Funestus`, unfed_An.funestus = `Number Unfed Funestus`, 
         fed_Other = `Number Fed.Others_1`, unfed_Other = `Number Unfed Others_1`) %>%
  mutate(
    location = NA,
    Culicine = NA,
    season = "dry",
    month = match(month, month.name), # converts month name to numeric value
    date = as.POSIXct(paste(year, month, day), format="%Y %m %d"),
    Anopheles = An.gambiae + An.funestus
  ) %>%
  dplyr::select(
    date, city, ward_name, settlement_type, household_code, method, season, location, 
    Anopheles, An.gambiae, An.funestus, Culicine, Other, 
    fed_An.gambiae, unfed_An.gambiae, fed_An.funestus, unfed_An.funestus, fed_Other, unfed_Other
  )

# make household code columns same format (char)
psc_wet_counts$household_code <- as.character(psc_wet_counts$household_code)
cdc_wet_counts$household_code <- as.character(cdc_wet_counts$household_code)
cdc_dry_counts$household_code <- as.character(cdc_dry_counts$household_code)

## -----------------------------------------------------------------------------------------------------------------------------------------
### Combine the 4 Dfs - CDC dry, CDC wet, PSC dry, PSC wet
## -----------------------------------------------------------------------------------------------------------------------------------------

all_ento_data <- bind_rows(cdc_dry_counts, cdc_wet_counts, psc_dry_counts, psc_wet_counts)

# recode wet season erroneous "informal" settlement type values to "slum"
all_ento_data <- all_ento_data %>%
  mutate(
    settlement_type = case_when(
      season == "wet" & settlement_type == "Informal" ~ "Slum",
      TRUE ~ settlement_type)
  ) %>%
  mutate( # recode "Anopheles" var to match number of "An. gambiae" and "An. funestus" collected
    Anopheles = if_else(
      Anopheles < (An.gambiae + An.funestus),
      An.gambiae + An.funestus,
      Anopheles
    )
  )

# recode the one Zango ward to Challenge (it has household_code = 14085 which all belong in Challenge ward and are Formal)
all_ento_data <- all_ento_data %>% 
  mutate(settlement_type = case_when(
    ward_name == "Zango" ~ "Formal",
    TRUE ~ settlement_type
  )) %>% 
  mutate(ward_name = case_when(
    ward_name == "Zango" ~ "Challenge",
    TRUE ~ ward_name
  ))

# df to check work
# sometimes, Anopheles were collected but weren't able to be identified as An. gambiae or An. funestus, so Anopheles count is sometimes larger.
check_df <- all_ento_data %>%
  select(Anopheles, An.gambiae, An.funestus) %>%
  dplyr::filter(!(Anopheles == 0 & An.gambiae == 0 & An.funestus == 0))

# save this formatted df
write.xlsx(all_ento_data, file.path(EntoDat, "all_ento_dry_wet_data.xlsx"))

## -----------------------------------------------------------------------------------------------------------------------------------------
### Pie Chart Map of Species Composition Across Settlement Types Only
## -----------------------------------------------------------------------------------------------------------------------------------------

species_for_map <- all_ento_data %>%
  group_by(settlement_type) %>% 
  summarise(
    total_Anopheles = sum(Anopheles, na.rm = TRUE),
    total_An.gambiae = sum(An.gambiae, na.rm = TRUE),
    total_An.funestus = sum(An.funestus, na.rm = TRUE),
    total_Culicine = sum(Culicine, na.rm = TRUE),
    total_Other = sum(Other, na.rm = TRUE),
    total_NonVector = total_Culicine + total_Other
  ) %>%
  select(settlement_type, total_Anopheles, total_NonVector)


# exclude settlement type = NA
#species_for_map = subset(species_for_map, !(species %in% c("total_Culicine")))
species_for_map = subset(species_for_map, !(settlement_type %in% c(NA)))

# add names of wards
species_for_map <- species_for_map %>%
  mutate(
    ward_name = case_when(
      settlement_type ==  "Slum" ~ "Agugu",
      settlement_type == "Informal" ~ "Challenge",
      settlement_type ==  "Formal" ~ "Olopomewa",
      TRUE ~ settlement_type
    )
  ) %>%
  select(settlement_type, ward_name, total_Anopheles, total_NonVector)

# Merge species data with shapefile for the sampled wards
sampled_wards <- ibadan.shp %>%
  dplyr::filter(ward_color == "Sampled") %>%
  left_join(species_for_map, by = c("WardName" = "ward_name"))

# Extract centroids and include WardName
ward_centroids <- sampled_wards %>%
  st_centroid() %>%
  st_as_sf() %>% # Ensure it remains an sf object
  select(WardName) %>%
  cbind(st_coordinates(.))

# Prepare data for the pie charts
pie_data <- sampled_wards %>%
  st_drop_geometry() %>% # Drop geometry for non-spatial operations
  select(WardName, total_Anopheles, total_NonVector) %>%
  pivot_longer(cols = starts_with("total_"), names_to = "species", values_to = "count") %>%
  left_join(ward_centroids, by = "WardName")

# Rename species for clarity
pie_data$species <- gsub("total_", "", pie_data$species)

# make wide-format df for plotting
pie_data_wide <- pie_data %>%
  select(WardName, species, count, X, Y) %>%
  pivot_wider(
    names_from = species,
    values_from = count,
  )

testprop <- species_for_map %>%
  mutate(
    prop = total_Anopheles / total_NonVector
  )

piecolors = c("#e69598", "#85bdde")

# plot the map - can't get pie chart colors to apply so using adobe illustrator to fill them in
map_species_comp <- ggplot() +
  #geom_sf(data = ibadan.shp, aes(fill = ward_color), color = "black", alpha = 0.8) +
  geom_scatterpie(
    data = pie_data_wide,
    aes(x = X, y = Y, group = WardName),
    cols = c("Anopheles", "NonVector"),
    pie_scale = 4,
  ) +
  coord_equal() +
  scale_fill_manual(values = piecolors) +
  #scale_fill_manual(values = c("Sampled" = "#F1F1F1", "Unsampled" = "#F1F1F1")) +
  map_theme() +
  labs(
    title = "Species Distribution in Sampled Wards of Ibadan",
    fill = "Ward Type"
  ) +
  theme(plot.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.key.height = unit(1.5, "cm"),
        legend.key.width = unit(1.5, "cm"))
map_species_comp

# save as .pdf
ggsave(filename = paste0(ResultDir, "/", Sys.Date(), '_species_pie_map.pdf'), plot = map_species_comp, width = 12, height = 12)

## -----------------------------------------------------------------------------------------------------------------------------------------
### TABLE 1 - RELATIVE ABUNDANCE of Species Found in Ibadan (Dry + Wet Season Data)
## -----------------------------------------------------------------------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(tibble)

# df to calculate counts of each species by season
species_inventory_by_season <- all_ento_data %>%
  group_by(season, settlement_type) %>% 
  summarise(
    total_An.gambiae = sum(An.gambiae, na.rm = TRUE),
    total_An.funestus = sum(An.funestus, na.rm = TRUE),
    total_Culicine = sum(Culicine, na.rm = TRUE),
    total_Other = sum(Other, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = starts_with("total_"), # include all species columns starting with "total_"
    names_to = "species",
    values_to = "count"
  ) %>%
  # calculate total count and proportions for each settlement type within each season
  group_by(season, settlement_type) %>%
  mutate(
    total_count = sum(count, na.rm = TRUE), # total count for the settlement type and season
    proportion = (count / total_count) * 100 # proportion for each species
  ) %>%
  ungroup()

# summarize counts by species and season
species_summary <- species_inventory_by_season %>%
  group_by(species, season) %>%
  summarise(count = sum(count), .groups = "drop") %>%
  group_by(season) %>%
  mutate(proportion = count / sum(count) * 100) %>%
  ungroup()

# rename species for presentation
species_summary <- species_summary %>%
  mutate(
    species = case_when(
      species == "total_An.gambiae" ~ "An. gambiae",
      species == "total_An.funestus" ~ "An. funestus",
      species == "total_Culicine" ~ "Culicine",
      species == "total_Other" ~ "Other",
      TRUE ~ species
    )
  )

# create abundance column with count and percentage
species_summary <- species_summary %>%
  mutate(
    abundance = paste0(count, " (", round(proportion, 1), "%)")
  )

# get grand total mosquito count
grand_total <- sum(species_summary$count)

# calculate total count and percentage per species (across both seasons)
species_totals <- species_summary %>%
  group_by(species) %>%
  summarise(
    count = sum(count, na.rm = TRUE),
    proportion = sum(count, na.rm = TRUE) / grand_total * 100
  ) %>%
  mutate(
    total = paste0(count, " (", round(proportion, 1), "%)")
  ) %>%
  select(species, total)

# pivot season columns wide and join with totals
species_combined <- species_summary %>%
  select(species, season, abundance) %>%
  pivot_wider(
    names_from = season,
    values_from = abundance
  ) %>%
  left_join(species_totals, by = "species") %>%
  mutate(
    species = factor(species, levels = c("An. gambiae", "An. funestus", "Culicine", "Other"))
  ) %>%
  arrange(species) %>%
  rename(
    Species = species,
    `Dry Season Abundance (%)` = dry,
    `Wet Season Abundance (%)` = wet,
    `Total Abundance (%)` = total
  )

# create total row with 100% proportions
final_totals <- tibble(
  Species = "Total",
  `Dry Season Abundance (%)` = paste0(sum(species_summary$count[species_summary$season == "dry"]), " (100%)"),
  `Wet Season Abundance (%)` = paste0(sum(species_summary$count[species_summary$season == "wet"]), " (100%)"),
  `Total Abundance (%)` = paste0(grand_total, " (100%)")
)

# combine species rows with final total row
final_table <- bind_rows(species_combined, final_totals)

# create the Word document
doc <- read_docx()

# add a title to the Word document
doc <- doc %>%
  body_add_par("Mosquito Species Composition by Season in Ibadan", style = "heading 1")

# add the table to the Word document with a default style
doc <- doc %>%
  body_add_table(value = final_table, style = "table_template")

# save the Word document to the specified directory
output_file <- file.path(NewFigDir, "table1.docx")
print(doc, target = output_file)


# -----------------------------------------------------------------------------------------------------------------------------#

# create a summary dataframe to calculate counts
method_df <- all_ento_data %>%
  dplyr::filter(!is.na(settlement_type)) %>% # remove observations for which settlement_type is NA
  mutate( # separate CDC by indoor and outdoor, PSC remains as-is
    collection_type = case_when(
      method == "CDC" & location == "Indoor" ~ "Indoor (CDC + PSC)",
      method == "CDC" & location == "Outdoor" ~ "Outdoor (CDC)",
      method == "PSC" ~ "Indoor (CDC + PSC)"
    )
  ) %>%
  group_by(settlement_type, collection_type, season) %>%
  summarise(
    An_gambiae_sum = sum(An.gambiae, na.rm = TRUE),
    An_funestus_sum = sum(An.funestus, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(An_gambiae_sum, An_funestus_sum), names_to = "species", values_to = "count")

# make wet df and dry df
wet_method_df <- method_df %>%
  dplyr::filter(season %in% c("wet"))
dry_method_df <- method_df %>%
  dplyr::filter(season %in% c("dry"))

wet_method_df_anopheles <- wet_method_df %>%
  mutate(species = "Anopheles") %>%
  group_by(settlement_type, collection_type, season, species) %>%
  summarise(count = sum(count), .groups = "drop")
dry_method_df_anopheles <- dry_method_df %>%
  mutate(species = "Anopheles") %>%
  group_by(settlement_type, collection_type, season, species) %>%
  summarise(count = sum(count), .groups = "drop")

# combine wet and dry season collapsed data
combined_df_anopheles <- bind_rows(wet_method_df_anopheles, dry_method_df_anopheles)

# create side-by-side bar plot by season within settlement_type facets
method_palette <- c("#8d9f87", "#f0dcca")

species_by_method_combined <- ggplot(combined_df_anopheles, aes(x = season, y = count, fill = collection_type)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ settlement_type) +
  geom_text(aes(label = count), position = position_stack(vjust = 0.5), size = 6, color = "white") +
  scale_fill_manual(
    values = method_palette,
    name = "Collection Method",
    labels = c("Indoor (CDC + PSC)", "Outdoor (CDC)")
  ) +
  labs(
    title = "Mosquito Species by Collection Method, Season, and Settlement Type",
    x = "Season",
    y = "Number of Anopheles Collected",
    fill = "Collection Method"
  ) +
  theme_manuscript() +
  theme(plot.title = element_text(size = 14))

# display the plot
species_by_method_combined

ggsave(file.path(NewFigDir, '_combined_species_collection_plot.pdf'), plot = species_by_method_combined, width = 10, height = 6)

## -----------------------------------------------------------------------------------------------------------------------------------------
### 3) Mosquito Species by Collection Method Only
## -----------------------------------------------------------------------------------------------------------------------------------------

# create a summary dataframe to calculate counts
collection_method_df <- all_ento_data %>%
  mutate( # separate CDC by indoor and outdoor, PSC remains as-is
    collection_type = case_when(
      method == "CDC" & location == "Indoor" ~ "CDC (Indoor)",
      method == "CDC" & location == "Outdoor" ~ "CDC (Outdoor)",
      method == "PSC" ~ "PSC"
    )
  ) %>%
  group_by(collection_type) %>%
  summarise(
    An_gambiae_sum = sum(An.gambiae, na.rm = TRUE),
    An_funestus_sum = sum(An.funestus, na.rm = TRUE),
    Culicine_sum = sum(Culicine, na.rm = TRUE),
    Other_sum = sum(Other, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(An_gambiae_sum, An_funestus_sum, Culicine_sum, Other_sum), names_to = "species", values_to = "count") %>%
  mutate(
    species = case_when(
      species == "An_gambiae_sum" ~ "An. gambiae", # rename species for plotting
      species == "An_funestus_sum" ~ "An. funestus",
      species == "Culicine_sum" ~ "Culicine",
      species == "Other_sum" ~ "Other",
      TRUE ~ species
    ),
    species = factor(species, levels = c("An. gambiae", "An. funestus", "Culicine", "Other")), # convert values to factors
    collection_type = factor(collection_type, levels = c("CDC (Indoor)", "CDC (Outdoor)", "PSC"))
  )

method_palette <- c("#ddaa33", "#bb5566", "#004488")

method_only_plot <- ggplot(collection_method_df, aes(x = species, y = count, fill = collection_type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_text(aes(label = count, color = collection_type), position = position_dodge(width = 0.8), vjust = -0.3, size = 6) +
  scale_fill_manual(values = method_palette, name = "Collection Method") +
  scale_color_manual(values = method_palette, guide = "none") + # match label text color to fill color
  labs(
    title = "Mosquito Counts by Collection Type and Species",
    x = "Species",
    y = "Mosquito Count",
    fill = "Collection Type"
  ) +
  theme_manuscript() +
  theme(
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  )
method_only_plot

ggsave(filename = paste0(NewFigDir, '_collection_method_plot.pdf'), plot = method_only_plot, width = 10, height = 6)

## -----------------------------------------------------------------------------------------------------------------------------------------
### 4) Blood Meal Status (PSC Data Only) by Species and Settlement Type
## -----------------------------------------------------------------------------------------------------------------------------------------

# filter all df for psc data
psc_bm_df <- all_ento_data %>%
  dplyr::filter(method == "PSC")

# make df with counts of blood meal status by species and settlement type
bm_status_by_settlement <- psc_bm_df %>%
  group_by(settlement_type) %>%
  summarise(
    total_fed_An.gambiae = sum(fed_An.gambiae, na.rm = TRUE),
    total_unfed_An.gambiae = sum(unfed_An.gambiae, na.rm = TRUE),
    total_fed_An.funestus = sum(fed_An.funestus, na.rm = TRUE),
    total_unfed_An.funestus = sum(unfed_An.funestus, na.rm = TRUE),
    total_fed_Other = sum(fed_Other, na.rm = TRUE),
    total_unfed_Other = sum(unfed_Other, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = -settlement_type,
    names_to = "blood_species",
    values_to = "count"
  ) %>%
  mutate(
    species = case_when(
      blood_species %in% c("total_fed_An.gambiae", "total_unfed_An.gambiae") ~ "An.gambiae",
      blood_species %in% c("total_fed_An.funestus", "total_unfed_An.funestus") ~ "An.funestus",
      blood_species %in% c("total_fed_Other", "total_unfed_Other") ~ "Other",
      TRUE ~ NA_character_
    ),
    blood_status = case_when(
      blood_species %in% c("total_fed_An.gambiae", "total_fed_An.funestus", "total_fed_Other") ~ "fed",
      blood_species %in% c("total_unfed_An.gambiae", "total_unfed_An.funestus", "total_unfed_Other") ~ "unfed",
      TRUE ~ NA_character_
    )
  ) %>%
  ungroup()

blood_palette = c("#a20f1b", "#f9bfbf")

blood_status_plot <- ggplot(bm_status_by_settlement, aes(x = species, y = count, fill = blood_status)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(
    values = blood_palette,
    name = "Blood Meal Status",
    labels = c("Fed", "Unfed")) +
  geom_text(aes(label = count), position = position_stack(vjust = 0.5), size = 4, color = "white") +
  facet_wrap(~settlement_type)+
  labs(title = "Distribution of Adult Larvae (Mosquitoes) by Blood Meal Status", x = "Species", y = "Number of Blood-Fed Adult Mosquitoes") +
  labs(subtitle = "By PSC Collection Only") +
  theme_manuscript() +
  theme(axis.text.x = element_text(size = 10)) +
  theme(legend.position = "right",
        plot.subtitle = element_text(hjust = 0.5),
        plot.title = element_text(hjust = 0.5)
  )
blood_status_plot

# save as .pdf
ggsave(file.path(NewFigDir, '_blood_status_plot.pdf'), plot = blood_status_plot, width = 8, height = 4)


## =========================================================================================================================================
### MOLECULAR ID ANALYSIS
## =========================================================================================================================================

molecular_df <- readxl::read_xlsx(file.path(WetData, "Copy of Molecular analysis of ibadan samples.xlsx"))

# remove larval data (anything not PSC or CDC), rename var names, delete empty column, data cleaning
molecular_df <- molecular_df %>%
  rename_with(~ gsub(" ", "_", tolower(.))) %>% 
  rename(lab_id = lab_i.d, lf_coinfection = "lf_result", ward_name = "area", blood_status = "status") %>% 
  dplyr::filter(method %in% c("PSC", "CDC")) %>% 
  mutate(
    # recode "half gravid" to "gravid"
    blood_status = case_when(blood_status == "Half gravid" ~ "Gravid", TRUE ~ blood_status),
    blood_status = case_when(blood_status == "gravid" ~ "Gravid", TRUE ~ blood_status),
    blood_status = case_when(blood_status == "fed" ~ "Fed", TRUE ~ blood_status),
    # create a separate variable for location (indoor/outdoor) for CDC data
    location = case_when(blood_status %in% c("Indoor") ~ "Indoor", blood_status %in% c("Outdoor") ~ "Outdoor", TRUE ~ NA_character_),
    # set "status" to NA for CDC collections (Fed/Unfed/Gravid was not reported for CDC data)
    blood_status = case_when(method %in% c("CDC") ~ NA, TRUE ~ blood_status)
  ) %>%
  # set time variable to match format of other dfs
  mutate(time = case_when(
    time == "6 to 7" ~ "6-7pm",
    time == "7 to 8" ~ "7-8pm",
    time == "8 to 9" ~ "8-9pm",
    time == "9 to 10" ~ "9-10pm",
    time == "10 to 11" ~ "10-11pm",
    time == "11 to 12" ~ "11-12am",
    time == "12 to 1" ~ "12-1am",
    time == "1 to 2" ~ "1-2am",
    time == "2 to 3" ~ "2-3am",
    time == "3 to 4" ~ "3-4am",
    time == "4 to 5" ~ "4-5am",
    time == "5 to 6" ~ "5-6am",
    time == "2 to 1" ~ "12-1pm", # data entry error, recoding
    time == "11 to 2" ~ "11-12pm",  # data entry error, recoding
    TRUE ~ time # keep other values as is
  )) %>%
  select(-`...8`)

write.csv(molecular_df, file = file.path(WetData, "cleaned_molecular_df_only_CDC_PSC.csv"))

## -----------------------------------------------------------------------------------------------------------------------------------------
### 5) Sporozoite Rate in CDC vs PSC Collections by Collection Method
## -----------------------------------------------------------------------------------------------------------------------------------------

# make df with counts of sporozoite positivity by collection method
sporozoite_data <- molecular_df %>%
  mutate( # separate CDC by indoor and outdoor, PSC remains as-is
    collection_type = case_when(
      method == "CDC" & location == "Indoor" ~ "Indoor CDC",
      method == "CDC" & location == "Outdoor" ~ "Outdoor CDC",
      method == "PSC" ~ "PSC"
    )
  ) %>%
  group_by(collection_type, species, sporozoite_result) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = sporozoite_result, values_from = count, values_fill = 0)

# An. gambiae is the only species with sporozoite positivity, so filter out other species + reshape data for plotting
sporozoite_gambiae_data <- sporozoite_data %>%
  dplyr::filter(species == "An. gambiae s.l") %>%
  select(collection_type, Negative, Positive, "NA") %>%
  tidyr::pivot_longer(cols = c(Negative, Positive, "NA"), 
                      names_to = "result", 
                      values_to = "count")

# calculate percentages of positivity by collection method, remove NA results
sporozoite_gambiae_data <- sporozoite_gambiae_data %>%
  group_by(collection_type) %>%
  mutate(
    total = sum(count),
    percentage = (count / total) * 100,
    label = paste0(round(percentage, 1), "%")
  ) %>% 
  dplyr::filter(result != "NA")

# plot sporozoite positivity data
spor_palette = c("#4b6043", "#9dba9a")

sporozoite_plot <- ggplot(sporozoite_gambiae_data, aes(x = collection_type, y = count, fill = result)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(count, " (", label, ")")), position = position_stack(vjust = 0.5), size = 6, color = "white") +
  labs(title = "An. Gambiae Sporozoite Results by Collection Method",
       x = "Collection Method",
       y = "Count",
       fill = "Sporozoite Result") +
  theme_manuscript() +
  scale_fill_manual(
    values = spor_palette,
    name = "Sporozoite Positivity",
    labels = c("Positive", "Negative"))
sporozoite_plot

# save as .pdf
ggsave(filename = paste0(NewFigDir, '_sporozoite_method_plot.pdf'), plot = sporozoite_plot, width = 10, height = 8)

# make another plot that is separated by ward (Agugu and Challenge)
sporozoite_data_wards <- molecular_df %>%
  mutate(
    collection_type = case_when(
      method == "CDC" & location == "Indoor" ~ "Indoor CDC",
      method == "CDC" & location == "Outdoor" ~ "Outdoor CDC",
      method == "PSC" ~ "PSC"
    )
  ) %>%
  group_by(ward_name, collection_type, species, sporozoite_result) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = sporozoite_result, values_from = count, values_fill = 0)

# filter for An. gambiae s.l and reshape data for plotting
sporozoite_gambiae_data_wards <- sporozoite_data_wards %>%
  dplyr::filter(species == "An. gambiae s.l") %>%
  select(ward_name, collection_type, Negative, Positive, `NA`) %>%
  pivot_longer(cols = c(Negative, Positive, `NA`), 
               names_to = "result", 
               values_to = "count")

# calculate percentages
sporozoite_gambiae_data_wards <- sporozoite_gambiae_data_wards %>%
  group_by(ward_name, collection_type) %>%
  mutate(
    total = sum(count),
    percentage = (count / total) * 100,
    label = ifelse(percentage > 0, paste0(round(percentage, 1), "%"), "") # only show label if percentage > 0
  ) %>% 
  dplyr::filter(result != "NA")

# plot
sporozoite_plot_wards <- ggplot(sporozoite_gambiae_data_wards, aes(x = collection_type, y = count, fill = result)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(count, " (", label, ")")), position = position_stack(vjust = 0.5), size = 5, color = "white") +
  labs(title = "An. Gambiae Sporozoite Results by Collection Method and Ward",
       x = "Collection Method",
       y = "Count",
       fill = "Sporozoite Result") +
  theme_manuscript() +
  scale_fill_manual(
    values = spor_palette,
    name = "Sporozoite Positivity",
    labels = c("Positive", "Negative", "NA")
  ) +
  facet_wrap(~ ward_name)
sporozoite_plot_wards

# save as .pdf
ggsave(filename = paste0(NewFigDir, '_sporozoite_method_ward_plot.pdf'), plot = sporozoite_plot_wards, width = 12, height = 8)


## =========================================================================================================================================
### HUMAN BITING RATE (HBR) - Using CDC Data
# Human Biting Rate (HBR) = Number of species collected divided by (number of nights × number of humans that slept in the house as bait)
## =========================================================================================================================================

# filter data to include only CDC wet and dry (no PSC data)
cdc_wet_dry = subset(all_ento_data, (method %in% c("CDC")))

# # subset data where total anopheles count is greater than zero (use both CDC wet and dry data)
# cdc_wet_dry_subset <- cdc_wet_dry[cdc_wet_dry$`Anopheles` > 0, ]

# remove rows for which all values are NA
cdc_wet_dry_subset <- cdc_wet_dry[!apply(cdc_wet_dry, 1, function(row) all(is.na(row))), ]

# remove rows for which settlement type is NA
cdc_wet_dry_subset <- subset(cdc_wet_dry_subset, !(settlement_type %in% c(NA)))

# -------------- data for plots grouped by settlement type, location, and season ----------------
# summarize anopheles caught by settlement classification and location
ano_caught_cdc <- cdc_wet_dry_subset %>% 
  group_by(settlement_type, location, season) %>%
  summarise(anopheles_caught = sum(Anopheles, na.rm = TRUE)) %>%  # sum Anopheles, handling NA
  ungroup()

# set number of night baits to 14 for dry season and 48 for wet season
ano_caught_cdc <- ano_caught_cdc %>%
  mutate(no_night_bait = ifelse(season == "dry", 14, 192))

# calculate HBR (# of mosquitoes collected / (number of nights x number of humans slept in the house as bait))
ano_caught_cdc <- ano_caught_cdc %>%
  mutate(HBR = anopheles_caught / no_night_bait)

# -------------- data for plots grouped by settlement type and season only -----------------------
ano_caught_cdc_settlement <- cdc_wet_dry_subset %>% 
  group_by(settlement_type, season) %>%
  summarise(anopheles_caught = sum(Anopheles, na.rm = TRUE)) %>%  # sum Anopheles, handling NA
  ungroup()

# set number of night baits to 14 for dry season and 192 for wet season
ano_caught_cdc_settlement <- ano_caught_cdc_settlement %>%
  mutate(no_night_bait = ifelse(season == "dry", 14, 192))

# calculate HBR (# of mosquitoes collected / (number of nights x number of humans slept in the house as bait))
ano_caught_cdc_settlement <- ano_caught_cdc_settlement %>%
  mutate(HBR = anopheles_caught / no_night_bait)

# ---------------------------------------- plots -------------------------------------------------
wet_dry_palette <- c("#d9af8d", "#0d80bf")

hbr_plot <- ggplot(data = ano_caught_cdc, aes(x = settlement_type, y = HBR, 
                                              group = interaction(location, season),
                                              colour = season,
                                              shape = location)) +
  scale_x_discrete(limits = c("Formal", "Informal", "Slum")) +
  geom_jitter(position = position_jitter(width = 0.1, height = 0), size = 5) + # jitter points as some HBR values are the same, causing overlap
  labs(y = "Human Biting Rate", x = "Settlement Type",
       title = "Human Biting Rate by Settlement Type, \nSeason, and Location of CDC Collection") +
  scale_color_manual(
    values = wet_dry_palette,
    name = "Season",
    labels = c("Dry", "Wet")
  ) +
  scale_shape_manual(
    values = c("Indoor" = 15, "Outdoor" = 17),
    name = "CDC"
  ) +
  theme_manuscript() +
  theme(
    plot.title = element_text(size = 12),
    legend.position = c(0.98, 0.98),
    legend.justification = c("right", "top"),
    legend.box = "horizontal",
    legend.box.background = element_rect(color = "black", size = 0.5),
    legend.spacing = unit(0.5, "cm"),
    legend.box.margin = margin(6, 6, 6, 6),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 14)
  )
hbr_plot

# save as .pdf
ggsave(file.path(NewFigDir, '_hbr_cdc_plot.pdf'), plot = hbr_plot, width = 8, height = 7)

# plot for hbr grouped only by settlement type and season only
hbr_plot_settlement <- ggplot(data = ano_caught_cdc_settlement, aes(x = settlement_type, y = HBR, 
                                              group = interaction(season),
                                              colour = season)) +
  scale_x_discrete(limits = c("Formal", "Informal", "Slum")) +
  geom_jitter(position = position_jitter(width = 0.1, height = 0), size = 5) + # jitter points as some HBR values are the same, causing overlap
  labs(y = "Human Biting Rate", x = "Settlement Type",
       title = "Human Biting Rate by Settlement Type and Season") +
  scale_color_manual(
    values = wet_dry_palette,
    name = "Season",
    labels = c("Dry", "Wet")
  ) +
  theme_manuscript() +
  theme(
    plot.title = element_text(size = 12),
    legend.position = c(0.98, 0.98),
    legend.justification = c("right", "top"),
    legend.box = "horizontal",
    legend.box.background = element_rect(color = "black", size = 0.5),
    legend.spacing = unit(0.5, "cm"),
    legend.box.margin = margin(6, 6, 6, 6),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 14)
  )
hbr_plot_settlement

# save as .pdf
ggsave(file.path(NewFigDir, '_settlement_hbr_cdc_plot.pdf'), plot = hbr_plot_settlement, width = 8, height = 7)

## =========================================================================================================================================
### ANOVA for Human Biting Rate (HBR)
## =========================================================================================================================================

# fit the three-way ANOVA model and summarize results
anova_HBR <- aov(HBR ~ season * settlement_type * location, data = ano_caught_cdc)

# extract the ANOVA table
anova_table <- summary(anova_HBR)

# calculate F-statistic and p-value for each term
anova_results <- data.frame(
  Df = anova_table[[1]]$Df,
  `Sum Sq` = anova_table[[1]]$`Sum Sq`,
  `Mean Sq` = anova_table[[1]]$`Mean Sq`,
  `F-value` = anova_table[[1]]$`Mean Sq` / anova_table[[1]]$`Mean Sq`[length(anova_table[[1]]$`Mean Sq`)],
  `P-value` = pf(
    anova_table[[1]]$`Mean Sq` / anova_table[[1]]$`Mean Sq`[length(anova_table[[1]]$`Mean Sq`)],
    anova_table[[1]]$Df,
    anova_table[[1]]$Df[length(anova_table[[1]]$Df)],
    lower.tail = FALSE
  )
)

# add significance column and interaction types
anova_results <- anova_results %>%
  mutate(
    Significance = ifelse(abs(P.value) < 0.05, "Yes", "No")
  ) %>%
  mutate(test = c("season", "settlement_type", "location", "season:settlement_type", 
                  "season:location", "settlement_type:location", "season:settlement_type:location")) %>%
  select(Df, test, Sum.Sq, Mean.Sq, F.value, P.value, Significance)

# export as table in word
doc <- read_docx()
doc <- doc %>%
  body_add_par("ANOVA results for HBR", style = "heading 1")
doc <- doc %>%
  body_add_table(value = anova_results, style = "table_template")
output_file <- file.path(ResultDir, "anova_hbr.docx")
print(doc, target = output_file)

## =========================================================================================================================================
### INDOOR RESIDUAL DENSITY (IRD) - Using PSC Data
# Indoor Resting Density (IRD) = Number of Anopheles mosquitoes collected divided by the total number of rooms sampled
## =========================================================================================================================================

# filter data to include only PSC wet and dry (no CDC data)
psc_wet_dry = subset(all_ento_data, (method %in% c("PSC")))

# summarize anopheles caught by settlement classification and location
ano_caught_psc <- psc_wet_dry %>% 
  group_by(settlement_type, season) %>%
  summarise(anopheles_caught = sum(Anopheles, na.rm = TRUE)) %>%  # sum Anopheles, handling NA
  ungroup()

# set number of rooms sampled to 40 for dry season and 120 for wet season
ano_caught_psc <- ano_caught_psc %>%
  mutate(no_rooms_sampled = ifelse(season == "dry", 40, 120))

# calculate IRD (# of mosquitoes collected / number of rooms sampled)
ano_caught_psc <- ano_caught_psc %>%
  mutate(IRD = anopheles_caught / no_rooms_sampled)

# create IRD plot
ird_plot <- ggplot(data = ano_caught_psc, 
                   aes(x = settlement_type, y = IRD, 
                       group = season, # Group by season for clear differentiation
                       colour = season)) + # Color points by season
  scale_x_discrete(limits = c("Formal", "Informal", "Slum")) +
  geom_point(size = 5) + # Add points for IRD
  labs(y = "Indoor Residual Density", x = "Settlement Type",
       title = "Indoor Residual Density by Settlement Type and Season") +
  scale_color_manual(
    values = wet_dry_palette, # Use your defined palette for seasons
    name = "Season",
    labels = c("Dry", "Wet") # Explicitly label seasons
  ) +
  theme_manuscript() +
  theme(
    plot.title = element_text(size = 12),
    legend.position = c(0.2, 0.95), # Place legend inside the plot (top-right corner)
    legend.justification = c("right", "top"), # Align legend box with top-right
    legend.box = "horizontal", # Align legends horizontally
    legend.box.background = element_rect(color = "black", size = 0.5), # Black outline
    legend.spacing = unit(0.5, "cm"), # Space between legend items
    legend.box.margin = margin(6, 6, 6, 6), # Padding inside the legend box
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 14)
  )
ird_plot

# save as .pdf
ggsave(file.path(NewFigDir, '_ird_psc_plot.pdf'), plot = ird_plot, width = 8, height = 6)

## =========================================================================================================================================
### ANOVA for Indoor Residual Density (IRD)
## =========================================================================================================================================

# fit the two-way ANOVA model and summarize results
anova_IRD <- aov(IRD ~ season * settlement_type, data = ano_caught_psc)

# extract the ANOVA table
anova_table_IRD <- summary(anova_IRD)

# calculate F-statistic and p-value for each term
anova_results_IRD <- data.frame(
  Df = anova_table_IRD[[1]]$Df,
  `Sum Sq` = anova_table_IRD[[1]]$`Sum Sq`,
  `Mean Sq` = anova_table_IRD[[1]]$`Mean Sq`,
  `F-value` = anova_table_IRD[[1]]$`Mean Sq` / anova_table_IRD[[1]]$`Mean Sq`[length(anova_table_IRD[[1]]$`Mean Sq`)],
  `P-value` = pf(
    anova_table_IRD[[1]]$`Mean Sq` / anova_table_IRD[[1]]$`Mean Sq`[length(anova_table_IRD[[1]]$`Mean Sq`)],
    anova_table_IRD[[1]]$Df,
    anova_table_IRD[[1]]$Df[length(anova_table_IRD[[1]]$Df)],
    lower.tail = FALSE
  )
)

# add significance column and interaction types
anova_results_IRD <- anova_results_IRD %>%
  mutate(
    Significance = ifelse(abs(P.value) < 0.05, "Yes", "No")
  ) %>%
  mutate(test = c("season", "settlement_type", "season:settlement_type")) %>%
  select(Df, test, Sum.Sq, Mean.Sq, F.value, P.value, Significance)

# export as table in word
doc <- read_docx()
doc <- doc %>%
  body_add_par("ANOVA results for IRD", style = "heading 1")
doc <- doc %>%
  body_add_table(value = anova_results_IRD, style = "table_template")
output_file <- file.path(ResultDir, "anova_ird.docx")
print(doc, target = output_file)

## =========================================================================================================================================
### Entomological Indicators (HBR, IRD, EIR, settlement type) by season and settlement type
## =========================================================================================================================================

# get HBR by season and settlement type
hbr_season_settlement <- cdc_wet_dry_subset %>% 
  group_by(season, settlement_type) %>%
  summarise(anopheles_caught = sum(Anopheles, na.rm = TRUE)) %>%  # sum Anopheles, handling NA
  ungroup()
hbr_season_settlement <- hbr_season_settlement %>%
  mutate(no_night_bait = ifelse(season == "dry", 14, 192))
hbr_season_settlement <- hbr_season_settlement %>%
  mutate(HBR = anopheles_caught / no_night_bait) %>%
  select(season, HBR)

# get IRD by season and settlement type
ird_season_settlement <- psc_wet_dry %>% 
  group_by(season, settlement_type) %>%
  summarise(anopheles_caught = sum(Anopheles, na.rm = TRUE)) %>%  # sum Anopheles, handling NA
  ungroup()
ird_season_settlement <- ird_season_settlement %>%
  mutate(no_rooms_sampled = ifelse(season == "dry", 40, 120))
ird_season_settlement <- ird_season_settlement %>%
  mutate(IRD = anopheles_caught / no_rooms_sampled) %>%
  select(season, IRD)

# get sporozoite rate by season and settlement type
sporozoite_season <- molecular_df %>%
  summarise(
    sporozoite_positive_rate = mean(sporozoite_result == "Positive", na.rm = TRUE)
  ) %>%
  mutate(season = "wet") %>%
  select(season, sporozoite_positive_rate) %>%
  bind_rows(tibble(season = "dry", sporozoite_positive_rate = NA))

# combine entomological indicators by season
indicators_season <- full_join(hbr_season, ird_season, by = "season") %>%
  full_join(sporozoite_season, by = "season")

# calculate entomological inoculation rate (EIR): human biting rate x sporozoite rate
indicators_season <- indicators_season %>%
  mutate(EIR = HBR * sporozoite_positive_rate)

# create long df
indicators_season_long <- indicators_season %>%
  pivot_longer(
    cols = c(HBR, IRD, sporozoite_positive_rate, EIR),
    names_to = "indicator",                      
    values_to = "value"       
  ) %>%
  mutate(
    indicator = case_when(
      indicator == "sporozoite_positive_rate" ~ "Sporozoite Positivity",
      TRUE ~ indicator
    ),
    season = case_when(
      season == "dry" ~ "Dry",
      season == "wet" ~ "Wet"
    )
  )

indicator_palette <- c("#f4dfcc", "#fa9583", "#2f4159", "#4097aa")

# bar chart faceted by season showing each of the entomological indicators
indicators_season_plot <- ggplot(indicators_season_long, aes(x = indicator, y = value, fill = indicator)) +
  geom_bar(stat = "identity", position = "dodge", show.legend = FALSE) +
  scale_fill_manual(
    values = indicator_palette, 
    name = "Indicators",
    labels = c("EIR", "HBR", "IRD", "Sporozoite\nPositivity")
  ) +
  facet_wrap(~season, ncol = 1) +
  labs(
    x = "Entomological Indicator",
    y = "Value",
    title = "Entomological Indicators by Season"
  ) +
  theme_manuscript() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  )
indicators_season_plot

# save as .pdf
ggsave(file.path(NewFigDir, '_indicators_season.pdf'), plot = indicators_season_plot, width = 8, height = 10)

## =========================================================================================================================================
### EIR by Ward Name
## This is the only variable in common between the CDC data and molecular data
## =========================================================================================================================================

# get hbr by ward name
hbr <- cdc_wet_dry_subset %>%
  group_by(ward_name, season) %>%
  summarise(anopheles_caught = sum(Anopheles, na.rm = TRUE), .groups = "drop") %>%  # sum Anopheles
  mutate(no_night_bait = ifelse(season == "dry", 14, 192)) %>%  # assign bait nights per season
  group_by(ward_name) %>%
  summarise(
    total_anopheles_caught = sum(anopheles_caught, na.rm = TRUE),
    total_bait_nights = sum(no_night_bait, na.rm = TRUE),  # sum bait nights across seasons
    .groups = "drop"
  ) %>%
  mutate(HBR = total_anopheles_caught / total_bait_nights) %>%  # calculate HBR
  select(ward_name, HBR)

# get sporozoite rate by ward name and indoor/outdoor
sporozoite <- molecular_df %>%
  group_by(ward_name) %>%
  summarise(
    sporozoite_positive_rate = mean(sporozoite_result == "Positive", na.rm = TRUE)
  ) %>%
  select(ward_name, sporozoite_positive_rate)

# join dfs
eir <- hbr %>%
  left_join(sporozoite, by = "ward_name")

# calculate entomological inoculation rate (EIR): human biting rate x sporozoite rate
eir <- eir %>%
  mutate(EIR = HBR * sporozoite_positive_rate)

# bar chart to show HBR, sporozoite rate, and EIR
eir_long <- eir %>%
  pivot_longer(cols = c(HBR, sporozoite_positive_rate, EIR),
               names_to = "metric", values_to = "value")

# bar chart
eir_plot <- ggplot(eir_long, aes(x = ward_name, y = value, fill = metric)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("HBR" = "#1f77b4", "sporozoite_positive_rate" = "#ff7f0e", "EIR" = "#2ca02c")) +
  labs(x = "Ward", y = "Value", fill = "Metric",
       title = "HBR, Sporozoite Positivity Rate, and EIR by Ward") +
  theme_manuscript()

ggsave(file.path(NewFigDir, '_EIR_bar_chart.pdf'), plot = eir_plot, width = 8, height = 6)

## =========================================================================================================================================
### Entomological Indicators (HBR, IRD, EIR) by settlement type
## =========================================================================================================================================

# get hbr by settlement type
hbr_settlement <- cdc_wet_dry_subset %>%
  group_by(settlement_type, season) %>%
  summarise(anopheles_caught = sum(Anopheles, na.rm = TRUE), .groups = "drop") %>%  # sum Anopheles
  mutate(no_night_bait = ifelse(season == "dry", 14, 48)) %>%  # assign bait nights per season
  group_by(settlement_type) %>%
  summarise(
    total_anopheles_caught = sum(anopheles_caught, na.rm = TRUE),
    total_bait_nights = sum(no_night_bait, na.rm = TRUE),  # sum bait nights across seasons
    .groups = "drop"
  ) %>%
  mutate(HBR = total_anopheles_caught / total_bait_nights) %>%  # calculate HBR
  select(settlement_type, HBR)

# get IRD by settlement type
ird_settlement <- psc_wet_dry %>%
  group_by(settlement_type, season) %>%
  summarise(anopheles_caught = sum(Anopheles, na.rm = TRUE), .groups = "drop") %>%  # sum Anopheles
  mutate(no_rooms_sampled = ifelse(season == "dry", 40, 120)) %>%  # assign rooms sampled per season
  group_by(settlement_type) %>%
  summarise(
    total_anopheles_caught = sum(anopheles_caught, na.rm = TRUE),
    total_rooms_sampled = sum(no_rooms_sampled, na.rm = TRUE),  # sum rooms sampled across seasons
    .groups = "drop"
  ) %>%
  mutate(IRD = total_anopheles_caught / total_rooms_sampled) %>%  # calculate IRD
  select(settlement_type, IRD)

# get sporozoite rate by settlement type
sporozoite_settlement <- molecular_df %>%
  group_by(ward_name) %>%
  summarise(
    sporozoite_positive_rate = mean(sporozoite_result == "Positive", na.rm = TRUE)
  ) %>%
  mutate(
    settlement_type = case_when(
      ward_name == "Agugu" ~ "Slum",
      ward_name == "Challenge" ~ "Informal",
    )
  ) %>%
  select(settlement_type, sporozoite_positive_rate) %>%
  bind_rows(tibble(settlement_type = "Formal", sporozoite_positive_rate = NA))

# combine entomological indicators by season
indicators_settlement <- full_join(hbr_settlement, ird_settlement, by = "settlement_type") %>%
  full_join(sporozoite_settlement, by = "settlement_type")

# calculate entomological inoculation rate (EIR): human biting rate x sporozoite rate
indicators_settlement <- indicators_settlement %>%
  mutate(EIR = HBR * sporozoite_positive_rate)

# create long df
indicators_settlement_long <- indicators_settlement %>%
  pivot_longer(
    cols = c(HBR, IRD, sporozoite_positive_rate, EIR),
    names_to = "indicator",                      
    values_to = "value"       
  ) %>%
  mutate(
    indicator = case_when(
      indicator == "sporozoite_positive_rate" ~ "Sporozoite Positivity",
      TRUE ~ indicator
    )
  )

# bar chart faceted by settlement type showing each of the entomological indicators
indicators_settlement_plot <- ggplot(indicators_settlement_long, aes(x = indicator, y = value, fill = indicator)) +
  geom_bar(stat = "identity", position = "dodge", show.legend = FALSE) +
  scale_fill_manual(
    values = indicator_palette, 
    name = "Indicators",
    labels = c("EIR", "HBR", "IRD", "Sporozoite\nPositivity")
  ) +
  facet_wrap(~settlement_type, nrow = 1) +
  labs(
    x = "Entomological Indicator",
    y = "Value",
    title = "Entomological Indicators by Settlement Type"
  ) +
  theme_manuscript() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  )
indicators_settlement_plot

# save as .pdf
ggsave(file.path(NewFigDir, '_indicators_settlement.pdf'), plot = indicators_settlement_plot, width = 12, height = 8)


## =========================================================================================================================================
### Co-Infection Analysis (Lymphatic Filariasis (LF))
## =========================================================================================================================================

# calculate rates of LF and sporozoite positivity
molecular_df_rates <- molecular_df %>%
  summarise(
    lf_positive_rate = mean(lf_coinfection == "Positive"),
    sporozoite_positive_rate = mean(sporozoite_result == "Positive")
  )

# calculate rates of positive cases for lf_coinfection and sporozoite_result by collection type
rates_by_method <- molecular_df %>%
  # create a new variable for collection type based on method and location
  mutate(
    collection_type = case_when(
      method == "CDC" & location == "Indoor" ~ "Indoor CDC",
      method == "CDC" & location == "Outdoor" ~ "Outdoor CDC",
      method == "PSC" ~ "PSC"
    )
  ) %>%
  group_by(collection_type) %>%
  # get rates of positive cases for lf_coinfection and sporozoite_result
  summarise(
    lf_positive_rate = mean(lf_coinfection == "Positive", na.rm = TRUE),
    sporozoite_positive_rate = mean(sporozoite_result == "Positive", na.rm = TRUE)
  )

## -----------------------------------------------------------------------------------------------------------------------------------------
### 1) Prevalence of LF Co-Infection:
# Calculate the proportion of mosquitoes with positive lf_coinfection results overall and stratified by location and method.
## -----------------------------------------------------------------------------------------------------------------------------------------

# reshape data to long format for easier plotting
coinfection_long <- rates_by_method %>%
  pivot_longer(
    cols = c(lf_positive_rate, sporozoite_positive_rate),
    names_to = "rate_type",
    values_to = "rate"
  ) %>%
  mutate(
    label = paste0(round(rate * 100, 1), "%") # round rate and convert to percentage for labeling
  )

# plot: prevalence of LF and sporozoites by collection method
coinfection_plot <- ggplot(coinfection_long, aes(x = collection_type, y = rate, fill = rate_type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(
    aes(label = label, y = rate / 2), # adjust y to center text vertically inside the bar
    position = position_dodge(width = 0.8), # match bar dodge position
    size = 5,
    color = "white" # ensure text is visible on bars
  ) +
  scale_fill_manual(
    values = c("lf_positive_rate" = "#264653", "sporozoite_positive_rate" = "#2a9d8f"),
    labels = c("LF Coinfection Rate", "Sporozoite Positivity Rate")
  ) +
  labs(
    x = "Collection Method",
    y = "Rate",
    fill = "Rate Type",
    title = "Rates of Lymphatic Filariasis (LF) and \nSporozoite Positivity by Collection Method"
  ) +
  theme_manuscript() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )
coinfection_plot

# save as .pdf
ggsave(file.path(NewFigDir, '_coinfection_plot.pdf'), plot = coinfection_plot, width = 12, height = 8)

# plot: prevalence by species
ggplot(molecular_df, aes(x = species, fill = lf_coinfection)) +
  geom_bar(position = "fill") +
  labs(y = "Proportion", x = "Species", fill = "LF Co-Infection") +
  theme_manuscript()

# plot: prevalence by ward
ggplot(molecular_df, aes(x = ward_name, fill = lf_coinfection)) +
  geom_bar(position = "fill") +
  labs(y = "Proportion", x = "Ward", fill = "LF Co-Infection") +
  theme_manuscript()

## -----------------------------------------------------------------------------------------------------------------------------------------
### 2) Co-Infection with Sporozoites:
# What proportion of LF-positive mosquitoes are also positive for sporozoites?
## -----------------------------------------------------------------------------------------------------------------------------------------

# plot: LF and sporozoite co-infection
ggplot(molecular_df, aes(x = sporozoite_result, y = lf_coinfection, color = species)) +
  geom_jitter() +
  labs(x = "Sporozoite Result", y = "LF Co-Infection", color = "Species") +
  theme_manuscript()


corr_plot <- ggplot(molecular_df %>% dplyr::filter(species == "An. gambiae s.l"), aes(x = species, fill = interaction(sporozoite_result, lf_coinfection))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("Positive.Positive" = "#1f77b4", "Positive.Negative" = "#ff7f0e", 
                               "Negative.Positive" = "#2ca02c", "Negative.Negative" = "#d62728"),
                    labels = c("Sporozoite Positive, LF Positive", "Sporozoite Positive, LF Negative",
                               "Sporozoite Negative, LF Positive", "Sporozoite Negative, LF Negative")) +
  labs(x = NULL, y = "Proportion", fill = "Combination of Results", 
       title = "Relationship Between Lymphatic Filariasis \nand Sporozoite Positivity in An. Gambiae") +
  theme_manuscript() +
  theme(axis.text.x = element_text(vjust = 1))

# save as .pdf
ggsave(filename = paste0(ResultDir, "/", Sys.Date(), '_corr_plot.pdf'), plot = corr_plot, width = 8, height = 10)
