# ==========================================================================================================================================
# Script Name: Plots not being used for Ento methods/results
# Author: Grace Legris (gracebea@gmail.com)
# Purpose: Saving code that is not being used
# ==========================================================================================================================================

# plot: total anopheles mosquitoes caught through PSC in Ibadan
psc_anopheles_plot <- ggplot(data = psc_grouped, aes(x = Month, y = total_mosquitoes, group = `Settlement Classification`, colour = `Settlement Classification`)) +
  scale_x_discrete(limits = c("January", "February", "March")) +
  geom_point(size = 3.0) +
  geom_line() +
  labs(
    y = "Total Number of Anopheles Mosquitos Caught",
    x = "Month of Collection (Ibadan)",
    title = "Anopheles Mosquites Collected Through Pyrethrum Spray Catches, \n January–March, 2023"
  ) +
  theme(plot.title = element_text(size = 12)) +
  theme_manuscript() +
  theme(
    legend.position = c(0.95, 0.95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  ) +
  ylim(0, 4)
psc_anopheles_plot

# save the plot
ggsave(paste0(ResultDir, "/", Sys.Date(), "_mosquitoes_collected_psc_ibadan.pdf"), psc_anopheles_plot, width = 8, height = 6)


## -----------------------------------------------------------------------------------------------------------------------------------------
### 1) Species Inventory + Relative Abundance Plot
## -----------------------------------------------------------------------------------------------------------------------------------------

palette <- c("#e8dab2", "#dd6e42", "#4f6d7a", "#c0d6df")

# species inventory
species_inventory <- all_ento_data %>%
  summarise(
    total_An.gambiae = sum(An.gambiae, na.rm = TRUE),
    total_An.funestus = sum(An.funestus, na.rm = TRUE),
    total_Culicine = sum(Culicine, na.rm = TRUE),
    total_Other = sum(Other, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "species", values_to = "count") %>%
  # add total count and calculate proportion of each mosquito species
  mutate(
    total_count = sum(count), # total count of mosquitoes
    proportion = count / total_count * 100  # proportion for each species
  )

# bar plot for total counts by species
species_inv_plot <- ggplot(species_inventory, aes(x = species, y = count, fill = species)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(
    values = palette, 
    name = "Species",
    labels = c("An.funestus", "An.gambiae", "Culicine", "Other")
  ) +
  geom_text(aes(label = count), vjust = -3, size = 4.5) +
  geom_text(aes(label = paste0("(", round(proportion, 1), "%)")), vjust = -1, size = 4.5) +
  scale_x_discrete(labels = c("An.funestus", "An.gambiae", "Culicine", "Other")) +
  labs(
    title = "Total Mosquito Counts by Species", 
    subtitle = "Both PSC and CDC Collection Methods", 
    x = "Species", 
    y = "Count",
  ) +
  theme_manuscript() +
  theme(legend.position = "none") + 
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 2500, by = 500), limits = c(0, 2700))
species_inv_plot

# save as .pdf
ggsave(filename = paste0(ResultDir, "/", Sys.Date(), '_species_inv_plot.pdf'), plot = species_inv_plot, width = 8, height = 8)


## -----------------------------------------------------------------------------------------------------------------------------------------
### 2) Comparison of Species Composition Across Seasons and Settlement Types Plot
## -----------------------------------------------------------------------------------------------------------------------------------------

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


# plot that excludes Culicines to better see Gambiae/Funestus proportion

palette_no_cul <- c("#7268A6", "#86a3C3", "#B6CEC7")

# remove culicines
species_inventory_by_season_no_cul = subset(species_inventory_by_season, !(species %in% c("total_Culicine")))

# remove settlement type = NA
species_inventory_by_season_no_cul = subset(species_inventory_by_season_no_cul, !(settlement_type %in% c(NA)))

# make new variable with percentage for labeling on the plot
species_inventory_by_season_no_cul <- species_inventory_by_season_no_cul %>%
  mutate(label = ifelse(proportion > 0, paste0(round(proportion, 1), "%"), "")) # only show label if percentage > 0

species_season_plot_no_cul <- ggplot(species_inventory_by_season_no_cul, aes(x = season, y = count, fill = species)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~settlement_type) +
  geom_text(aes(label = count), position = position_stack(vjust = 0.5), size = 4, color = "white") +
  scale_fill_manual(
    values = palette_no_cul,
    name = "Species",
    labels = c("An. gambiae", "An. funestus", "Other")
  ) +
  labs(
    title = "Mosquito Species Composition by \nSeason and Settlement Type",
    x = "Season",
    y = "Count"
  ) +
  scale_x_discrete(labels = c("Dry", "Wet")) +
  theme_manuscript() +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    plot.subtitle = element_text(hjust = 0.5),
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )
species_season_plot_no_cul

# save as .pdf
ggsave(filename = paste0(ResultDir, "/", Sys.Date(), '_species_by_season_no_cul.pdf'), plot = species_season_plot_no_cul, width = 8, height = 6)

## -----------------------------------------------------------------------------------------------------------------------------------------
### Plot Comparison of Species Composition Across Settlement Types Only
## -----------------------------------------------------------------------------------------------------------------------------------------

species_inventory_by_settlement <- all_ento_data %>%
  group_by(settlement_type) %>% 
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
  )

# exclude settlement type = NA and culicines
species_inventory_by_settlement = subset(species_inventory_by_settlement, !(species %in% c("total_Culicine")))
species_inventory_by_settlement = subset(species_inventory_by_settlement, !(settlement_type %in% c(NA)))

species_inventory_by_settlement <- species_inventory_by_settlement %>%
  # calculate total count and proportions for each settlement type within each season
  group_by(settlement_type) %>%
  mutate(
    total_count = sum(count, na.rm = TRUE), # total count for the settlement type
    proportion = (count / total_count) * 100 # proportion for each species
  ) %>%
  ungroup()

# plot
species_settlement_plot <- ggplot(species_inventory_by_settlement, aes(x = settlement_type, y = count, fill = species)) +
  geom_bar(stat = "identity", position = "stack") +
  #facet_wrap(~settlement_type) +
  geom_text(aes(label = count), position = position_stack(vjust = 0.5), size = 4, color = "white") +
  scale_fill_manual(
    values = palette_no_cul,
    name = "Species",
    labels = c("An. gambiae", "An. funestus", "Other")
  ) +
  labs(
    title = "Mosquito Species Composition \nby Settlement Type",
    x = "Settlement Type",
    y = "Count"
  ) +
  #scale_x_discrete(labels = c("Dry", "Wet")) +
  theme_manuscript() +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    plot.subtitle = element_text(hjust = 0.5),
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )
species_settlement_plot

## -----------------------------------------------------------------------------------------------------------------------------------------
### 3) Comparison of Species Composition by Collection Method (Indoor CDC, Outdoor CDC, PSC)
## -----------------------------------------------------------------------------------------------------------------------------------------

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

method_palette <- c("#8d9f87", "#f0dcca", "#696d7d")

# plot wet season species composition data
wet_species_by_method <- ggplot(wet_method_df_anopheles, aes(x = species, y = count, fill = collection_type)) +
  geom_bar(stat = "identity") +
  facet_wrap(~settlement_type) +
  geom_text(aes(label = count), position = position_stack(vjust = 0.5), hjust = -0.1, size = 6, color = "white") +
  scale_fill_manual(
    values = method_palette,
    name = "Collection Method",
    labels = c("Indoor (CDC + PSC)", "Outdoor (CDC)")
  ) +
  scale_x_discrete(labels = c("Count")) +
  labs(
    title = "Mosquito Species by Collection Method \nand Settlement Type: Wet Season",
    x = "Count of Anopheles Collected",
    y = "Number of Mosquitoes",
    fill = "Collection Method"
  ) +
  theme_manuscript() + 
  theme(plot.title = element_text(size = 14))
wet_species_by_method

# plot dry season species composition data
dry_species_by_method <- ggplot(dry_method_df_anopheles, aes(x = species, y = count, fill = collection_type)) +
  geom_bar(stat = "identity") +
  facet_wrap(~settlement_type) +
  geom_text(aes(label = count), position = position_stack(vjust = 0.5), hjust = -0.1, size = 6, color = "white") +
  scale_fill_manual(
    values = method_palette,
    name = "Collection Method",
    labels = c("Indoor (CDC + PSC)", "Outdoor (CDC)")
  ) +
  scale_x_discrete(labels = c("Count")) +
  labs(
    title = "Mosquito Species by Collection Method \nand Settlement Type: Dry Season",
    x = "Count of Anopheles Collected",
    y = "Number of Mosquitoes",
    fill = "Collection Method"
  ) +
  theme_manuscript() + 
  theme(plot.title = element_text(size = 14))
dry_species_by_method

# save as .pdf
ggsave(filename = paste0(ResultDir, "/", Sys.Date(), '_wet_species_method_plot.pdf'), plot = wet_species_by_method, width = 12, height = 8)
ggsave(filename = paste0(ResultDir, "/", Sys.Date(), '_dry_species_method_plot.pdf'), plot = dry_species_by_method, width = 12, height = 8)

## -----------------------------------------------------------------------------------------------------------------------------------------
### 3b) Comparison of Species Composition by Collection Method (Indoor CDC, Outdoor CDC, PSC)
### Edit: Combine dry and wet seasons into one plot, add percentage labels, group PSC with indoor CDC
## -----------------------------------------------------------------------------------------------------------------------------------------

# create a summary dataframe to calculate counts and percentages
updated_method_df <- all_ento_data %>%
  dplyr::filter(!is.na(settlement_type)) %>% # remove observations with missing settlement_type
  mutate(
    collection_type = case_when( # group indoor CDC and PSC together, outdoor CDC remains "outdoor"
      method == "CDC" & location == "Indoor" ~ "Indoor (CDC + PSC)",
      method == "PSC" ~ "Indoor (CDC + PSC)",
      method == "CDC" & location == "Outdoor" ~ "Outdoor (CDC)"
    )
  ) %>%
  group_by(settlement_type, collection_type, season) %>%
  summarise(
    An_gambiae_sum = sum(An.gambiae, na.rm = TRUE),
    An_funestus_sum = sum(An.funestus, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(An_gambiae_sum, An_funestus_sum), names_to = "species", values_to = "count") %>%
  group_by(settlement_type, season) %>%
  mutate(percentage = (count / sum(count)) * 100)

season_palette <- c("wet" = "#0d80bf", "dry" = "#d9af8d")

# Create grouped bar plot
species_distribution_plot <- ggplot(updated_method_df, aes(x = species, y = count, fill = season)) +
  geom_bar(stat = "identity", position = "dodge") +  # Group bars by season
  geom_text(aes(label = count),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 6) + # Add counts
  # geom_text(aes(label = paste0(count, " (", round(percentage, 1), "%)")),
  #           position = position_dodge(width = 0.9), vjust = -0.5, size = 3) + # Add counts & percentages
  facet_grid(settlement_type ~ collection_type) +  # Facet by settlement type and collection method
  scale_fill_manual(values = season_palette, name = "Season", labels = c("Dry", "Wet")) +
  scale_x_discrete(labels = c("An. funestus", "An. gambiae")) +
  labs(
    title = "Mosquito Species Distribution by Season, Settlement Type, and Collection Method",
    x = "Species",
    y = "Number of Mosquitoes",
    fill = "Season"
  ) +
  theme_manuscript() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    strip.text = element_text(size = 14)
  )

species_distribution_plot

# save as .pdf
ggsave(filename = paste0(ResultDir, "/", Sys.Date(), '_updated_species_distribution.pdf'), plot = species_distribution_plot, width = 10, height = 12)

## -----------------------------------------------------------------------------------------------------------------------------------------
### Sporozoite positivity plot that includes NAs
## -----------------------------------------------------------------------------------------------------------------------------------------

# plot sporozoite positivity data
spor_palette = c("#4b6043", "#9dba9a", "#ECECEC")

sporozoite_plot <- ggplot(sporozoite_gambiae_data, aes(x = collection_type, y = count, fill = result)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 4) +
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
