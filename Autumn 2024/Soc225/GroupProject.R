#Graph 1
require(ggplot2)
require(dplyr)
require(readr)
carbon <- read_csv("Autumn 2024/Soc225/Carbon Dioxide Emission Estimates.csv")
species <- read_csv("Autumn 2024/Soc225/Threatened Species.csv")

carbon = rename(carbon, Country = `CO2 emission estimates`)
carbon = rename(carbon, Carbon_Value = `Value`)
species = rename(species, Country = `Threatened species`)
species = rename(species, Species_Value = `Value`)


filtered_data_carbon <- carbon %>%
  filter(Series == "Emissions per capita (metric tons of carbon dioxide)")

merged_data <- left_join(species, filtered_data_carbon, by = c("Country", "Year"))

filtered_merge <- merged_data %>% 
  filter(Year == "2017")

filtered_merge2 <- filtered_merge %>%
  filter(!is.na(Carbon_Value), Series.x == "Threatened Species: Total (number)")


plot_unzoomed <- ggplot(filtered_merge2, aes(x = Carbon_Value, y = Species_Value)) +
  geom_point(color = "blue", size = 2, alpha = 0.6) +  # Scatter plot
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # Linear regression line
  labs(
    title = "Relationship Between Carbon Emissions and Threatened Species (Unzoomed)",
    x = "Carbon Emissions (Metric Tons per Capita)",
    y = "Number of Threatened Species"
  ) +
  theme_minimal()

plot_zoom <- ggplot(filtered_merge2, aes(x = Carbon_Value, y = Species_Value)) +
  geom_point(color = "blue", size = 2, alpha = 0.6) +  # Scatter plot
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # Linear regression line
  labs(
    title = "Relationship Between Carbon Emissions and Threatened Species (Zoomed)",
    x = "Carbon Emissions (Metric Tons per Capita)",
    y = "Number of Threatened Species"
  ) +
  theme_minimal() +
  ylim(0,500)

lm_model <- lm(Species_Value ~ Carbon_Value, data = filtered_merge2)
summary(lm_model)

par(mfrow = c(2, 2))  # Arrange plots in a 2x2 grid
lm_plot <- plot(lm_model)

ggsave("plot_unzoomed.png", plot = plot_unzoomed, width = 8, height = 6,dpi=300)
ggsave("plot_zoomed.png", plot = plot_zoom, width = 8, height = 6,dpi=300)
ggsave("plot_lm.png", plot = lm_plot, width = 8, height = 6,dpi=300)


png("Autumn 2024/Soc225/lm_diagnostics.png", width = 800, height = 800)

# Arrange the plots in a 2x2 grid
par(mfrow = c(2, 2))

# Plot the diagnostic plots
lm_plot <- plot(lm_model)

# Close the device to save the file
dev.off()

gdp <- read_csv("Autumn 2024/Soc225/UNdata_Export_20241122_154003154.csv")

gdp = rename(gdp, Country = `Country or Area`)
gdp = rename(gdp, GDP_Value = Value)

new_merge <- left_join(gdp, filtered_merge2, by = "Country")

new_merge <- new_merge %>%
  mutate(Prosperity_Category = case_when(
    GDP_Value <= 1135 ~ "Low prosperity",
    GDP_Value > 1135 & GDP_Value <= 4465 ~ "Lower-Mid prosperity",
    GDP_Value > 4465 & GDP_Value <= 13845 ~ "High-Mid prosperity",
    GDP_Value > 13845 ~ "High prosperity",
    TRUE ~ NA_character_  
  ))


avg_by_category <- new_merge %>%
  group_by(Prosperity_Category) %>%
  summarise(avg_species_value = mean(Species_Value, na.rm = TRUE))

# Plot with vertical lines at the average for each GDP category
gdp_cat_species2 <- ggplot(new_merge, aes(x = Species_Value, fill = Prosperity_Category)) +
  geom_histogram() +  # Create histogram for Threatened Species
  facet_wrap(~ Prosperity_Category) +  # Facet by Prosperity Category with free y scales
  labs(
    title = "Threatened Species Across GDP Categories",
    x = "Number of Threatened Species",
    y = "Number of Countries",
    fill = "Prosperity Category"
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +  # Adjust y-axis breaks
  theme_minimal() +
  theme(legend.position = "none") +
  xlim(0, 800) +
  geom_vline(data = avg_by_category, aes(xintercept = avg_species_value), 
             color = "red", linetype = "dashed", linewidth = 1) +  # Add vertical line for averages
  geom_text(data = avg_by_category, aes(x = avg_species_value+200, y = 7, 
                                        label = paste("Avg: ", round(avg_species_value, 0))),
            color = "red", fontface = "italic", size = 4, vjust = 1)

gdp_cat_species2
ggsave("species_gdp.png", plot = gdp_cat_species2, width = 8, height = 6,dpi=300)

lm_model2 <- lm(Species_Value ~ GDP_Value, data = new_merge)
summary(lm_model2)
