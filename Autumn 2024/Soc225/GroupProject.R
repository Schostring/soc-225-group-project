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
  filter(!is.na(Carbon_Value))


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


