library(shiny)
library(ggplot2)
library(dplyr)

server <- function(input, output, session) {
  
  # Data processing code
  carbon <- read_csv("Carbon Dioxide Emission Estimates.csv")
  species <- read_csv("Threatened Species.csv")
  
  carbon <- carbon %>%
    rename(Country = `CO2 emission estimates`, Carbon_Value = Value) %>%
    filter(Series == "Emissions per capita (metric tons of carbon dioxide)")
  
  species <- species %>%
    rename(Country = `Threatened species`, Species_Value = Value)
  
  merged_data <- left_join(species, carbon, by = c("Country", "Year"))
  
  filtered_merge2 <- merged_data %>%
    filter(Year == "2017", 
           !is.na(Carbon_Value), 
           Series.x == "Threatened Species: Total (number)")
  
  gdp <- read_csv("UNdata_Export_20241122_154003154.csv") %>%
    rename(Country = `Country or Area`, GDP_Value = Value)
  
  new_merge <- reactive({
    left_join(gdp, filtered_merge2, by = "Country") %>%
      mutate(Prosperity_Category = case_when(
        GDP_Value <= 1135 ~ "Low prosperity",
        GDP_Value > 1135 & GDP_Value <= 4465 ~ "Lower-Mid prosperity",
        GDP_Value > 4465 & GDP_Value <= 13845 ~ "High-Mid prosperity",
        GDP_Value > 13845 ~ "High prosperity",
        TRUE ~ NA_character_
      ))
  })
  
  observe({
    updateSelectInput(session, "prosperity",
                      choices = c("All", unique(new_merge()$Prosperity_Category)))
  })
  
  # Filtered data for plots
  filtered_data <- reactive({
    data <- new_merge()
    if (input$prosperity != "All") {
      data <- data %>% filter(Prosperity_Category == input$prosperity)
    }
    data <- data %>% filter(Species_Value >= input$species_range[1],
                            Species_Value <= input$species_range[2])
    data
  })
  
  observe({
    species_values <- new_merge()$Species_Value
    updateSliderInput(session, "species_range",
                      min = min(species_values, na.rm = TRUE),
                      max = max(species_values, na.rm = TRUE),
                      value = c(min(species_values, na.rm = TRUE), 
                                max(species_values, na.rm = TRUE)))
  })
  
  # Outputs (same as before)
  output$scatterplot1 <- renderPlot({
    ggplot(filtered_data(), aes(x = Carbon_Value, y = Species_Value)) +
      geom_point(aes(color = Prosperity_Category), alpha = 0.7) +
      geom_smooth(method = "lm", se = FALSE, color = "black") +
      labs(title = "Carbon Emissions vs Threatened Species",
           x = "Carbon Emissions (Metric Tons per Capita)",
           y = "Number of Threatened Species") +
      theme_minimal()
  })
  
  output$scatterplot2 <- renderPlot({
    ggplot(filtered_data(), aes(x = log10(GDP_Value), y = Species_Value)) +
      geom_point(aes(color = Prosperity_Category), alpha = 0.7) +
      geom_smooth(method = "lm", se = FALSE, color = "black") +
      labs(title = "Log GDP vs Threatened Species",
           x = "Log GDP Value (USD)",
           y = "Number of Threatened Species") +
      theme_minimal()
  })
  
  output$histogram <- renderPlot({
    ggplot(filtered_data(), aes(x = Species_Value, fill = Prosperity_Category)) +
      geom_histogram() +
      facet_wrap(~ Prosperity_Category) +
      labs(title = "Distribution of Threatened Species by Prosperity",
           x = "Number of Threatened Species",
           y = "Count") +
      theme_minimal()
  })
  
  output$model_summary <- renderPrint({
    lm_model <- lm(Species_Value ~ log10(GDP_Value), data = filtered_data())
    anova_model <- aov(Species_Value ~ Prosperity_Category, data = filtered_data())
    list(
      "Linear Model Summary (Species to GDP):" = summary(lm_model),
      "ANOVA for Prosperity Levels (Species to GDP):" = summary(anova_model)
    )
  })
}