# Thomas Massie, 03.10.2017, Zurich




# Load libraries.
library(tidyverse)
library(ggplot2)
library(TTR)
library(lubridate)
library(smooth)
library(zoo)
library(tidyquant)
library(scales)
library(shiny)
library(shinythemes)




# ------------------------------------------
# Load data.
dd.allCountries <- read_csv("~/Library/Mobile Documents/com~apple~Clouddocs/Data analysis/Global temperature change/GlobalLandTemperaturesByCountry.csv")
str(dd.allCountries)
dd.overall <- read_csv("~/Library/Mobile Documents/com~apple~Clouddocs/Data analysis/Global temperature change/GlobalTemperatures.csv")
str(dd.overall)




# ------------------------------------------
# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("simplex"),
                titlePanel("Climate data"),
                sidebarLayout(
                  sidebarPanel(
                    # textInput("txt", "Text input:", "text here"),
                    selectInput(inputId = "selected.country", 
                                label = "Country:", 
                                choices = unique(dd.allCountries$Country), 
                                selected = NULL, multiple = FALSE,
                                selectize = TRUE, width = NULL, size = NULL),
                    selectInput(inputId = "min.year", 
                                label = "from (year):", 
                                choices = unique(year(dd.allCountries$dt)), 
                                selected = 1850, multiple = FALSE,
                                selectize = TRUE, width = NULL, size = NULL),
                    selectInput(inputId = "max.year", 
                                label = "to (year):", 
                                choices = unique(year(dd.allCountries$dt)), 
                                selected = 2013, multiple = FALSE,
                                selectize = TRUE, width = NULL, size = NULL)
                    # sliderInput("range", "Range:",
                    #             min = , max = max(dd.overall$Year),
                    #             value = c(1880,2010))
                  ),
                  mainPanel(
                    plotOutput(outputId = "figure1"),
                    plotOutput(outputId = "figure2")
                  )
                )
)




# ------------------------------------------
# Define UI for application that draws a histogram
server <- function(input, output) {
  
  dataInput <- reactive({
    
    # ------------------------------
    # Parameters
    
    # 'min.year' and 'max.year' for analysis.
    range    <- c(input$min.year, input$max.year)
    # range    <- c(1880, 2010)
    
    # Choose country for plotting.
    selected.country = input$selected.country
    
    
    # ------------------------------
    # Data wrangling.
    
    # Adding indicator 'Year' and filtering for specific country.
    
    dd.allCountries <- dd.allCountries %>%
      mutate(Month = month(dt, label = TRUE, abbr = TRUE)) %>%
      mutate(Year = year(dt)) %>%
      filter(Year >= range[1] & Year < range[2])
    
    dd.overall <- dd.overall %>%
      mutate(Month = month(dt, label = TRUE, abbr = TRUE)) %>%
      mutate(Year = year(dt)) %>%
      filter(Year >= range[1] & Year < range[2])
    
    # Summarise data to obtain annual temperatures.
    
    dd.allCountries.sum <- dd.allCountries %>%
      group_by(Year, Country) %>%
      summarize(Mean.Annu.Temp = mean(AverageTemperature, na.rm = TRUE),
                SD.Annu.Temp = sd(AverageTemperature, na.rm = TRUE)) %>%
      arrange(Country)
    
    dd.overall.sum <- dd.overall %>%
      group_by(Year) %>%
      summarize(Mean.Annu.Land.Temp = mean(LandAverageTemperature, na.rm = TRUE),
                SD.Annu.Land.Temp = sd(LandAverageTemperature, na.rm = TRUE)) 
    
    
    # ------------------------------
    # A list of output data.
    
    list(dd.allCountries = dd.allCountries, 
         dd.allCountries.sum = dd.allCountries.sum, 
         dd.overall = dd.overall,
         dd.overall.sum = dd.overall.sum)
    
  })
  
  
  output$figure1 <- renderPlot({
    # ------------------------------------------
    # Ploting annual cycles.
    
    # Plot data for selected country
    dd <- dataInput()[["dd.allCountries"]]
    dd %>%
      group_by(Country) %>%
      filter(Country == input$selected.country) %>%
      # group_by(Year) %>%
      ggplot(aes(x = Month, 
                 y = AverageTemperature,
                 group = Year,
                 color = Year)) +
      scale_color_distiller(palette = "Spectral") +
      geom_line(size = 0.8, alpha = 0.3) +
      theme_classic() +
      theme(legend.justification = c(0, 0), legend.position = c(0.04, 0.6)) +
      guides(color = guide_colorbar(barwidth = 0.5, barheight = 6)) +
      xlab("Month") +
      ylab("Temperature (°Celcius)") +
      labs(title = input$selected.country)
  })
  
  output$figure2 <- renderPlot({
    # ------------------------------------------
    # Ploting time series.
    
    # Plot data for selected country
    dd <- dataInput()[["dd.allCountries.sum"]]
    # Plot averaged time series for selected country.
    dd %>%
      group_by(Country) %>%
      filter(Country == input$selected.country) %>%
      ggplot(aes(x = Year, 
                 y = Mean.Annu.Temp)) +
      geom_point(size = 0.5, alpha = 0.5) +
      geom_line(size = 0.5, alpha = 0.5) +
      geom_smooth(color = "#1172B0", alpha = 0.8, span = 0.4) +
      geom_ma(ma_fun = SMA, n = 30, size = 0.4, color = "#F73531", linetype = 1) +
      # theme_bw() +
      theme_classic() +
      xlab("Year") +
      ylab("Temperature (°Celcius)") +
      labs(title = input$selected.country)
  })
  
}







# Run the application 
shinyApp(ui = ui, server = server)

