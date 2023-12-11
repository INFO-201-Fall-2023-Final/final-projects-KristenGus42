#Head function: https://www.digitalocean.com/community/tutorials/head-and-tail-function-r
library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(ggplot2)
library(sf)
library(maps)

source("main.R")

# Data Loading and Preprocessing
# alcohol_df <- read.csv("alcohol_df.csv")
# life_df <- read.csv("life_df.csv")
# merged_df <- merge(life_df, alcohol_df, by = c("Country", "Year"))

processed_df <- merged_df[, c("Country", "Life.expectancy", "Alcohol", "percentage.expenditure", "WHO.Region", "Status")]
processed_df <- na.omit(processed_df)
processed_df$Average_Alcohol_Consumption <- processed_df$Alcohol

# Bar Graph Data
reg_df <- merged_df 
WHO_regions <- c("Africa", "Americas", "South-East Asia", "Europe", "Eastern Mediterranean", "Western Pacific")

# Map Data 
world_map <- st_as_sf(map('world', plot = FALSE, fill = TRUE))

## ADDING
helper <- merged_df

helper <- filter(helper, Year == 2015)
helper <- filter(helper, Sex == "Both sexes")
helper_vec <- helper$Country
helper_vec[170] <- "USA"
helper_vec[43] <- "Czech Republic"
helper_vec[44] <- "North Korea"
helper_vec[76] <- "Iran"
helper_vec[89] <- "Laos"
helper_vec[106] <- "Micronesia"
helper_vec[130] <- "South Korea"
helper_vec[131] <- "Moldova"
helper_vec[155] <- "syria"
helper_vec[168] <- "UK"
helper_vec[169] <- "Tanzania"
helper_vec[174] <- "Venezuela"
helper_vec[175] <- "Vietnam"
helper_vec[133] <- "Russia"


helper$Country <- helper_vec
## ADDING

m_df <- merge(world_map, helper, by.x = "ID", by.y = "Country")
m_df_filtered <- m_df

selected_columns <- c('ID', 'Average_Alcohol_Consumption', 'geometry', 'Year', 'WHO.Region', 'Status')
m_df_filtered <- m_df[, selected_columns, drop = FALSE]

# Filter rows for the year 2015
m_df_filtered <- subset(m_df_filtered, Year == 2015)

# Keep only unique country entries
m_df_filtered <- unique(m_df_filtered)

# UI Definition
ui <- navbarPage(
  title = "Sippin' and Spending: A Global Study",
  tags$head(
    tags$style(HTML("
            body { 
                font-family: 'Calibri', sans-serif; 
            }
            h4, h1 { 
                color: #006400;  /* Dark green color */
                font-weight: bold;
            }
        "))
  ),
  
  # Home tab
  tabPanel("Home",
           fluidRow(
             column(6,
                    tags$img(src = "https://i.ibb.co/j6YWGYq/image.png", 
                             height = 550, 
                             width = "100%", 
                             style = "padding-left: 20px; padding-right: 10px;")
             ), 
             column(6, style = "padding-right: 50px;", 
                    h1("Welcome to Sippin' and Spending!"),
                    div(style = "font-size: 14px;",
                        h4("Project Overview"),
                        p("In this project we aim to analyze the correlation between alcohol consumption and life expectancy and how this relates to the economic landscapes of both developed and developing countries. This data provides important insights on how countries spend their money, lifestyle choices, and societal wellbeing."),
                        p("Understanding these connections can inform public health initiatives, economic policies, and individual choices, fostering a more comprehensive perspective on global well-being."),
                        p("It is interesting to see if alcohol consumption actually leads to lower life expectancy and if the status of the region people live in affects how much alcohol is accessible or that they consume."),
                        h4("Data Sources"),
                        p("We used data from the World Health Organization which contained information on the development status of countries, alcohol consumption per person in liters as well as average life expectancies in years."),
                        h4("Key Findings"),
                        p("We uncovered that developed countries with higher spending amounts have a larger average alcohol consumption as well as lower life expectancies than developing countries. To visualize these trends, please go through each webpage for interactive displays of this data!"),
                        p(a(href = "https://www.who.int/health-topics/alcohol#tab=tab_1", "Learn more about alcohol consumption on the WHO Alcohol Webpage", target = "_blank"))
                    )
             )
           )
  ),
  
  # Scatterplot Analysis tab
  tabPanel("Scatterplot Analysis",
           sidebarLayout(
             sidebarPanel(
               selectInput("statusFilter", "Select Country Classification", choices = c("All Countries", "Developed", "Developing")),
               div(
                 style = "background-color: whitesmoke; padding: 10px; border-radius: 5px; color: grey; text-align: left; margin-top: 10px;",
                 p("This scatterplot illustrates the correlation between life expectancy, alcohol consumption, and expenditure across various countries. Each dot represents a country, with its position indicating life expectancy and average alcohol consumption. The size of each dot corresponds to the country's expenditure on alcohol, providing insights into different health and economic dimensions."),
                 p(style = "font-style: italic;", "Note: Hover over the plot to see metrics for each country. Expenditure values range from 0 to 2000 USD.")
               )
             ),
             mainPanel(
               plotlyOutput("scatterPlot")
             )
           )
  ),
  
  # Additional tabs (placeholders for now)
  tabPanel("World Map Trends", 
           tags$h3("Welcome! This map shows the average alcohol consumption between 2000 and 2015 on a gradient for select countries.
            You can click/hover on the map to see the name of the country and its alchohol consumption amount.
            You can also select different regions to zoom into that area and learn if it is a region with developed 
            or developing countries!",style = "font-size: 16px;"),
           plotlyOutput(outputId = "map"),
           selectInput("region", "Select a Region", choices = c("", unique(m_df_filtered$WHO.Region))),
           verbatimTextOutput("regionInfo")
  ),
  tabPanel("Consumption Barchart",
           sidebarLayout(
             sidebarPanel(
               p("These graphs compare total expenditure to average alcohol consumption in specified regions. These graphs help zoom in to specific regions to observe trends that can be seen at a larger level. Here it can be observed that most regions and countries with a higher average rate of drinking, there is a higher total expenditure. This means that the more citizens drink, the more the country is spending, particularly on health."),
               selectInput(
                 inputId = "regName",
                 label = "Select a Region",
                 choices = WHO_regions
               ), 
               radioButtons("choice", "Do you want to see all of the countries, or restrict the view?",
                            c("All" = "all", 
                              "Restrict" = "res"), 
                            selected = "all"),
               sliderInput(
                 inputId = "slideAlc",
                 label = "How many of the top alcohol consumption:",
                 min = 0,
                 max = 10,
                 value = 5
               )
             ),
             mainPanel(
               plotlyOutput("barGraph"),
               plotlyOutput("alcGraph")
             )
           )
          )
)

# Server Logic
server <- function(input, output) {
  # Back end Bar graph: 
  filtered <- reactive({
    if(input$choice == "all") {
      reg<- input$regName
      return(merged_df[merged_df$WHO.Region== reg, ])
    } else {
      reg <- input$regName
      reg_df <- merged_df[merged_df$WHO.Region== reg, ]
      reg_df <- arrange(reg_df, Average_Alcohol_Consumption)
      reg_df <- filter(reg_df, Year == "2000")
      reg_df <- filter(reg_df, Sex == "Both sexes")
      reg_df <- head(reg_df, input$slideAlc)
      return(reg_df)
    }
  })
  
  output$barGraph <- renderPlotly({
    p <- plot_ly(data = filtered(), 
                 x = ~Country, 
                 y = ~Average_Alcohol_Consumption,
                 type = 'bar')  %>% layout(
                   title = "Average Alcohol Consumption per Country",
                   xaxis = list(title = "Country"),
                   yaxis = list(title = "Average Alcohol Consumption (in Liters)")
                 )
  })
  
  output$alcGraph <- renderPlotly({
    # Generate plot using plotly
    p <- plot_ly(data = filtered(), 
                 x = ~Country, 
                 y = ~Total.expenditure,
                 type = 'bar', 
                 color = "red") %>% layout(
                   title = "Total Expenditure",
                   xaxis = list(title = "Country"),
                   yaxis = list(title = "Total Expenditure")
                 )
    
  })
  
  
  # Back end Scatter Plot:
  filtered_data <- reactive({
    if (input$statusFilter == "All Countries") {
      processed_df
    } else {
      processed_df[processed_df$Status == input$statusFilter, ]
    }
  })

  output$scatterPlot <- renderPlotly({
    p <- plot_ly(
      data = filtered_data(), 
      x = ~Average_Alcohol_Consumption, 
      y = ~Life.expectancy,
      size = ~percentage.expenditure, 
      color = ~WHO.Region, 
      type = 'scatter', 
      mode = 'markers', 
      text = ~paste("Country:", Country, "<br>Spending (USD):", percentage.expenditure, "<br>Alcohol Consumption:", Average_Alcohol_Consumption),
      hoverinfo = "text"
    ) %>% layout(
      title = "Average Alcohol Consumption vs Life Expectancy",
      xaxis = list(title = "Average Alcohol Consumption (in Liters)"),
      yaxis = list(title = "Life Expectancy (in Years)")
    )
    p
  })
  
  # Back end Map 
  output$map <- renderPlotly({
    #Creates a new df that only has countries of that region, and only they will be plotted on map
    selected_region <- input$region
    filtered_data <- m_df_filtered
    if (selected_region != "") {
      filtered_data <- m_df_filtered[m_df_filtered$WHO.Region == selected_region, ]
    }
    
    map <- ggplot(filtered_data, aes(fill = Average_Alcohol_Consumption, text = paste(ID, "<br>Consumption: ", Average_Alcohol_Consumption, "liters"))) +
      geom_sf(color = "white", size = 0.2) +
      scale_fill_gradient(low = "lightcoral", high = "darkred", name = "Consumption (Liters)") +
      theme_minimal() +
      labs(title = "Average Alcohol Consumption 2000-2015")
    
    ggplotly(map, tooltip = c("text", "Consumption (Liters)"))
    
    
    
  })
  
  output$regionInfo <- renderText({
    # Displays a line stating if the most countries in that region are developed or developing
    selected_region <- input$region
    if (is.null(selected_region) || selected_region == "") {
      return(NULL)
    }
    
    filtered_data <- m_df_filtered
    
    if (selected_region != "") {
      filtered_data <- m_df_filtered[m_df_filtered$WHO.Region == selected_region, ]
    }
    
    majority_status <- if (nrow(filtered_data) > 0) {
      count_by_status <- table(filtered_data$Status)
      majority_status <- names(count_by_status)[which.max(count_by_status)]
    } else {
      NA
    }
    
    label_text <- if (is.na(majority_status)) {
      ""
    } else {
      paste("The majority of countries in this region are", majority_status)
    }
    
    return(label_text)
  })
  
  
}

# Run the application 
shinyApp(ui, server)
