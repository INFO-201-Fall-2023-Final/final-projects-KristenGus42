library(shiny)
library(shinydashboard)
library(plotly)

# Data Loading and Preprocessing
alcohol_df <- read.csv("alcohol_df.csv")
life_df <- read.csv("life_df.csv")
merged_df <- merge(life_df, alcohol_df, by = c("Country", "Year"))

processed_df <- merged_df[, c("Country", "Life.expectancy", "Alcohol", "percentage.expenditure", "WHO.Region", "Status")]
processed_df <- na.omit(processed_df)
processed_df$Average_Alcohol_Consumption <- processed_df$Alcohol 

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
                    # Medium-sized image on the left
                    tags$img(src = "https://files.oaiusercontent.com/file-e0jtck1kbOPLwlipmA9p8tGk?se=2023-12-08T05%3A09%3A43Z&sp=r&sv=2021-08-06&sr=b&rscc=max-age%3D3599%2C%20immutable&rscd=attachment%3B%20filename%3Dimage.png&sig=qS7dPkEkDoLRFNZxxjQ/6Kk052glrra8htGDQ99iTnU%3D", 
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
  tabPanel("World Map Trends"),
  tabPanel("Consumption Barchart")
)

# Server Logic
server <- function(input, output) {
  # Reactive expression for filtered data
  filtered_data <- reactive({
    if (input$statusFilter == "All Countries") {
      processed_df
    } else {
      processed_df[processed_df$Status == input$statusFilter, ]
    }
  })
  
  # Plotly scatterplot rendering logic
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
}

# Run the application 
shinyApp(ui, server)
