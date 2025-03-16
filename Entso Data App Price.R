#
#
#               Shiny Entso E Price Data Retrieval App
#
#
#
#                  note that this app requires the packages below
#
#
#            
#
#install.packages("shiny")
#install.packages("shinydashboard")
#install.packages("httr")
#install.packages("xml2")
#install.packages("DT")
#install.packages("ggplot2")
#install.packages("shinyWidgets")
#install.packages("shinyBS")
#install.packages("plotly")
#install.packages("shinyalert)
#
#
#
#
#                                  also to use this app you need to obtain an API key 
#
#                                    from the ENTSO - E website.  This is free
#
#              go to :   https://uat-transparency.entsoe.eu/content/static_content/Static%20content/web%20api/how_to_get_security_token.html
#
#
#
#                      insert your API key here
#
#
#
api_key <- "f49357e3-3b21-428c-8cdb-2bb21fbd577f"


# Load required libraries
library(shiny)
library(shinydashboard)
library(httr)
library(xml2)
library(DT)  
library(ggplot2)  
library(shinyWidgets)  
library(shinyBS)  
library(plotly)
library(shinyalert)
library(lubridate)



# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "ENTSO-E Data App"),
  #
  #
  #              Sidebar
  #
  #
  dashboardSidebar(
    width = 150,  
    sidebarMenu(
      menuItem("Price Data", tabName = "Price", icon = icon("euro-sign"))
    )
  ),
  #
  #
  #              Dashboard body formatting
  #
  #
  dashboardBody(
    tags$head(
      tags$style(HTML("
    .modal-lg, .modal-dialog {
      width: 98% !important;
      max-width: 98% !important;
      height: 98vh !important; 
      max-height: 98vh !important;
    }
    .modal-content {
      height: 97vh !important; 
      display: flex !important;
      flex-direction: column !important;
    }
    .modal-body {
      flex: 1 !important;
      overflow-y: auto !important; 
      display: flex !important;
      flex-direction: column !important;
      justify-content: center !important;
    }
    "))
    ),
    #
    #    
    #           Full Screen Button
    #
    #
    bsModal("modal_plot", "Full Screen Price Plot", "fullscreen_plot", size = "large",
            div(class = "fullscreen-modal",
                style = "height: 95vh; display: flex; flex-direction: column;",
                div(style = "flex: 1; overflow-y: auto;", 
                    plotlyOutput("fullscreen_price_plot", height = "100%"))
            )
    ),
    #
    #
    #        Tab Items
    #
    #
    tabItems(
      # 
      #
      #
      #                    Price Tab
      #
      tabItem(tabName = "Price",
              fluidRow(
                column(4,
                       box(
                         title = "Select Date Range", 
                         status = "primary",
                         solidHeader = TRUE,
                         width = NULL,  
                         dateRangeInput("price_date_range", "Date Range:",
                                        start = Sys.Date(),  
                                        end = Sys.Date() + 1,
                                        format = "yyyy-mm-dd")
                       )
                ),
                column(2,
                       box(
                         title = "Country 1",  
                         status = "primary",
                         solidHeader = TRUE,
                         width = NULL,  
                         selectInput("price_zone1", "Country 1:",
                                     choices = c("Germany" = "10Y1001A1001A82H", 
                                                 "France" = "10YFR-RTE------C",
                                                 "Ireland" = "10Y1001A1001A59C"),
                                     selected = "10Y1001A1001A82H")  
                       )
                ),
                column(2,
                       box(
                         title = "Country 2",  
                         status = "primary",
                         solidHeader = TRUE,
                         width = NULL,  
                         selectInput("price_zone2", "Country 2:",
                                     choices = c("None" = "None", 
                                                 "Germany" = "10Y1001A1001A82H", 
                                                 "France" = "10YFR-RTE------C",
                                                 "Ireland" = "10Y1001A1001A59C"),
                                     selected = "None")  
                       )
                ),
                column(2,
                       box(
                         title = "Country 3",  
                         status = "primary",
                         solidHeader = TRUE,
                         width = NULL,  
                         selectInput("price_zone3", "Country 3:",
                                     choices = c("None" = "None", 
                                                 "Germany" = "10Y1001A1001A82H", 
                                                 "France" = "10YFR-RTE------C",
                                                 "Ireland" = "10Y1001A1001A59C"),
                                     selected = "None")  
                       )
                ),
                column(2,
                       box(
                         title = "Actions",
                         status = "primary",
                         solidHeader = TRUE,
                         width = NULL,  
                         actionButton("get_price_data", "Get Data")
                       )
                )
              ),
              fluidRow(
                box(
                  title = HTML("Prices Table &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; note: ZULU time and hour ENDING"),
                  status = "info",
                  solidHeader = TRUE,
                  width = 4,  
                  DTOutput("price_table"),  
                  br(),
                  downloadButton("download_price_csv", "Download CSV")  
                ),
                box(
                  title = "Price Plot",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 8,
                  plotlyOutput("price_plot"), 
                  
                  div(style = "position: absolute; top: 10px; right: 10px;",
                      actionBttn("fullscreen_plot", label = "Full Screen", 
                                 style = "simple", color = "primary"))
                )
              )
              )
      
    )
  ) 
)



# Define Server
server <- function(input, output) {
  
  # **Function to Fetch and Parse ENTSO-E Price Data**
  get_price_data <- function(start_date, end_date, bidding_zone) {
    api_key 
    
    # Convert dates to ENTSO-E API format
    start_date <- format(as.POSIXct(start_date, tz="UTC"), "%Y%m%d%H%M")
    end_date <- format(as.POSIXct(end_date, tz="UTC"), "%Y%m%d%H%M")
    
    # Map bidding zone ID to country name
    country_name <- switch(bidding_zone,
                           "10Y1001A1001A82H" = "Germany",
                           "10YFR-RTE------C" = "France",
                           "10Y1001A1001A59C" = "Ireland",
                           "None" = NULL)
    
    if (is.null(country_name)) {
      return(data.frame(datetime = character(), price = numeric(), country = character(), stringsAsFactors = FALSE))
    }
    
    url <- paste0("https://web-api.tp.entsoe.eu/api?",
                  "securityToken=", api_key,
                  "&documentType=A44",
                  "&in_Domain=", bidding_zone,
                  "&out_Domain=", bidding_zone,
                  "&periodStart=", start_date,
                  "&periodEnd=", end_date,
                  "&processType=A01")  
    
    response <- GET(url)
    
    if (http_status(response)$category != "Success") {
      return(data.frame(datetime = NA, price = NA, country = country_name, Message = "API Request Failed"))
    }
    
    content_xml <- content(response, as = "text", encoding = "UTF-8")
    xml_content <- read_xml(content_xml)
    
    ns <- c(ns = "urn:iec62325.351:tc57wg16:451-3:publicationdocument:7:3")
    
    time_series_nodes <- xml_find_all(xml_content, ".//ns:TimeSeries", ns)
    time_series_60m <- time_series_nodes[
      xml_text(xml_find_first(time_series_nodes, ".//ns:Period/ns:resolution", ns)) == "PT60M"
    ]
    
    prices_df <- data.frame(datetime = character(), price = numeric(), country = character(), stringsAsFactors = FALSE)
    
    for (ts in time_series_60m) {
      start_time <- xml_text(xml_find_first(ts, ".//ns:Period/ns:timeInterval/ns:start", ns))
      start_time <- as.POSIXct(start_time, format="%Y-%m-%dT%H:%MZ", tz="UTC")
      
      points <- xml_find_all(ts, ".//ns:Period/ns:Point", ns)
      
      for (pt in points) {
        position <- as.numeric(xml_text(xml_find_first(pt, ".//ns:position", ns)))
        price <- as.numeric(xml_text(xml_find_first(pt, ".//ns:price.amount", ns)))
        
        # 
        timestamp <- start_time + (position - 1) * 60 * 60 - hours(1)  
        
        prices_df <- rbind(prices_df, data.frame(datetime = timestamp, price = price, country = country_name, stringsAsFactors = FALSE))
      }
    }
    
    return(prices_df)
  }
  
  # Trigger API Call When Button is Clicked
  price_data <- eventReactive(input$get_price_data, {
    req(input$price_date_range)
    
    if (input$price_date_range[1] > input$price_date_range[2]) {
      shinyalert("Error", "Start Date cannot be after End Date", type = "error")
      return(NULL)  
    }
    
    req(input$price_zone1)
    
    data1 <- get_price_data(input$price_date_range[1], input$price_date_range[2], input$price_zone1)
    data2 <- get_price_data(input$price_date_range[1], input$price_date_range[2], input$price_zone2)
    data3 <- get_price_data(input$price_date_range[1], input$price_date_range[2], input$price_zone3)
    
    combined_data <- rbind(data1, data2, data3)
    combined_data$datetime <- as.POSIXct(combined_data$datetime, tz="UTC")
    combined_data$price <- as.numeric(combined_data$price)
    combined_data <- combined_data[order(combined_data$datetime), ]
    
    return(combined_data)
  })
  
  # Table Output
  output$price_table <- renderDT({
    req(price_data())  
    datatable(price_data(), options = list(pageLength = 25, autoWidth = TRUE))
  })
  
  # Plot Output
  output$price_plot <- renderPlotly({
    req(price_data())  
    ggplotly(
      ggplot(price_data(), aes(x = datetime, y = price, color = country, group = country)) +
        geom_line(size = 1) + geom_point(size = 2) +
        scale_x_datetime(date_labels = "%Y-%m-%d %H:%M", date_breaks = "6 hours") +  
        scale_color_manual(values = c("Germany" = "darkblue", "France" = "red", "Ireland" = "darkgreen")) +
        labs(title = "Price Plot", x = "Datetime (Hour Ending)", y = "Price (EUR/MWh)", color = "Country") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 80, vjust = 1, hjust = 1))
    )
  })
  
  # Full Screen Plot Output
  output$fullscreen_price_plot <- renderPlotly({
    ggplotly(ggplot(price_data(), aes(x = datetime, y = price, color = country, group = country)) +
               geom_line(size = 1) + geom_point(size = 2) +
               scale_x_datetime(date_labels = "%Y-%m-%d %H:%M", date_breaks = "6 hours") +  
               scale_color_manual(values = c("Germany" = "darkblue", "France" = "red", "Ireland" = "darkgreen")) +
               labs(title = "Price Plot", x = "Datetime (Hour Ending)", y = "Price (EUR/MWh)", color = "Country") +
               theme_minimal() +
               theme(axis.text.x = element_text(angle = 80, vjust = 1, hjust = 1))
    )
  })  
  
  # CSV Download Button
  output$download_price_csv <- downloadHandler(
    filename = function() {
      paste("entso_e_prices_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(price_data(), file, row.names = FALSE)
    }
  )
}


shinyApp(ui, server)

