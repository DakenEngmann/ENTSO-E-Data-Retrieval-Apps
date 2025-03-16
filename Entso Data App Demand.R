#
#
#               Shiny Demand Data Retrieval App
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
#install.packages("lubridate")
#install.packages("dplyr")
#install.packages("jsonlite")
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
library(dplyr)
library(jsonlite)


#
#
#=============================  USER INTERFACE   ==============================
#
# 
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
      
      menuItem("Demand", tabName = "Demand", icon = icon("plug"))  
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
    #        Tab Items
    #
    #
    tabItems(
      # 
      #
      #
      #
      #
#--------------------   Demand Tab  -------------------------------------------
      #
      #
      tabItem(tabName = "Demand",
              fluidRow(
                column(4,
                       box(
                         title = "Select Date Range", 
                         status = "primary",
                         solidHeader = TRUE,
                         width = NULL,  
                         dateRangeInput("demand_date_range", "Date Range:",
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
                         selectInput("demand_country1", "Country 1:",
                                     choices = c("Germany" = "10Y1001A1001A83F", 
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
                         selectInput("demand_country2", "Country 2:",
                                     choices = c("None" = "None", 
                                                 "Germany" = "10Y1001A1001A83F", 
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
                         selectInput("demand_country3", "Country 3:",
                                     choices = c("None" = "None", 
                                                 "Germany" = "10Y1001A1001A83F", 
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
                         actionButton("get_demand_data", "Get Demand Data")
                       )
                )
              ),
              fluidRow(
                box(
                  title = HTML("Demand Table &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; note: ZULU time and hour ENDING"),
                  status = "info",
                  solidHeader = TRUE,
                  width = 4,  
                  DTOutput("demand_table"),
                  downloadButton("download_demand_csv", "Download CSV")
                ),
                box(
                  title = "Demand Plot",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 8,
                  plotlyOutput("demand_plot"),
                  
                  # Add button to trigger fullscreen modal
                  div(style = "position: absolute; top: 10px; right: 10px;",
                      actionBttn("fullscreen_demand", label = "Full Screen", 
                                 style = "simple", color = "primary"))
                )
              ),
              
              # Fullscreen modal for demand plot
              bsModal("modal_demand", "Full Screen Demand Plot", "fullscreen_demand", size = "large",
                      plotlyOutput("fullscreen_demand_plot", height = "95vh")
              )
      )
      
    )
  ) 
)
#
#
#=================================    SERVER  ==================================
#
# 
#
server <- function(input, output) {
#
#
#---------------------------  Demand Tab Server Items  -------------------------
#
#  
  # Function to fetch and process ENTSO-E data
  fetch_demand_data <- function(country_code, start_date, end_date, api_key) {
    demand_api_url <- paste0(
      "https://web-api.tp.entsoe.eu/api?",
      "securityToken=", api_key,
      "&documentType=A65&processType=A16",
      "&outBiddingZone_Domain=", country_code,
      "&periodStart=", format(start_date, "%Y%m%d%H%M"),
      "&periodEnd=", format(end_date, "%Y%m%d%H%M")
    )
    demand_response <- GET(demand_api_url)
    
    if (http_status(demand_response)$category == "Success") {
      demand_content_xml <- content(demand_response, as = "text", encoding = "UTF-8")
      demand_xml_content <- read_xml(demand_content_xml)
      demand_ns <- c(demand_ns = "urn:iec62325.351:tc57wg16:451-6:generationloaddocument:3:0")
      demand_points <- xml_find_all(demand_xml_content, ".//demand_ns:TimeSeries//demand_ns:Point", demand_ns)
      
      valid_start_time <- as.POSIXct(
        xml_text(xml_find_first(demand_xml_content, ".//demand_ns:Period/demand_ns:timeInterval/demand_ns:start", demand_ns)), 
        format = "%Y-%m-%dT%H:%MZ", tz = "UTC"
      ) + minutes(15)
      
      demand_df <- data.frame(
        datetime = valid_start_time + (as.numeric(xml_text(xml_find_all(demand_points, "demand_ns:position", demand_ns))) - 1) * 900,
        demand = as.numeric(xml_text(xml_find_all(demand_points, "demand_ns:quantity", demand_ns)))
      )
      
      return(demand_df)
    } else {
      return(NULL)
    }
  }
  
  #  fetch Ireland demand data from JSON source
  fetch_ireland_data <- function(start_date, end_date) {
    api_url <- "https://www.smartgriddashboard.com/DashboardService.svc/data"
    query_params <- list(
      area = "demandactual",
      region = "ALL",
      datefrom = format(start_date, "%d-%b-%Y %H:%M"),
      dateto = format(end_date, "%d-%b-%Y %H:%M")
    )
    response <- GET(api_url, query = query_params)
    
    if (status_code(response) == 200) {
      load <- fromJSON(content(response, as = "text"))
      if (!is.null(load$Rows)) {
        demand_df <- data.frame(
          datetime = as.POSIXct(load$Rows$EffectiveTime, format = "%d-%b-%Y %H:%M:%S", tz = "UTC"),
          demand = as.numeric(load$Rows$Value)
        )
        return(demand_df)
      } else {
        return(NULL)
      }
    } else {
      return(NULL)
    }
  }
  
  # Reactive data fetch on button click
  demand_data <- eventReactive(input$get_demand_data, {
    api_key
    start_date <- as.POSIXct(input$demand_date_range[1], tz = "UTC")
    end_date <- as.POSIXct(input$demand_date_range[2], tz = "UTC") + days(1)
    
    df_list <- list()
    
    countries <- c("Germany"="10Y1001A1001A83F", "France"="10YFR-RTE------C", "Ireland"="10Y1001A1001A59C")
    
    for (country_input in c(input$demand_country1, input$demand_country2, input$demand_country3)) {
      if (country_input == "10Y1001A1001A59C") {
        df_ireland <- fetch_ireland_data(start_date, end_date)
        if (!is.null(df_ireland)) {
          df_ireland$country <- "Ireland"
          df_list[[length(df_list) + 1]] <- df_ireland
        }
      } else if (country_input != "None") {
        df_other <- fetch_demand_data(country_input, start_date, end_date, api_key)
        if (!is.null(df_other)) {
          df_other$country <- names(which(countries == country_input))
          df_list[[length(df_list) + 1]] <- df_other
        }
      }
    }
    
    if (length(df_list) > 0) {
      combined_df <- bind_rows(df_list)
      combined_df
    } else {
      NULL
    }
  })
  
  # Output Demand Table
  output$demand_table <- renderDT({
    df <- demand_data()
    if (!is.null(df)) {
      datatable(df, options = list(pageLength = 25))
    }
  })
  
  # Standard Demand Plot (Main UI)
  output$demand_plot <- renderPlotly({
    req(demand_data())  # Ensure data is available
    
    ggplotly(
      ggplot(demand_data(), aes(x = datetime, y = demand, color = country, group = country)) +
        geom_line(size = 1) + 
        geom_point(size = 2) +
        scale_x_datetime(date_labels = "%Y-%m-%d %H:%M", date_breaks = "6 hours") +  
        scale_color_manual(values = c("Germany" = "darkblue", "France" = "red", "Ireland" = "darkgreen")) +
        labs(title = "Demand Plot", x = "Datetime (Hour Ending)", y = "Demand (MW)", color = "Country") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 80, vjust = 1, hjust = 1))
    )
  })
  
  # Fullscreen Demand Plot (Modal)
  output$fullscreen_demand_plot <- renderPlotly({
    req(demand_data())  # Ensure data is available
    
    ggplotly(
      ggplot(demand_data(), aes(x = datetime, y = demand, color = country, group = country)) +
        geom_line(size = 1) + 
        geom_point(size = 2) +
        scale_x_datetime(date_labels = "%Y-%m-%d %H:%M", date_breaks = "6 hours") +  
        scale_color_manual(values = c("Germany" = "darkblue", "France" = "red", "Ireland" = "darkgreen")) +
        labs(title = "Full Screen - Demand Plot", x = "Datetime (Hour Ending)", y = "Demand (MW)", color = "Country") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 80, vjust = 1, hjust = 1))
    )
  })
  
  
  # Download CSV functionality
  output$download_demand_csv <- downloadHandler(
    filename = function() {
      paste("demand_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      df <- demand_data()  # Get the reactive dataset
      if (!is.null(df)) {
        write.csv(df, file, row.names = FALSE)  # Save CSV file
      }
    }
  )
  
  
}



shinyApp(ui, server)

