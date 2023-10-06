source("MTO in IQR.R")

# Libraries
library(shiny)
library(shinythemes)
library(DT)
library(dplyr)

# UI
ui <- fluidPage(
  
  # Custom CSS for navy blue and text color
  tags$head(
    tags$style(HTML("
      .navbar {
        background-color: #000080;
      }
      .navbar-default .navbar-brand,
      .navbar-default .navbar-nav>li>a {
        color: #ffffff;
      }
    ")),
    tags$link(rel = "shortcut icon", href = "www/VenturaFoodsLogo.png")
  ),
  
  # Navbar with logo
  navbarPage("MTO - Ship Location Exposure Data Web App", id = "nav",
             tabPanel("Main Data",
                      fluidRow(
                        column(12,
                               tags$img(src = "VenturaFoodsLogo.png", height = 72, width = 650, align = "left")  # Logo
                        )
                      ),
                      fluidRow(
                        column(12,
                               selectInput("filter_loc", "Filter by Location:", choices = c("All", sort(unique(as.numeric(as.character(mto_tab_2$Location))))), selected = "All")
                        )
                      ),
                      fluidRow(
                        column(12,
                               selectInput("filter_item_2", "Filter by Item:", choices = c("All", sort(unique(mto_tab_2$Item))), selected = "All")
                        )
                      ),
                      fluidRow(
                        column(12,
                               tags$div("Should you have any queries, feel free to reach out to Stan Lee at slee@venturafoods.com", style = "position: absolute; top: -50px; right: 15px;")
                        )
                      ),
                      DTOutput("data_table")
             ),
             tabPanel("Summary by Location",
                      fluidRow(
                        column(12,
                               tags$img(src = "VenturaFoodsLogo.png", height = 72, width = 650, align = "left")  # Logo (same size as the first page)
                        )
                      ),
                      fluidRow(
                        column(12,
                               h3("Exposure Amount by Location")
                        )
                      ),
                      fluidRow(
                        column(12,
                               tags$div("Should you have any queries, feel free to reach out to Stan Lee at slee@venturafoods.com", style = "position: absolute; top: -50px; right: 15px;")
                        )
                      ),
                      DTOutput("summary_table")
             )
  )
)

# Server
server <- function(input, output, session) {
  
  # Existing code for tables, unchanged
  output$data_table <- renderDT({
    data_filtered <- mto_tab_2
    
    if (input$filter_loc != "All") {
      data_filtered <- data_filtered[data_filtered$Location == as.numeric(input$filter_loc), ]
    }
    
    if (input$filter_item_2 != "All") {
      data_filtered <- data_filtered[data_filtered$Item == input$filter_item_2, ]
    }
    
    data_filtered <- data_filtered %>% arrange(as.numeric(Location))  # Sort by location
    
    datatable(data_filtered, rownames = FALSE,
              extensions = "Buttons",  # Enable the Buttons extension
              options = list(
                pageLength = 100,
                scrollY = TRUE,
                dom = "Blfrtip",  # Specify the button options
                buttons = c("copy", "csv", "excel", "pdf")  # Include the download buttons
              )) %>% 
      formatCurrency("Unit Cost", currency = '$', digits = 2) %>%  
      formatCurrency("Exposure Amount", currency = '$', digits = 0)
  })
  
  output$summary_table <- renderDT({
    summary_data <- mto_tab_3 
    
    datatable(summary_data, rownames = FALSE,
              extensions = "Buttons",  # Enable the Buttons extension
              options = list(
                pageLength = 100,
                scrollY = TRUE,
                dom = "Blfrtip",  # Specify the button options
                buttons = c("copy", "csv", "excel", "pdf")  # Include the download buttons
              )) %>% 
      formatCurrency("Exposure Amount", currency = '$', digits = 0) %>% 
      formatStyle(columns = 'Location', 
                  target = 'row',
                  backgroundColor = styleEqual("Grand Total", "lightblue"))
  })
}

# Run the app
shiny::shinyApp(ui, server)
