library(shiny)
library(janitor)
library(hereR)
library(DT)
library(tmap)
library(leaflet)

# Define UI for data upload app ----
ui <- fluidPage(
    
    fluidRow(style = "height:100px;",
             column(3,
                    style = "border-right: 1px solid white",
                    span(img(src = "wsp_logo.png", height = 30), 
                         # "Dashboard Dev Demo",
                         style="color: #9239F6;font-size:30px")
                         # style="font-size:30px")
             ),
             column(3,
                    offset = 6,
                    style = "background: #903495",
                    br(),
                    img(src='snapshot.png', width="10%", height="0%"))),
    
    # fluidRow(#style = "border-top: 1px solid white;height:20px;background-color: #903495;"
    #     style = "border-top: 1px solid white;height:10px;"),
    
    
    # App title ----
    titlePanel("Uploading Files"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(

        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Select a file ----
            fileInput("file1", "Choose CSV File",
                      multiple = TRUE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            # Input: Select quotes ----
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Select number of rows to display ----
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head"),
            

            actionButton("do", "Click Me"),
            passwordInput("password", "Set personal HERE API key:"),
            actionButton("go", "GeoCode it!")
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Uploaded Data",
                                 DT::dataTableOutput("contents")),
                        tabPanel("Geocoded Map",
                                 leafletOutput("mapSelected", height = 700)),
                        tabPanel("Geocode Data",
                                 DT::dataTableOutput("geocoded")))
            
            # # Output: Data file ----
            # tableOutput("contents")
            
        )
        
    )
)

# Define server logic to read selected file ----
server <- function(input, output) {
    
    output$contents <- DT::renderDataTable({
        req(input$file1)
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote) %>% 
            remove_constant()
        
        if(input$disp == "head") {
            return(head(df))
        }
        else {
            return(df)
        }
    })
    
    uploadInput <- reactive({
        req(input$file1)
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote, stringsAsFactors = F) %>% 
            remove_constant()
        
        if(input$disp == "head") {
            return(head(df))
        }
        else {
            return(df)
        }
    })
    
    geocodeInput <- reactive({
        req(input$go)
        req(uploadInput())
        
        isolate(input$password) %>%  
            set_key()
        
        geocode(uploadInput()[,1], autocomplete = FALSE)
    })
    
    output$contents <- DT::renderDataTable({
        uploadInput()
    })
    
    output$geocoded <- DT::renderDataTable({
        geocodeInput() %>%  
            # data.table()
            datatable(extensions = 'Buttons',
                      options = list(
                          buttons = c('copy', 'csv', 'excel')),
                      class = "display")
    })

    
    #-------Selected Map-------#
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    output$mapSelected <- renderLeaflet({
        GeoCoded_Records = geocodeInput()
        tmap_leaflet(tm_shape(GeoCoded_Records) + 
                         tm_dots(title = "address"))
    })
}



# Run the app ----
shinyApp(ui, server)
