# library(shiny)
# library(janitor)
# library(hereR)
# library(DT)
# library(tmap)
# library(leaflet)
# library(shinycssloaders)

source("global.R")

# options(shiny.maxRequestSize = 15*1024^2)

# Define UI for data upload app ----
ui <- fluidPage(
    
    # Top Row~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    fluidRow(style = "height:50px; border-bottom: 1px solid black",
             column(4,
                    style = "border-right: 1px solid white",
                    # span(img(src = "wsp_logo.png", height = "100%"), 
                    #      # "Dashboard Dev Demo",
                    #      style="color: #9239F6;font-size:30px")
                    #      # style="font-size:30px")
                    img(src = "wsp_logo.png", height = "49px")
                    # ,
                    # img(src = "dig_logo.png", height = "49px")
             ), 
             column(2, offset = 5,
                    # wellPanel(style = "height:49px", textOutput("currentTime"))
                    ) ),
    
    fluidRow(style = "border-top: 1px solid white;height:10px;"),

    # App title ----
    titlePanel("GeocodeR"),
    tabsetPanel(type = "pills",
    tabPanel("Addresses",
                # Sidebar layout~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                sidebarLayout(
                    sidebarPanel(
                        textInput("email", "Your personal email:", 
                                  placeholder = "Enter email..." ),
                        passwordInput("digger", "DIG Team key:", 
                                      placeholder = "Enter key..." ),
                        passwordInput("password", "HERE API key:", 
                                      placeholder = "Enter key..." ),
                        
                        # Horizontal line ----
                        tags$hr(style="border-color: purple;"),
                        fileInput("file1", "Choose CSV File",
                                  multiple = TRUE,
                                  accept = c("text/csv",
                                             "text/comma-separated-values,text/plain",
                                             ".csv")),
                        actionButton("geocode", "GeoCode it!"),
                        
                        # Horizontal line ----
                        tags$hr(style="border-color: purple;"),
                        
                        # Input: Checkbox if file has header ----
                        checkboxInput("header", "Header", TRUE),
                        verbatimTextOutput("oText"),
                        
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
                        
                        
                        # Input: Select number of rows to display ----
                        radioButtons("disp", "Display",
                                     choices = c(Head = "head",
                                                 All = "all"),
                                     selected = "all")
                    ),
                    
                    # Main panel for displaying outputs ----
                    mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Uploaded Data",
                                             DT::dataTableOutput("contents") %>% 
                                                 withSpinner(color="#0dc5c1")),
                                    tabPanel("Geocode Data",
                                             br(),
                                             wellPanel("Successful Geocoded Records"), 
                                             DT::dataTableOutput("geocodedgood") %>% 
                                                 withSpinner(color="#0dc5c1"), 
                                             br(),
                                             tags$hr(style="border-color: purple;"),
                                             br(),
                                             wellPanel("Failed Geocoded Records"),
                                             DT::dataTableOutput("geocodedbad") %>% 
                                               withSpinner(color="#0dc5c1")),
                                    tabPanel("Geocoded Map",
                                             mapviewOutput("mapSelected", height = 700) %>% 
                                                 withSpinner(color="#0dc5c1"))) 
                    )
                )
                
    ),
    tabPanel("Coordinates", 
             # Sidebar layout~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
             sidebarLayout(
                 # Sidebar panel for inputs ----
                 sidebarPanel(
                     
                     # Input: Select a file ----
                     fileInput("file_coord", "Choose CSV File",
                               multiple = TRUE,
                               accept = c("text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv")),
                     
                     # Horizontal line ----
                     tags$hr(style="border-color: purple;"),
                     
                     # Input: Checkbox if file has header ----
                     checkboxInput("header_coord", "Header", TRUE),
                     
                     # Input: Select separator ----
                     radioButtons("sep_coord", "Separator",
                                  choices = c(Comma = ",",
                                              Semicolon = ";",
                                              Tab = "\t"),
                                  selected = ","),
                     
                     # Input: Select quotes ----
                     radioButtons("quote_coord", "Quote",
                                  choices = c(None = "",
                                              "Double Quote" = '"',
                                              "Single Quote" = "'"),
                                  selected = '"'),
                     
                     
                     # Input: Select number of rows to display ----
                     radioButtons("disp_coord", "Display",
                                  choices = c(Head = "head",
                                              All = "all"),
                                  selected = "all"),
                     
                     # Horizontal line ----
                     tags$hr(style="border-color: purple;"),
                     
                     passwordInput("password_coord", "Set personal HERE API key:"),
                     actionButton("go_coord", "GeoCode it!")
                     
                 ),
                 
                 # Main panel for displaying outputs ----
                 mainPanel(
                     tabsetPanel(type = "tabs",
                                 tabPanel("Uploaded Data",
                                          DT::dataTableOutput("contents_coord")%>% 
                                              withSpinner(color="#0dc5c1")),
                                 tabPanel("Geocode Data",
                                          DT::dataTableOutput("geocoded_coord")%>% 
                                              withSpinner(color="#0dc5c1")),
                                 tabPanel("Geocoded Map",
                                          leafletOutput("mapSelected_coord", height = 700)%>% 
                                              withSpinner(color="#0dc5c1")))
                     
                 )
             )         
    ))
)

#server====================================================================================================================================================================================================
#==========================================================================================================================================================================================================
#==========================================================================================================================================================================================================
#==========================================================================================================================================================================================================
#==========================================================================================================================================================================================================
#==========================================================================================================================================================================================================
#==========================================================================================================================================================================================================

# Define server logic to read selected file ----
server <- function(input, output, session) {
  
  #utility======================================================================
  #=============================================================================
  #=============================================================================
  # email = "mike.gaunt.404@gmail.com"
  
  time_start = Sys.time()
  output$currentTime <- renderText({
    invalidateLater(1000, session)
    
    td = (as.numeric(Sys.time()-time_start)%/% 1) %>% 
      seconds_to_period()
    paste("Time on App: ", paste(td@hour, minute(td), second(td), sep = ":"))
  })
  
  #shows html popup at start of application
  observeEvent("", {
    showModal(modalDialog(
      includeHTML("./www/intro.html"),
      size = "l",
      easyClose = TRUE,
    ))
  })
  
  #adrresses====================================================================
  #=============================================================================
  #=============================================================================
  
  #input~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #gets data from user
  #cleans, displays, and prepares/saves data for batch command
  addressInput <- reactive({
    
    validate(
      need(input$file1 != "", "Please select a data set to geocode.")
    )
    req(isolate(input$digger))
    
    if (isolate(input$digger) == "53attlesound3rs"){
      
    df = read.csv(input$file1$datapath,
                  header = input$header,
                  sep = input$sep,
                  quote = input$quote, stringsAsFactors = F) %>% 
      remove_constant()
    
    } else {
      
      showModal(modalDialog(
        title = "Error:",
        HTML("DIG password incorrect!!"),
        size = "l",
        easyClose = TRUE))
    }
  })
  
  #===========================================================================
  #this codeblock currently wordks and saves out the batch coded status
  #requires key and address input
  #takes adress input and converts to batch ready format 
  #makes batch command and executes it
  name = eventReactive(input$geocode, {
    validate(
      need(addressInput() != "", "Please provide HERE API key and press `GeoCode it!` button once data has been loaded.")
    )
    req(addressInput())
    key = isolate(input$password)
    email = isolate(input$email)
    
    addressInput() %>%
      set_colnames("searchText") %>%
      mutate(recId = rownames(.) %>%
               as.numeric()) %>%
      write.table("./data/overwrite_testttkkkk.txt", 
                  row.names = F,
                  quote = F,
                  sep = "|") 
    
    batchout = paste0(cURL = 'curl -X POST -H "Content-Type: text/plain" --data-binary @',
                      "./data/overwrite_testttkkkk.txt",
                      ' "https://batch.geocoder.ls.hereapi.com/6.2/jobs?&apiKey=',
                      key,
                      '&action=run&header=true&inDelim=|&outDelim=,&outCols=recId,latitude,longitude,locationLabel&outputcombined=true&mailTo=',
                      email) %>%
      system(intern = T)
    
  })
  
  
  
  observeEvent(input$geocode, {
    req(name())
    
    batch_message = tail(name(), 1)
    print(str_detect(batch_message, "RequestId"))
    print(batch_message)
    
    if ( !str_detect(batch_message, "RequestId")) {
      
      if (str_detect(batch_message, "Details")){
        error_msg = batch_message %>%
          gsub(".*<Details>", "\\1",.) %>%
          gsub("</Details>.*", "\\1",.) %>%
          str_remove_all("[:punct:]") %>%
          str_trim()
      } else {
        error_msg = batch_message %>%
          gsub('.*error_description', "\\1",.) %>%
          gsub('\"}".*', "\\1",.) %>%
          str_remove_all("[:punct:]") %>%
          str_trim()
      }
      
      showModal(modalDialog(
        title = "Geocode submission status:",
        paste("An error has occured - details:", error_msg),
        size = "l",
        easyClose = TRUE))
      
    } else {
      
      batch_message %>%
        gsub(".*<Status>", "\\1",.) %>%
        gsub("</Status>.*", "\\1",.) %>%
        paste0("Data pakcage ", .) %>%
        message()
      
      job_code = batch_message %>%
        gsub(".*<RequestId>", "\\1",.) %>%
        gsub("</RequestId>.*", "\\1",.) %>%
        str_trim()
      
      showModal(modalDialog(
        title = "Geocode submission status:",
        HTML(paste("The data package has been accepted by the HERE api. <br> 
        Your batch job code is: <br>
        <strong> ", job_code, " </strong>. 
        <br> 
        <br/>
        You can download your data without the help of this app by pasting the job code and your api key into the following browser command: <br> 
        <strong>https://batch.geocoder.ls.hereapi.com/6.2/{JOBCODE HERE REMOVE BRACES}/result?apiKey={API KEY HERE REMOVE BRACES}</strong>. <br>
        Otherwise you can download and view your geocoded data with this application.")),
        size = "l",
        easyClose = TRUE))
    }
  })
  
  batch_download = eventReactive(input$geocode, {
    req(name())
    
    key = isolate(input$password)
    
    batch_message = tail(name(), 1)
    
    if(str_detect(batch_message, "Request")){
      
      job_code = batch_message %>%
        gsub(".*<RequestId>", "\\1",.) %>%
        gsub("</RequestId>.*", "\\1",.) %>%
        str_trim()
      
      status_request_url = paste0("https://batch.geocoder.ls.hereapi.com/6.2/jobs/",
                                  job_code,
                                  "?=status&apiKey=", 
                                  key)
      
      i = 0
      while(TRUE){
        print(i)
        if (i > 120){
          showModal(modalDialog(
            title = "Geocode job status:",
            "Your batch job has taken longer than 120 seconds to run.",
            size = "l",
            easyClose = TRUE))
          break 
          
        }
        
        status_info = GET(url = status_request_url) %>% 
          content(type = "text") %>%  
          jsonlite::fromJSON()
        
        if (status_info$Response$Status == "completed"){
          break
        }
        
        Sys.sleep(time = 2) #Time in seconds
        i = i + 2
      }
      
      paste0("curl https://batch.geocoder.ls.hereapi.com/6.2/jobs/",
             job_code,
             "/result?apiKey=", 
             key, 
             " --output ./output/geocode.zip") %>%
        system(.)
      
      batch_download = TRUE
    } else {
      batch_download = FALSE
    }
  })
  
  observeEvent(batch_download(), {
    if (batch_download() == T){
      showModal(modalDialog(
        title = "Geocode job status:",
        "Your batch job has been completed and is ready for download.",
        size = "l",
        easyClose = TRUE))
    }
  })
  
  output$contents <- DT::renderDataTable({
    addressInput() %>%   
      datatable()
  })
  
  zipppedInput = eventReactive(batch_download(),{
    req(batch_download())
    unzipped = unzip("./output/geocode.zip", exdir = "./output", overwrite = TRUE)
    
    unzipped  %>%
      fread()
  })
  
  output$geocodedgood <- DT::renderDataTable({
    req(zipppedInput())
    
    zipppedInput() %>%
      na.omit() %>% 
      clean_names() %>% 
      datatable(extensions = 'Buttons', options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      ))
  })
  
  output$geocodedbad <- DT::renderDataTable({
    req(zipppedInput())
    
    index = zipppedInput() %>% 
      data.table() %>%  
      .[is.na(latitude), recId] 
    
    addressInput() %>%  
      data.table() %>%  
      .[index] %>%  
      clean_names() %>%
      datatable(extensions = 'Buttons', options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      ))
  })
  
  output$mapSelected <- renderLeaflet({
    req(zipppedInput())
    
    GeoCoded_Records = zipppedInput() %>%
      clean_names() %>% 
      mutate(coord = paste(round(latitude, 4), round(longitude, 4), sep = ", ")) %>% 
      na.omit() %>%
      st_as_sf(., coords = c("longitude", "latitude"), crs = st_crs(4326))
    
    tmap_leaflet(tm_shape(GeoCoded_Records) +
                   tm_dots(col = "#28ADA8",
                           # id = "locationLabel",
                           title = "coord", 
                           alpha = .7, size = 1, scale = .5, shape = 21,
                           border.lwd = 1, border.col = "black", border.alpha = 1,
                           popup.vars = c("Address" = "location_label",
                                          "Lat/Long" = "coord"))) 

  })
  
  
  
  #coordinates~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #input~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  addressInput_coord <- reactive({
    validate(
      need(input$file_coord != "", "Please select a data set to geocode.")
    )
    df <- read.csv(input$file_coord$datapath,
                   header = input$header_coord,
                   sep = input$sep_coord,
                   quote = input$quote_coord, stringsAsFactors = F) %>% 
      remove_constant() %>%  
      clean_names() %>% 
      dplyr::select(contains(c("lon", "lat")), everything()) %>% 
      rename(longitude = 1, latitude = 2) %>%  
      filter(!is.na(longitude) & !is.na(longitude)) %>%  
      st_as_sf(., coords = c("longitude", "latitude"), crs = st_crs(4326)) %>%  
      mutate(ID = rownames(.)) 
    
    if(input$disp_coord == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
  })
  
  output$contents_coord <- DT::renderDataTable({
    addressInput_coord() %>%  
      datatable()
  })
  
  
  
  #geocode~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  geocodeInput_coord <- reactive({
    validate(
      need(input$password_coord != "", "Please provide HERE API key and press `GeoCode it!` button once data has been loaded.")
    )
    
    req(input$go_coord)
    req(addressInput_coord())
    
    isolate(input$password_coord) %>%  
      set_key()
    
    reverse_geocode(addressInput_coord(), results = 1, landmarks = FALSE, url_only = FALSE) 
  })
  
  output$geocoded_coord <- DT::renderDataTable({
    geocodeInput_coord() %>%  
      datatable(extensions = 'Buttons',
                options = list(
                  paging = TRUE,
                  searching = TRUE,
                  fixedColumns = TRUE,
                  autoWidth = TRUE,
                  ordering = TRUE,
                  dom = 'tB',
                  buttons = c('copy', 'csv', 'excel')),
                class = "display")
  })
  
  
  #mapping~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$mapSelected_coord <- renderLeaflet({
    GeoCoded_Coordinates = geocodeInput_coord()  %>% 
      mutate(coord = geometry %>%  as.character())
    
    Input_Coordinates = addressInput_coord() %>% 
      mutate(coord = geometry %>%  as.character())
    
    
    if(nrow(Input_Coordinates)>1000){
      GeoCoded_Coordinates = GeoCoded_Coordinates %>%  sample_n(1000)
      Input_Coordinates = Input_Coordinates  %>% 
        filter(ID %in% unique(GeoCoded_Coordinates$id))
      
    }
    
    map = tm_shape(Input_Coordinates) +
      tm_dots(col = "#28ADA8",
              id = "",
              alpha = .7, size = 1, scale = .5, shape = 21,
              border.lwd = 1, border.col = "black", border.alpha = 1,
              popup.vars = c("Parent Point ID" = "ID", 
                             "Lat/Long" = "coord")) +
      tm_shape(GeoCoded_Coordinates) + 
      tm_dots(id = "label", 
              col = "#7A3A9A",
              alpha = .7, size = 1, scale = .5, shape = 21, 
              border.lwd = 1, border.col = "black", border.alpha = 1, 
              popup.vars = c("Parent Point ID" = "id",
                             "Sub-Point ID" = "rank",
                             "Dist. to input coord." = "distance",
                             "Geocode Level" = "level",
                             "Lat/Long" = "coord"))
    
    
    
    map %>%  tmap_leaflet()
    
  })
  
  
}



# Run the app ----
shinyApp(ui, server)
