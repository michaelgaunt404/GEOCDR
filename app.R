source("global.R")


# Define UI for data upload app ----
ui <- fluidPage(
  introjsUI(), 

    # Top Row~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    fluidRow(style = "height:50px; border-bottom: 1px solid black",
             column(6,
                    style = "border-right: 1px solid white",
                    # span(img(src = "wsp_logo.png", height = "100%"), 
                    #      # "Dashboard Dev Demo",
                    #      style="color: #9239F6;font-size:30px")
                    #      # style="font-size:30px")
                    span(img(src = "wsp_logo.png", height = 49), 
                         "GeocodR Dashboard",
                         # style="color: #9239F6;font-size:30px")
                         style="font-size:30px")
                    # img(src = "geo.png", height = "49px"),
                    # img(src = "wsp_logo.png", height = "49px")
                    # ,
                    # img(src = "dig_logo.png", height = "49px")
             ), 
             column(2, offset = 5,
                    # wellPanel(style = "height:49px", textOutput("currentTime"))
                    ) ),
    
    fluidRow(style = "border-top: 1px solid white;height:10px;"),

    # App title ----
  # titlePanel("GeocodR"),
  tabsetPanel(type = "pills",
              tabPanel("Addresses",
                       # Sidebar layout~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                       sidebarLayout(
                         sidebarPanel(
                           actionButton("btn","Take Tour", icon("question-circle"), 
                                        style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                           
                           # Horizontal line ----
                           tags$hr(style="border-color: purple;"),
                           
                           introBox(data.step = 1, 
                                    data.intro = "Input your WSP email so that you results can be emailed to you.",
                                    textInput("email", "Your WSP email:", 
                                              placeholder = "Enter email..." )),
                           introBox(data.step = 2, 
                                    data.intro = "Input the WSP Security Code here.",
                                    textInput("digger", "WSP Security Code:", 
                                              placeholder = "Enter key..." )),
                           introBox(data.step = 3, 
                                    data.intro = "Input your personal HERE API key.",
                                    textInput("password", "HERE API key:", 
                                              placeholder = "Enter key..." )),
                           
                           # Horizontal line ----
                           tags$hr(style="border-color: purple;"),
                           introBox(data.step = 4, 
                                    data.intro = "Browse for files to geocode here. ATTENTION! Files need to be in csv format with a single column with an 'Addresses' header. Once uploaded, your data will show up to the right. SUG",
                                    fileInput("file1", "Choose CSV File",
                                              multiple = TRUE,
                                              accept = c("text/csv",
                                                         "text/comma-separated-values,text/plain",
                                                         ".csv"))),
                           tags$hr(style="border-color: purple;"),
                           introBox(data.step = 6, 
                                    data.intro = "Click this button once all data fields have been filled and data has been uploaded. You will be prompted with a pop indicating if your geocoding has commenced or failed.",
                                    actionButton("geocode", "Geocode it!", icon("paper-plane"), 
                                                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                           
                           
                           # Horizontal line ----
                           tags$hr(style="border-color: purple;"),
                           
                           introBox(data.step = 5, 
                                    data.intro = "If your data looks <i>strange</i> it may be due to how it's formatted - eg tab-delimited or quotation issues. 
                                    You can use these inputs to augment your data to be expected by this application. <stong>Caution:</stong> these are advanced controls and you will not need them if your file is formatted as described previously.",
                                    
                                    # box(width = "100%",
                                    #     height = 275,
                                    #     background = "light-blue", solidHeader = TRUE,
                                    #     collapsed = T, 
                                    #     collapsible = T,
                                        
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
                                                     selected = '"')
                                    # )
                                    
                           )
                         ),
                         
                         # Main panel for displaying outputs ----
                         mainPanel(
                           tabsetPanel(type = "tabs",
                                       tabPanel("Uploaded Data",
                                                DT::dataTableOutput("contents") %>% 
                                                  withSpinner(color="#0dc5c1")),
                                       tabPanel("Geocoded Data",
                                                br(),
                                                wellPanel("Successfully Geocoded Records"), 
                                                DT::dataTableOutput("geocodedgood") %>% 
                                                  withSpinner(color="#0dc5c1"), 
                                                br(),
                                                tags$hr(style="border-color: purple;"),
                                                br(),
                                                wellPanel("Failed Geocoded Records"),
                                                DT::dataTableOutput("geocodedbad") %>% 
                                                  withSpinner(color="#0dc5c1")),
                                       tabPanel("Geocoded Map",
                                                wellPanel("Mapped Sample of Geocoded Records"),
                                                leafletOutput("mapSelected", height = 700) %>% 
                                                  withSpinner(color="#0dc5c1"))) 
                         )
                       )
                       
              ),
              tabPanel("Help/Tutorial",
                       # Sidebar layout~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                       sidebarLayout(
                         sidebarPanel(
                           # introBox(data.step = 7, 
                           #          data.intro = "Input your personal email so that you results can be emailed to you.",
                                    HTML("This page serves as an example of how to use this web application. <br> 
                                    <strong>Data has already been geocoded and mapped (see tabs right)</strong>. <br>
                                    <br/>
                                   <strong>Example Data tab:</strong> The example data can be downloaded by clicking the <strong>csv button</strong>. <br>
                                   <strong>Example Geocode Data tab:</strong> The geocoded example data is displayed on this tab - succesful and unsuccesful geocoded records will be reutnred to you here. 
                                         It is best to download both of these files for your own runs. <br>
                                   <strong>Mapped Geocoded Records tab:</strong> This tab displays a sample (MAX-1K) of the succesfully geocoded records."),
                                    br(),
                                    tags$hr(style="border-color: purple;"),
                                    selectInput("variable", "Example Data:",
                                                c("National Parks/Monuments" = "nat_parks",
                                                  "California Addresses" = "california"))
                                    # )
                           ,
                           
                           # Horizontal line ----
                           tags$hr(style="border-color: purple;")
                         ),
                         
                         # Main panel for displaying outputs ----
                         mainPanel(
                           tabsetPanel(type = "tabs",
                                       tabPanel("Example Data",
                                                DT::dataTableOutput("exData") %>% 
                                                  withSpinner(color="#0dc5c1")),
                                       tabPanel("Example Geocode Data",
                                                br(),
                                                wellPanel("Successfully Geocoded Records"), 
                                                DT::dataTableOutput("exDatagood") %>% 
                                                  withSpinner(color="#0dc5c1"), 
                                                br(),
                                                tags$hr(style="border-color: purple;"),
                                                br(),
                                                wellPanel("Failed Geocoded Records"),
                                                DT::dataTableOutput("exDatabad") %>% 
                                                  withSpinner(color="#0dc5c1")),
                                       tabPanel("Example Geocoded Map",
                                                wellPanel("Mapped Geocoded Records")
                                                ,
                                                leafletOutput("mapexData", height = 700) %>%
                                                  withSpinner(color="#0dc5c1")
                                                )
                           ) 
                         )
                       )
                       
              )
              
  )
)

#server====================================================================================================================================================================================================
#==========================================================================================================================================================================================================
#==========================================================================================================================================================================================================
#==========================================================================================================================================================================================================
#==========================================================================================================================================================================================================
#==========================================================================================================================================================================================================
#==========================================================================================================================================================================================================

# Define server logic to read selected file ----
server <- shinyServer(function(input, output, session) {
  
  #utility======================================================================
  #=============================================================================
  #=============================================================================
  observeEvent(input$btn,
               introjs(session))
  
  #shows html popup at start of application
  observeEvent("", {
    showModal(modalDialog(
      includeHTML("./www/intro.html"),
      size = "l",
      easyClose = TRUE
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
  
  observe({
    req(addressInput())
    addressInput() %>%
      data.table() %>% 
      set_colnames("searchText") %>%
      mutate(recId = rownames(.) %>%
               as.numeric()) %>%
      write.table("overwrite.txt", 
                  row.names = F,
                  quote = F,
                  sep = "|")
  })
  
  #===========================================================================
  #this codeblock currently wordks and saves out the batch coded status
  #requires key and address input
  #takes adress input and converts to batch ready format 
  #makes batch command and executes it
  name = eventReactive(input$geocode, {
    validate(
      need(addressInput() != "", "Please provide HERE API key and press `Geocode it!` button once data has been loaded.")
    )
    key = isolate(input$password)
    email = isolate(input$email)
    
    batchout = paste0(cURL = 'curl -X POST -H "Content-Type: text/plain" --data-binary @',
                      "overwrite.txt",
                      ' "https://batch.geocoder.ls.hereapi.com/6.2/jobs?&apiKey=',
                      key,
                      '&action=run&header=true&inDelim=|&outDelim=,&outCols=recId,latitude,longitude,locationLabel&outputcombined=true&mailTo=',
                      email, 
                      '"') %>%
      system(intern = T)
    
  })
  
  observe(name())
  
  observe({
    req(name())
    
    batch_message = tail(name(), 1)
    
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
        HTML(paste("<h3>Data package successfully sent to HERE API!</h3> <br>
            Batch Job Code: <br>
            <strong> ", job_code, " </strong> <br>
            <em>Retain until results have been retrieved.</em> <br>
            <em>The job code has also been emailed to the provided email address.</em>
            <br>
            <br/>
            You can download your data directly from a web browser by pasting the job code (above) and your api key into the following browser command: <br>
            <strong>https://batch.geocoder.ls.hereapi.com/6.2/jobs/{{JOBCODE_HERE_REMOVE_BRACES}}/result?apiKey={{API_KEY_HERE_REMOVE_BRACES}}</strong> <br>
            Otherwise you can view and download your geocoded data with this application. <br>
            Use web download on web-application timeout or retrieval failure. <br>
            <em>Do not refresh this page until data has been retreived, a message will prompt you when the process is complete.</em>")),
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
        # print(i)
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
    addressInput() %>% datatable()
  })
  
  zipppedInput = eventReactive(batch_download(),{
    req(batch_download())
    unzipped = unzip("./output/geocode.zip", exdir = "./output", overwrite = TRUE)
    print(unzipped)

    unzipped  %>%
      fread()
  })
  
  output$geocodedgood <- DT::renderDataTable(server = FALSE, {
    validate(
      need(input$file1 != "", "Please select a data set to geocode.")
    )
    req(zipppedInput())
    
    zipppedInput() %>%
      na.omit() %>% 
      clean_names() %>% datatable(extensions = 'Buttons', options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
        ))
  })
  
  output$geocodedbad <- DT::renderDataTable(server = FALSE, {
    validate(
      need(input$file1 != "", "Please select a data set to geocode.")
    )
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
    validate(
      need(input$file1 != "", "Please select a data set to geocode.")
    )
    validate(
      need(input$geocode != "", "Please press 'geocode it' button.")
    )
    req(zipppedInput())
    
    GeoCoded_Records = zipppedInput() %>%
      clean_names() %>% 
      mutate(coord = paste(round(latitude, 4), round(longitude, 4), sep = ", ")) %>% 
      na.omit() %>%
      st_as_sf(., coords = c("longitude", "latitude"), crs = st_crs(4326))
    
    if (nrow(GeoCoded_Records)>500){
      GeoCoded_Records = GeoCoded_Records %>%  
        sample_n(500)
    }
    
    tmap_leaflet(tm_shape(GeoCoded_Records) +
                   tm_dots(col = "#28ADA8",
                           title = "coord", 
                           alpha = .7, size = 1, scale = .5, shape = 21,
                           border.lwd = 1, border.col = "black", border.alpha = 1,
                           popup.vars = c("Address" = "location_label",
                                          "Lat/Long" = "coord"))) 
  })
  
  observeEvent("", {
    output$exData <- DT::renderDataTable(server = FALSE, {
      paste0("./data/example_data/", input$variable, ".csv") %>%
        read.csv() %>%
        datatable(extensions = 'Buttons', options = list(
          dom = 'Bfrtip',
          buttons = c('csv')
        ))
      })
    
    output$exDatagood <- DT::renderDataTable(server = FALSE, {
      paste0("./data/example_data/", input$variable, ".txt") %>%
        fread() %>% 
        na.omit() %>% 
        clean_names() %>% 
        datatable(extensions = 'Buttons', options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
        ))
    })
    
    output$exDatabad<- DT::renderDataTable(server = FALSE, {
      index = paste0("./data/example_data/", input$variable, ".txt") %>%
        fread() %>%  
        .[is.na(latitude), recId] 
      
      paste0("./data/example_data/", input$variable, ".csv") %>%
        read.csv() %>%  
        data.table() %>% 
        .[index,] %>% 
        clean_names() %>%
        datatable(extensions = 'Buttons', options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
        ))
    })
    
    output$mapexData <- renderLeaflet({
      GeoCoded_Records = paste0("./data/example_data/", input$variable, ".txt") %>%
        fread() %>% 
        na.omit() %>% 
        clean_names() %>% 
        mutate(coord = paste(round(latitude, 4), round(longitude, 4), sep = ", ")) %>% 
        na.omit() %>%
        st_as_sf(., coords = c("longitude", "latitude"), crs = st_crs(4326))
      
      tmap_leaflet(tm_shape(GeoCoded_Records) +
                     tm_dots(col = "#28ADA8",
                             title = "coord", 
                             alpha = .7, size = 1, scale = .5, shape = 21,
                             border.lwd = 1, border.col = "black", border.alpha = 1,
                             popup.vars = c("Address" = "location_label",
                                            "Lat/Long" = "coord"))) 
    })
  })
  
  
  
  #coordinates~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #input~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # addressInput_coord <- reactive({
  #   validate(
  #     need(input$file_coord != "", "Please select a data set to geocode.")
  #   )
  #   df <- read.csv(input$file_coord$datapath,
  #                  header = input$header_coord,
  #                  sep = input$sep_coord,
  #                  quote = input$quote_coord, stringsAsFactors = F) %>% 
  #     remove_constant() %>%  
  #     clean_names() %>% 
  #     dplyr::select(contains(c("lon", "lat")), everything()) %>% 
  #     rename(longitude = 1, latitude = 2) %>%  
  #     filter(!is.na(longitude) & !is.na(longitude)) %>%  
  #     st_as_sf(., coords = c("longitude", "latitude"), crs = st_crs(4326)) %>%  
  #     mutate(ID = rownames(.)) 
  #   
  #   if(input$disp_coord == "head") {
  #     return(head(df))
  #   }
  #   else {
  #     return(df)
  #   }
  # })
  # 
  # output$contents_coord <- DT::renderDataTable({
  #   addressInput_coord() %>%  
  #     datatable()
  # })
  # 
  # 
  # 
  # #geocode~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # geocodeInput_coord <- reactive({
  #   validate(
  #     need(input$password_coord != "", "Please provide HERE API key and press `GeoCode it!` button once data has been loaded.")
  #   )
  #   
  #   req(input$go_coord)
  #   req(addressInput_coord())
  #   
  #   isolate(input$password_coord) %>%  
  #     set_key()
  #   
  #   reverse_geocode(addressInput_coord(), results = 1, landmarks = FALSE, url_only = FALSE) 
  # })
  # 
  # output$geocoded_coord <- DT::renderDataTable({
  #   geocodeInput_coord() %>%  
  #     datatable(extensions = 'Buttons',
  #               options = list(
  #                 paging = TRUE,
  #                 searching = TRUE,
  #                 fixedColumns = TRUE,
  #                 autoWidth = TRUE,
  #                 ordering = TRUE,
  #                 dom = 'tB',
  #                 buttons = c('copy', 'csv', 'excel')),
  #               class = "display")
  # })
  # 
  # 
  # #mapping~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # output$mapSelected_coord <- renderLeaflet({
  #   GeoCoded_Coordinates = geocodeInput_coord()  %>% 
  #     mutate(coord = geometry %>%  as.character())
  #   
  #   Input_Coordinates = addressInput_coord() %>% 
  #     mutate(coord = geometry %>%  as.character())
  #   
  #   
  #   if(nrow(Input_Coordinates)>1000){
  #     GeoCoded_Coordinates = GeoCoded_Coordinates %>%  sample_n(1000)
  #     Input_Coordinates = Input_Coordinates  %>% 
  #       filter(ID %in% unique(GeoCoded_Coordinates$id))
  #     
  #   }
  #   
  #   map = tm_shape(Input_Coordinates) +
  #     tm_dots(col = "#28ADA8",
  #             id = "",
  #             alpha = .7, size = 1, scale = .5, shape = 21,
  #             border.lwd = 1, border.col = "black", border.alpha = 1,
  #             popup.vars = c("Parent Point ID" = "ID", 
  #                            "Lat/Long" = "coord")) +
  #     tm_shape(GeoCoded_Coordinates) + 
  #     tm_dots(id = "label", 
  #             col = "#7A3A9A",
  #             alpha = .7, size = 1, scale = .5, shape = 21, 
  #             border.lwd = 1, border.col = "black", border.alpha = 1, 
  #             popup.vars = c("Parent Point ID" = "id",
  #                            "Sub-Point ID" = "rank",
  #                            "Dist. to input coord." = "distance",
  #                            "Geocode Level" = "level",
  #                            "Lat/Long" = "coord"))
  #   
  #   
  #   
  #   map %>%  tmap_leaflet()
  #   
  # })
  
  
})



# Run the app ----
shinyApp(ui, server)
