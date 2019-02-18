library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Tidy HansaRd"),
  p("Download text files from the Hansard website, then upload them here and they will automatically be tidied for analysis."),
  a("Example Hansard", href="https://hansard.parliament.uk/lords/2017-03-01/debates/EE9DF3A9-2E05-4568-8CF8-A61F11172391/EuropeanUnion(NotificationOfWithdrawal)Bill"),
  p("You can upload multiple files, and when you click download these will be downlaoded as zipped CSVs."),
  p("Be sure to check out the R script working behind the scenes if you'd like more control."),
  a("R script", href="https://hansard.parliament.uk/lords/2017-03-01/debates/EE9DF3A9-2E05-4568-8CF8-A61F11172391/EuropeanUnion(NotificationOfWithdrawal)Bill")
  
  
  # Sidebar with a slider input for number of bins 
  fluidRow(
    column(6,
          fileInput("hansard", "Upload Hansard plaintext file(s) here.", multiple = T, accept = c(".txt"), width = NULL),
          downloadButton("downloadZippedCSV", "Download")
           ),
    column(6,
      h4("Example Output"),
      tableOutput(outputId = "table")
  )
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  library(tidyverse)
  source("hansard_cleaner_source.R")
  
  input_files <- reactive({
    inFile <- input$hansard
    
    lst <- list()
    for(i in 1:nrow(inFile)){
      lst[[i]] <- read_file(inFile[[i, 'datapath']])
    }
    
    outlist <- list()
    for(i in seq_along(lst)) {
      outlist[[i]] <- process_hansard(lst[[i]])
    }
    outlist
  }) 
  
  input_metadata <- reactive({
    inFile <- input$hansard
    
    outlist <- list()
    for(i in 1:nrow(inFile)) {
      outlist[[i]] <- get_hansard_shiny_key(inFile$datapath[[i]], inFile$name[[i]])
    }
    outlist
  }) 
  
  output_table <- reactive({
    inFile <- input$hansard
    
    if (is.null(inFile))
      return(NULL)
    
    f <- read_file(inFile$datapath[[1]])
    
    out <- process_hansard(f)
    
    out
  })
  
  output$intable <- renderTable({
    inFile <- input$hansard
    # if (is.null(inFile))
    #   return(NULL)
    inFile
  })
  
  # output$testtable <- renderTable({
  #   if(length(input_files()) > 0) {
  #     return(input_files()[[1]])
  #   } else {
  #     return(tibble())
  #   }
  #   })
  
  output$testtable2 <- renderTable({
    # inFile <- input$hansard
    # if (is.null(inFile))
    #   return(NULL)
    # out <- NULL
    # if(!is.null(inFile)) {
    #   out <- get_hansard_shiny_key(inFile$datapath[[1]], inFile$name[[1]])
    # }
    # out
    input_metadata()[[1]]
  })

  output$table <- renderTable({
    if(!is.null(output_table())) {
      head(output_table()) %>%
        mutate(text=str_sub(text,1,100)) 
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(output_table(), file, row.names = F)
    }
  )
  
  output$download <- downloadHandler(
    #https://stackoverflow.com/questions/43916535/download-multiple-csv-files-with-one-button-downloadhandler-with-r-shiny
    filename = function(){
      paste0(input$text,".zip")
      
    },
    content = function(file){
      #go to a temp dir to avoid permission issues
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- NULL;
      
      
      #loop through the files
      for (i in 1:input$hansard){
        #write each sheet to a csv file, save the name
        fileName <- paste(input$text,"_0",i,".csv",sep = "")
        write.table(input_files()[i],fileName,sep = ';', row.names = F, col.names = T)
        files <- c(fileName,files)
      }
      #create the zip file
      zip(file,files)
    }
  )
  
  output$downloadZippedCSV <- downloadHandler(
    filename = function() {
      "zippedCSV.zip"
    },
    content = function(file) {
      # go to temp dir to avoid permission issues
      # owd <- setwd(tempdir())
      # on.exit(setwd(owd))
      
      # create list of dataframes and NULL value to store fileNames
      listDataFrames <- input_files()
      listDataFrames <- map(listDataFrames, as.data.frame)
      metadata <- input_metadata()
      names(listDataFrames) <- unlist(map(metadata, "hansard_datetime_final"))
      allFileNames <- NULL
      
      # loop through each dataframe
      for(i in 1:length(listDataFrames)) {
        # write each dataframe as csv and save fileName
        fileName <- paste0(names(listDataFrames)[i], ".csv")
        write_csv(listDataFrames[[i]], fileName, col_names=TRUE)
        allFileNames <- c(fileName, allFileNames)
      }
      
      # write the zip file
      zip(file, allFileNames) 
      
    }
  )
  
}

app <- shinyApp(ui=ui, server=server)
runApp(app, launch.browser=TRUE)
