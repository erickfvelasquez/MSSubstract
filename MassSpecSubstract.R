#Small app for mspurity 
#Author Erick Velasquez 

library(msPurity)
library(shiny)
library(mzR)

## ui ## 

ui <- fluidPage(
  ##app title
  titlePanel("MS Spectrum Substractor"),
  sidebarLayout(
    sidebarPanel(
      "Spectrum 1",
      fileInput('file1', 'Import MS Spectrum 1'),
      "Spectrum 2",
      fileInput('file2', 'Import MS Spectrum 2'),
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ','),
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   '"'),
      downloadLink('downloadData', 'Download Processed MS')
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Raw Spectrums", 
                 h3("Spectrum 1"),
                 plotOutput("spec1"),
                 h3("Spectrum 2"),
                 plotOutput("spec2")),
        tabPanel(
          "Processed Spectrum",
          h3("Resulting Spectrum"),
          plotOutput("resultspec")
        )
      )
    )
  )
)

### server ###

server <- function(input, output){
  #import the csv files with 4 2 columns peaklist andintensity 
  raw.spec1 <- reactive({
    
    inFile <- input$file1
    if(is.null(inFile))
      return(NULL)

    data <- read.csv(inFile$datapath, header = input$header, sep=input$sep, 
                     quote = input$quote)
    
    data
  })
  
  raw.spec2 <- reactive({
    
    inFile <- input$file2
    if(is.null(inFile))
      return(NULL)
    
    data <- read.csv(inFile$datapath, header = input$header, sep=input$sep, 
                     quote = input$quote)
    
    data
  })
  
  #create the output of the mass spec in plots
  output$spec1 <- renderPlot({
    if(is.null(raw.spec1))
      return(NULL)
    
    pl<- raw.spec1()
    plot(pl$MZ, pl$Intensity, type="h", lwd=1)
  })
  
  output$spec2 <- renderPlot({
    if(is.null(raw.spec2))
      return(NULL)
    
    pl<- raw.spec2()
    plot(pl$MZ, pl$Intensity, type="h", lwd=1)
  })
  
  
  #Process the mass spec to substract one from the other
  processed.spec <- reactive({
    ms1 <- raw.spec1()
    ms2 <- raw.spec2()
    
    ms3 <- subtractMZ(ms1$MZ, ms2$MZ, ms1$Intensity, ms2$Intensity, ppm = 5, s2bthres = 10)
    ms3 <- as.data.frame(ms3)
    names(ms3) <- c("MZ")
    ms4 <- ms1[ms1$MZ %in% ms3$MZ,]
    ms4
    #print(ms4)
  })
  
  output$resultspec <- renderPlot({
    pl <- processed.spec()
    plot(pl$MZ, pl$Intensity, type="h", lwd=1)
    
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
     paste('data-', Sys.Date(), '.csv', sep='')
   },
   content = function(con) {
     write.csv(processed.spec(), con)
   }
  )
  
}


shinyApp(ui = ui, server = server)
