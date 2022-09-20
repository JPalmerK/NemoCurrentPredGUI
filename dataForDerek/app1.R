#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
rm(list=ls())
library(shiny)

library(ggplot2)
library(data.table)
library(lubridate)
library(readr)
library('curl')
library(hrbrthemes)
library(shinyTime)
hrbrthemes::import_roboto_condensed()


metaDf=data.frame(locs=c("East Point Out", "Java Island Out",
                         "Turn Point Out", 'Halibut Island Out',
                         'Kelp Reef Out', 'Discovery Island In',
                         'Discovery Island Out', 'Lime Kiln',
                         'Kellet Bluff In', 'Turn Point In',
                         'Sandy Point In', 'East Point In' ),
                  y= c(349,345,345,330,313,289,285,303,321,341,343,346),
                  x= c(299,279,249,241,236,223,224,236,241,247,275, 299))


n=0

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("UBC Tidal Data Forecast"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      # Set to custom time 
  
      
      sidebarPanel(
            # Input: Choose dataset ----
            selectInput("dataset", "Choose a dataset:",
                        choices = metaDf$locs),
            
            # Button
            downloadButton("downloadData", "Download")
            
        ),

        # Show a plot of the generated distribution
        mainPanel(

           # Output: Tabset w/ plot, summary, and table ----
           tabsetPanel(type = "tabs",
                       tabPanel( # Pass in a Date object
                         dateInput("date4", "Max Look Ahead (max two days):", 
                                   value = Sys.Date()+2),
                                "Plot", plotOutput("distPlot"), 
                               
                                ),
                       tabPanel("Data", tableOutput("table")),
                       tabPanel('CurrentTime',textOutput('CurrentTime') )
           )

        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  # Print the time in hh:mm:ss everytime it changes
    output$CurrentTime<-
      renderText(
        paste(
        as.character(paste(Sys.Date(),'T', '00:00:00Z', sep='\n')),
        as.character(Sys.time())), sep='\n')

    
    # Figure out whether a day needs to be subtracted
    currentDate<-reactive({now('utc')})

    
    # Get the correctly formated URL
    URL<-reactive({     
      # get the data
      LocName= input$dataset
      x1 = as.character(metaDf$x[metaDf$locs==LocName])
      y1 = as.character(metaDf$y[metaDf$locs==LocName])
      xgrid =paste('[(',x1,'):1:(',x1,')]', sep='')
      ygrid = paste('[(',y1,'):1:(',y1,')]', sep='')
      
      # # start date in UTC
      # dataStart = as.POSIXct(input$date4-days(2), 'umt')
      # hour(dataStart)=0
      # minute(dataStart)=0
      # second(dataStart)=0
      # 
      # dataStop = dataStart+days(2)+hours(11)+minutes(30)
      # startDate= strptime(dataStart, format ="%Y-%m-%d")
      
      startDate = paste(input$date4-days(2),'T', '00:00:00Z', sep='')
      stopDate = paste(input$date4,'T', '00:11:30Z', sep='')
      timeFormat = paste('time[(', startDate,':1:', stopDate, ')],', sep='')
      
      east = paste('VelEast10[(', startDate, '):1:(', stopDate,')]',paste(ygrid,xgrid,sep=''),',', sep='')
      north = paste('VelNorth10[(', startDate, '):1:(', stopDate,')]',paste(ygrid,xgrid,sep=''), sep='')
      
      
      url = paste('https://salishsea.eos.ubc.ca/erddap/griddap/ubcSSfDepthAvgdCurrents1h.csv?',
                  east,north, sep='')
    })
    
    # Pull the data from the website
    updateData<-reactive({
   
    data=fread(URL())
    
    
    # Check to see if there is an error, if yes then substract a day and throw a warning
    data=data[-1,]
    data[,c(2)]= as.numeric(unlist(data[,c(2)]))
    data[,c(3)]= as.numeric(unlist(data[,c(3)]))
    data[,c(4)]= as.numeric(unlist(data[,c(4)]))
    data[,c(5)]= as.numeric(unlist(data[,c(5)]))
    
    data$time1 = as.POSIXct(data$time, tz="UTC","%Y-%m-%dT%H:%M:%OS")
    
    # Calculate current magnitude and direction
    data$currentMag = sqrt(data$VelEast10^2+data$VelNorth10^2)
    data$currentDirec = atan2(data$VelNorth10,data$VelEast10)*(180/pi)
    data$currentDirec[data$currentDirec<0]=data$currentDirec[data$currentDirec<0]+360
    return(data)})
    
    
    # Create the plot of the tidal predictions
    output$distPlot <- renderPlot({
        
        coeff = 360
        data1 = data.frame(updateData())
        

        splineData =data.frame(spline(data1$time1, data1$currentDirec))
        splineData$time = seq(min(data1$time1), max(data1$time1),
                              length.out=nrow(splineData))
        data2 = data.frame((spline(data1$time1, data1$currentMag)))
        splineData$Mag = data2$y
        colnames(splineData)[2]<-'direction'
        ggplot() +
            
            geom_point(data= data1, 
                      aes(y=currentDirec,x=time1), color='black') + 
            geom_line(data= splineData, 
                       aes(y=direction,x=time), color='black') + 
            geom_point(data=data1,
                      aes(x=time1,y=currentMag * coeff), color ='red') + # Divide by 10 to get the same range than the temperature
            geom_line(data= splineData, 
                      aes(y=Mag* coeff,x=time), color='red') + 
            theme_bw()+
          
            theme(
                axis.title.y = element_text(color = 'black', size=13),
                axis.title.y.right = element_text(color = 'red', size=13))+   
            scale_y_continuous(
                
                # Features of the first axis
                name = "Current Direction (degrees)",
                # Add a second axis and specify its features
                sec.axis = sec_axis(~./coeff, name="Current Magnitude")
            )+
          ggtitle(as.character(paste(Sys.Date(),'T', '00:00:00Z', sep='')))
                
        
    })
    
    # Display the data
    # Generate an HTML table view of the data ----
    output$table <- renderTable({
       data<- updateData()
       data<-data[,-c('time')]
    })
    
    # Downloadable csv of selected dataset ----
    output$downloadData <- downloadHandler(
        filename = function() {
            paste(input$dataset,'_',Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(updateData(), file, row.names = FALSE)
        }
    )
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
