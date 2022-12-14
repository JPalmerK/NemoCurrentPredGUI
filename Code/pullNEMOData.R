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
source('functions.R')
hrbrthemes::import_roboto_condensed()


metaDf=data.frame(locs=c("East Point Out", "Java Island Out",
                         "Turn Point Out", 'Halibut Island Out',
                         'Kelp Reef Out', 'Discovery Island In',
                         'Discovery Island Out', 'Lime Kiln',
                         'Kellet Bluff In', 'Turn Point In',
                         'Sandy Point In', 'East Point In' ),
                  y= c(349,345,345,330,313,289,285,303,321,341,343,346),
                  x= c(299,279,249,241,236,223,224,236,241,247,275, 299))


#conversion from knots to m/s
knots2mps = 0.514444;

# conversion from mps to knots
mps2knots = 1/knots2mps

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  
  titlePanel( title =  'SalishSeaCast Current Forecast - Haro and Boundary'), 
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    # Set to custom time 
    
    sidebarPanel(
      # Input: Choose dataset ----
      
      selectInput("dataset", "Choose a dataset:",
                  choices = metaDf$locs),
      dateInput("date4", "Max Look Ahead (max two days from now):", 
                value = Sys.Date()+2),
      # Button
      downloadButton("downloadData", "Download"),
      tags$img(src = "vpa.jpg",height=75,width=600),
      tags$img(src = "SMRU.png",height=144,width=555)
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", 
                           plotOutput("magPlot"), 
                           plotOutput("dirPlot"),),
                  tabPanel("ExportData", tableOutput("table")))))
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
  
  # Pull the data from the website
  updateData<-reactive({
    
    
    URL =getURL(input, metaDf)
    
    data=fread(URL)
    
    #data=fread(URL())
    
    
    
    # Check to see if there is an error, if yes then substract a day and throw a warning
    data=data[-1,]
    data[,c(2)]= as.numeric(unlist(data[,c(2)]))
    data[,c(3)]= as.numeric(unlist(data[,c(3)]))
    data[,c(4)]= as.numeric(unlist(data[,c(4)]))
    data[,c(5)]= as.numeric(unlist(data[,c(5)]))
    
    data$time1 = as.POSIXct(data$time, tz="UTC","%Y-%m-%dT%H:%M:%OS")
    
    # Calculate current magnitude and direction
    data$currentMag = sqrt(data$VelEast10^2+data$VelNorth10^2)
    
    data$currentMag<-data$currentMag*mps2knots
    
    colnames(data)[1:7]<-c('time','gridY', 'gridX','VelE_mps','VelN_mps','time1','Mag_kn')
    Heading_deg<-currentDirection(data)
    data$Heading_deg<-Heading_deg
    data$Pacific =format(data$time, usetz=TRUE, tz="America/Vancouver")
    
    
    return(data)})
  
  
  # Create the plot of the tidal predictions
  output$magPlot <- renderPlot({
    
    coeff = 90
    data1 = data.frame(updateData())
    
    
    splineData =data.frame(spline(data1$time1,data1$Mag_kn))
    splineData$time = seq(min(data1$time1), max(data1$time1),length.out=nrow(splineData))
    splineData$Pacific = as.POSIXct(
      format(splineData$time, usetz=TRUE, tz="America/Vancouver"))
    data2 = data.frame((spline(data1$time1, data1$Mag_kn)))
    
    
    
    splineData$Mag = data2$y
    verticalLineLoc =as.Date(min(data1$time1))+days(1)
    
    minDate = as.POSIXct(format(min(data1$time1),
                                usetz=TRUE, tz="America/Vancouver"))
    
    maxDate = as.POSIXct(format(max(data1$time1),
                                usetz=TRUE, tz="America/Vancouver"))
    hour(minDate)=0
    minute(minDate)=0
    second(minDate)=0
    
    hour(maxDate)=23
    minute(maxDate)=59
    second(maxDate)=59
    
    # Time breaks
    Majorbreaks =seq(minDate, 
                     maxDate, by = "6 hour")
    Minorbreaks =seq(minDate, 
                     maxDate, by = "2 hour")
    
    ggplot() +
      geom_line(data= splineData, 
                aes(y=Mag,x=Pacific), color='red') +            
      geom_point(data=data1,
                 aes(
                   x=as.POSIXct(format(time1, 
                                       usetz=TRUE, tz="America/Vancouver")),
                   y=Mag_kn), color ='red')  +
      theme_bw()+
      theme( panel.grid.major.x = element_blank(),
             panel.grid.minor.x = element_blank(),
             axis.line = element_line(colour = "black"),
             axis.text.x = element_text(size=14),
             axis.text.y=element_text(size=14),
             axis.title.y = element_text(size = 16),
             axis.title.x= element_text(size =16)
             )+
    
       ggtitle(as.character(paste(Sys.Date(),'T', '00:00:00Z', sep='')))+
       xlab('Prediction Time (Pacific)')+
       ylab('Current Magnitude (knots)')+
      geom_vline(xintercept =Majorbreaks, colour="grey40", size=0.8)+
      geom_vline(xintercept =Minorbreaks, colour="grey95", size=0.7)+
     geom_vline(xintercept = as.POSIXct(format(
        verticalLineLoc,usetz=TRUE, tz="America/Vancouver")), 
        color = "blue",  lwd = 1) 
    

    
  })
  
  # Create the direction predictions
  output$dirPlot <- renderPlot({
    data1 = data.frame(updateData())
    verticalLineLoc =as.Date(min(data1$time1))+days(1)
    splineData =data.frame(spline(data1$time1,data1$Heading_deg))
    splineData$time = seq(min(data1$time1), max(data1$time1),
                          length.out=nrow(splineData))
    splineData$Pacific = as.POSIXct(
      format(splineData$time, usetz=TRUE, tz="America/Vancouver"))
    
    colnames(splineData)[2]<-'direction'
    minDate = as.POSIXct(format(min(data1$time1),
                                usetz=TRUE, tz="America/Vancouver"))
    
    maxDate = as.POSIXct(format(max(data1$time1),
                                usetz=TRUE, tz="America/Vancouver"))
    hour(minDate)=0
    minute(minDate)=0
    second(minDate)=0
    
    hour(maxDate)=23
    minute(maxDate)=59
    second(maxDate)=59
    
    # Time breaks
    Majorbreaks =seq(minDate, 
               maxDate, by = "6 hour")
    Minorbreaks =seq(minDate, 
                     maxDate, by = "2 hour")
  
    
    ggplot()+
      geom_line(data= splineData, 
                aes(y=direction,x=Pacific), color='black') +            
      geom_point(data=data1,
                 aes(x=as.POSIXct(
                   format(time1, usetz=TRUE, tz="America/Vancouver")),
                   y=Heading_deg), color ='Black') + 
  
      theme(axis.ticks.x=element_blank()) + #Hides ALL ticks!
      theme(
        axis.ticks.x=element_blank(),
        panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"),
            axis.text.x = element_text(size=14),
            axis.text.y = element_text(size=14),
            axis.title.y = element_text(size = 16),
            axis.title.x= element_text(size =16))+
      xlab('Prediction Time (Pacific)')+
      ylab('Current Heading (deg)')+
      ylim(c(0,400))+
      scale_y_continuous(breaks=seq(0,360,by=45), 
                         labels = c('0-N','45-NE','90-E', 
                                    '135-SE', '180-S', '225-SW',
                                    '270-W', '315-NW', '360-N'))+
      geom_hline(yintercept = seq(0,360,by=45), colour="grey90", size=0.7)+
      geom_vline(xintercept =Majorbreaks, colour="grey40", size=0.8)+
      geom_vline(xintercept =Minorbreaks, colour="grey95", size=0.7)+
        geom_vline(xintercept = as.POSIXct(format(
        verticalLineLoc,usetz=TRUE, tz="America/Vancouver")), 
        color = "blue",  lwd = 1) 
    
    
  })
  
  # Display the data
  # Generate an HTML table view of the data ----
  output$table <- renderTable({
    dataOut<- updateData()
    
    dataOut$Pacific = dataOut$time1
    dataOut$Pacific =format(dataOut$Pacific, usetz=TRUE, tz="America/Vancouver")
    dataOut$Heading_deg= as.integer(dataOut$Heading_deg)
    dataOut<-dataOut[,c('Pacific','Mag_kn', 'Heading_deg')]
    
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset,'_',Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      
      dataOut = updateData()
      dataOut$Pacific = dataOut$time1
      dataOut$Pacific =format(dataOut$Pacific, usetz=TRUE, 
                              tz="America/Vancouver")
      
      dataOut<-dataOut[,c('Pacific','Mag_kn', 'Heading_deg')]
      dataOut$Heading_deg= as.integer(dataOut$Heading_deg)
      
      dataOut<-dataOut[,c('Pacific','Mag_kn', 'Heading_deg')]
      write.csv(dataOut, file, row.names = FALSE)
    }
  )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
