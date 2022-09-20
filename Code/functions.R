
getURL<-function(input,metaDf){
  #Function to construct the URL for the data
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
  stopDate = paste(input$date4,'T', '00:05:30Z', sep='')
  timeFormat = paste('time[(', startDate,':1:', stopDate, ')],', sep='')
  
  east = paste('VelEast10[(', startDate, '):1:(', stopDate,')]',paste(ygrid,xgrid,sep=''),',', sep='')
  north = paste('VelNorth10[(', startDate, '):1:(', stopDate,')]',paste(ygrid,xgrid,sep=''), sep='')
  
  
  url = paste('https://salishsea.eos.ubc.ca/erddap/griddap/ubcSSfDepthAvgdCurrents1h.csv?',
              east,north, sep='')
 return(url) 
}

currentDirection <- function(data) {

  
  CurrentAngle = atan(data$VelN_mps/data$VelE_mps)*180/pi
  
  # get the quandrants
  data$quandrant=1
  data$quandrant[sign(data$VelE_mps)==  1 & sign(data$VelN_mps)== -1]=2
  data$quandrant[sign(data$VelE_mps)== -1 & sign(data$VelN_mps)== -1]=3
  data$quandrant[sign(data$VelE_mps)== -1 & sign(data$VelN_mps)== 1]=4
  
  
  
  # quad 1
  data$heading[sign(CurrentAngle)==1 &data$quandrant==1]=
  90- CurrentAngle[sign(CurrentAngle)==1 &data$quandrant==1]
  
  data$heading[sign(CurrentAngle)==-1 &data$quandrant==1]=
  -CurrentAngle[sign(CurrentAngle)==-1 &data$quandrant==1]+90
  
  
  # quad 2
  data$heading[sign(CurrentAngle)==1 &data$quandrant==2]=
  90- CurrentAngle[sign(CurrentAngle)==1 &data$quandrant==2]
  
  data$heading[sign(CurrentAngle)==-1 &data$quandrant==2]=
  -CurrentAngle[sign(CurrentAngle)==-1 &data$quandrant==2]+90
  
  # quad 3
  data$heading[sign(CurrentAngle)==1 &data$quandrant==3]=
  360- 90-CurrentAngle[sign(CurrentAngle)==1 &data$quandrant==3]
  
  data$heading[sign(CurrentAngle)==-1 &data$quandrant==3]=
  180- CurrentAngle[sign(CurrentAngle)==-1 &data$quandrant==3]
  
  
  # quad 4
  data$heading[sign(CurrentAngle)==1 &data$quandrant==4]=
  270 + CurrentAngle[sign(CurrentAngle)==1 &data$quandrant==4]
  
  data$heading[sign(CurrentAngle)==-1 &data$quandrant==4]=
  360-90-CurrentAngle[sign(CurrentAngle)==-1 &data$quandrant==4]
  
  heading<-as.array(data$heading)
  
  return(heading)
}