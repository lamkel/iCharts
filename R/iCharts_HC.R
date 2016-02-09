# HighCharts #


# Dynamically generate drop down selection list #

dyn.param.list <- function(dat){		
  
  # dat in json format from /R/.val/json #		
  
  rdat <- jsonlite::fromJSON(dat)		
  numvars <- names(rdat)[which(sapply(rdat, class) %in% c("integer","numeric"))] # Can be expanded #		
  chrvars <- names(rdat)[which(sapply(rdat, class) %in% c("character","logical"))] # Can be expanded (e.g. dates) #		
  return(jsonlite::toJSON(list("dat"=dat,		
                               "$scope.xvalues" = numvars, 		
                               "$scope.yvalues" = numvars,		
                               "$scope.plottypes" = c("scatterChart"),		
                               "$scope.pointcolors" = c("blue", "green", "red", "orange", "black"),		
                               "$scope.groupcolors" = chrvars,		
                               "$scope.pointsizes" = c(1:10),		
                               "$scope.groupsizes" = numvars)))}	


# Main Plot Engire #

InteractiveChart.hc <- function(dat, xvalue, yvalue, plottype, pointcolor, groupcolor, pointsize, groupsize){

  # Work on test data in dropbox #
  
  dbcsv <- "testdata_scatterchart.csv"  
  mykey <- "k0rud5ehlmgaxnn"
  rdat <- repmis::source_DropboxData(dbcsv, key=mykey, sep=",", header=TRUE)
  
  # Run before generating plots #

  overall <- data.frame(Overall="OVERALL", size = 1)
  names(overall)[1:2] <- c("OVERALL", "SAME")
   
  # Chart function #
  
  h1 <- rCharts::hPlot(x = xvalue, 
                       y = yvalue,
                       data = cbind(rdat, overall), 
                       type = plottype, 
                       group = groupcolor, 
                       size = groupsize,
                       radius = pointsize,
                       margin = list(left = 80, bottom = 100),
                       height = 400, 
                       width = 650)
  
  h1$chart(zoomType = "xy") # Add zoom capability #
  h1$exporting(enabled = T)
  #h1$legend(align = 'right', verticalAlign = 'top', layout = 'vertical')
  h1$plotOptions(scatter = list(marker = list(symbol = 'circle')))

  # If choosing one colour only add color column #
  
  if (groupcolor == "OVERALL") {
    h1$plotOptions(series = list(color = pointcolor))
  }   
    
   return(h1)
}   

inlineChart.hc <- function(dat, xvalue, yvalue, plottype, pointcolor, groupcolor, pointsize, groupsize){ 
  p1 <- InteractiveChart.hc(dat, xvalue, yvalue, plottype, pointcolor, groupcolor, pointsize, groupsize) 
  p1$set(height = 650) 
  paste(capture.output(p1$show('inline')), collapse ='\n') # Actual function to plot charts #
} 




  
   		


