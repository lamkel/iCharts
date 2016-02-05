# Pick out numeric and character variables to create dynamic parameter selection list #

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
                               "$scope.groupsizes" = numvars
                               ))) 
   
}
   
# HighCharts Scatter Plot #         

InteractiveChart <- function(dat, xvalue, yvalue, plottype, pointcolor, groupcolor, pointsize, groupsize){

  # Run before generating plots #
  overall <- data.frame(Overall="OVERALL", size = 1)
  names(overall)[1:2] <- c("OVERALL", "SAME")
        
  # Codes starting from here are affected by the user's choice #
  # If choosing one colour only add color column #
  if (groupcolor == "OVERALL") {
   overall[,"color"] <- pointcolor
  }   
  rdat <- jsonlite::fromJSON("http://localhost:9637/ocpu/tmp/x0716ac8832/R/.val/json")
#  rdat <- jsonlite::fromJSON(dat)
  
   # Chart function #
   p1 <- rCharts::nPlot(as.formula(paste(yvalue,"~",xvalue)),  
                        data = cbind(rdat, overall), 
                        group = groupcolor,
                        type = plottype,
                        margin = list(left = 80, bottom = 100),
                        height = 400, 
                        width = 650)
   
   eval(parse(text = paste0("p1$chart(size = '#! function(d){return d.", groupsize, "} !#')")))
   p1$chart(sizeRange = c(10* pointsize, 100 * pointsize))
   p1$chart(showControls=TRUE, showDistX = TRUE, showDistY = TRUE)

   p1$xAxis(axisLabel = xvalue)
   p1$yAxis(axisLabel = yvalue)

   return(p1)
}   

inlineChart <- function(dat, xvalue, yvalue, plottype, pointcolor, groupcolor, pointsize, groupsize){ 
 p1 <- InteractiveChart(dat, xvalue, yvalue, plottype, pointcolor, groupcolor, pointsize, groupsize) 
 p1$set(height = 650) 
 paste(capture.output(p1$show('inline')), collapse ='\n') # Actual function to plot charts #
} 




