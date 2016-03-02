# HighCharts #
# Main Plot Engine #

InteractiveChart.hc.sb <- function(dat, xvalue, yvalue, 
                                   plottype, pointcolor, groupcolor, pointsize, groupsize, 
                                   addRegline){

  # Convert json into df #
  rdat <- jsonlite::fromJSON(dat)
  
  # Run before generating plots #
  overall <- data.frame(Overall="OVERALL", size = 1, stringsAsFactors=FALSE)
  names(overall)[1:2] <- c("OVERALL", "SAME")
  rdat <- cbind(rdat, overall)
  
  # Fill in missing values for the legend order #
  rdat[which(rdat[groupcolor]==""), groupcolor] <- "Missing Value"
  
  # Number of unique groupcolor levels #
  groupcolorl <- nrow(unique(rdat[groupcolor]))

  # Main plot function #
  
  if (plottype %in% c("scatter","bubble")) {
    
   # Adding Regression Line #

    if (addRegline == "YES") {
     yvar <- rdat[,yvalue]
     xvar <- rdat[,xvalue]
     regresults <- lm(yvar ~ xvar)
     regresults <- data.frame(rdat[xvalue], 
                              predict(regresults, newdata=data.frame(yvar,xvar)), 
                              "0_ Regression line _0", 0)
     names(regresults)[2:4] <- c(yvalue, groupcolor, groupsize)
     rdat <- rbind(rdat[,c(xvalue, yvalue, groupcolor, groupsize)], regresults)
     plottype.final <- c("line", rep(plottype,groupcolorl))
    } else {
      rdat <- rdat[,c(xvalue, yvalue, groupcolor, groupsize)]
      plottype.final <- plottype     
    }
    
     if (plottype == "scatter") {
       plottitle <- paste("Scatter Plot with", yvalue, "as Y-axis and", xvalue, "as X-axis, group by", groupcolor)
     } else {
       plottitle <- paste("Bubble Chart with", yvalue, "as Y-axis and", xvalue, "as X-axis, group by", groupcolor, "and bubble size by", groupsize)       
     }
    
     h1 <- rCharts::hPlot(x = xvalue, 
                          y = yvalue,
                          data = rdat, 
                          type = plottype.final, 
                          group = groupcolor, 
                          size = groupsize,
                          radius = pointsize,
                          title = plottitle)

     # If choosing one colour only add color column # 
     # Group all options into one plotOptions statement to prevent overwritting the previous #
  
     if (groupcolor == "OVERALL") {
       if (plottype == "scatter") {
        h1$plotOptions(scatter = list(marker = list(symbol = 'circle'), color = pointcolor))
       } else {
        h1$plotOptions(bubble = list(color = pointcolor, minSize = 3, maxSize = 50)) 
       }   
     } else {
       if (plottype == "scatter") {
         h1$plotOptions(scatter = list(marker = list(symbol = 'circle')))
       } else {
         h1$plotOptions(bubble = list(minSize = 3, maxSize = 50)) 
       }   
     }
  }
  
  h1$chart(zoomType = "xy") # Add zoom capability #
  h1$exporting(enabled = T)
  return(h1)
  
}   

inlineChart.hc.sb <- function(dat, xvalue, yvalue, plottype, pointcolor, groupcolor, pointsize, groupsize, addRegline){ 
  p1 <- InteractiveChart.hc.sb(dat, xvalue, yvalue, plottype, pointcolor, groupcolor, pointsize, groupsize, addRegline) 
  p1$set(height = 650) 
  paste(capture.output(p1$show('inline')), collapse ='\n') # Actual function to plot charts #
} 





