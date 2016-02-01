# NVD3 scatterChart #

InteractiveChart <- function(xvalue, yvalue, plottype, pointcolor, groupcolor, pointsize, groupsize){
  
  # communicate with Tyler to seek location of file #
  #   dat <- data.table::fread("C:/Users/Kelvin/Dropbox/Statistics Solutions (my own Folder)/testdata_scatterchart.csv",
  #                            data.table=FALSE, verbose = TRUE, na.strings="", header=TRUE) 
  
  # Work on test data in dropbox #
  dbcsv <- "testdata_scatterchart.csv"  
  mykey <- "k0rud5ehlmgaxnn"
  dat <- repmis::source_DropboxData(dbcsv, key=mykey, sep=",", header=TRUE)
  

   numvars <- names(dat)[which(sapply(dat, class) %in% c("integer","numeric"))] # Can be expanded #
   chrvars <- names(dat)[which(sapply(dat, class) %in% c("character","logical"))] # Can be expanded (e.g. dates) #
      
   # Run before generating plots #
   overall <- data.frame(Overall="OVERALL", size = 1)
   names(overall)[1:2] <- c("OVERALL", "SAME")
   
   # If choosing one colour only add color column #

   if (groupcolor == "OVERALL") {
     overall[,"color"] <- pointcolor
   }   

   # Chart function #
   p1 <- rCharts::nPlot(as.formula(paste(yvalue,"~",xvalue)),  
                        data = cbind(dat, overall), 
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

   # Add Custom tooltip #
      
   #tooltip.output <- paste0("#! function(key, x, y){return '", xvalue, ": ' + xvalue + '<br>", 
   #                                                             yvalue, ": ' + yvalue ;} !#")
   
   #p1$chart(tooltipContent = tooltip.output)

   # Avoid using addcontrols to retrieve control values #
   # Use save instead #
   
   return(p1)
}   

saveChart <- function(xvalue, yvalue, plottype, pointcolor, groupcolor, pointsize, groupsize){ 
 p1 <- InteractiveChart(xvalue, yvalue, plottype, pointcolor, groupcolor, pointsize, groupsize) 
 p1$set(height = 700) 
 p1$save('output.html', cdn = T) # To pull $scope.controls #
   return(invisible()) 
} 

inlineChart <- function(xvalue, yvalue, plottype, pointcolor, groupcolor, pointsize, groupsize){ 
 p1 <- InteractiveChart(xvalue, yvalue, plottype, pointcolor, groupcolor, pointsize, groupsize) 
 p1$set(height = 650) 
 paste(capture.output(p1$show('inline')), collapse ='\n') # Actual function to plot charts #
} 




