# NVD3 scatterChart #

InteractiveChart <- function(xvar, yvar, plottype, pointcolor, groupcolor, pointsize, groupsize){
  
   # communicate with Tyler to seek location of file #
  # communicate with Tyler to seek location of file #
  #   dat <- data.table::fread("C:/Users/Kelvin/Dropbox/Statistics Solutions (my own Folder)/testdata_scatterchart.csv",
  #                            data.table=FALSE, verbose = TRUE, na.strings="", header=TRUE) 
  
  # Work on test data in dropbox #
  dbcsv <- "testdata_scatterchart.csv"  
  mykey <- "qcbiamjetc7tvbc"
  dat <- repmis::source_DropboxData(dbcsv, key=mykey, sep=",", header=TRUE)
  

   numvars <- names(dat)[which(sapply(dat, class) %in% c("integer","numeric"))] # Can be expanded #
   chrvars <- names(dat)[which(sapply(dat, class) %in% c("character","logical"))] # Can be expanded (e.g. dates) #
      
   # Run before generating plots #
   overall <- data.frame(Overall="Overall", size = 1, color = pointcolor)
   names(overall)[1:2] <- c("<Overall>", "<Equal Size>")

   # Chart function #
   p1 <- rCharts::nPlot(as.formula(paste(yvar,"~",xvar)),  
                        data = cbind(dat, overall), 
                        group = groupcolor,
                        type = plottype,
                        margin = list(left = 80, bottom = 80),
                        showControls = FALSE,
                        showDistX = TRUE,
                        showDistY = TRUE,
                        height = 400, 
                        width = 650)
   
   eval(parse(text = paste0("p1$chart(size = '#! function(d){return d.", groupsize, "} !#')")))
   
   if (groupcolor == "Overall") {
     p1$chart(color = '#! function(d){return d.color} !#')
   }
   
   p1$chart(
     sizeRange = c(pointsize, 50 * pointsize),
     margin = list(left = 80, bottom = 80),
     showControls = FALSE,
     showDistX = TRUE,
     showDistY = TRUE
   ) 
   
   p1$xAxis(axisLabel = xvar)
   p1$yAxis(axisLabel = yvar)
   
   # Avoid using addcontrols to retrieve control values #
   # Use save instead #
   
   return(p1)
}   

saveChart <- function(xvar, yvar, plottype, pointcolor, groupcolor, pointsize, groupsize){ 
 p1 <- InteractiveChart(xvar, yvar, plottype, pointcolor, groupcolor, pointsize, groupsize) 
 p1$set(height = 700) 
 p1$save('output.html', cdn = T) # To pull $scope.controls #
   return(invisible()) 
} 

inlineChart <- function(xvar, yvar, plottype, pointcolor, groupcolor, pointsize, groupsize){ 
 p1 <- InteractiveChart(xvar, yvar, plottype, pointcolor, groupcolor, pointsize, groupsize) 
 p1$set(height = 650) 
 paste(capture.output(p1$show('inline')), collapse ='\n') # Actual function to plot charts #
} 




