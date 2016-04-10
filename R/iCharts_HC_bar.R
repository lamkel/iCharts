# HighCharts Histogram-Density Plot #

InteractiveChart.hc.bar <- function(xvalue, yvalue, wtvar, outtype, stack, theme){

require("highcharter")

# to_json reads faster than toJSONArray2 #
  
zip_vectors_ = function(..., names = F){
    x = list(...)
    y = lapply(seq_along(x[[1]]), function(i) lapply(x, pluck_(i)))
    if (names) names(y) = seq_along(y)
    return(y)
}
  
pluck_ = function (element){
    function(x) x[[element]]
}
  
to_json = function(df, orient = "columns", json = T){
    dl = as.list(df)
    dl = switch(orient, 
                columns = dl,
                records = do.call('zip_vectors_', dl),
                values = do.call('zip_vectors_', setNames(dl, NULL))
    )
    if (json){
      dl = rjson::toJSON(dl)
    }
    return(dl)
}  
    
gURL <- "https://docs.google.com/spreadsheets/d/1KSl4z1msk0EdVhh0JOWNkWp8vCyqjf14LLIcaYV6Wl0/edit?usp=sharing"
rdat <- as.data.frame(gsheet::gsheet2tbl(gURL))  
rdat[,"overall"] <- "OVERALL"
  
rdat[rdat[,xvalue] == "",xvalue] <- "_Missing_"

if (yvalue != "_NONE_") {
 rdat[rdat[,yvalue] == "",yvalue] <- "_Missing_"  
 if (wtvar != "_NONE_") {
   plotdata <- as.data.frame.matrix(questionr::wtd.table(x = rdat[, xvalue], y = rdat[, yvalue], weights = rdat[, wtvar])) 
 } else {
   plotdata <- as.data.frame.matrix(questionr::wtd.table(x = rdat[, xvalue], y = rdat[, yvalue]))   
 }
 
 xnames <- row.names(plotdata)
 y.title <- yvalue
 main.title <- paste(yvalue, "Frequency by", xvalue, "Group")
 data.series <- paste(
   paste0("hc_add_series(data = plotdata[,'", names(plotdata), "'], name = '", names(plotdata), "')"),
   collapse = " %>% ")
 data.series <- paste("hc <- hc %>%", data.series)
 
} else {
  if (wtvar != "_NONE_") {
    plotdata <- as.data.frame(questionr::wtd.table(x = rdat[, xvalue], weights = rdat[, wtvar])) 
  } else {
    plotdata <- as.data.frame(questionr::wtd.table(x = rdat[, xvalue]))   
  }  
  
  xnames <- as.character(plotdata[,"Var1"])
  y.title <- "Frequency"
  main.title <- paste("Frequency by", xvalue, "Categories")
  data.series <- "hc <- hc %>% hc_add_series(data = plotdata[,'Freq'], name = xvalue)"
  
} 

# Main Plot Engine #

hc <- highchart() %>% 
  hc_chart(type = "column") %>%
  hc_title(text = main.title) %>% 
  hc_xAxis(categories = xnames) %>% 
  hc_yAxis(title = list(text = y.title), stackLabels = list(enabled = FALSE)) %>% 
  hc_chart(zoomType = "xy") %>%
  hc_exporting(enabled = TRUE) 
  eval(parse(text = data.series))

  if (outtype == "PERCENT") {
    
    hc <- hc %>% 
          hc_plotOptions(column = list(stacking = "percent")) %>%
          hc_tooltip(crosshairs = TRUE, 
                     headerFormat = '<span style="font-size:10px">{point.key}</span><table><br>',
                     pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y}</b> ({point.percentage:.0f}%)<br/>',
                     shared = TRUE,
                     useHTML = TRUE)           
  } else {
    
    if (stack == "YES") {
      hc <- hc %>% hc_plotOptions(column = list(stacking = "normal")) 
    }
    
    hc <- hc %>% hc_tooltip(crosshairs = TRUE, 
                            headerFormat = '<span style="font-size:10px">{point.key}</span><table><br>',
                            shared = TRUE,
                            useHTML = TRUE)
  }

  if (theme == "Google") {
   hc <- hc %>% hc_add_theme(hc_theme_google())
  } else 
    if (theme == "Dotabuff") {
      hc <- hc %>% hc_add_theme(hc_theme_db())
  } else 
    if (theme == "Economist") {
      hc <- hc %>% hc_add_theme(hc_theme_economist()) 
  } else 
    if (theme == "Gridlight") {
      hc <- hc %>% hc_add_theme(hc_theme_gridlight())    
  } else 
    if (theme == "fivethirtyeight.com") {
      hc <- hc %>% hc_add_theme(hc_theme_538())   
  } else 
    if (theme == "Sand Signika") {
      hc <- hc %>% hc_add_theme(hc_theme_sandsignika())    
  } else 
    if (theme == "Dark Unica") {
      hc <- hc %>% hc_add_theme(hc_theme_darkunica())
  } else 
    if (theme == "Chalk Board") {
      hc <- hc %>% hc_add_theme(hc_theme_chalk()) 
  }  
    
 return(hc)

}

iframe.hc.bar <- function(xvalue, yvalue, wtvar, outtype, stack, theme){
  p1 <- InteractiveChart.hc.bar(xvalue, yvalue, wtvar, outtype, stack, theme) 
  htmlwidgets::saveWidget(p1, file = "barchart.html", selfcontained = FALSE, background = "transparent")
}

# Test #
#InteractiveChart.hc.bar(xvalue = "Stakeholder", yvalue = "EcTier", wtvar = "_NONE_", stack = "NO", theme = "Google", outtype = "FREQUENCY")
#InteractiveChart.hc.bar(xvalue = "EcTier", yvalue = "Stakeholder", wtvar = "Population", stack = "NO", theme = "Economist", outtype = "FREQUENCY")
#InteractiveChart.hc.bar(xvalue = "Stakeholder", yvalue = "overall", wtvar = "Q1Culture", stack = "YES",theme = "Google", outtype = "FREQUENCY")
#InteractiveChart.hc.bar(xvalue = "Stakeholder", yvalue = "_NONE_", wtvar = "_NONE_", stack = "YES", theme = "Gridlight", outtype = "FREQUENCY")
#InteractiveChart.hc.bar(xvalue = "Stakeholder", yvalue = "Region", wtvar = "Q1Culture", stack = "YES",theme = "Google", outtype = "FREQUENCY")
#InteractiveChart.hc.bar(xvalue = "Stakeholder", yvalue = "Region", wtvar = "Q1Culture", stack = "YES",theme = "Google", outtype = "PERCENT")
#InteractiveChart.hc.bar(xvalue = "Stakeholder", yvalue = "Region", wtvar = "Q1Culture", stack = "NO",theme = "Google", outtype = "PERCENT")
InteractiveChart.hc.bar(xvalue = "EcTier", yvalue = "Stakeholder", wtvar = "Population", stack = "NO", theme = "Economist", outtype = "FREQUENCY")

##############################################################################################################
############################################### For Production ###############################################
##############################################################################################################

