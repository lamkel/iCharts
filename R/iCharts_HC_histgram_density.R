# HighCharts Histogram-Density Plot #

InteractiveChart.hc.hd <- function(xvalue, yvalue, bwidth, theme){

require("highcharter")
  
gURL <- "https://docs.google.com/spreadsheets/d/1KSl4z1msk0EdVhh0JOWNkWp8vCyqjf14LLIcaYV6Wl0/edit?usp=sharing"
rdat <- as.data.frame(gsheet::gsheet2tbl(gURL))  
  
.xval <- rdat[,xvalue][-which(is.na(rdat[,xvalue]))]
basehist <- hist(.xval, breaks = bwidth)

if (yvalue == "Frequency") {

  basehist.df <- data.frame(x = basehist$mids, y = basehist$counts, 
                           z = paste(basehist$breaks[1:length(basehist$breaks)-1], "to" ,basehist$breaks[2:length(basehist$breaks)]),
                           stringsAsFactors = FALSE)

  js.tooltip <- "function(){return '<b>Range:</b> ' + this.point.z + '<br><b>Frequency:</b> '+ this.point.y;}" 

  } else {

  basehist.df <- data.frame(x = basehist$mids, y = basehist$density, 
                           z = paste(basehist$breaks[1:length(basehist$breaks)-1], "to" ,basehist$breaks[2:length(basehist$breaks)]),
                           stringsAsFactors = FALSE) 

  js.tooltip <- "function(){         
                   var text = '';
                    if(this.series.name == 'main') {
                       text = '<b>Range:</b> ' + this.point.z + '<br><b>Density:</b> '+ this.point.y;
                    } else {
                       text = '<b>Kernel Density:</b> '+ this.point.y;
                    }
                    return text;}"
  } 

# Main Plot Engine #

hc <- highchart() %>% 
  hc_title(text = paste("Distribution of", xvalue)) %>% 
  hc_xAxis(title = list(text = xvalue)) %>% 
  hc_yAxis(title = list(text = yvalue)) %>% 
  hc_add_series(data = rCharts::toJSONArray2(basehist.df, json = FALSE,names = TRUE), 
                type = "column", name = "main") %>%
  hc_legend(enabled = FALSE) %>%
  hc_plotOptions(column = list(groupPadding = 0, pointPadding = 0, borderWidth = 0.5, shadow = FALSE)) %>% 
  hc_tooltip(borderWidth=0, followPointer=TRUE, followTouchMove=TRUE, shared = FALSE,
             formatter = JS(js.tooltip)) %>%
  hc_chart(zoomType = "xy") %>%
  hc_exporting(enabled = TRUE) 
  
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
    
  # Add kernel density to density plot #
 
  if (yvalue == "Density") {
   .den <- density(.xval) 
   kd <- data.frame(x = .den$x[which(.den$x>=min(.xval) & .den$x<=max(.xval))],
                    y = .den$y[which(.den$x>=min(.xval) & .den$x<=max(.xval))])
   hc <- hc_add_series_scatter(hc, kd$x, kd$y, name = "kd")
  }

 return(hc)

}

inlineChart.hc.hd <- function(dat, xvalue, yvalue, bwidth, theme, addKernelDensity){ 
  p1 <- InteractiveChart.hc.hd(dat, xvalue, yvalue, bwidth, theme, addKernelDensity) 
  p1$set(height = 650) 
  paste(capture.output(p1$show('inline')), collapse ='\n') # Actual function to plot charts #
} 

  
##############################################################################################################
############################################### For Production ###############################################
##############################################################################################################

InteractiveChart.hc.hd.prod <- function(dat, xvalue, yvalue, bwidth, theme){
  
  require("highcharter")
  
  rdat <- jsonlite::fromJSON(dat)

  .xval <- rdat[,xvalue][-which(is.na(rdat[,xvalue]))]
  basehist <- hist(.xval, breaks = bwidth)
  
  if (yvalue == "Frequency") {
    
    basehist.df <- data.frame(x = basehist$mids, y = basehist$counts, 
                              z = paste(basehist$breaks[1:length(basehist$breaks)-1], "to" ,basehist$breaks[2:length(basehist$breaks)]),
                              stringsAsFactors = FALSE)
    
    js.tooltip <- "function(){return '<b>Range:</b> ' + this.point.z + '<br><b>Frequency:</b> '+ this.point.y;}" 
    
  } else {
    
    basehist.df <- data.frame(x = basehist$mids, y = basehist$density, 
                              z = paste(basehist$breaks[1:length(basehist$breaks)-1], "to" ,basehist$breaks[2:length(basehist$breaks)]),
                              stringsAsFactors = FALSE) 
    
    js.tooltip <- "function(){         
    var text = '';
    if(this.series.name == 'main') {
    text = '<b>Range:</b> ' + this.point.z + '<br><b>Density:</b> '+ this.point.y;
    } else {
    text = '<b>Kernel Density:</b> '+ this.point.y;
    }
    return text;}"
} 
  
  # Main Plot Engine #
  
  hc <- highchart() %>% 
    hc_title(text = paste("Distribution of", xvalue)) %>% 
    hc_xAxis(title = list(text = xvalue)) %>% 
    hc_yAxis(title = list(text = yvalue)) %>% 
    hc_add_series(data = rCharts::toJSONArray2(basehist.df, json = FALSE,names = TRUE), 
                  type = "column", name = "main") %>%
    hc_legend(enabled = FALSE) %>%
    hc_plotOptions(column = list(groupPadding = 0, pointPadding = 0, borderWidth = 0.5, shadow = FALSE)) %>% 
    hc_tooltip(borderWidth=0, followPointer=TRUE, followTouchMove=TRUE, shared = FALSE,
               formatter = JS(js.tooltip)) %>%
    hc_chart(zoomType = "xy") %>%
    hc_exporting(enabled = TRUE) 
  
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
  
  # Add kernel density to density plot #
  
  if (yvalue == "Density") {
    .den <- density(.xval) 
    kd <- data.frame(x = .den$x[which(.den$x>=min(.xval) & .den$x<=max(.xval))],
                     y = .den$y[which(.den$x>=min(.xval) & .den$x<=max(.xval))])
    hc <- hc_add_series_scatter(hc, kd$x, kd$y, name = "kd")
  }
  
  return(hc)
  
}

inlineChart.hc.hd.prod <- function(dat, xvalue, yvalue, bwidth, theme){ 
  p1 <- InteractiveChart.hc.hd.prod(dat, xvalue, yvalue, bwidth, theme) 
  p1$set(height = 650) 
  paste(capture.output(p1$show('inline')), collapse ='\n') # Actual function to plot charts #
} 
