#' Open an interactive map of DNA samples collected in your browser
#' 
#' 
#' 
#' @param WD  character string; folder location of the files "DNA_sample.csv" and "MCP_98_12.shp" e.g C:/Users/cyrilm/Desktop"
#' @return interactive map of DNA samples of the individual selected
#' @export 
#' @usage DNA_app(C:/Users/cyrilm/Desktop") 



######## load the function ####
DNA_app <- function(WD){
  setwd(WD)
  library(shiny)
  library(sp)
  library(rgdal)
  library(leaflet)
  library(htmltools)
  r_colors <- rgb(t(col2rgb(colors()) / 255))
  names(r_colors) <- colors()
  
  app <- shinyApp(
    
    ui <- bootstrapPage(
      tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
      leafletOutput("mymap", width = "100%", height = "100%"),
      absolutePanel(top = 10, right = 10,
                    
                    textInput("text", label = "ID (e.g. G142-14 )", 
                              value = "G142-14"),
                    
                    textInput("text1", label = "Territory polygons (e.g. winter 2001/02= 0102)", 
                              value = "0102"),
                    actionButton("recalc", "Search"))),   
    
    
    
    
    
    
    server <- function(input, output, session) {
      
      DNA <-read.csv("DNA_territory1.csv" )
      DNA_33<-SpatialPointsDataFrame(DNA[c("Y_Koord_UTM33","X_Koord_UTM33")],DNA[c("SLU_ID","SamplingDate","IDnbr","territory","BirthTerr","SamplID")])
      proj4string(DNA_33)<-CRS("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs")
      DNA_33<-spTransform(DNA_33, CRS("+proj=longlat +datum=WGS84"))
      DNA_33$lat <- coordinates(DNA_33)[,1]
      DNA_33$long <- coordinates(DNA_33)[,2]
      
      
      clear_cut_clip <- readOGR(".", "MCP_98_14")
      clear_cut_clip<-spTransform(clear_cut_clip, CRS("+proj=longlat +datum=WGS84"))
      
      
      
      
      ## create react event for layer markers
      points <- eventReactive(input$recalc, {
        f <- subset(DNA_33, SLU_ID %in% input$text) 
        
      }, ignoreNULL = FALSE)
      
      
      ## create react event for pop ups markers
      
      points_popa <- eventReactive(input$recalc, {
        f1 <- subset(DNA_33, SLU_ID %in% input$text)
        pops<- f1$SLU_ID
        
        return(pops)
      }, ignoreNULL = FALSE)
      
      
      points_pop <- eventReactive(input$recalc, {
        f1 <- subset(DNA_33, SLU_ID %in% input$text)
        pops<- f1$SamplingDate
        
        return(pops)
        
      }, ignoreNULL = FALSE)
      points_pop1 <- eventReactive(input$recalc, {
        f1 <- subset(DNA_33, SLU_ID %in% input$text)
        pops<- f1$SamplID
        
        return(pops)
        
      }, ignoreNULL = FALSE)
      
      
      ## create react event for pop ups polygons
      
      
      pol <- eventReactive(input$recalc, {
        f1 <- subset(clear_cut_clip, YEAR %in% input$text1) 
        
      }, ignoreNULL = FALSE)
      
      
      ## create react event for pop ups markers
      #territory
      pol_pop <- eventReactive(input$recalc, {
        f1 <- subset(clear_cut_clip, YEAR %in% input$text1) 
        pops<- f1$Revir
        
        return(pops)
        
      }, ignoreNULL = FALSE)
      
      # winter
      pol_pop1 <- eventReactive(input$recalc, {
        f1 <- subset(clear_cut_clip, YEAR %in% input$text1) 
        pops<- f1$YEAR
        
        return(pops)
        
      }, ignoreNULL = FALSE)
      
      
      ##output map
      
      
      output$mymap <- renderLeaflet({
        leaflet() %>%
          addProviderTiles("OpenTopoMap")%>%# check here the list http://leaflet-extras.github.io/leaflet-providers/preview/index.html
          addMarkers(data = points()[,c("lat","long")],
                     #popup= htmlEscape(points_pop())
                     popup = paste("<strong>SLU_ID: </strong>", points_popa(), "<br>",
                                   "<strong>Sample_ID: </strong>", points_pop1(), "<br>",
                                   "<strong>Sampling date: </strong>", points_pop(), "<br>"))%>%
          addPolygons(data=pol(),fillColor ="red",color="red",
                      
                      popup = paste("<strong>Territory: </strong>", pol_pop(), "<br>",
                                    "<strong>Winter: </strong>", pol_pop1(), "<br>"))
        
      })
    }
  )
  
  
  
 print(app)
  
  
}


## run the function 
## ID= character string; ID of the Individual  
## winter= character string; winter of the MCP e.g. for MCP 2007/2008 write "0708"; for 2000/2001, write "0001"
## Working directory; character string; folder location of the files DNA_sample.csv and MCP_98_12.shp

