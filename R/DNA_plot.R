#' Open an interactive map of DNA samples collected in your browser
#' 
#' 
#' 
#' @param ID character string; Genetic ID of the Individual 
#' @param winter character string; winter of the MCP e.g. for MCP 2007/2008 write "0708"; for 2000/2001, write "0001"
#' @param WD character string; folder location of the files DNA_sample.csv and MCP_98_12.shp
#' @return interactive map of DNA samples of the individual selected
#' @export 
#' @usage DNA_plot(ID="G1-11",winter="1011",WD="C:/My_documents/Phd_Cyril/DNA/interactive_map/function_plot") 



######## load the function ####
DNA_plot <- function(ID,winter,WD){
  library(devtools)
  library(rleafmap)
  library(rgdal)
  
  setwd(WD)
  #DNA
  DNA <- read.csv("DNA_territory1.csv", header = TRUE, sep = ",")
  DNA_33<-SpatialPointsDataFrame(DNA[c("Y_Koord_UTM33","X_Koord_UTM33")],DNA[c("SLU_ID","SamplingDate","IDnbr","territory","BirthTerr","SamplID")])
  proj4string(DNA_33)<-CRS("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs")
  DNA_33<-spTransform(DNA_33, CRS("+proj=longlat +datum=WGS84"))
  DNA_33<-DNA_33[DNA_33$SLU_ID==ID ,]
  
  if(length(DNA_33$SLU_ID) < 1 ) 
    stop(paste("No DNA samples for ID ", ID,sep=""))
  
  
  # MCP
  MCP <- readOGR(".", "MCP_98_14",verbose = FALSE)
  MCP<-spTransform(MCP, CRS("+proj=longlat +datum=WGS84"))
  lev<-levels(MCP$YEAR)
  MCP$YEAR<-as.character(MCP$YEAR)
  if(length(MCP[MCP$YEAR==winter,]$YEAR) < 1 ) 
    stop(paste("No MCP for year ", winter, "; supported values are","9899 to",  lev[length(lev)-2], sep=""))
  
  MCP1<-MCP[MCP$YEAR==winter,]  
  
  
  # Creating two tile layer objects
  stamen.bm <- basemap("stamen.toner")
  mapquest.bm <- basemap("mapquest.map")
  
  # Creating two data layer objects (stations and density grid)
  mcp <- spLayer(MCP1, stroke = T, fill.col = 1,
                 popup = paste(
                   paste("Territory=", as.character(MCP1$Revir),sep=""),
                   paste("Winter=", as.character(MCP1$YEAR),sep=""),
                   sep=" "))
  
  
  dna <- spLayer(DNA_33, popup = c(paste(
    paste("Sample_ID=", as.character(DNA_33$SamplID),sep=""),
    paste("SLU_ID_=", as.character(DNA_33$SLU_ID),sep=""), 
    paste("Sampling_date_=", as.character(DNA_33$SamplingDate),sep=""),
    paste("Territory_=", as.character(DNA_33$territory),sep=""),
    paste("Birth_Territory_=", as.character(DNA_33$BirthTerr),sep=""),
    sep=" "))) 
  
  lines_sp <- SpatialLines(list(Lines(list(Line(DNA_33)),ID="a")))
  lines1 <- spLayer(lines_sp) 
  
  # Creating an UI object with a layer control
  my.ui <- ui(layers = "topright")
  
  #Generating the map
  writeMap( mapquest.bm, mcp, dna,lines1,
            width = 1500, height = 700, interface = my.ui,
            setView = c(60.5, 15), setZoom = 6,directView="browser")  
}


## run the function 
## ID= character string; ID of the Individual  
## winter= character string; winter of the MCP e.g. for MCP 2007/2008 write "0708"; for 2000/2001, write "0001"
## Working directory; character string; folder location of the files DNA_sample.csv and MCP_98_12.shp

