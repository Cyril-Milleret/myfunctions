#' Compute a terrain ruggedness index followin sappington et al 2007
#' 
#' it returns the sum of the absolute values of the elevation differences between a central grid cell and its 8 neighboring cells
#' 
#' @author Cyril Milleret 
#' @param r is the SpatialPixel(DataFrame)
#' @param layer the layer where elevation is stored (for instance r$elevation)
#' @usage Terrain <- TRI(r= veg_mask,layer=veg_mask$elevation)
#' @export 

TRI <- function(r=r,layer=r$layer){
  require(sp)
  temp<-as.data.frame(r@grid)
  
  row<-temp[1,3]## number of cells on the row
  col<-temp[2,3]## number of cells on the columns
  
  
  notdo_rigth <- row*c(1:col)
  notdo_left<-seq(1,row*col,by=row)
  notdo_above<-seq(1,row,by=1)
  notdo_below<-seq(row*col-row+2 ,row*col-1,by=1)
  
  all_<-c(notdo_rigth,notdo_left,notdo_above,notdo_below)
  all_<-unique(all_)
  
  ## erase some
  notdo_rigth<- notdo_rigth[!notdo_rigth %in% c(row,row*col) ] # is not in 
  notdo_left<- notdo_left[!notdo_left %in% c(1,row*col-row+1) ] # is not in 
  notdo_above<- notdo_above[!notdo_above %in% c(1,row) ] # is not in 
  
  
  TRI <- NULL
  for ( i in 1:length(layer)){
    
    if(!i %in% all_){ 
      TRI[i]<- sum(abs(
        layer[i-1] - layer[i]),
        abs(layer[i+1] - layer[i]),
        
        abs(layer[i-row+1] - layer[i]),
        abs(layer[i-row] - layer[i]),
        abs(layer[i-row-1] - layer[i]), 
        
        abs(layer[i+row+1] - layer[i]),
        abs(layer[i+row] - layer[i]),
        abs(layer[i+row-1] - layer[i]))
    } else{
      
      
      if(i %in% notdo_rigth){
        TRI[i]<- sum(abs(
          layer[i-1] - layer[i]),
          abs(layer[i-row] - layer[i]),
          abs(layer[i-row-1] - layer[i]),
          abs(layer[i+row] - layer[i]),
          abs(layer[i+row-1] - layer[i]))
      } else{
        
        if(i %in% notdo_left){
          TRI[i]<- sum(abs(
            layer[i+1] - layer[i]),
            abs(layer[i-row] - layer[i]),
            abs(layer[i-row+1] - layer[i]),
            abs(layer[i+row] - layer[i]),
            abs(layer[i+row+1] - layer[i]))
        } else{
          
          if(i %in% notdo_above){
            TRI[i]<- sum(abs(
              layer[i-1] - layer[i]),
              abs(layer[i+1] - layer[i]),
              abs(layer[i+row+1] - layer[i]),
              abs(layer[i+row] - layer[i]),
              abs(layer[i+row-1] - layer[i]))
          } else{
            
            if(i %in% notdo_below){
              TRI[i]<- sum(abs(
                layer[i-1] - layer[i]),
                abs(layer[i+1] - layer[i]),
                abs(layer[i-row+1] - layer[i]),
                abs(layer[i-row] - layer[i]),
                abs(layer[i-row-1] - layer[i]))
            }  else{
              
              
              
              
              
              if(i ==row){# rigth corner up
                TRI[i]<- sum(
                  abs(layer[i-1] - layer[i]),
                  abs(layer[i+row] - layer[i]),
                  abs(layer[i+row-1] - layer[i])
                )
              } else{
                
                if(i == 1){# left corner up
                  TRI[i]<- sum(abs(
                    layer[i+1] - layer[i]),
                    abs(layer[i+row] - layer[i]),
                    abs(layer[i+row+1] - layer[i]))
                } else{
                  
                  if(i == row*col-row+1){# left corner down
                    TRI[i]<- sum(abs(
                      layer[i+1] - layer[i]),
                      abs(layer[i-row] - layer[i]),
                      abs(layer[i-row+1] - layer[i]))
                  } else{
                    
                    if(i == row*col){# rigth corner down
                      TRI[i]<- sum(abs(
                        layer[i-1] - layer[i]),
                        abs(layer[i-row] - layer[i]),
                        abs(layer[i-row-1] - layer[i]))
                    }
                  }}}}}}}}
  }
  
  return(TRI)
}