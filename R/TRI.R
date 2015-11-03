#' Compute a terrain ruggedness index following sappington et al 2007
#' 
#' it returns a raster layer with the sum of the absolute values of the elevation differences between a central grid cell and its 8 neighboring cells
#' 
#' @author Cyril Milleret 
#' @param r1 is the raster layer to compute TRI from
#' @param gri is the resolution of the grid in which TRI should be calculated. grid=3 (default) will look like this :
#' @param 1,1,1,
#' @param 1,0,1,
#' @param 1.1,1  where 0 is the focal cell where the index is calculated and 1 the cells use to calculated the relative index
#' @param na.rm whether NA cells should be computed or not. 
#' @usage Terrain <- TRI(r1= veg_mask,res=3,na.rm=TRUE)
#' @export 
#' @examples  
#'  library(raster)
#'  r1 <- raster(nrows=10, ncols=10, xmn=0, xmx=10)
#'  r1
#'  values(r1) <- c(1:100)
#'  hey <- TRI(r1, gri=7, na.rm=TRUE)
#'  image(hey)
#'  matrix(values(r1), ncol=4, byrow=T)
#'  values(hey)


TRI<-function(r1,gri=3,na.rm=TRUE){
  require(raster)
  m <- matrix(rep(1,(gri*gri) ), byrow=T,nrow=gri)
  foc <- median(c(1:(gri*gri)))
  
  func <- function(x) (sum(abs((x[(foc)]- x[(c(1:(foc-1),(foc+1):(gri*gri)))])))) 
  r2<-focal(r1,m,fun=func)
  
  return(r2)
}

