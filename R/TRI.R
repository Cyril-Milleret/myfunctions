#' Compute a terrain ruggedness index followin sappington et al 2007
#' 
#' it returns a raster layer with the sum of the absolute values of the elevation differences between a central grid cell and its 8 neighboring cells
#' 
#' @author Cyril Milleret 
#' @param r1 is the raster layer to compute TRI from
#' @param gri is the resolution of the grid in which TRI should be calculated. grid=3 will look like this :
#'        1,1,1,
#'        1,0,1,
#'        1.1,1  where 0 is the focal cell where the index is calculated and 1 the cells use to calculated the relative index
#' @usage Terrain <- TRI(r1= veg_mask,res=3)
#' @export 

TRI<-function(r1,gri=3){
  m <- matrix(c(1:(gri*gri)), byrow=T,nrow=gri)
  gri <- 3## griolution of the TRI
  foc <- median(c(1:(gri*gri)))
  
  func <- function(x) (sum(abs((x[foc]- x[c(1:(foc-1),(foc+1):(gri*gri))])))) 
  r2<-focal(r1,m,fun=func)
  
  return(r2)
}

