
#' Calculate area of overalp between two probability density function
#' 
#'   
#' 
#' @author Cyril Milleret 
#' @param da a density object See \code{density()}
#' @param db a density object See \code{density()}
#' 
#' @return the area of overlap (percentages)   between the 2 curves
#' @usage overlap_curves(da,db)
#'
#' @export 
#' 



overlap_curves<-function(da,db){
  d <- data.frame(x=da$x, a=da$y, b=db$y)
  
  # calculate intersection densities
  d$w <- pmin(d$a, d$b)
  
  # integrate areas under curves
  library(sfsmisc)
  total <- integrate.xy(d$x, d$a) + integrate.xy(d$x, d$b)
  intersection <- integrate.xy(d$x, d$w)
  
  # compute overlap coefficient
  overlap <- 2 * intersection / total
  return(overlap)
}