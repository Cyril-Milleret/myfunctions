#' add confidence intervals to a plot
#' 
#' Provide x and y vectors of values and if you have Standard error values or upper condidence intervals and lower confidence itnervals 
#' 
#' @author Cyril Milleret 
#' @param x vectors of values on the x axis
#' @param y vectors of values on the y axis
#' @param SE vectors of values on the for the standard error if stder=TRUE
#' @param stder TRUE if you have SE values (1.96*SE), FALSE if you already have CI limits
#' @param low_ci vector with lower confidence limits
#' @param up_ci vector with upper confidence limits
#' @param z how large should the end of the forizontal line of the confidence interval, default is 0.1
#' @param col color, default is black
#' @param lwd line width, default is1
#' @param lty line type, default is 1
#' @usage conf_inter(x, y, se=SE, stder=TRUE, low_ci=lowci, up_ci=upci, z=0.1,col="black",lty=1, lwd=1) 
#' @export 
#' 
#' 
#' 
conf_inter<- function(x, y, se=SE, stder=TRUE, low_ci=lowci, up_ci=upci, z=0.1,col="black",lty=1, lwd=1){
  
  
if(stder==TRUE){
  
  if(length(SE)!=length(x) | length(y)!=length(x))
    stop("x, y and SE have not the same length")
  
    for ( i in 1:length(x)){
    segments(x[i] , y[i] + (1.96*SE[i]) ,x[i] , y[i] -(1.96*SE[i]) ,col=col  ,lty = lty, lwd= lwd  )
    segments(x[i]-z, y[i] + (1.96*SE[i]), x[i]+z, y[i] + (1.96*SE[i]))
    segments(x[i]-z, y[i] - (1.96*SE[i]), x[i]+z, y[i] - (1.96*SE[i]))
    }
}
  
  if(stder==FALSE){
    if(length(lowci)!=length(x) | length(y)!=length(x) | length(upci)!=length(x))
      stop("x, y and SE have not the same length")
    
    
    for ( i in 1:length(x)){
      segments(x[i] , lowci[i] ,x[i] , upci[i] ,col=col, lty =lty, lwd= lwd  )
      segments(x[i]-z, lowci[i], x[i]+z, lowci[i])
      segments(x[i]-z, upci[i], x[i]+z, upci[i])
    }
  }
}

