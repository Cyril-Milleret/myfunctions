#' Merge two SpatialPointsDataFrame


#' @author Cyril Milleret 
#' @param sp1 first spatialPointsdataFrame 
#' @param sp2 second spatialPointsdataFrame 
#' @return a merged SpatialPointsDataFrame. it uses the projection of sp1 if the projection is not null
#' @export 
#' @usage mergespdf(sp1,sp2)
mergespdf<-function(sp1,sp2){

temp<-rbind(as.data.frame(sp1),as.data.frame(sp2))

sp<-SpatialPointsDataFrame(temp[,c(length(temp[1,]) -1, length(temp[1,]))], temp[,c(1: (length(temp[1,]) -2)) ])


if(!is.na(proj4string(sp1))){
  proj4string(sp)<- CRS(proj4string(sp1))
  
}
return(sp)
}
