#' Read a  spatial vector data from postgres
#' 
#' it returns the spatialPolygonDataFrame with its projection, but it only works if there is one single projection system in the table 
#' 
#' @author Philipp Hunziker <https://philipphunziker.wordpress.com/2014/07/20/transferring-vector-data-between-postgis-and-r/>
#' @param con set the connection with the database:  con <- dbConnect(dbDriver("PostgreSQL"), dbname="postgis", port=5432, host="localhost", user="postgres", password="pw")
#' @param schemaname  name of the schema in which the table is stored. 'public' is the default 
#' @param tablename  string name of the table to be imported
#' @param geomcol the field name in which geometry info are stored 
#' @param idcol Name of the column with unique IDs to be used in the ID slot of the spatial objects (not relevant for point data)
#' @usage dbwritespatial(con, myspatialdata.spdf, schemaname="myschema", tablename="myspatialdata", replace=FALSE)
#' @export




dbreadspatial <- function(con, schemaname="public", tablename, geomcol="the_geom", idcol=NULL) {
  require(sp)
  require(rgeos)
  require(rgdal)
  require(RPostgreSQL) 
  # con:          A PostgreSQL connection (from RPostgreSQL)
  # schemaname:   Target schema name
  # tablename:    Target table name
  # geomcol:      Name of the geometry column in the target table (target table may not have more than one geometry column!)
  # idcol:        Name of the column with unique IDs to be used in the ID slot of the spatial objects (not relevant for point data)
  
  ## Build query and fetch the target table
  # Get column names
  q.res <- dbSendQuery(con, statement=paste("SELECT column_name FROM information_schema.columns WHERE table_name ='", tablename, "' AND table_schema ='", schemaname, "';", sep=""))
  schema.table = paste(schemaname, ".", tablename, sep="")
  q.df <- fetch(q.res, -1)
  # Some safe programming
  if (!(geomcol %in% q.df[,1])) {stop(paste("No", geomcol, "column in specified table."))}
  if (!is.null(idcol)) {
    if (!(idcol %in% q.df[,1])) {stop(paste("Specified idname '", idcol, "' not found.", sep=""))}
  }
  # Get table
  query <- paste("SELECT", paste(q.df[,1][q.df[,1] != geomcol], collapse=", "), paste(", ST_ASTEXT(", geomcol, ") AS the_geom FROM", sep=""), schema.table, ";")
  t.res <- dbSendQuery(con, statement=query)
  t.df <- fetch(t.res, -1)
  
  ## Get geometry ID column number
  if (!is.null(idcol)) {
    idcolnum <- which(names(t.df) == idcol)
  } else {
    t.df$id.new <- 1:nrow(t.df)
    idcolnum <- which(names(t.df) == "id.new")
  }
  
  ## Get geometry column number
  geomcolnum <- which(names(t.df) == geomcol)
  
  ## Build spatial data frame using OGR
  write.df <- t.df[,geomcolnum,drop=FALSE]
  names(write.df) <- "WKT"
  filename <- paste("vector_", as.character(format(Sys.time(), "%H_%M_%S")), sep="")
  filename.csv <- paste(filename, ".csv", sep="")
  write.csv(write.df, paste(gsub("[\\]", "/", tempdir()), "/", filename.csv, sep=""), row.names=TRUE)
  down.spdf <- readOGR(dsn=paste(gsub("[\\]", "/", tempdir()), "/", filename.csv, sep=""), layer=filename, verbose=FALSE)
  rv <- file.remove(paste(gsub("[\\]", "/", tempdir()), "/", filename.csv, sep=""))
  data.df <- data.frame(t.df[,-geomcolnum])
  names(data.df) <- names(t.df)[-geomcolnum]  
  
  ## set the projection 
  # obtain the projection from the table
  query_srid <- paste(  "SELECT ST_SRID(",geomcol,") FROM ", schemaname, ".", tablename, " LIMIT 1;", sep="")
  SRID <- dbGetQuery(con, statement=query_srid)
  # set the new projection 
  EPSG <- make_EPSG()
  proje <- EPSG[grep(SRID[1,1], EPSG$code),]

  # For Spatial Points Data Frame  
  if (grepl("POINT", t.df[1,geomcolnum])) {
    spatial.df <-  SpatialPointsDataFrame(down.spdf@coords, data.df, match.ID=FALSE)
    if(!is.na(proje$prj4)){ 
    proj4string(spatial.df) <-  CRS(proje$prj4)# set the projection
    }
  }
  # For Spatial Polygons/Lines Data Frame    
  if (grepl("POLYGON", t.df[1,geomcolnum]) | grepl("LINE", t.df[1,geomcolnum])) {
    spatial.df <- down.spdf
    spatial.df@data <- data.df
    spatial.df <- spChFIDs(spatial.df, paste(t.df[,idcolnum]))
    if(!is.na(proje$prj4)){ 
      proj4string(spatial.df) <-  CRS(proje$prj4)# set the projection
    }    
  }
  return(spatial.df)
}