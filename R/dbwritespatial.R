#' Write spatial vector data in postgres
#' 
#' @author Philipp Hunziker <https://philipphunziker.wordpress.com/2014/07/20/transferring-vector-data-between-postgis-and-r/>
#' @param con set the connection with the database:  con <- dbConnect(dbDriver("PostgreSQL"), dbname="postgis", port=5432, host="localhost", user="postgres", password="pw")
#' @param spatial.df  spatial dataframe to be sent in postgres
#' @param name  name of the new table to be created
#' @param SRID set the SRID number ; e.g 3021 is for RT90
#' @param replace if FALSE existing table is not replaced. True table is replaced 


#' @usage dbwritespatial(con, myspatialdata.spdf, schemaname="myschema", tablename="myspatialdata", replace=FALSE)
#' @export

dbwritespatial <- function(con, spatial.df, schemaname="public", tablename, SRID, replace=FALSE) {
  require(rgeos)
  require(RPostgreSQL)
  # con:          A PostgreSQL connection (from RPostgreSQL)
  # spatial.df:   A Spatial Data Frame object
  # schemaname:   Target schema name
  # tablename:    Target table name
  # replace:      Replace the target table if it already exists
  # Create well known text and add to spatial DF
  spatialwkt <- writeWKT(spatial.df, byid=TRUE)
  spatial.df$wkt <- spatialwkt
  
  # Add temporary unique ID to spatial DF
  spatial.df$spatial_id <- 1:nrow(spatial.df)
  
  # Set column names to lower case
  names(spatial.df) <- tolower(names(spatial.df))
  
  # Upload DF to DB
  data.df <- spatial.df@data
  rv <- dbWriteTable(con, c(schemaname, tablename), data.df, overwrite=replace, row.names=FALSE)  
  # Create geometry column and clean up table
  schema.table <- paste(schemaname, ".", tablename, sep="")
  query1 <- paste("ALTER TABLE ", schema.table, " ADD COLUMN the_geom GEOMETRY;", sep="")
  query2 <- paste("UPDATE ", schema.table, " SET the_geom = ST_SetSRID( ST_GEOMETRYFROMTEXT(t.wkt),", SRID,") FROM ", schema.table, " t  WHERE t.spatial_id = ", schema.table, ".spatial_id;", sep="")
  query3 <- paste("ALTER TABLE ", schema.table, " DROP COLUMN spatial_id;")
  query4 <- paste("ALTER TABLE ", schema.table, " DROP COLUMN wkt;")
  er <- dbSendQuery(con, statement=query1)
  er <- dbSendQuery(con, statement=query2)
  er <- dbSendQuery(con, statement=query3)
  er <- dbSendQuery(con, statement=query4)
  
  return(TRUE)
}