
#' Index of habitat niche overalp
#' 
#' Derive an index (0: complete separation, 1: complete overlap) from a kselect object to estimate a degree of habitat niche overlap between species/indviduals   
#' 
#' @author Cyril Milleret 
#' @param X a kselect (adehabitatHS) object
#' 
#' @param kernel a character string giving the smoothing kernel to be used. Default is "gaussian". See \code{density()}
#' @param bw the smoothing bandwidth to be used to compute the kernel density. Default is "nrd0". See \code{density()}
#' @return A list of paired matrices with an overlap index between each individuals calculated on each axis and a Weighted (based on eigen values of the kselect) overlap index calculated on all axes
#' @usage niche_overlap_index(X, kernel = "gaussian", bw = "nrd0") 
#' @details Uses scores obtained for each individual on each dimension of the ecological space of the Kselect and compute a non-parametric kernel estimation. 
#' Area of overlap between distribution of the two indviduals is calculated. Eigen values of the kslect are then used to calculated an overlap weihgted index.
#'  \deqn{O_{ij} = \int_{b}^{a} \left [ f(x) - g(x) \right ]dx}
#'  \deqn{\bar{O_{ij}} =  \frac{\sum_{e}^{n} O_{ij_{e}} \lambda _{e}}{\sum_{e}^{n} \lambda _{e}}}
#' 
#' @export 
#' @examples 
#' library(adehabitatHS)
#' data(puechabonsp)
#'
#' locs <- puechabonsp$relocs
#' map <- puechabonsp$map
#' 
#' ## compute the home range of animals (e.g. using the minimum convex polygon)
#'  
#' pc <- mcp(locs[,"Name"])
#' hr <- hr.rast(pc, map)
#' 
#' ## Compute the number of relocation in each pixel of the map
#' cp <- count.points(locs[,"Name"], map)
#' 
#' ## prepares the data for the kselect analysis
#' x <- prepksel(map, hr, cp)
#' tab <- x$t
#' 
#' ## We call a new graphic window
#' ## A K-select analysis
#' acp <- dudi.pca(tab, scannf = FALSE, nf = 4)
#' kn <- kselect(acp, x$factor, x$weight,
#'               scannf = FALSE, nf = 4)
#' 
#' # compute habitat niche overlap 
#' niche_overlap_index(kn, kernel = "gaussian", bw = "nrd0")




niche_overlap_index <- function(X, kernel = "gaussian", bw = "nrd0") {
  require(adehabitatHS)
  require(sfsmisc)
  
  if(is.factor(X$initfac) == FALSE)
    stop("X$initfac should be a factor")
  
  
  # get list of individuals 
  factr <- unique(X$initfac)
  
  # create empty list to store the data
  m <- list()
  eig_perc <- X$eig[1 : ncol(X$ls)] / sum(X$eig[1 : ncol(X$ls)])
  
  ### compile all the data needed in a single dataf
  knr <- data.frame(X$ls)# axis
  knr$initfac <- X$initfac  # factors (animals)
  knr$initwei <- X$initwei  # utilisation weight
  
  knr <- knr[knr$initwei > 0, ]
  
  
  
  ## loop for each axis of the
  for (j in 1 : ncol(X$ls)) {
    ## create the interval vector from min and maximum data observed
    #adding a buffer so that tails aren't cut off
    
    lower <- min(knr[,j]) - 0.1 
    upper <- max(knr[,j]) + 0.1
    
    gc()
    store <- list()
    for ( i in 1 : length(factr)) { # for each ID
      
      tmp <- knr[knr$initfac==factr[i],] #subset for each IDS
      
      # compute density
      dens <- density(tmp[,j], from=lower,to= upper,
                      weights = tmp$initwei/sum(tmp$initwe), 
                      kernel = kernel,
                      bw = bw)
      store[[i]] <- dens
      gc()
      
    }
    
    # convert names as numeric for the following loop
    fact_num  <- as.numeric(factr) 
    # get all combinations of id possible
    exp_gri <- expand.grid(ID = fact_num, ID1 = fact_num)
    gc()
    
    # loop on it to calculate pianka index for each ID.
    for ( z in 1:nrow(exp_gri)){
      da <- store[[ (exp_gri[z,1]) ]]
      db <- store[[ (exp_gri[z,2]) ]]
      
      d <- data.frame(x=da$x, a=da$y, b=db$y)
      
      # calculate intersection densities
      d$w <- pmin(d$a, d$b)
      
      # integrate areas under curves
      total <- integrate.xy(d$x, d$a) + integrate.xy(d$x, d$b)
      intersection <- integrate.xy(d$x, d$w)
      
      # compute overlap coefficient
      exp_gri$overlap[z] <- 2 * intersection / total
      
    }
    ## store the results in a paired matrix for each  
    m[[j]] <- matrix(exp_gri$overlap, ncol=length(fact_num), byrow =T, dimnames = list(factr, factr) )
    
  }
  
  # now replace the name in the list 
  axis_nb <- c(1: ncol(kn$ls))
  names(m) <- sapply(m, function(x) paste("Overlap index axis",axis_nb,sep=" "))[,1]
  
  gc()
  
  m1<-list()
  ## calculate the overall weighted pianka index of all axis 
  for ( a in 1:ncol(kn$ls)) {
    m1[[a]]<- m[[a]] * eig_perc[a]
    
  }
  
  ##store the results in object m 
  m$"weighted overlap index" <-  apply(simplify2array(m1), c(1,2), sum)
  m <- lapply(m , function(x) round(x, digit=5))# round it 
  return(m)
}

