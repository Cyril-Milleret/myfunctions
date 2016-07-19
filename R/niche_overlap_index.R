
#' Index of habitat niche overalp
#' 
#' Derive an index (0: complete separation, 1: complete overlap) from a kselect object to estimate a degree of habitat niche overlap between species/indviduals   
#' 
#' @author Cyril Milleret 
#' @param X a kselect (adehabitatHS) object
#' 
#' @param kernel a character string giving the smoothing kernel to be used. Default is "gaussian". See \code{density()}
#' @param bw the smoothing bandwidth to be used to compute the kernel density. Default is "nrd0". See \code{density()}
#' @param permutations logical, if permutations should be computed or not
#' @param nb.permutations number of permutations to compute
#' @param hypothesis the H0 to be tested: c("segregation","overlap")
#' 
#' @return A list of paired matrices with an overlap index between each individuals calculated on each axis and a Weighted (based on eigen values of the kselect) overlap index calculated on all axes
#' @usage niche_overlap_index(X, kernel = "gaussian", bw = "nrd0") 
#' @details Uses scores obtained for each individual on each dimension of the ecological space of the Kselect and compute a non-parametric kernel estimation. 
#' Area of overlap between distribution of the two indviduals is calculated. Eigen values of the kslect are then used to calculated an overlap weihgted index.
#'  \deqn{O_{ij} = \int_{b}^{a} \left [ f(x) - g(x) \right ]dx}
#'  \deqn{\bar{O_{ij}} =  \frac{\sum_{e}^{n} O_{ij_{e}} \lambda _{e}}{\sum_{e}^{n} \lambda _{e}}}
#' i and j are the two individuals to be compared, f(x) and f(x) the two probability functions of indviduals i and j and e each of the axes. 
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
#' niche_overlap_index(kn, kernel = "gaussian", bw = "nrd0",permutations=TRUE,nb.permutations=100,hypothesis="segregation")


# setwd("C:/Users/cyrilm/Desktop")
# load("kn.Rdata")
# initfac <-sapply(strsplit(as.character(kn$initfac), "_"), function(x) x[4])
# initfac [initfac %in% c("2011","2012","2013","2014","2015")] <- "Wolf"
# 
# 
# initfac <- as.factor(initfac)




niche_overlap_index<- 
  function(X,initfac=initfac ,kernel = "gaussian", bw = "nrd0", permutations=TRUE, nb.permutations=100, hypothesis="segregation") {
    require(adehabitatHS)
    require(sfsmisc)
    X$initfac <- initfac
    if(is.factor(X$initfac) == FALSE)
      stop("X$initfac should be a factor")
    
    if(hypothesis=="segregation"){
      hypothesis <-"less"
    }
    
    if(hypothesis=="overlap"){
      hypothesis <-"greater"
    }
    
    
    # get list of individuals 
    factr <- sort(unique(X$initfac))
    
    # create empty list to store the data
    m <- list()
    m2 <- list()
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
      
      store <- list()
      for ( i in 1 : length(factr)) { # for each ID
        
        tmp <- knr[knr$initfac==factr[i],] #subset for each IDS
        
        # compute density
        dens <- density(tmp[,j], from=lower,to= upper,
                        weights = tmp$initwei/sum(tmp$initwe), 
                        kernel = kernel,
                        bw = bw)
        store[[i]] <- dens
      }
      
      # convert names as numeric for the following loop
      fact_num  <- as.numeric(factr) 
      # get all combinations of id possible
      exp_gri <- expand.grid(ID = fact_num, ID1 = fact_num)
      exp_gri$ID_2 <- c(1:length(exp_gri$ID1))
      
      
      mw <- matrix(exp_gri$ID_2, ncol = length(fact_num), 
                   byrow = T, dimnames = list(factr, factr))
      
      mw1<- mw [upper.tri(mw)]
      
      
      exp_gri$overlap <- 0
      gc()
      
      # loop on it to calculate pianka index for each ID.
      for ( z in 1:length(mw1)){
        da <- store[[ (exp_gri[mw1[z],1]) ]]
        db <- store[[ (exp_gri[mw1[z],2]) ]]
        
        d <- data.frame(x=da$x, a=da$y, b=db$y)
        
        # calculate intersection densities
        d$w <- pmin(d$a, d$b)
        
        # integrate areas under curves
        total <- integrate.xy(d$x, d$a) + integrate.xy(d$x, d$b)
        intersection <- integrate.xy(d$x, d$w)
        
        # compute overlap coefficient
        exp_gri$overlap[mw1[z]] <- 2 * intersection / total
        
        ###if permutations 
        if(permutations==TRUE){
          knr1<-knr 
          
          pseudo_over<-0
          for (b in 1:nb.permutations){
            knr1$initfac<- sapply(knr1$initfac, function(x) factr[sample(1:length(factr),1)] )
            
            val1<- knr1[knr1$initfac==factr[exp_gri[z,1]],]
            val2<- knr1[knr1$initfac==factr[exp_gri[z,2]],]
            
            dens1 <- density(val1$RS1, from=lower,to= upper, weights = val1$initwei/sum(val1$initwei), kernel ="gaussian")
            dens2 <- density(val2$RS1, from=lower,to= upper, weights = val2$initwei/sum(val2$initwei), kernel ="gaussian")
            
            
            pseudo_over[b] <- overlap_curves(dens1, dens2)
          }
          
          ran.test <- as.randtest(sim= pseudo_over, obs= exp_gri$overlap[z],alter="less" )
          
          #if you want to see if it is random 
          exp_gri$pvalue[z] <- ran.test$pvalue
          
        }
      }
      ## store the results in a paired matrix for each  
      m[[j]] <- matrix(exp_gri$overlap, ncol=length(fact_num), byrow =T, dimnames = list(factr, factr) )
      if(permutations==TRUE){
        m2[[j]] <- matrix(exp_gri$pvalue, ncol=length(fact_num), byrow =T, dimnames = list(factr, factr) )
      }
      
    }
    
    # now replace the name in the list 
    axis_nb <- c(1: ncol(X$ls))
    names(m) <- sapply(m, function(x) paste("Overlap index axis",axis_nb,sep=" "))[,1]
    if(permutations==TRUE){
      names(m2) <- sapply(m2, function(x) paste("Pvalues index axis",axis_nb,sep=" "))[,1]
    }
    gc()
    
    m1<-list()
    ## calculate the overall weighted pianka index of all axis 
    for ( a in 1:ncol(X$ls)) {
      m1[[a]]<- m[[a]] * eig_perc[a]
      
    }
    
    ##store the results in object m 
    m$"weighted overlap index" <-  apply(simplify2array(m1), c(1,2), sum)
    m <- lapply(m , function(x) round(x, digit=3))# round it 
    if(permutations==TRUE){
      m$"p-values" <-    lapply(m2 , function(x) round(x, digit=3))
    }
    return(m)
  }

