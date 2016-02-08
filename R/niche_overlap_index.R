
#' Index of habitat niche overalp
#' 
#' Derive a Pianka index from a kselect object to estimate a degree of habitat niche overlap between species/indviduals   
#' 
#' @author Cyril Milleret 
#' @param X a kselect (adehabitatHS) object
#' @param bin length of the intervals to calculate proportion of used scores
#' @return A list of paired matrix with a Pianka index calculated on each axis and a Weighted (based on eigen values) Pianka index calculated on all axes
#' @usage niche_overlap_index(X, bin=0.10) 
#'
#' @export 
#' @examples 
#' library(adehabitatHS)
#' data(puechabonsp)
#'
#' locs <- puechabonsp$relocs
#' map <- puechabonsp$map
#' 
#' ## compute the home range of animals (e.g. using the minimum convex
#' ## polygon)
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
#' niche_overlap_index(kn,bin=0.1)



niche_overlap_index <- function(X, bin = 0.10) {
  require(adehabitatHS)
  X$initfac <- as.factor(X$initfac)
  ## bin the data to count how many points are within each interval.
  ## create the interval vector from min and maximum data observed
  ## add 0.1 to make sure all data fit whitin intervals
  interv  <- seq( min(X$ls) - 0.1, max(X$ls) + 0.1, by = bin)
  #here create another interval to have the complete distribution of intervals
  interv1 <- seq (min(X$ls) - 0.1, max(X$ls) + 0.1, by = 0.02)
  eig_perc <- X$eig / sum(X$eig)## calculate marginality explained (% ) on each axes
  
  # create empty list to store the data
  m <- list()
  
  ## loop for each axis of the
  for (j in 1 : ncol(X$l1)) {
    # store data for each IDS
    store <- list()
    
    # get list of all possible  interval 
    possible_intervals <- data.frame(table(cut(interv1, b = interv)))
    gc()
    #replace name of the column
    colnames(possible_intervals)[1] <- c("interval")
    
    # get list of individuals 
    factr <- unique(X$initfac)
    
    ### compile all the data needed in a single dataf
    knr <- data.frame(X$ls)# axis
    knr$initfac <- X$initfac  # factors (animals)
    knr$initwei <- X$initwei  # utilisation weight
    gc()
    for ( i in 1 : length(factr)) { # for each ID
      
      tmp <- knr[knr$initfac==factr[i],] #subset for each IDS
      gc()
      tmp$interval <- cut(tmp[,j], b = interv) # split in interval
      merge_inter <- merge(possible_intervals, tmp, by="interval")## merge it with intervals 
      
      # count the number of pixels that have been used more than once.  
      # use tapply to know in which interval there are
      tmp1 <- data.frame(tapply(merge_inter[merge_inter$initwei>1, ]$initwei,
                                merge_inter[merge_inter$initwei>1, ]$interval, sum))
      # change name for the merge
      tmp1$Var1 <- row.names(tmp1)
      colnames(tmp1)[1] <- "sum"
      
      #  count for each intervals freq of interval utilisation 
      intervals <- data.frame(table(cut(tmp[,j], b = interv)))
      gc()
      ### merge it with all informatiosn 
      tmp_merge <- merge(intervals, tmp1,by="Var1")
      ## replace NA by zero
      tmp_merge$sum[ is.na(tmp_merge$sum) ] <- 0
      ## calculate the real frequency fo use
      tmp_merge$tot <- tmp_merge$sum + tmp_merge$Freq
      ## change name of the columns so i can merge
      colnames(tmp_merge)[1] <- "interval"
      ##merge it 
      merge_inter <- merge(possible_intervals, tmp_merge, by="interval")
      # calculate the prop of use for each intervals 
      merge_inter$prop.use <- merge_inter$sum / sum(merge_inter$sum)
      # store it in the list 
      store[[(factr[i])]] <- merge_inter$prop.use
    }
    
    # convert names as numeric for the following loop
    fact_num  <- as.numeric(factr) 
    # get all combinations of id possible
    hey <- expand.grid(ID = fact_num, ID1 = fact_num)
    gc()
    # loop on it to calculate pianka index for each ID.
    for ( z in 1:nrow(hey)){
      hey$pianka[z] <- pianka(store[[ (hey[z,1]) ]],store[[(hey[z,2]) ]])
    }
    ## store the results in a paired matrix for each  
    m[[j]] <- matrix(hey$pianka, ncol=4, byrow =T, dimnames = list(factr, factr) )
    
  }
  
  # now replace the name in the list 
  axis_nb <- c(1: ncol(kn$l1))
  names(m) <- sapply(m, function(x) paste("Pianka overlap axis",axis_nb,sep=" "))[,1]
  
  gc()
  
  m1<-list()
  ## calculate the overall weighted pianka index of all axis 
  for ( a in 1:ncol(kn$l1)) {
    m1[[a]]<- m[[a]] * eig_perc[a]
    
  }
  
  ##store the results in object m 
  m$"weighted Pianka index" <-  apply(simplify2array(m1), c(1,2), sum)
  m <- lapply(m , function(x) round(x, digit=5))# round it 
  return(m)
}