pianka <- function(species1, species2){
  sum((species1/100) * (species2/100))/
    sqrt((sum((species1 / 100)^2)*
            (sum((species2 / 100)^2))))
}
