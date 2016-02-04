\name{pianka}
\alias{pianka}
\title{Calculate pianka's Index}
\usage{
pianka(species1,species2)
}
\description{
Calculate piankas Index from two vectors. In each vectors is the percentage of food items consummed by the 2 species.

}


\arguments{
\item{species1}{Vector with the proportion of food items for species 1}

\item{species2}{Vector with the proportion of food items for species 2 }


}

\examples{
names    <- c("rodents","insectivores","hare",
"carnivores","carrion","insects","plants",
"birds","ungulates")
wolf    <- c(0.6,0,0,8.5,0,0.01,0.042,0.1,90.7)
red_fox <- c(29,9.8,15,2.5,29.4,1.3,9.5,3.5,0)
pianka(wolf,red_fox)
}
