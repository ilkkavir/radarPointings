\name{range2llh}
\title{Conversion from range to lat, lon, h}
\alias{range2llh}
\description{Coordinate conversion from range (and site locations) to
  lat, lon, height.}
\usage{range2llh( r , llhT , azelT , llhR=llhT )}
\arguments{
  \item{r}{ Target range [m]}
  \item{llhT}{c(latitude [deg] , longitude [deg] , ellipsoid height [m]
    of the transmitter site. )}
  \item{azelT}{ c( azimuth [deg] , elevation [deg] ) of the transmitter beam.}
  \item{llhR}{ c(latitude [deg] , longitude [deg] , height [m] ) of the
    receiver site.}
}
\value{
  c( latitude [deg] , longitude [deg] , ellipsoid height [m]) of the
  target. 
}

\author{Ilkka Virtanen (University of Oulu, Finland) \cr
  \email{ilkka.i.virtanen@oulu.fi}}