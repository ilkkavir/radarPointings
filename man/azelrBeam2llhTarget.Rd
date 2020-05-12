\name{azelrBeam2llhTarget}
\title{Conversion from az, el, r to lat, lon, h}
\alias{azelrBeam2llhTarget}
\description{Coordinate conversion from beam azimuth, elevation, range
  to target latitude, longitude, height.}
\usage{azelrBeam2llhTarget( azelr , llhSite )}
\arguments{
  \item{azelr}{ c( azimuth [deg] , elevation [deg] , range [m] ) }
  \item{llhSite}{ c(latitude [deg] , longitude [deg] , ellipsoid height
    [m] ) of the radar.}
}
\value{ c( latitude [deg] , longitude [deg] , ellipsoid height [m] ) }

\author{Ilkka Virtanen (University of Oulu, Finland) \cr
  \email{ilkka.i.virtanen@oulu.fi}}