\name{llhTarget2azelrBeam}
\title{Conversion from lat, lon, h to az, el, r}
\alias{llhTarget2azelrBeam}
\description{Coordinate conversion from latitude, longitude, height to
  azimuth, elevation, range.}
\usage{llhTarget2azelrBeam( llhTarget , llhSite )}
\arguments{
  \item{llhTarget}{c(latitude [deg] , longitude [deg] , ellipsoid height
    [m]) of the target.}
  \item{llhSite}{ c( latitude [deg] , longitude [deg] , ellipsoid height
    [m]) of the radar site.}
}
\value{ c( azimuth [deg] , elevation [deg] , range [m])}

\author{Ilkka Virtanen (University of Oulu, Finland) \cr
  \email{ilkka.i.virtanen@oulu.fi}}