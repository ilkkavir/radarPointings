\name{azelrBeam2xyzTarget}
\title{ Conversion from az, el, r to geocentric cartesian system.}
\alias{azelrBeam2xyzTarget}
\description{ Coordinate conversion from beam azimuth, elevation, range
  to target coordinates in geocentric cartesian system. }
\usage{azelrBeam2xyzTarget( azelr , llhSite )}
\arguments{
  \item{azelr}{ c( azimuth [deg] , elevation [deg] , range [m] ) }
  \item{llhSite}{ c(latitude [deg] , longitude [deg] , ellipsoid height
    [m] ) of the radar.}
}
\value{ c(x,y,z), target coordinates in geocentric cartesian system. }

\author{Ilkka Virtanen (University of Oulu, Finland) \cr
  \email{ilkka.i.virtanen@oulu.fi}}