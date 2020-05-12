\name{LatLonH2xyz}
\title{Conversion from lat, lon, h to geocentric cartesian }
\alias{LatLonH2xyz}
\description{Coordinate conversion from latitude, longitude, height to
  geocentric cartesian coordinate system. }
\usage{ LatLonH2xyz( llh , a=6378137 , b=6356752.3142 )}
\arguments{
  \item{llh}{ c( latitude [deg] , longitude [deg] , ellipsoid height [m]
    ) }
  \item{a}{Equatorial radius}
  \item{b}{Polar}
  Default a and b are for the WGS-84 ellipsoid.
}
\value{ c(x,y,z), geocentric cartesian coordinates. }

\author{Ilkka Virtanen (University of Oulu, Finland) \cr
  \email{ilkka.i.virtanen@oulu.fi}}