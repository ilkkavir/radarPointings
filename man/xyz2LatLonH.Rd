\name{xyz2LatLonH}
\title{Conversion from cartesian coordinates to lat, lon, h}
\alias{xyz2LatLonH}
\description{Coordinate transform from geocentric cartesian coordinates
  to latitude, longitude, height.}
\usage{xyz2LatLonH( xyz ,  a=6378137 , b=6356752.3142 )}
\arguments{
  \item{xyz}{ c(x,y,z), Geocentric cartesian coordinates [m]}
  \item{a}{Equatorial radius [m]}
  \item{b}{Polar radius [m]}
  Default a and b are for the WGS-84 ellipsoid.
}
\value{ c( latitude [deg] , longitude [deg] , height [m] )}

\author{Ilkka Virtanen (University of Oulu, Finland) \cr
  \email{ilkka.i.virtanen@oulu.fi}}