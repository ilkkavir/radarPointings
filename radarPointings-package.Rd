\name{radarPointings-package}
\alias{radarPointings}
\title{Radar pointings}
\description{Radar pointing direction calculations}
\details{Coordinate transform tools for calculating radar pointing
  directions and geographic coordinates of radar targets.

  \tabular{ll}{
    Package: \tab radarPointings \cr
    Version: \tab 0.1-3 \cr
    Date: \tab 2013-06-04\cr
    License: FreeBSD\tab \cr
    Depends: R ()\tab \cr
    Built: \tab \cr
  }
  Index:

  \code{\link{radarSites}} Coordinates a incoherent scatter radar facilities
  
  \code{\link{azelrBeam2llhTarget}} Conversion from azimuth, elevation,
  range to latitude, longitude, height.
  
  \code{\link{azelrBeam2xyzTarget}} Conversion from azimuth, elevation,
  height to geocentric cartesian coordinates. 
  
  \code{\link{ellipsoidNormal}} Normal vector of the WGS-84 ellipsoid.
  
  \code{\link{ENZlocal}} Local east, north, and south directions. 
  
  \code{\link{LatLonH2xyz}} Conversion from latitude, longitude, height
  to geocentric cartesian coordinates.
  
  \code{\link{llhTarget2azelrBeam}} Conversion to latitude, longitude,
  height to azimuth, elevation, range.
  
  \code{\link{range2llh}} Conversion from (azimuth, elevation) range to
  latitude, longitude, height.
  
  \code{\link{scattVector.llhazelr}} Scattering wave vector.
  
  \code{\link{xyz2LatLonH}} Conversion from geocentric cartesian
  coordinates to latitude, longitude, height.
  
  
}

\author{Ilkka Virtanen (University of Oulu, Finland) \cr
  \email{ilkka.i.virtanen@oulu.fi}}

\keyword{ package }