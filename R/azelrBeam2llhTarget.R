## file:azelrBeam2llhTarget.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.

##
## Conversion from azimuth, elevation, range to
## latitude, longitude, height
##
## Arguments:
##  azelr   a vector of azimuth, elevation, and range.
##          azimuth in degrees 0 ... 360 (north through east)
##          elevation in degrees 0...90
##          range in metres
##  llhSite a vector of latitude, longitude, and height
##          of the site at which azelr are measured.
##          latitude in degrees, positive (0...90)
##          in northern hemishpere, negative (0...-90)
##          in southern hemisphere
##          longitude in degrees east (0...360)
##          (ellipsoid) height in metres
##
## Returns:
##  c( lat , lon , h ) latitude, longitude, and height of the target point
##
##  All coordinates are with respect to the WGS-84 ellipsoid.
##

azelrBeam2llhTarget <- function( azelr , llhSite )
  {

    return( xyz2LatLonH( azelrBeam2xyzTarget( azelr , llhSite ) ) )

  }
