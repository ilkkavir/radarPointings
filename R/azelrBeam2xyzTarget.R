## file:azelrBeam2xyzTarget.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.

##
## Conversion from azimuth, elevation, range at given location to cartesian coordinates
##
## Arguments:
##  azelr   a vector of azimuth, elevation, and range.
##          azimuth in degrees 0 ... 360 (north through east)
##          elevation in degrees 0...90
##          range in metres
##  llhSite a vector of latitude, longitude, and height of the site at which azelr are measured
##          latitude in degrees, positive (0...90) in northern hemishpere, negative (0...-90) in southern hemisphere
##          longitude in degrees east (0...360)
##          height in metres
##
## Returns:
##  c( x , y , z ) cartesian coordinates of the target point
##
## All coordinates are with respect to the WGS-84 ellipsoid.
##

azelrBeam2xyzTarget <- function( azelr , llhSite )
  {

    # site location in cartesian coordinates
    xyzS <- LatLonH2xyz( llhSite )

    # local east, north, and up
    xyzENU <- ENUlocal( llhSite[1:2] )

    # azimuth and elevation in radians
    az <- azelr[1]*pi/180
    el <- azelr[2]*pi/180
    
    # beam direction in the cartesian system
    xyzBeam <- cos( az ) * cos( el ) * xyzENU[["north"]] + sin( az ) * cos( el ) * xyzENU[["east"]] + sin( el ) * xyzENU[["up"]]

    # target position in cartesian coordinates
    xyzTarget <- xyzS + xyzBeam * azelr[3]

   return( xyzTarget )


  }
