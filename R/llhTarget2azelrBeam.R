## file:llhTargetazelrBeam.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.

##
## Conversion from latitude, longitude, height to
## azimuth, elevation, range.
##
## Arguments:
##  llhTarget  a vector of latitude, longitude, and
##             height of the radar target. Latitude in
##             degrees -90 .. 90 (positive at northern hemisphere)
##             Longitude in degrees 0...360 (positive towards east)
##             (ellipsoid) height in metres
## llhsite     a vector of latitude, longitude, and height of the radar site
##
## Returns:
##  c( az , el , r ) a vector of azimuth, elevation, and range
##                   of the target, when looked from the radar site
##                   azimuth in degrees 0 ... 360, from north through east
##                   elevation in degrees 0 .. 90
##                   range is distance  of the target from the site, in metres
##
##  All coordinates are with respect to the WGS-84 ellipsoid.
##

llhTarget2azelrBeam <- function( llhTarget , llhSite )
  {

    # target location in cartesian coordinates
    xyzT <- LatLonH2xyz( llhTarget )
    
    # site location in cartesian coordinate
    xyzS <- LatLonH2xyz( llhSite )

    # local east, north, and up in EFEC coordinates
    xyzENU <- ENUlocal( llhSite[1:2] )

    # local upwards direction at the site
    xyzU <- xyzENU[["up"]]

    # local eastward direction at the site
    xyzE <- xyzENU[["east"]]

    # local norhward direction at the site
    xyzN <- xyzENU[["north"]]

    # beam pointing in cartesian coordinates
    xyzBeam <- xyzT - xyzS
    
    # horizontal eastward component of the pointing direction
    beamE <- sum( xyzE * xyzBeam )

    # horizontal northward component of the pointing direction
    beamN <- sum( xyzN * xyzBeam )

    # beam azimuth
    beamAz <- atan2( beamE , beamN )
    if( beamAz < 0 ) beamAz <- beamAz + 2*pi
    beamAz <- beamAz * 180 / pi

    # beam elevation
    # the argument is forced in between -1 and 1, because with zenith-pointing beams numerical inaccuracies could cause problems
    cosarg <-  ( sum( xyzU * xyzBeam ) ) / ( sqrt( sum( xyzU**2 ) ) * sqrt( sum( xyzBeam**2 ) ) )
    cosarg <- min( cosarg ,  1 )
    cosarg <- max( cosarg , -1 )
    beamEl <- ( pi/2 - acos( cosarg ) ) * 180 / pi

    # with zenith pointing the azimuth will get arbitrary values, set it to zero
    if( beamEl==90 ) beamAz <- 0

    # range
    beamR <- sqrt( sum( xyzBeam**2 ) )

    return( c( az=beamAz , el=beamEl , r=beamR ) )


  }
