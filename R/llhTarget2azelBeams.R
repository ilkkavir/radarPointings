## file:llhTargetazelBeams.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.

##
## Conversion from latitude, longitude, height of the scattering volume
## to azimuth, elevation, of transmitter and receiver beams, given that
## the transmitter is at llhT, and the receiver at llhR.
##
## Arguments:
##  llhTarget  a vector of latitude, longitude, and
##             height of the radar target. Latitude in
##             degrees -90 .. 90 (positive at northern hemisphere)
##             Longitude in degrees 0...360 (positive towards east)
##             (ellipsoid) height in metres
## llhT        a vector of latitude, longitude, and height of the transmitter site
## llhR        a vector of latitude, longitude, and height of the transmitter site
##
## Returns:
##  azel       a list with elements azelT and azelR, each of which is a
##             three-vector c(az,el,r)
##                   azimuth in degrees 0 ... 360, from north through east
##                   elevation in degrees 0 .. 90
##                   range is distance  of the target from the site, in metres
##
##  All coordinates are with respect to the WGS-84 ellipsoid.
##

llhTarget2azelBeams <- function( llhTarget , llhT , llhR )
  {

      azelT <- llhTarget2azelrBeam( llhTarget=llhTarget , llhSite=llhT )
      azelR <- llhTarget2azelrBeam( llhTarget=llhTarget , llhSite=llhR )

    return( list( azelT=azelT[1:2] , azelR=azelR[1:2] ) )


  }
