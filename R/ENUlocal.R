## file:ENUlocal.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.

##
## Local eastward, northward, and up directions (ENU base vectors)
## in EFEC (earth-centred earth-fixed, geocentric cartesian)
## coordinates.
##
## Arguments:
##   latlon  latitude and longitude in degrees (also longer vectors
##           are accepted, but only the first two entries are used)
##           latitude 0..90 at northern hemisphere, 0...-90 at
##           southern hemisphere longitude in degrees east 0...360
##
## Returns:
##  list( east=c(x,y,z) , north=c(x,y,z) , up=c(x,y,z) )
##          a list of unit vectors pointing towards
##          east, north, and zenith, in cartesian coordinates
##

ENUlocal <- function( latlon )
  {

    # local vertical direction
    xyzU <- ellipsoidNormal( latlon[1:2] )

    # local eastward direction
    xyzE <- c( -xyzU[2] , xyzU[1] , 0 )

    # local norhward direction at the site
    xyzN <- c(
              xyzU[2] * xyzE[3] - xyzU[3] * xyzE[2] ,
              xyzU[3] * xyzE[1] - xyzU[1] * xyzE[3] ,
              xyzU[1] * xyzE[2] - xyzU[2] * xyzE[1]
              )

    names( xyzU ) <- c('x','y','z')
    names( xyzE ) <- c('x','y','z')
    names( xyzN ) <- c('x','y','z')

    xyzU <- xyzU / sqrt(sum(xyzU**2))
    xyzE <- xyzE / sqrt(sum(xyzE**2))
    xyzN <- xyzN / sqrt(sum(xyzN**2))

    return( list( east=xyzE , north=xyzN , up=xyzU ) )

  }
