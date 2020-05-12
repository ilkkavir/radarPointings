## file:EFEC2ENU.latlon.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.

##
## Coordinate transform from EFEC (earth-fixed earth-centred)
## to ENU (east, north, up) system.
##
## Arguments:
##  xyzVec The vector to be transformed in EFEC coordinates
##  latlon Latitude and longitude of the point at which the
##         ENU coordinates are calculated.
##
## Returns:
##  enuVec The vector xyzVec in ENU coordinates
##

EFEC2ENU.latlon <- function( xyzVec , latlon )
    {

        # local east, north, up
        xyzENU <- ENUlocal( latlon )

        # rotation matrix
        rotMat <- matrix( c(xyzENU[["east"]],xyzENU[["north"]],xyzENU[["up"]]),ncol=3,byrow=T)
        
        # rotate 
        vecRot <- c(matrix(xyzVec,nrow=1)%*%t(rotMat))

        return(vecRot)

    }
