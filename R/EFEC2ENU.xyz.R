## file:EFEC2ENU.xyz.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.

##
## Coordinate transform from EFEC (earth-fixed earth-centred)
## to ENU (east, north, up) system.
##
## Arguments:
##  xyzVec The vector to be transformed in EFEC coordinates
##  xyzPos EFEC coordinates of the point at which the ENU
##         coordinates are calculated
##
## Returns:
##  enuVec The vector xyzVec in ENU coordinates
##

EFEC2ENU.xyz <- function( xyzVec , xyzPos )
    {
        # Position as latitude and longitude
        llhPos <- xyz2LatLonH( xyz=xyzPos )

        return( EFEC2ENU.latlon( xyzVec=xyzVec , latlon=llhPos[1:2] ) )
        
    }
