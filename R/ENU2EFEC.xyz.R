## file:ENU2EFEC.xyz.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.

##
## Conversion from local ENU (east,north,up) system
## to earth-fixed earth-centred (EFEC) cartesian system
##
## Arguments:
##  enuVec c(east,north,up) coordinates
##  xyzPos EFEC coordinates of the point at which the conversion is calculated
##
## Returns:
##  c(x,y,z) The vector in EFEC system
##
##

ENU2EFEC.xyz <- function( enuVec , xyzPos )
    {
        # Position as latitude and longitude
        llhPos <- xyz2LatLonH( xyz=xyzPos )

        return( ENU2EFEC.latlon( enuVec , llhPos[1:2] ) )
    }
