## file:ENU2EFEC.latlon.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.

##
## Conversion from local ENU (east,north,up) system
## to earth-fixed earth-centred (EFEC) cartesian system
##
## Arguments:
##  enuVec c(east,north,up) coordinates
##  latlon c(lat,lon) of the point at which the conversion is calculated
##
## Returns:
##  c(x,y,z) The vector in EFEC system
##
##

ENU2EFEC.latlon <- function( enuVec , latlon )
    {

        # local ENU base vectors in EFEC
        enu <- ENUlocal( latlon )

        efecVec <- enuVec[1]*enu[["east"]] + enuVec[2]*enu[["north"]] + enuVec[3]*enu[["up"]]

        names(efecVec) <- c('x','y','z')

        return(efecVec)
    }
