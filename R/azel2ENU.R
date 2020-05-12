## file:azel2ENU.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.

##
## Conversion from azimuth and elevation to
## local cartesian ENU (east, north, up ) system
##
## Arguments:
##  azel c( azimuth , elevation ) in degrees
##
## Returns:
##  c(east,north,up) the pointing direction in ENU system


azel2ENU <- function( azel )
    {
        az <- azel[1]*pi/180
        el <- azel[2]*pi/180

        xy <- cos(el)

        return( c( east=xy*sin(az) , north=xy*cos(az) , up=sin(el) ) )
        
    }
