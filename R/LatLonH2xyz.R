## file:LatLonH2xyz.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.

##
## Conversion from elliptic latitude, longitude, height
## to cartesian EFEC coordinates
##
##
## Argumnets:
##  llh a vector of latitude, longitude, and height.
##      Latitude and longitude in degrees, height
##      in metres. latitude is negative (0...-90)
##      in southern hemisphere and positive (0...90)
##      in northern hemisphere. longitude is
##      positive towards east (0...360)
##  a   equatorial radius in m ( optional )
##  b   polar radius in m ( optional )
##
## Returns:
## c( x, y, z ) cartesian coordinates
##
## The default values for a and b are for the
## WGS-84 ellipsoid used in the GPS system.
## The GRS-80 ellipsoid used in the ITRS
## (International Terrestrial Reference System)
## is practically identical with WGS-84, there is
## only 1 mm difference in polar radius
## (b=6356752.3142 (WGS-84) vs. b=6356752.3141 (GRS-80))
##
##

LatLonH2xyz <- function( llh , a=6378137 , b=6356752.3142 )
    {
          
        # Conversion to radians
        lat <- pi * llh[1] / 180
        lon <- pi * llh[2] / 180
        
        h   <- llh[3]
        
        aecc <- acos( b / a )
      
        N <- a / sqrt( 1 - ( sin( lat ) * sin( aecc ) )**2 )
        
        x <- ( N + h ) * cos( lat ) * cos( lon )
        y <- ( N + h ) * cos( lat ) * sin( lon )
        z <- ( N * ( cos( aecc )**2 ) + h ) * sin( lat )
        
        return( c( x=x , y=y , z=z ) )
        
    }
