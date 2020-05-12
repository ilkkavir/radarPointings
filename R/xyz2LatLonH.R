## file:radarSites.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.

##
## Conversion from cartesian coordinates to elliptic latitude, longitude, height
##
## Arguments:
##  xyz a vector of  cartesian coordinates in m
##  a   equatorial radius in m ( optional )
##  b   polar radius in m ( optional )
##
## Returns:
##  c( lat , lon , h )  a vector of latitute, longitude, and height. Angles in degrees, height in metres.
##
## The default values for a and b are for the WGS-84 ellipsoid used in the GPS system.
## The GRS-80 ellipsoid used in the ITRS (International Terrestrial Reference System) is practically identical,
## with WGS-84, there is only 1 mm difference in polar radius (b=6356752.3142 (WGS-84) vs. b=6356752.3141 (GRS-80))
##
## This function was copied from J. Vierinen
##

xyz2LatLonH <- function( xyz ,  a=6378137 , b=6356752.3142 )
  {

    x <- xyz[1]
    y <- xyz[2]
    z <- xyz[3]
    
    aecc  <- acos( b / a )
    phit  <- atan( z / sqrt( x**2 + y**2 ) )
    betac <- atan( ( 1 / cos( aecc ) ) * tan( phit ) )
    phip  <- phit

    for( i in seq(4) ){
      phic  <- atan( ( z + b * ( sin( betac )**3 ) * ( tan( aecc )**2 ) ) / ( sqrt( x**2 + y**2 ) - a * ( cos( betac )**3 ) * ( sin( aecc )**2 ) ) )
      betac <- atan( cos( aecc ) * tan( phic ) )
      phip  <- phic
      betap <- betac 
    }

    N <- a / sqrt( 1 - ( sin( phic ) * sin( aecc ) )**2 )
    h <- ( 1 / cos( phic ) ) * sqrt( x**2 + y**2 ) - N


    lat <- 180 * phic / pi
    lon <- 180 * atan( y / x ) / pi

    names( lat ) <- NULL
    names( lon ) <- NULL
    names( h ) <- NULL
    
    return( c( lat=lat , lon=lon , h=h ) )


  }

