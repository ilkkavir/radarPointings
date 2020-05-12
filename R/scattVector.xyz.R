## file:scattVector.xyz.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.

##
## Scattering wave vector and the angle between
## incident and scattered waves.
##
## Arguments:
##  xyzT   c(x,y,z) position of the transmitter in cartesian
##         coordinates. Use the function LatLonH2xyz to transform from 
##         latitudes and longitudes
##  xyzR   c(x,y,z) position of the receiver antenna
##  xyzS   c(x,y,z) position of the scatterer
##  freq.Hz Carrier frequency
## 
## Returns:
##  A list with elements "k.EFEC", "k.ENU" and "phi". "k.EFEC"
##  is the scattering wave vector in geocentric  cartesian
##  (EFEC) coordinates and "k.ENU" the same vector in local
##  ENU (east, north, up ) system. "phi" is the angle
##  between incident and scattered waves in degrees.
##
## All coordinates with respect to the WGS-84 ellipsoid
##

scattVector.xyz <- function( xyzT , xyzR , xyzS , freq.Hz=224e6 ){
  
    # incident wave vector
    ki   <- xyzS - xyzT
    ki   <- ki / sqrt(sum(ki**2))

    # scattered wave vector
    ks   <- xyzR - xyzS
    ks   <- ks / sqrt(sum(ks**2))

    rlist <- list()
    
    # wave vector of the fluctuations from which we get  scattering
    rlist[["k.EFEC"]]    <- ( ks - ki )  * freq.Hz / 299792458. * 2. * pi

    # conversion to ENU
    rlist[["k.ENU"]] <- EFEC2ENU.xyz( xyzVec=rlist[["k.EFEC"]] , xyzPos=xyzS )

    # angle between incident and scattered waves
    dotprod <- max( -1 , min( 1 , sum( ki * ks ) ) )
    rlist[["phi"]]  <- acos( dotprod ) * 180  / pi

    return(rlist)

}

