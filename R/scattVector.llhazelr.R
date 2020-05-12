## file:scattVector.llhazelr.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.

##
## Scattering wave vector and the angle between
## incident and scattered waves.
##
## Arguments:
##   llhT      c( latitude , longitude , height ) of the transmitter site
##             latitude in degrees, 0...90 in northern hemisphere, 0...-90 in southern hemisphere
##             longitude in degrees east 0...360
##             ellipsoid height in metres
##   azelT     c( azimuth , elevation ) of the transmitter beam, in degrees, azimuth from north through east 0 ... 360
##   llhR      c( latitude , longitude , height )  of the receiving site
##    r        target range in metres. Range is measured as one half of signal path length from the transmitter, via the target, to the receiver.
##             (range reduces to distance for monostatic systems)
##    freq.Hz  carrier frequency
##
## Returns:
##  A list with elements "k" and "phi". "k" is the scattering
##  wave vector in cartesian coordinates. "phi" is the angle
##  between incident and scattered waves in degrees.
##
## All coordinates with respect to the WGS-84 ellipsoid
##

scattVector.llhazelr <- function( llhT , azelT , llhR , r , freq.Hz=224e6 )
  {

    # positions of transmitter, receiver, and target in cartesian coordinates
    xyzT <- LatLonH2xyz( llhT )
    xyzR <- LatLonH2xyz( llhR )
    xyzS <- LatLonH2xyz( range2llh( r=r , llhT=llhT , azelT=azelT , llhR = llhR ) )

    return( scattVector.xyz( xyzT=xyzT , xyzR=xyzR , xyzS=xyzS , freq.Hz=freq.Hz ) )

  }
