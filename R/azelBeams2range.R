## file:azelBeams2range.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.

##
## Conversion from azimuth, elevation, of TX and RX beams
## to reception range
##
## Arguments:
##  azelT   a vector of azimuth and elevation of the transmitter site.
##          azimuth in degrees 0 ... 360 (north through east)
##          elevation in degrees 0...90
##  azelR   a vector of azimuth and elevation of the receiver site.
##          azimuth in degrees 0 ... 360 (north through east)
##          elevation in degrees 0...90
##  llhT    a vector of latitude, longitude, and height
##          of the transmitter site
##          latitude in degrees, positive (0...90)
##          in northern hemishpere, negative (0...-90)
##          in southern hemisphere
##          longitude in degrees east (0...360)
##          (ellipsoid) height in metres
##  llhR    a vector of latitude, longitude, and height
##          of the receiver site
##
## Returns:
##  range to the beam intersection in metres. Range defined
##  as one half of the signal path length. 
##
##  All coordinates are with respect to the WGS-84 ellipsoid.
##
azelBeams2range <- function( azelT , azelR , llhT , llhR ){

    # Return NA the radar is monostatic
    if(all(abs(llhT-llhR)<1e-3)) return(NA)

    # Transmitter site location in EFEC
    xyzT <- LatLonH2xyz( llhT )
    # Receiver site location in EFEC
    xyzR <- LatLonH2xyz( llhR )

    # Target position in EFEC
    xyzTarg <- intersectLocation( xyzT , xyzR , azelT , azelR )

    # One half of the signal path length
    return( ( sqrt( sum( (xyzTarg - xyzT )**2) ) + sqrt( sum( (xyzTarg - xyzR)**2 ) ) ) / 2 )

}
