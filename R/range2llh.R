## file:range2llh.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.

##
## Conversion of radar range to latitude, longitude, height
##
## Arguments:
##  r     range in metres. Range is here defined as
##        half of signal path length from transmitter
##        via the target to the receiver, so that
##        range reduces to mutual distance in
##        monstatic systems.
##  llhT  a  vector of latitude, longitude, and height
##        of the transmitter site. Latitude in degrees,
##        0...90 in northern hemisphere and 0...-90 in
##        southern hemisphere. Longitude in degrees east,
##        0...360, (ellipsoid) height in metres
##  azelT azimuth and elevation of the transmitter
##        pointing direction. azimuth degrees from
##        north through east elevation in degrees
##  llhR  a vector of latitude, longitude, and height
##        of the receiver site
##
## Returns:
##   llh  ellipsoid latitude, longitude, and height
##        latitude in degrees, 0...90 in northern
##        hemisphere, 0...-90 in southern hemisphere
##        longitude in degrees east 0...360
##        height in metres
##

range2llh <- function( r , llhT , azelT , llhR=llhT )
  {
    
    # cartesian coordinates of the transmitter site
    xyzT <- LatLonH2xyz( llhT )

    # cartesian coordinates of the receiver site
    xyzR <- LatLonH2xyz( llhR )

    # local east, north, up
    xyzENU <- ENUlocal( llhT )

    # azimuth and elevation in radians
    az <- azelT[1] * pi / 180
    el <- azelT[2] * pi / 180

    # Transmitter beam direction in cartesian system
    xyzBeam <- cos( az ) * cos( el ) * xyzENU[["north"]] + sin( az ) * cos( el ) * xyzENU[["east"]] + sin( el ) * xyzENU[["up"]]

    # a cartesian vector pointing from the transmitter to the receiver
    xyzTR <- xyzR - xyzT

    # throw an error if the distance between sites is longer than twice the range
    if( sqrt(sum(xyzTR**2)) > 2*r ){
        warning("Range is shorter than half of site separation, returning NA")
        return(c(lat=NA,lon=NA,h=NA))
    }

    # if the above vector is very short, this is obviously a monostatic system
    if( sqrt( sum( xyzTR**2 ) ) < 100 ) return( xyz2LatLonH( azelrBeam2xyzTarget( c( azelT[1:2] , r ) , llhT ) ) )

    # for bistatic systems, calculate the angle between beam pointing direction and the vector xyzTR
    cosalpha <- sum( xyzBeam * xyzTR ) / ( sqrt( sum( xyzBeam**2 ) ) * sqrt( sum( xyzTR**2 ) ) )

    # solve target distance from the transmitter (r is one half of the signal path length!)
    d <- ( ( 2 * r )**2 - sum( xyzTR**2 ) ) / 2 / ( 2 * r - sqrt( sum( xyzTR**2 ) ) * cosalpha )

    # now we can solve in the same way that was used for a monostatic system
     return( xyz2LatLonH( azelrBeam2xyzTarget( c( azelT[1:2] , d ) , llhT ) ) )

  }
