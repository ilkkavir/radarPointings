## file:height2range.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.

##
## Finds the range corresponding to beam intersection
## at height h
##
## Arguments:
##  llhT  latitude, longitude, height of the transmitter site
##  azelT azimuth, elevation of the transmitter beam
##  llhR  latitude, longitude, height of the receiver site
##  h     height
##
## Returns:
##  r     one half of the signal path length from the transmitter
##        via height h to the receiver, [m].
##
##  All coordinates are with respect to the WGS-84 ellipsoid.
##

height2range <- function( llhT , azelT , llhR=llhT , h )
    {

        if( all(llhT==llhR)){
            if(azelT[2]==90) return(h)
        }

        # transmitter location in EFEC
        xyzT <- LatLonH2xyz( llhT )

        # receiver location in EFEC
        xyzR <- LatLonH2xyz( llhR )

        # transmitter beam direction in EFEC
        xyzBeam <- ENU2EFEC.latlon( enuVec=azel2ENU( azelT ) , latlon=llhT[1:2] )

        # approximation assuming flat Earth
        rT <- h * sin( azelT[2]*pi/180 )
        xyzS <- xyzT + rT*xyzBeam
        r0 <- ( rT + sqrt( sum( ( xyzS - xyzR )**2 ) ) ) / 2

        # iterate to improve the accuracy
        sol <- nlm(
            f=function(p,llhT,azelT,llhR,h){return(abs(range2llh(r=p,llhT=llhT,azelT=azelT,llhR=llhR)['h']-h)**2)},
            p=r0,
            llhT=llhT,
            azelT=azelT,
            llhR=llhR,
            h=h,
            steptol=1e-3
            )
        

        if(sol$code < 3) return(sol$estimate)
        if( abs( sol$estimate - r0 ) < 1) return(sol$estimate)

        warning("Iteration diverged, using flat Earth approximation")
        return(r0)


    }
