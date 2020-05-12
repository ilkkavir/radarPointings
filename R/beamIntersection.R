## file:beamIntersectin.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.

##
## Arguments:
##  llhT    c(latitude,longitude,height) of the transmitter in degrees / metres
##  llhR    c(latitude,longitude) of the receiver in degrees / metres
##  azelT   c(azimuth,elevation) of the transmitter in degrees
##  azelR   c(azimuth,elevation) of the receiver in degrees
##  fwhmT   full width at half maximum of transmitter beam when pointed to zenith
##  fwhmR   full width at half maximum of receiver beam when pointed to zenith
##  phArrT  Is the transmitter antenna a phased array (TRUE) or a dish (FALSE)
##  phArrR  Is the receiver antenna a phased array (TRUE) or a dish (FALSE)
##  freq.Hz transmitter carrier frequency in Hz
##  stdXY   logical, calculate beam intersection widths in directions perpendicular to k?
##  rMonost range at which to calculate the beam shape if the system is monostatic [m]
##
##

beamIntersection <- function( llhT , llhR , azelT , azelR , fwhmT , fwhmR , phArrT , phArrR , freq.Hz ,stdXY=FALSE , rMonost=100000 )
    {

        # Site locations in EFEC
        xyzT <- LatLonH2xyz( llhT )
        xyzR <- LatLonH2xyz( llhR )

        # Check if this is a monostatic TX pair
        if( sqrt( sum( (xyzT - xyzR)**2 ) ) < 1000 )
            {
                monostatic <- TRUE
                # A point at rMonost m range
                xyzTarg <- xyzT + rMonost*ENU2EFEC.xyz( azel2ENU( azelT ) , xyzT )
            }else{
                monostatic <- FALSE
                # Beam axis intersection point
                xyzTarg <- intersectLocation( xyzT , xyzR , azelT , azelR )
            }


        # Scattering wave vector at beam axis intersection
        k <- scattVector.xyz( xyzT , xyzR , xyzTarg , freq.Hz )

        # Radar pulse covariance matrices
        covT <- radarPulse.gaussian( fwhmT , 1e7 , xyzT , xyzTarg , phArrT )
        covR <- radarPulse.gaussian( fwhmR , 1e7 , xyzR , xyzTarg , phArrR )

        # Integrals over 2D marginal distributions in plane perpendicular to the beam
        # Instead of using pointing direction and beam widths, the whole beams are
        # rotated because it makes this calculation independent of the actual
        # beam shape model
        xyzTTarg <- xyzTarg - xyzT
        xyzRTarg <- xyzTarg - xyzR
        ovec3T <- xyzTTarg / sqrt(sum(xyzTTarg**2))
        ovec3R <- xyzRTarg / sqrt(sum(xyzRTarg**2))
        ovec2T <- normalUnitVector.cartesian( ovec3T , (ovec3T+rnorm(3)))
        ovec2R <- normalUnitVector.cartesian( ovec3R , (ovec3R+rnorm(3)))
        ovec1T <- normalUnitVector.cartesian( ovec2T , ovec3T )
        ovec1R <- normalUnitVector.cartesian( ovec2R , ovec3R )
        intT <- 2*pi*sqrt(det(rotateCovarMatrix( covT , ovec1T , ovec2T , ovec3T , inverse=TRUE )[1:2,1:2]))
        intR <- 2*pi*sqrt(det(rotateCovarMatrix( covR , ovec1R , ovec2R , ovec3R , inverse=TRUE )[1:2,1:2]))



        # Beam intersection covariance matrix
        covTR <- covarProduct( covT , covR )

        # Standard deviation of the marginal distribution in k vector direction
        stdRange <- sqrt( marginVar( covTR , k[["k.EFEC"]] ) )

        # Standard deviation of the marginal distribution in directions normal to k
        # x normal to the plane determined by the two beams, y along this plane
        if(stdXY)
            {
                if (monostatic)
                    {
                        # pick a random vector normal to k for a monostatic system
                        xvec <- normalUnitVector.cartesian( k[["k.EFEC"]] , rnorm(3) )
                        warning('Monostatic radar, calculating stdX and stdY at range rMonost=', round(rMonost) , ' m.')
                    }else{
                        xvec <- normalUnitVector.cartesian( k[["k.EFEC"]] , xyzTTarg )
                    }
                yvec <- normalUnitVector.cartesian( k[["k.EFEC"]] , xvec)
                stdX <- sqrt( marginVar( covTR, xvec ) )
                stdY <- sqrt( marginVar( covTR, yvec ) )
            }else{
                stdX <- stdY <- NA
            }

        # ranges to the beam intersection from the two sites
        r <- c( T=sqrt(sum(xyzTTarg**2)) , R=(sqrt(sum(xyzTTarg**2))+sqrt(sum(xyzRTarg**2)))/2)

        # gain integral over the whole beam intersection, normalized with square of the receiver range
        if(monostatic)
            {
                gainInt <- (2*pi)**(3/2)*sqrt(det(covTR)) / intT / intR * 100000**2
                r <- c(T=NA,R=NA)
            }else{
                gainInt <- (2*pi)**(3/2)*sqrt(det(covTR)) / intT / intR * r["R"]**2
            }
        names(gainInt) <- ''

        return( list( gainInt=gainInt , stdRange=stdRange , stdX=stdX , stdY=stdY ,  k.EFEC=k[["k.EFEC"]] , k.ENU=k[["k.ENU"]] , phi=k[["phi"]] , intersect.EFEC=xyzTarg ,  intersect.llh=xyz2LatLonH(xyzTarg) , freq.Hz=freq.Hz , range=r , monostatic=monostatic))
    }
