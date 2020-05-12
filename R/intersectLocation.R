## file:intersectLocation.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.

##
## Approximate beam intersection point
##
## Arguments:
##  xyzT  c(x,y,z), transmitter location in cartesian EFEC coordinates [m]
##  xyzR  c(x,y,z), receiver location in cartesian EFEC coordinates [m]
##  azelT c(az,el), transmitter azimuth and elevation [degrees]
##  azelR c(az,el), receiver azimuth and elevation [degrees]
##
## Returns:
##  c(x,y,z) Beam axis intersection point in cartesian EFEC
##

intersectLocation <- function( xyzT , xyzR , azelT , azelR )
  {
      # estimate of the beam intersection point
    
      # pointing directions in EFEC coordinates
      pdirT <- ENU2EFEC.xyz( azel2ENU( azelT ) , xyzT )
      pdirR <- ENU2EFEC.xyz( azel2ENU( azelR ) , xyzR )

      # check if the beams are parallel, in that case they
      # obviously do not have an intersection
      if( abs(vectorAngle.cartesian(pdirT,pdirR)) < 1e-3) return(c(NA,NA,NA))

      # form a linear inverse problem, whose solution gives distances from the sites
      m <- xyzT - xyzR
      A <- matrix( c(pdirR,-pdirT),ncol=2,byrow=FALSE)
      R <- qr.solve(a=A,b=m)

      intersect <- xyzT + R[2]*pdirT

      names(intersect) <- c('x','y','z')

      return(intersect)
  }
