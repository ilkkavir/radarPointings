## file:rotateCovarMatrix.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.

##
## Rotate covariance matrix given in coordinate system with
## base vectors x, y, z into system with base vectors
## c(1,0,0), c(0,1,0), c(0,0,1), or the other way round
## 
## Arguments:
##   covarPulse  3 x 3 covariance matrix
##   x, y, z     orthogonal unit basis vectors of the coordinate system, in which covarPulse is given
##   inverse     Rotate from the system with base vectors x, y, z (FALSE)
##               or to the x,y,z system (TRUE)
## 
## Returns:
##   
##   3 x 3 covariance matrix in the geocentric coordinate system
## 
rotateCovarMatrix <- function(covarPulse,x,y,z,inverse=FALSE){
  
  rotMat <- matrix(c(x,y,z),byrow=F,nrow=3)

  if(inverse) rotMat <- t(rotMat)

  return( (rotMat%*%covarPulse%*%t(rotMat)) )
  
}
