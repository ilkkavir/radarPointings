## file:vectorProduct.cartesian.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.

## 
## 
## A unit vector perpendicular to both vec1 and vec2
## 
## Arguments:
##
##  vec1 c(x1,y1,z1) a vector in cartesian coordinates
##  vec2 c(x2,y2,z2)
## 
## Returns:
## 
## c(xn,yn,zn) NaN:s if vec1 and vec2 are parallel
##

normalUnitVector.cartesian <- function(vec1,vec2){
  
  nvec <- vectorProduct.cartesian(vec1,vec2)
  nvec <- nvec / sqrt(sum(nvec**2))

  names(nvec) <- c('x','y','z')
  return(nvec)


}
