## file:vectorAngle.cartesian.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.

## 
## angle between vectors 
## 
## Arguments:
## 
##  vec1     c(x,y,z) vector 1 in Cartesian coordinates
##  vec2     c(x,y,z) vector 2 in Cartesian coordinates
##  vecn     c(x,y,z) normal of the plane of vec1 and vec2, determines the positive direction of rotation
##  degrees  logical, if TRUE, the output is in degrees, otherwise in radians
## 
## Returns:
## 
##   angle between the vectors vec1 and vec2
## 

vectorAngle.cartesian <- function(vec1,vec2,vecn=NULL,degrees=T){

  if(sum(abs(vec1))==0) return(0)
  if(sum(abs(vec2))==0) return(0)

  arg <- sum(vec1*vec2) / (sqrt(sum(vec1**2) * sum(vec2**2)))

  if(abs(arg)>1){
    if((abs(arg)-1)<1e-10) arg <- sign(arg)
  }

  s <- 1
  if(!is.null(vecn)){
    if(sum(vectorProduct.cartesian(vec1,vec2)*vecn)<0) s <- -1
  }

  if (degrees) return(s*180/pi*acos( arg ))

  return(s*acos( arg ))

}

