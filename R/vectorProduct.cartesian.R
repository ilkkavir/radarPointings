## file:vectorProduct.cartesian.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.

## 
## Vector (crossed) product of three-dimensional vectors x and y
## 
## Arguments:
## 
## x   c(x1,x2,x3)  a real vector
## y   c(y1,y2,y3)  a real vector
## 
## Returns:
## 
## c(z1,z2,z3) a three dimensional vector x cross y
## 

vectorProduct.cartesian <- function(x,y){

  if(length(x)!=3) error('length(x) must be 3')
  if(length(y)!=3) error('length(y) must be 3')

  return( c(x[2]*y[3]-x[3]*y[2],
          x[3]*y[1]-x[1]*y[3],
          x[1]*y[2]-x[2]*y[1]) )

}
