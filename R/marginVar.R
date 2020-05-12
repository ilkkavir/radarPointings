## file:marginVar.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.

##
## Variance of the marginal distribution of 3D gaussian
## with covariance matrix covar in direction of vector
## vec
##
## Arguments:
##  covar 3 x 3 covariance matrix
##  vec   a 3-vector
##
## Returns
##  var   variance of the marginal distribution

marginVar <- function( covar , vec )
    {
        rotVec <- matrix( vec/sqrt(sum(vec**2)) , nrow=1 )

        return( c( rotVec %*% covar %*% t(rotVec) ) )

    }
