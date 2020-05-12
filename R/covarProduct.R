## file:covarProduct.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.

##
## Covariance matrix of a product of two 3D gaussians
## with covariance matrices covar1 and covar2
##
## Arguments:
##  covar1 , covar2  3 x 3 covariance matrices
##
## Returns:
##  A 3 x 3 covariance matrix of the product distribution
##

covarProduct <- function( covar1 , covar2 )
    {

        return(
            tryCatch(
                solve(
                    tryCatch(solve(covar1),error=function(e){matrix(NA,ncol=3,nrow=3)})
                    +
                    tryCatch(solve(covar2),error=function(e){matrix(NA,ncol=3,nrow=3)})
                    ),error=function(e){matrix(NA,ncol=3,nrow=3)}
                )
            )
    }
