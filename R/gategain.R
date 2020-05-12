gategain <- function( inter , rlims , maxdev=Inf )
    {

        lambda <- 299792458/inter[["freq.Hz"]]

        if(inter[["monostatic"]])
            {
                rlims2 <- (rlims-mean(rlims))
            }else{
                rlims2 <- (rlims - inter[["range"]]["R"]) * sin( inter[["phi"]]*pi/360)
            }
        # if both limits are at the same side of the intersection,
        # check that they are not too far away
        if( prod(rlims2) > 0 ){
            if( min( abs(rlims2) ) > (maxdev*inter[["stdRange"]]) ) return(NA)
        }

        # the last term is from circular polarization assumption
        return( ( pnorm( rlims2[2] , sd=inter[["stdRange"]] ) - pnorm( rlims2[1] , sd=inter[["stdRange"]]) ) * inter[["gainInt"]] *  lambda**2/4/pi * .5 * (1+cos(inter[["phi"]]*pi/180)**2) / mean(rlims)**2 / (rlims[2]-rlims[1]) )
    }
