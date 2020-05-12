## file:pulseCovar.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.

##
## 
## Covariance matrix defining a gaussian pulse shape in 3D. 
##
## Coordinates:
##        z-axis along the beam
##        x-axis horizontal near the antenna
##        y-axis perpendicular to x and z, such that the coordinate system is right-handed
## 
## Arguments:
##    stdBeam     standard deviation of a symmetric zenith-pointed beam
##    stdPulse    standard deviation of the Gaussian envelope defining pulse shape in z-direction
##    zenithAngle zenith angle of the beam in radians
##    distAT      distance from the antenna to the target (scaling factor of beam widths
##     phArr      logical, if TRUE, phased-array antennas with beam widening as function of tilt angle are assumed, otherwise 
##                beam width does not depent on pointing direction
## 
## Returns:
##    3 x 3 covariance matrix of the 3D pulse shape)
## 
## Modification by M. Orispaa (May-4-2012)
##   - beam widening corrected
##   - For this, the fwhm (angle) is needed. In order to not mess with the original arguments, we have to do some extra work
##   - beam eccentrity in not taken into account!! The difference should be negligible.

pulseCovar <- function(stdBeam,stdPulse,zenithAngle,distAT,degrees=F,phArr){

	if (phArr)
	{
  		# fwhm of the beam (in radians)
  		fwhmBeam <- std2fwhm(stdBeam) 	
  		if (degrees) zenithAngle <- zenithAngle/180*pi
  		
                if (fwhmBeam > pi)
                    {
                        ywidth <- fwhmBeam
                    }else{
                        
                        theta0 <- sin(fwhmBeam/2)
                        sinAngle <- sin(zenithAngle)
                        angle.left <- asin(sinAngle-theta0)
                        if (sinAngle+theta0 < 1)
                            {
                                angle.right <- asin(sinAngle+theta0)
                            }
                        else
                            {
                                angle.right <- pi/2
                            }
                        ywidth <- angle.right - angle.left
                    }
  		stdy <- fwhm2std(ywidth)
  		
  		return(diag( c( stdBeam * distAT, stdy * distAT, stdPulse)**2))
	}
	else
	{
		return(diag( c( stdBeam * distAT,  stdBeam * distAT, stdPulse)**2))
	}


}

