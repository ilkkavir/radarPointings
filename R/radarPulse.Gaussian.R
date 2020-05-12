## file:radarPulse.gaussian.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.

##
## 3D Gaussian distribution of a radar pulse with Gaussian beam
## pattern and gaussian pulse shape. The beam is symmetric at zenith.
## 
## Arguments:
## 
##  fwhmBeam  full width at half maximum of the beam cross section when the beam is pointed to zenith
##  fwhmPulse full width at half maximum of the Gaussian transmission envelope, in us
##  xyzAnt    c(x,y,z) position of the antenna in Cartesian EFEC system
##  xyzTarg   c(x,y,z) position of te target in Cartesian EFEC system
##  phArr     logical, if TRUE, phased-array antennas with beam widening as function of tilt angle are assumed, otherwise 
##             beam width does not depent on pointing direction
##

radarPulse.gaussian <- function( fwhmBeam , fwhmPulse , xyzAnt , xyzTarg , phArr ){

  # vector pointing from the antenna to the target
  vecAT <- xyzTarg - xyzAnt

  # distance from the antenna to the target
  distAT <- sqrt(sum(vecAT**2))

  # a unit vector to the target direction is used as the z-axis when defining pulse shapes
  zPulse <- vecAT/distAT

  # local ENU base vectors at the antenna location
  xyzENU <- ENUlocal( xyz2LatLonH( xyzAnt ) )


  # normal of the beam axis, horizontal at the antenna location (this is the x-axis direction when defining beam shape)
  # If the beam was pointed to zenith, select a random z-axis (this will be rotated to the geographic system anyway)
  if(vectorAngle.cartesian(vecAT,xyzENU[["up"]])==0){
    xPulse <- normalUnitVector.cartesian(zPulse,runif(3))
  }else{
    xPulse <- normalUnitVector.cartesian(zPulse,xyzENU[["up"]])
  }

  # the y-axis is perpendicular to x and z, and we have a right-handed system
  yPulse <- normalUnitVector.cartesian(zPulse,xPulse)
  
  # conversion of fwhm (in degrees) to standard deviations (in radians)
  stdBeam  <- fwhm2std(fwhmBeam)*pi/180
  # conversion of fwhm to standard deviation, both in km
  stdPulse <- fwhm2std(fwhmPulse)

  # zenith angle of the beam, in radians
  zenithAngle <- vectorAngle.cartesian(zPulse,xyzENU[["up"]],degrees=F)
  
  # The 3D covariance matrix of the pulse in the xyz-system calculated above
  covarPulse <- pulseCovar(stdBeam,stdPulse,zenithAngle,distAT,phArr=phArr)

  # The same covariance matrix in the Cartesian EFEC system
  covarPulse.EFEC <- rotateCovarMatrix(covarPulse,xPulse,yPulse,zPulse)

  # return the covariance in EFEC coordinates
  return( covarPulse.EFEC )

}

