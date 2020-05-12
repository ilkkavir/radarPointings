## file:radarSites.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.

##
## A collection of geographic latitude, longitude, height coordinates of a few existing radar sites
## 
## Returns:
##   a named list of coordinates with latitudes and longitudes in degrees, and height in m
##

radarSites <- function()
  {

    return(
      list(
          # Tromso
          TRO = xyz2LatLonH( xyz=c(  2106790 ,  734793 , 5955184 ) ),
          VHF = xyz2LatLonH( xyz=c(  2106790 ,  734793 , 5955184 ) ),
          UHF = xyz2LatLonH( xyz=c(  2106790 ,  734793 , 5955184 ) ),
          # Kiruna 
          KIR = xyz2LatLonH( xyz=c(  2259041 ,  841711 , 5885639 ) ),
          # Sodankyla
          SOD = xyz2LatLonH( xyz=c(  2200819 , 1103389 , 5864323 ) ),
          # Kilpisjarvi
          KIL = xyz2LatLonH( xyz=c(  2136818.264 , 810038.592 , 5935299.531 ) ),
          HBA = xyz2LatLonH( xyz=c(  2136818.264 , 810038.592 , 5935299.531 ) ),
          LBA = xyz2LatLonH( xyz=c(  2136833.225 , 810088.740 , 5935285.279 ) ),
          # Longyearbyen
          ESR = xyz2LatLonH( xyz=c(  1262647 , 362744 , 6220902 ) ),
          # Millstone Hill
          MLH = c(  42.62 , 288.51 , 146 ),
          # Jicamarca
          JRO = c( -11.95 , 283.13 , 520 )
        )
      )



  }
