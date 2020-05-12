## file:ellipsoidNormal.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.

##
## Ellipsoid normal vector in Cartesian coordinates
##
## Argumnets:
##  latlon Latitude and longitude in degrees
##
## Returns:
##  c(x,y,z) Normal vector in cartesian coordinates
##

ellipsoidNormal <- function( latlon )
  {

    lat <- latlon[1]
    lon <- latlon[2]
    

    return(
      c(
        x = cos( lat * pi / 180 ) * cos( lon * pi / 180 ) ,
        y = cos( lat * pi / 180 ) * sin( lon * pi / 180 ) ,
        z = sin( lat * pi / 180 )
        )
      )


  }
