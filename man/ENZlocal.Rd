\name{ENZlocal}
\title{Local east, north, zenith}
\alias{ENZlocal}
\description{Local east, north and zenith direction vectors in geocentric
  cartesian coordinates. }
\usage{ENZlocal( latlon )}
\arguments{
  \item{latlon}{ c( latitude [deg] , longitude [deg] )}
}
\value{

  list( east , north , zenith), each element is a 3-vector c(x,y,z) in
  geocentric cartesian system. 
  
}

\author{Ilkka Virtanen (University of Oulu, Finland) \cr
  \email{ilkka.i.virtanen@oulu.fi}}