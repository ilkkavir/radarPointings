\name{scattVector.llhazelr}
\title{Scattering wave vector}
\alias{scattVector.llhazelr}
\description{Scattering wave vector and angle between incident and scattered waves for a bistatic radar system in geocentric cartesian coordinates.}
\usage{scattVector.llhazelr( llhT , azelT , llhR , r , freq.Hz=224e6 )}
\arguments{
  \item{llhT}{c( latitude [deg] , longitude [deg] , height [m] ) of the
    transmitter site}
  \item{azelT}{c( azimuth [deg] , elevation [deg] ) of the transmitter beam}
  \item{llhR}{c(latitude [deg] , longitude [deg] , height [m] ) of the
    receiver site}
  \item{r}{ Target range [m]. Range is defined as one half of signal path length from the transmitter, via the target, to the receiver.}
  \item{freq.Hz}{ Transmitter carrier frequency [Hz]}
}
\value{
  list( k , phi ) k is the wave vector of waves from which the signal
  is scattered, in geocentric cartesian coordinates. phi is the angle
  between incident and scattered waves in degrees. 
}

\author{Ilkka Virtanen (University of Oulu, Finland) \cr
  \email{ilkka.i.virtanen@oulu.fi}}