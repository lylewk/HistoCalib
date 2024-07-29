#' @title Calculate the Rx (extrapolation) statistic and its probability value
#' @description Calculates the Rx statistic using Brown's (1993: eqn 5.55)
#' equation and finds the probability value from a chi-square distribution with
#' one degree of freedom.
#' @details This script takes as input the gpower transformed OPD and average
#' complete secondary osteon diameter in mm. from the mid-shaft 6th rib.
#' This data comes from the vector x.new.  This vector was written by the
#' interactive script convert()
#' @return returns the Rx value and its probability value.
#' @references Brown, P. J. (1993). Measurement, Regression, and Calibration.
#' New York: Oxford University Press.
#' 82(2), 199-220.

calc_Rx<-function ()
{

b=parms$Bx.y[-1]
z2=as.numeric(x.new[-1]-parms$y.mu)

est=as.numeric(parms$B.GLS%*%z2)
est2=as.numeric(t(b)%*%z2)

cla.z=parms$by.x*est
inv.z=parms$by.x*est2
d3=(cla.z-inv.z)
Rx=as.numeric(t(d3)%*%parms$omega%*%d3)
prob=1-pchisq(Rx,1)
sto=c(Rx,prob)
names(sto)=c('Rx','p-value')
return(sto)
}
