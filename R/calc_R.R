#' @title Calculate the R (discrepancy) statistic and its probability value
#' @description Calculates the R statistic using Brown's (1993: eqn 5.55)
#' equation and finds the probability value from a chi-square distribution with
#' one degree of freedom.
#' @details This script takes as input the gpower transformed OPD and average
#' complete secondary osteon diameter in mm. from the mid-shaft 6th rib.
#' This data comes from the vector x.new.  This vector was written by the
#' interactive script convert()
#' @return returns the R value and its probability value.
#' @references Brown, P. J. (1993). Measurement, Regression, and Calibration.
#' New York: Oxford University Press.
#' 82(2), 199-220.

calc_R<-function()
{

b=parms$Bx.y[-1]

z2=as.numeric(x.new[-1]-parms$y.mu)

est=as.numeric(parms$B.GLS%*%z2)
est2=as.numeric(t(b)%*%z2)

cla.z=parms$by.x*est
d3=(z2-cla.z)
R=as.numeric(t(d3)%*%parms$omega%*%d3)
prob=1-pchisq(R,1)

sto=c(R,prob)
names(sto)=c('R','p-value')
return(sto)
}
