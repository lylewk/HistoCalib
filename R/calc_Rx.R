#' Find Rx (extrapolation statistic) and its probability value
#'
#' This function finds the Rx statistic (Brown 1993: eqn 5.55)
#' and its probability value
#'
#' @references
#' Brown, P. J. (1993). Measurement, Regression, and Calibration.
#' New York: Oxford University Press.
#'
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
