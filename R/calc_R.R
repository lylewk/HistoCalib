#' Find R (discrepancy statistic) and its probability value
#'
#' There are no parameters
calc.R<-function()
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
