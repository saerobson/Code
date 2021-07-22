
##### Iterating Death, Start with alpha #####
n=150 #n is total number of days 
N=66100000 - 1890000  #N is population size, N = S+I+D , UK-NI 

alpha=rep(1,n)
alpha[1]=1
beta=rep(0,n) #sum(beta)=1

ro=3.01#3.203

h=rep(0,n) #hazard function at infected age j

S=rep(0,n)
I=matrix(rep(0,n^2),nrow=n)
inf=860
S[1]=N-inf

II=rep(0,n)
  II[1]=860
D=rep(0,n)

lamda=matrix(rep(0,n^2),nrow=n)

mean=5.2 #mean= shape*scale
var=2.96 #var=shape

shape=338/37 
scale=37/65
rate=1/scale

#beta
dgamma(x=5,rate=rate,shape=shape)
pgamma(q=120,rate=rate,shape=shape)
beta[1]=pgamma(q=3/2,rate=rate,shape=shape)

#Hazard ratio
dispersion=0.0546
probability=18.69/(18.69+0.0546) #mean /(mean+dispersion)
inf_fatal_rate=0.0204#0.00724
dnbinom(x=1,size=dispersion,prob=probability)

P=rep(0,n)
lamsum=rep(0,n)
lamsum2=rep(0,n)
p=rep(0,n)
d=rep(0,n)
dd=rep(0,n)
c=rep(0,n)
m=rep(0,n)
P[1]=dnbinom(x=1,mu=18.69,size=(1/0.0546))
h[1]=inf_fatal_rate*dnbinom(x=1,size=dispersion,prob=probability)/(1-inf_fatal_rate*P[1])
p[1]=dnbinom(x=1,mu=18.69,size=(1/0.0546))

dnbinom(size)
dnbinom(x=3,mu=18.69,size=(1/0.0546))
dnbinom(x=20,mu=18.69,size=0.0546)
tstar=31.57

#Find alpha, beta, h and P
for ( i in 2:n) {p[i]=dnbinom(x=i,mu=18.69,size=(1/0.0546))
P[i]=sum(p[1:i])
  h[i]=inf_fatal_rate*p[i]/(1-inf_fatal_rate*P[i-1])
  beta[i]=pgamma(q=(i+0.5),rate=rate,shape=shape) - pgamma(q=(i-0.5),rate=rate,shape=shape)
  m[i]=pnbinom(q=i,mu=18.69,size=(1/0.0546))
  if (i<floor(tstar)){ alpha[i]= 1}
  else if(i >= ceiling(tstar)){ alpha[i]= 0.2814}
  else {alpha[i] = tstar -floor(tstar) + 0.2814*(ceiling(tstar)-tstar)}
  }

beta1=data.frame(beta)
beta1$Day=c(1:n)
names(beta1)=c("Probability","Day")

########### Find distribution of I0 ##########
#420 E W
#860 E W S
M=66100000 - 1890000 - 5470000 #M is population size, N = S+I+D , UK-NI (-S)
#III=matrix(rep(0,n^2),nrow=n)
III=rep(0,n)
III[1]=1/n
#III[1,j]=1
lam=rep(0,n)
for (i in 1:n) {lam[i]=ro*beta[i]/M}
SS=M-420
for (j in 2:n) {III[j]=M*(1-h[j-1])*III[j-1]/(SS*sum(lam[1:(j-1)]%*%III[1:j-1]))}


### Newton Raps ####
install.packages("pracma")
install.packages("rootSolve")
library(pracma)
library(rootSolve)
x=rep(0,n)
fsolve(func,x0=1/n)
func= function(x) { 
  x/X[0] - M*(1-h[1])*X[j-1]/(SS*lam[1])
  }

x0=rep(1/n,n)

resid = function(x,params)  {
  n=length(x)
  h = params[,1]
  lam = params[,2]
  res=rep(0,n)
  res[1]=sum(x)-1
  for (i in 2:(n)) { 
  res[i]=  x[i]/x[1] - (1-h[i-1])*x[i-1]/(lam[1:n]%*%x[1:n])
}
  return(res)
}

params = matrix(c(h,lam),ncol=2)
fsolve(resid,x0,maxiter=1000,params=params)
multiroot(resid,start=x0,maxiter=100,positive=TRUE,params = matrix(c(h,lam),ncol=2) ) 
nloptr(x0=x0,eval_f=resid,lb=rep(0,n),opts=list("algorithm" = "NLOPT_GN_DIRECT","xtol_rel"=1.0e-8 ),h=h,lam=lam)

func2= function(x) {x -M*(1-h[j])*x/(SS*sum(lam[1:(j-1)]%*%x[1:j-1])) }
x=rep(0,(n+1))
x[1]=1/n

for (i in 2:(n+1)) {
 if (i==2){
  func=function(y) {y/x[1]- M*(1-h[i-1])*x[i-1]/(SS*sum(lam[1:(i-1)]%*%x[1:i-1]))
  } }
  else {func=function(y) {y/x[2]- M*(1-h[i-1])*x[i-1]/(SS*sum(lam[1:(i-1)]%*%x[1:i-1]))}
   }
  x[i]=uniroot(func,lower=-1,upper=1,extendInt = "yes")$root
}
total=x[2:length(x)]
sum(total)
420*total/sum(total) 



x[i]=fsolve(func,x0)
func=function(y) {y/x[1]- M*(1-h[i])*x[i-1]/(SS*sum(lam[1:(i-1)]%*%x[1:i-1]))
  return(y)}


##### Plots from Figure 1 #########
barplot(beta,col = "light blue",ylim=c(0,0.25),xlim=c(1,15),xlab="Day,j")
barplot(p,col = "light blue",ylim=c(0,0.001),xlim=c(1,60),xlab="Day,j")
plot(h,col = "blue",ylim=c(0,0.0015),xlim=c(1,60),xlab="Day,j")

#Find S,I,D, c{appendix}
for (i in 1:n){

  for (j in 1:i) {
    lamda[i,j]=ro*alpha[i]*beta[j]/N
  if (i<n){
    I[i+1,j+1]=I[i,j]-h[j]*I[i,j]}
  }
  lamlam=0
  for (k in 1:i){lamlam=lamlam+lamda[i,k]*I[i,k]}
  lamsum2[i]=lamlam
  lamsum[i]=lamda[i,1:j]%*%I[i,1:j]
  d[i]=h[1:j]%*%I[i,1:j]
  if (i<n){
    I[i+1,1]=S[i]*lamsum[i]

    S[i+1]=S[i]-I[i+1,1]
    D[i+1]=D[i]+d[i]
    c[i]=sum(I[i+1,1:j])/sum(I[i,1:j])
    }
  
}

#### Number Deaths on each Day ############
DD=data.frame(D)
DD$Day=c(1:n)
#DD$Date=as.Date()
DD$Death[1]=0
for (i in 2:n){DD$Death[i]=DD$D[i]-DD$D[(i-1)]} #gives number died on each day

DeathByDay=ggplot(DD)
DeathByDay=DeathByDay + geom_point(aes(x=Day,y=Death),color="magenta",size=1)
DeathByDay=DeathByDay+labs(title="Daily Number of Deaths")
DeathByDay=DeathByDay+ylab("Daily Deaths")
DeathByDay

