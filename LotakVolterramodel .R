library(deSolve)
library(scatterplot3d)

single<-function(t,state,params){
	with(as.list(c(state,params)),{
		dN<-r*N*(1-N/K)
		list(dN)
	})
}
params<-c(r=2.2,K=50)
state<-c(N=10)
times<-seq(0,100,1)
pop<-ode(y=state,times=times,func=single,parms=params,method="euler")
plot(pop)
?ode

parameters<-c(r=1.15,alpha=.01,e=.1,q=.5)
state<-c(N=500,P=20)

lvmodel<-function(t,state,parameters){
	with(as.list(c(state,parameters)),{
		#rate of change
		dN<-(r*N)-(alpha*N*P)
		dP<-(e*alpha*N*P)-(q*P)
		
		#return rate of change
		list(c(dN,dP))
	})
}


times<-seq(0,100,by=.1)
system.time(
out<-ode(y=state,times=times,func=lvmodel,parms=parameters)
)
out
png("/Users/borrejx06/Desktop/lvmod2.png")
plot(out[,2],out[,3],typ="o",pch=20,cex=.5)
dev.off()
abline(v=.5/.001)
abline(h=1.15/.01)
min(out[,2])
plot(out[,2],typ="l")
points(out[,3],typ="l",lty=2)


parameters2<-c(r=2,alpha=.1,e=.1,q=.5,K=600)
state<-c(N=50,P=20)
times<-seq(0,100,by=.1)
lvmodel2<-function(t,state,parameters2){ #with density dependence
	with(as.list(c(state,parameters2)),{
		#rate of change
		dN<-(r*N*(1-N/K))-(alpha*N*P)
		dP<-(e*alpha*N*P)-(q*P)
		
		#return rate of change
		list(c(dN,dP))
	})
}

out2<-ode(y=state,times=times,func=lvmodel2,parms=parameters2)
png("/Users/borrejx06/Desktop/lvmod_dd2.png")
plot(out2[,2],out2[,3],typ="o",pch=20,cex=.5,)
abline(v=.5/.01,col="blue") #dP/dt = 0
abline(h=2*((1/.1)-50/(600*.1)),col="blue") #dN/dt = 0
dev.off()
#plot(out2[,2],typ="l",ylim=c(0,100))
#points(out2[,3]+30,typ="l",lty=2)
##
#Equilibrium # Pred when there is density dependence in the prey
#P=r*((1/alpha)-N/(K*alpha))


parameters3<-c(r=2,alpha=.5,e=.1,q=.25,K=600,h=.1)
state<-c(N=500,P=100)
lvmodel3<-function(t,state,parameters3){ #with density dependence and A-G fr
	with(as.list(c(state,parameters3)),{
		#rate of change
		dN<-(r*N*(1-N/K))-((alpha*N)/(P+alpha*h*N))*P
		dP<-(e*((alpha*N)/(P+alpha*h*N))*P)-(q*P)
		
		#return rate of change
		list(c(dN,dP))
	})
}

times<-seq(0,100,by=.1)
out3<-ode(y=state,times=times,func=lvmodel3,parms=parameters3)

plot(out3[,2],out3[,3],typ="o",pch=20,cex=.5)

### EQ PRED this is incorrect, rethink this
##
## P = (K*alpha)/(r*(K-N)) - (K*alpha*N*h)/(K-N) + (alpha*N*N*h)/(K-N)
##
###EQ PREY
##
## N=
##
parms<-matrix(nrow=1000,ncol=7)
state.var<-matrix(nrow=1000,ncol=3)

for(i in 1:1000){
r<-rnorm(1,1,1)
a<-runif(1,0,1)
a2<-runif(1,0,1)
e<-runif(1,0,1)
e2<-runif(1,0,1)
mu<-runif(1,0,1)
k<-runif(1,100,10000)

parms[i,]<-c(r,a,a2,e,e2,mu,k)

N<-rnorm(1,300,50)
N1<-rnorm(1,50,10)
N2<-rnowm(1,50,10)

state.var[i,]<-c(N,N1,N2)

parameters<-c(r=r,alpha=a,alpha2=a2,e=e,e2=e2,q=mu,K=k)
#set1: 2,.1,.5,.1,.1,.5,500
#set2: 2,.12,.25,.01,.1,.5,5000
state<-c(N=250,N1=15,N2=25)
#set1:1000,80,20
#set2:250,15,50

lvmodel<-function(t,state,parameters){
	with(as.list(c(state,parameters)),{
		#rate of change
		dN<-r*N*(1-N/K)-alpha*N*N1
		dN1<-e*alpha*N*N1-alpha2*N1*N2-q*N1
		dN2<-e2*alpha2*N1*N2-q*N2
		
		#return rate of change
		list(c(dN,dN1,dN2))
	})
}


times<-seq(0,200,by=.1)

out<-ode(y=state,times=times,func=lvmodel,parms=parameters)
#tail(out)
#png("~/Desktop/3spLVmod_3.png",width=900,height=900,pointsize=20)
scatterplot3d(out[,2],out[,3],out[,4],xlab="N",ylab="N1",zlab="N2",typ="o",pch=20,cex.symbols=.75)
#dev.off()
#1 is stable eq, 2 is wonky unstable, 3 is death spiral

c.mat<-matrix(nrow=2001,ncol=1)
c.mat[1,1]<-"blue"
c.mat[2:2001,1]<-"black"
?scatterplot3d
#stability analysis
N<-250
N1<-15
N2<-50
r<-2
alpha<-.12
alpha2<-.25
e<-.01
e2<-.1
q<-.5
K<-5000

J<-matrix(nrow=3,ncol=3)
J[1,1]<-r-2*r*(N/K)-alpha*N1
J[1,2]<--alpha*N
J[1,3]<-0
J[2,1]<-e*alpha*N1
J[2,2]<-e*alpha*N-alpha2*N2
J[2,3]<--alpha2*N1
J[3,1]<-0
J[3,2]<-e2*alpha2*N2
J[3,3]<-e2*alpha2*N1-q
J
max(Re(eigen(J)$values))
-1/max(Re(eigen(J)$values))

########################################################
########################################################
########################################################

params<-matrix(nrow=1000,ncol=7)
state.var<-matrix(nrow=1000,ncol=3)
temp.var<-matrix(nrow=1000,ncol=3)
mins<-matrix(nrow=1000,ncol=3)
eigenval<-c()
returns<-c()
for(i in 1:1000){
gr<-rnorm(1,2,1)
a1<-runif(1,0,1)
a2<-runif(1,0,1)
e1<-runif(1,0,1)
e2<-runif(1,0,1)
mu<-runif(1,0,1)
k<-as.integer(runif(1,100,10000))

params[i,]<-c(r,a,a2,e,e2,mu,k)

sN<-as.integer(rnorm(1,300,50))
sN1<-as.integer(rnorm(1,50,10))
sN2<-as.integer(rnorm(1,50,10))

state.var[i,]<-c(N,N1,N2)

parameters<-c(r=gr,alpha=a1,alpha2=a2,e=e1,e2=e2,q=mu,K=k)
#set1: 2,.1,.5,.1,.1,.5,500
#set2: 2,.12,.25,.01,.1,.5,5000
state<-c(N=sN,N1=sN1,N2=sN2)
#set1:1000,80,20
#set2:250,15,50

lvmodel<-function(t,state,parameters){
	with(as.list(c(state,parameters)),{
		#rate of change
		dN<-r*N*(1-N/K)-alpha*N*N1
		dN1<-e*alpha*N*N1-alpha2*N1*N2-q*N1
		dN2<-e2*alpha2*N1*N2-q*N2
		
		#return rate of change
		list(c(dN,dN1,dN2))
	})
}


times<-seq(0,100,by=.1)

out<-ode(y=state,times=times,func=lvmodel,parms=parameters)
temp.var[1,1]<-var(out[,1])/mean(out[,1])
temp.var[1,2]<-var(out[,2])/mean(out[,2])
temp.var[1,3]<-var(out[,3])/mean(out[,3])

mins[i,1]<-min(out[,1])
mins[i,2]<-min(out[,2])
mins[i,3]<-min(out[,3])

J<-matrix(nrow=3,ncol=3)
J[1,1]<-r-2*r*(N/k)-a*N1
J[1,2]<--a*N
J[1,3]<-0
J[2,1]<-e*a*N1
J[2,2]<-e*a*N-a2*N2
J[2,3]<--a2*N1
J[3,1]<-0
J[3,2]<-e2*a2*N2
J[3,3]<-e2*a2*N1-mu

eigenval[i]<-max(Re(eigen(J)$values))
returns[i]<--1/max(Re(eigen(J)$values))

}
head(parms)