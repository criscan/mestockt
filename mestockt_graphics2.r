
 rm(list=ls()) # erasure all objects
   # system('mestockt -ind Arenque5b.dat -nox')  # for model running
   # shell(paste("copy for_R.rep Arenque5b.rep",sep=""))
   # shell(paste("copy mestockt.par Arenque5b.par",sep=""))


source('read.admb.R')
source('read.admbFit.R')
source('por_recluta_r.R')


data <-read.rep('Arenque5.rep')


attach(data)
Yrs=seq(1995,2023.75,0.25)

target=0.6

barplot(Desembarques[1,]/1000~Yrs, cex.lab=1, xlab="Año",ylab="Toneladas (miles)", col = "gray",ylim = c(0,max(Desembarques[1,]/1000)*1.1), 
        main="Desembarques")
grid(nx = NULL, ny = NULL, lty = 2, col = "gray",lwd = 1)
box()

#Ajustes--------------------------------------------------------------------------

par(mfrow = c(3, 3))
sim=20
  
max=max(CPUE);
ubi=which(CPUE[1,]>0)
plot(Yrs[ubi],(CPUE[1,ubi]),ylab="Indice",xlab="Año",main="CPUE",ylim = c(0,max((CPUE[which(CPUE>0)]))),
     pch=sim,type="b",cex=1.5)

X0=Yrs[ubi]
Y0=CPUE[2,ubi]
cvF=cv_cpue
li=Y0-1.96*cvF*Y0
ls=Y0+1.96*cvF*Y0

x=c(X0,X0[seq(length(X0),1,-1)])
y=c(li,ls[seq(length(ls),1,-1)])
polygon(x,y,col="#DCDCDC",border="#DCDCDC")


lines(Yrs[ubi],(CPUE[2,ubi]),col="red",lwd=2)
lines(Yrs[ubi],(CPUE[1,ubi]),pch=sim,type="b",cex=1.5)


barplot((CPUE[1,ubi]-CPUE[2,ubi])/sd(CPUE[1,ubi]-CPUE[2,ubi])~Yrs[ubi],ylab="Residual std",xlab="Año",main="residual CPUE",col="gray")
abline(h=0,col="red",lwd=2)

hist((CPUE[1,ubi]-CPUE[2,ubi])/sd(CPUE[1,ubi]-CPUE[2,ubi]),xlab="Residual std",main="residual CPUE")



# plot(Yrs,(Desembarques[1,]),ylab="Toneladas",xlab="Año",main="Desembarque",ylim = c(0,max((Desembarques))),
#      pch=sim,type="b",cex=1.5)
# lines(Yrs,(Desembarques[2,]),col="red",lwd=2)

maxi=max(Surveys[1,]);
ubi=which(Surveys[1,]>0)

plot(Yrs,Surveys[2,],type="l",col="red",lwd=2,ylim = c(0,1.2*max(Surveys)),ylab="Indice",xlab="Año",
     main="Crucero",xlim=c(Yrs[min(which(Surveys[1,]>0))],Yrs[max(which(Surveys[1,]>0))]))
X0=Yrs
Y0=Surveys[2,]
cvF=cv_cru

li=Y0-1.96*cvF*Y0
ls=Y0+1.96*cvF*Y0
x=c(X0,X0[seq(length(X0),1,-1)])
y=c(li,ls[seq(length(ls),1,-1)])
polygon(x,y,col="#DCDCDC",border="#DCDCDC")

lines(Yrs[ubi],(Surveys[1,ubi]),pch=sim,type="b",cex=1.5)

lines(Yrs,Surveys[2,],type="l",col="red",lwd=2)


barplot((Surveys[1,ubi]-Surveys[2,ubi])/sd(Surveys[1,ubi]-Surveys[2,ubi])~Yrs[ubi],ylab="Residual std",xlab="Año",main="residual Surveys",col="gray")
abline(h=0,col="red",lwd=2)

hist((Surveys[1,ubi]-Surveys[2,ubi])/sd(Surveys[1,ubi]-Surveys[2,ubi]),xlab="Residual std",main="residual Surveys")




sim=20
min=min(Lmed_flo[2:3,])
max=max(Lmed_flo[2:3,])
plot(Yrs[Lmed_flo[1,]],Lmed_flo[2,],ylab="Talla promedio",xlab="Año",main="Talla promedio flota",ylim = c(min,max),
     pch=sim,type="b",cex=1.5)

X0=Yrs[Lmed_flo[1,]]
Y0=Lmed_flo[3,]
cvF=0.02
li=Y0-1.96*cvF*Y0
ls=Y0+1.96*cvF*Y0

x=c(X0,X0[seq(length(X0),1,-1)])
y=c(li,ls[seq(length(ls),1,-1)])
polygon(x,y,col="#DCDCDC",border="#DCDCDC")


lines(Yrs[Lmed_flo[1,]],Lmed_flo[3,],lwd=2,col="red")
lines(Yrs[Lmed_flo[1,]],Lmed_flo[2,],pch=sim,type="b",cex=1.5)


barplot((Lmed_flo[2,]-Lmed_flo[3,])/sd(Lmed_flo[2,]-Lmed_flo[3,])~Yrs[Lmed_flo[1,]],ylab="Residual std",xlab="Año",main="residual Lmed_flo",col="gray")
abline(h=0,col="red",lwd=2)

hist((Lmed_flo[2,]-Lmed_flo[3,])/sd(Lmed_flo[2,]-Lmed_flo[3,]),xlab="Residual std",main="residual Lmed_flo")



#Comps_tallas_marginal----------------------------------------------------------------------

par(mfrow = c(1, 2))
dl=0.5*(Tallas[2]-Tallas[1])


plot(Tallas-dl, Frec_marg_flo[1,],col="gray",type="s",lwd=5,xlab="Talla",ylab="Proporcion",main="Flota")
lines(Tallas,Frec_marg_flo[2,],col="red",lwd=2)

plot(Tallas-dl, Frec_marg_srv[1,],col="gray",type="s",lwd=5,xlab="Talla",ylab="Proporcion",main="Cruceros")
lines(Tallas,Frec_marg_srv[2,],col="red",lwd=2)


#Comps_tallas_f----------------------------------------------------------------------

n=length(Lmed_flo[1,])
filas=4 
cols=4
dl=0.5*(Tallas[2]-Tallas[1])

par(mfcol = c(5, 4))
for (i in 1:n)
{
  plot(Tallas-dl,Frecs_capt_obs[i,],main=paste(Yrs[Lmed_flo[1,i]]),type="s",lwd=3, col="gray",ylab="", xlab="Talla",
               ylim=c(0,max(c(Frecs_capt_obs,Frecs_capt_pred))))
  lines(Tallas,Frecs_capt_pred[i,],col="red",lwd=2)
  
}


#Comps_tallas_s----------------------------------------------------------------------

n=length(Lmed_srv[1,])
filas=2
cols=round(n/filas)
if (cols==0)
{cols=1}

par(mfcol = c(2, 3))

for (i in 1:n)
{

  plot(Tallas-dl,Frecs_srv_obs[i,],main=paste(c(Yrs[Lmed_srv[1,i]]),"c"),type="s",lwd=3, col="gray",
       ylab="", xlab="Talla",ylim=c(0,max(c(Frecs_srv_obs,Frecs_srv_pred))))
  lines(Tallas,Frecs_srv_pred[i,],col="red",lwd=2)

}

#qqplot_tallas---

library(MASS) #libreria 
par(mfcol=c(2,2))
Residuals <- as.vector(Frecs_capt_obs)-as.vector(Frecs_capt_pred)
Residuals=Residuals/sd(Residuals)
xfit<-seq(min(Residuals),max(Residuals),length=40) 
yfit<-dnorm(xfit) 
hist(Residuals, freq=FALSE, xlab="Std. residuals (Comps flota)", ylab="Frequency")
lines(xfit, yfit,col="red")
qqnorm(Residuals,xlab="Theoric quantiles", ylab="Std. residuals (Comps flota)",col="gray")
lines(xfit, xfit,col="red")

Residuals <- as.vector(Frecs_srv_obs)-as.vector(Frecs_srv_pred)
Residuals=Residuals/sd(Residuals)
xfit<-seq(min(Residuals),max(Residuals),length=40) 
yfit<-dnorm(xfit) 
hist(Residuals, freq=FALSE, xlab="Std. residuals (Comps cruceros)", ylab="Frequency")
lines(xfit, yfit,col="red")
qqnorm(Residuals,xlab="Theoric quantiles", ylab="Std. residuals (Comps cruceros)",col="gray")
lines(xfit, xfit,col="red")



#Selectividad----------------------------------------------------------------------

par(mfrow = c(1, 2))

if(length(Sel_f)>length(L_edad))
{matplot(L_edad,t(Sel_f),type="l",lwd=2,lty = 1,xlab="Talla",ylab="Proporcion",
         main="Selectividad Flota",col="gray",ylim=c(0,1))}

if (length(Sel_f)==length(L_edad))
{plot(L_edad,Sel_f,type="l",lwd=2,lty = 1,xlab="Talla",ylab="Proporcion",
      main="Selectividad Flota",col="gray",ylim=c(0,1))}


lines(L_edad,Madurez_edad,col="red",lwd = 2)
#grid(nx = NULL, ny = NULL, lty = 2, col = "gray",lwd = 1)

if(length(Sel_srv)>length(L_edad))
{matplot(L_edad,t(Sel_srv),type="l",lwd=2,lty = 1,xlab="Talla",ylab="Proporcion",
         main="Selectividad Crucero",col="gray",ylim=c(0,1))}

if (length(Sel_srv)==length(L_edad))
{plot(L_edad,Sel_srv,type="l",lwd=2,lty = 1,xlab="Talla",ylab="Proporcion",
      main="Selectividad Crucero",col="gray",ylim=c(0,1))}

lines(L_edad,Madurez_edad,col="red",lwd = 2)
#grid(nx = NULL, ny = NULL, lty = 2, col = "gray",lwd = 1)


#Poblacionales----------------------------------------------------------------------

par(mfrow = c(2, 2))

sdR=Reclutamientos[3,]
li=Reclutamientos[2,]-1.96*sdR
ls=Reclutamientos[2,]+1.96*sdR
nyrs=length(Biomasa_desovante)
plot(Yrs,Reclutamientos[2,],ylim = c(0,max(Reclutamientos[2,]+1.96*sdR)*1.01),type="l",
     ylab="Reclutamientos",xlab="Año",pch = 16)
x=c(Yrs,Yrs[seq(length(Yrs),1,-1)])
y=c(li,ls[seq(length(ls),1,-1)])
polygon(x,y,col="#DCDCDC",border="#DCDCDC")
lines(Yrs,Reclutamientos[2,],lwd="2")
lines(Yrs,Reclutamientos[1,],lwd="2",col="red")
#grid(nx = NULL, ny = NULL, lty = 2, col = "gray",lwd = 1)

plot(Yrs,Dev_log_R,type="l",xlab="Años",ylab="Desvio log_R",lwd=2,col="black")
#grid(nx = NULL, ny = NULL, lty = 2, col = "gray",lwd = 1)
abline(h=0,col="red",lwd = 2)


v=rep(c(1,2,3,4),29)
r1=mean(Reclutamientos[2,which(v==1)])
r2=mean(Reclutamientos[2,which(v==2)])
r3=mean(Reclutamientos[2,which(v==3)])
r4=mean(Reclutamientos[2,which(v==4)])
devr2=c(r1,r2,r3,r4)
trim=c(1,2,3,4)
barplot(devr2-mean(devr2)~trim, ylab="",xlab="Trimestre")



#Comps_edad-talla------------
par(mfrow = c(2, 1))
nedades=length(No)

plot(Tallas,No[1]*Prob_talla[1,],type="l",xlab="Talla",ylab="Densidad",main="Componentes de edades")

for (i in 2:nedades)
{ 
  lines(Tallas,No[i]*Prob_talla[i,])
}

abline(v=L_edad,lty=2, col="gray")
lines(Tallas,No[1]*Prob_talla[1,],col="red",lwd=2)
text(round(L_edad[1],2),100,paste(round(L_edad[1],2)),col="blue")


plot(L_edad[1:nedades-1],L_edad[2:nedades],type="b", xlab="L(edad)",ylab="L(edad+1)",main="Crecimiento individual")
abline(v=L_edad[1:nedades-1],lty=2, col="gray")
abline(h=L_edad[2:nedades],lty=2, col="gray")

#grid(nx = NULL, ny = NULL, lty = 2, col = "gray",lwd = 1)


#Analisis por recluta------------
M=Loo_k_M_Lo_alfa_beta_h[3]
h=Loo_k_M_Lo_alfa_beta_h[7]
tmax=length(L_edad)


if(length(Sel_f)>length(L_edad))
{nsel=length(Sel_f[,1])
Selec=Sel_f[nsel,]}

if(length(Sel_f)==length(L_edad))
{Selec=Sel_f}

ypr_out<-por_recluta_r(tmax,Selec,Madurez_edad,Peso_edad,target,1.0,M,dts)
attach(ypr_out)
Ftar=Ftar[1]
BPRtar=BPRtar[1]
YPRtar=YPRtar[1]

par(mfrow = c(2, 1))

id=which(Y>0)
plot(Fcr[id],Y[id]/max(Y),type="l", col="green", lwd=2, main="Analisis por recluta", xlab="Mortalidad por pesca", ylab="BPR, YPR relativos",ylim = c(0,1))
lines(Fcr[id],B[id]/max(B), col="red", lwd=2)
#lines(Ftar, BPRtar/max(B),type="p", lwd=5)
text(Ftar, 0,Ftar)
grid(nx = NULL, ny = NULL, lty = 2, col = "gray",lwd = 1)
abline(h = target, lty = 2,lwd=1)
abline(v = Ftar, lty = 2,lwd=1)


#Biomasa y F con IC-----------------------------------------------------------


li=Biomasa_desovante[1,]-1.96*Biomasa_desovante[2,]
ls=Biomasa_desovante[1,]+1.96*Biomasa_desovante[2,]
nyrs=length(Biomasa_desovante)

plot(Yrs,Biomasa_desovante[1,],ylim = c(0,max(Biomasa_desovante[1,]+1.96*Biomasa_desovante[2,])*1.01),type="l",
     ylab="Biomasa",xlab="Año",pch = 16,lwd=2,main="Biomasa")
x=c(Yrs,Yrs[seq(length(Yrs),1,-1)])
y=c(li,ls[seq(length(ls),1,-1)])

polygon(x,y,col="#DCDCDC",border="#DCDCDC")  

lines(Yrs,Biomasa_desovante[1,],lwd=2)
B0=mean(Biomasa_desovante[1,]/SPR[1,])
abline(h = target*B0, col = "red",lty = 2,lwd=2)



par(mfrow = c(2, 2))


li=Mort_F[1,]-1.96*Mort_F[2,]
ls=Mort_F[1,]+1.96*Mort_F[2,]
nyrs=length(Mort_F)

plot(Yrs,Mort_F[1,],ylim = c(0,max(Mort_F[1,]+1.96*Mort_F[2,])*1.01),type="l",
     ylab="F",xlab="Año",pch = 16,main="Mortalidad por pesca")
x=c(Yrs,Yrs[seq(length(Yrs),1,-1)])
y=c(li,ls[seq(length(ls),1,-1)])

polygon(x,y,col="#DCDCDC",border="#DCDCDC")  

lines(Yrs,Mort_F[1,],lwd=2)
abline(h = Ftar, col = "red",lty = 2,lwd=2)


#SPR------------------------------------------------------

#par(mfrow = c(2, 1))

Bvirgen=Biomasa_desovante[1,]/SPR[1,]
plot(Yrs,Bvirgen,type="l",ylim=c(0, max(Bvirgen)),xlab="Años",ylab="Biomasa",lwd=2,col="green",main="Biomasa desovante", cex.lab=1.2,cex.main=1.2)
lines(Yrs,Biomasa_desovante[1,],col="black",lwd=2)
legend("bottomleft",c("Sin pesca", "Explotada"),lty =c(1,1,1),col=c("green","black"),lwd=2,bty="n",cex=1.0)
#grid(nx = NULL, ny = NULL, lty = 2, col = "gray",lwd = 1)
abline(h = target*B0, col = "red",lty = 2,lwd=2)
abline(h = B0, col = "green",lty = 2,lwd=2)


plot(Yrs,SPR[1,],type="l",xlab="Años",ylab="SPR",ylim=c(0, max(SPR)),lwd=2,col="black",main="Potencial reproductivo", cex.lab=1.2,cex.main=1.2)

li=SPR[1,]-1.96*SPR[2,]
ls=SPR[1,]+1.96*SPR[2,]

x=c(Yrs,Yrs[seq(length(Yrs),1,-1)])
y=c(li,ls[seq(length(ls),1,-1)])

polygon(x,y,col="#DCDCDC",border="#DCDCDC")  


lines(Yrs,SPR[1,],col="black",lwd=2)
#grid(nx = NULL, ny = NULL, lty = 2, col = "gray",lwd = 1)
abline(h = target, col = "red",lty = 2,lwd=2)



#Kobe---------------------------------------------------------------------
nyrs=length(Yrs)

p_low=1-pnorm(mean(SPR[1,nyrs-3:nyrs]),target,mean(SPR[2,nyrs-3:nyrs]))
p_high=pnorm(mean(Mort_F[1,nyrs-3:nyrs]),Ftar,mean(Mort_F[2,nyrs-3:nyrs]))


BRMS=B0*target
FRMS=Ftar

X0=mean(SPR[1,nyrs-3:nyrs])/target
Y0=mean(Mort_F[1,nyrs-3:nyrs])/FRMS

plot(SPR[1,]/target,Mort_F[1,]/FRMS,pch = 16,ylab="F/Frms",xlab="SPR", xlim = c(0,max(SPR[1,]/target)), ylim = c(0,max(Mort_F[1,]/FRMS)*1.5), 
     type="o",col="white",lty="dashed",main=paste("SPR=",round(X0,2)," (risk_SE=",round(p_low,3),")",
                                                  " F/Frms=",round(Y0,2)," (risk_SP=",round(p_high,3),")"))

polygon(c(0,1,1,0),c(0,0,1,1),col="yellow1") #amarillo
polygon(c(1,1.1*max(SPR[1,]/target),1.1*max(SPR[1,]/target),1),c(0,0,1,1),col="green") #verde
polygon(c(1,1.1*max(SPR[1,]/target),1.1*max(SPR[1,]/target),1),c(1,1,1.5*max(Mort_F[1,]/FRMS),1.5*max(Mort_F[1,]/FRMS)),col="yellow1") #amarillo
polygon(c(0,1,1,0),c(1,1,1.5*max(Mort_F[1,]/FRMS),1.5*max(Mort_F[1,]/FRMS)),col="tomato1") #rojo

#lines(SPR[1,]/target,Mort_F[1,]/FRMS,pch = 16, type="o",col="black",lty="dashed")
#text(SPR[1,]/target*.95,Mort_F[1,]/FRMS,paste(Yrs),cex=0.8)



lines(X0,Y0,type="p",col="blue",pch = 16,cex=2)

cvF=mean(Mort_F[2,nyrs-3:nyrs])/mean(Mort_F[1,nyrs-3:nyrs]);
cvB=mean(SPR[2,nyrs-3:nyrs])/mean(SPR[1,nyrs-3:nyrs]);

arrows(X0,Y0-1.96*cvF*Y0,X0,Y0+1.96*cvF*Y0,
       length = 0.1, code = 3, angle = 90, lwd=1)

arrows(X0-1.96*cvB*X0,Y0,X0+1.96*cvB*X0,Y0,
       length = 0.1, code = 3, angle = 90,lwd=1)

abline(v=.5,lty=2)
box()

#Genera excel--------------------------------------------------------------------------------------

ubi=which(Yrs>2018)

R0=Reclutamientos[1,1]
Variables=data.frame(Año=Yrs[ubi],Biomasa=Biomasa_desovante[1,ubi],R_R0=Reclutamientos[2,ubi]/R0,
                     Fcr=Mort_F[1,ubi], F_Fmrs=Mort_F[1,ubi]/Ftar,B_Brms=Biomasa_desovante[1,ubi]/BRMS,
                     B_B0=Biomasa_desovante[1,ubi]/B0, SPR=SPR[1,ubi])

write.csv(round(Variables,2), 'Var_Pobl.csv',  
          row.names = F,)

write.table(round(Variables,2), 'Var_Pobl.txt',  
          row.names = F,)


Variables2=data.frame(Año=Yrs,Desembarques=Desembarques[1,],CPUErel=CPUE[1,],
                     B.Acustica=Surveys[1,])

write.csv(Variables2, 'Data_usada.csv',  
          row.names = F,)


LL=data.frame(value=round(c(Likeval[1:5], sum(Likeval)),2))
rownames(LL)=c("L_CPUE","L_Cru","L_desemb","Lprop_f","Lprop_cru","LTotal")
write.csv(LL, 'Verosimil.csv', row.names = T)



#Compara-----------------------------

data <-read.rep('Arenque5b.rep')
attach(data)


par(mfrow = c(1, 2))

p_low=1-pnorm(mean(SPR[1,nyrs-3:nyrs]),target,mean(SPR[2,nyrs-3:nyrs]))
p_high=pnorm(mean(Mort_F[1,nyrs-3:nyrs]),Ftar,mean(Mort_F[2,nyrs-3:nyrs]))

Bvirgen=Biomasa_desovante[1,]/SPR[1,]

li=Biomasa_desovante[1,]-1.96*Biomasa_desovante[2,]
ls=Biomasa_desovante[1,]+1.96*Biomasa_desovante[2,]
nyrs=length(Biomasa_desovante)

plot(Yrs,Biomasa_desovante[1,],ylim = c(0,max(round(ls))),type="l",
     ylab="Biomasa",xlab="Año",pch = 16,lwd=2,main="Biomasa")
x=c(Yrs,Yrs[seq(length(Yrs),1,-1)])
y=c(li,ls[seq(length(ls),1,-1)])
polygon(x,y,col="#DCDCDC",border="#DCDCDC")  

lines(Yrs,Biomasa_desovante[1,],lwd=2)
B0=Biomasa_desovante[1,1]/SPR[1,1]
abline(h = target*B0, col = "red",lty = 2,lwd=2)


lines(Yrs,Biomasa_desovante[1,],lwd=2, col="blue")

#Kobe-----------------------------------
nyrs=length(SPR[1,])
X0=mean(SPR[1,nyrs-3:nyrs])/target
Y0=mean(Mort_F[1,nyrs-3:nyrs])/FRMS

plot(SPR[1,]/target,Mort_F[1,]/FRMS,pch = 16,ylab="F/Frms",xlab="SPR", xlim = c(0,max(SPR[1,]/target)), ylim = c(0,max(Mort_F[1,]/FRMS)*1.5), 
     type="o",col="white",lty="dashed",main=paste("SPR=",round(X0,2)," (risk_SE=",round(p_low,3),")",
                                                  " F/Frms=",round(Y0,2)," (risk_SP=",round(p_high,3),")"))

polygon(c(0,1,1,0),c(0,0,1,1),col="yellow1") #amarillo
polygon(c(1,1.1*max(SPR[1,]/target),1.1*max(SPR[1,]/target),1),c(0,0,1,1),col="green") #verde
polygon(c(1,1.1*max(SPR[1,]/target),1.1*max(SPR[1,]/target),1),c(1,1,1.5*max(Mort_F[1,]/FRMS),1.5*max(Mort_F[1,]/FRMS)),col="yellow1") #amarillo
polygon(c(0,1,1,0),c(1,1,1.5*max(Mort_F[1,]/FRMS),1.5*max(Mort_F[1,]/FRMS)),col="tomato1") #rojo

lines(X0,Y0,type="p",col="blue",pch = 16,cex=2)

cvF=mean(Mort_F[2,nyrs-3:nyrs])/mean(Mort_F[1,nyrs-3:nyrs]);
cvB=mean(SPR[2,nyrs-3:nyrs])/mean(SPR[1,nyrs-3:nyrs]);

arrows(X0,Y0-1.96*cvF*Y0,X0,Y0+1.96*cvF*Y0,
       length = 0.1, code = 3, angle = 90, lwd=1)

arrows(X0-1.96*cvB*X0,Y0,X0+1.96*cvB*X0,Y0,
       length = 0.1, code = 3, angle = 90,lwd=1)

abline(v=.5,lty=2)
box()


