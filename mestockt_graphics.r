
rm(list=ls()) # erasure all objects
library(areaplot)
library(MASS)
source('read.admb.R') 
source('read.admbFit.R')
source('por_recluta_r.R')

name="datos"  # nombre del archivo sin extensión 


system(paste('mestockt -ind ',name,'.dat -nox',sep=""))  # for model running
shell(paste('copy for_R.rep ',name,'.rep',sep=""))  # for model running



data <-read.rep('for_R.rep')


attach(data)

target=0.4

par(mfrow = c(2, 2))
sim=20


plot(Yrs, Desembarques[1,]/1000, cex.lab=1, xlab="Año",ylab="Toneladas (miles)",ylim = c(0,max(Desembarques[1,]/1000)*1.1), 
        main="Desembarques", pch=sim,type="b",cex=1.5)
lines(Yrs, Desembarques[2,]/1000,col="red",lwd=2)
grid()

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
grid()


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
cv_acus_post=sd(log(Surveys[1,ubi])-log(Surveys[2,ubi]))
grid()


sim=20
min=min(Lmed_flo[2:3,])
max=max(Lmed_flo[2:3,])
plot(Lmed_flo[1,],Lmed_flo[2,],ylab="Talla promedio",xlab="Año",main="Talla promedio flota",ylim = c(min,max),
     pch=sim,type="b",cex=1.5)

X0=Lmed_flo[1,]
Y0=Lmed_flo[3,]
cvF=0.02
li=Y0-1.96*cvF*Y0
ls=Y0+1.96*cvF*Y0

x=c(X0,X0[seq(length(X0),1,-1)])
y=c(li,ls[seq(length(ls),1,-1)])
polygon(x,y,col="#DCDCDC",border="#DCDCDC")


lines(Lmed_flo[1,],Lmed_flo[3,],lwd=2,col="red")
lines(Lmed_flo[1,],Lmed_flo[2,],pch=sim,type="b",cex=1.5)




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
  plot(Tallas-dl,Frecs_capt_obs[i,],main=paste(Lmed_flo[1,i]),type="s",lwd=3, col="gray",ylab="", xlab="Talla",
               ylim=c(0,max(c(Frecs_capt_obs,Frecs_capt_pred))))
  lines(Tallas,Frecs_capt_pred[i,],col="red",lwd=2)
  
}


#Comps_tallas_s----------------------------------------------------------------------

n=length(Lmed_srv[1,])
filas=2
cols=round(n/filas)
if (cols==0)
{cols=1}

par(mfcol = c(2, 2))

for (i in 1:n)
{

  plot(Tallas-dl,Frecs_srv_obs[i,],main=paste(c(Lmed_srv[1,i]),"c"),type="s",lwd=3, col="gray",
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

#Crecimiento------------
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

par(mfrow = c(2, 1))

R0=Reclutamientos[1,1]
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





#Analisis por recluta------------
M=Loo_k_Lo_alfa_beta_M_h_dt[6]
h=Loo_k_Lo_alfa_beta_M_h_dt[7]
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

#Biomasa y F con IC-----------------------------------------------------------


par(mfrow = c(2, 2))

li=Biomasa_desovante[1,]-1.96*Biomasa_desovante[2,]
ls=Biomasa_desovante[1,]+1.96*Biomasa_desovante[2,]
nyrs=length(Biomasa_desovante)

plot(Yrs,Biomasa_desovante[1,],ylim = c(0,max(Biomasa_desovante[1,]+1.96*Biomasa_desovante[2,])*1.01),type="l",
     ylab="Biomasa",xlab="Año",pch = 16,lwd=2,main="Biomasa")
x=c(Yrs,Yrs[seq(length(Yrs),1,-1)])
y=c(li,ls[seq(length(ls),1,-1)])

polygon(x,y,col="#DCDCDC",border="#DCDCDC")  

lines(Yrs,Biomasa_desovante[1,],lwd=2)
B0=B[1]*R0
abline(h = R0*BPRtar, col = "red",lty = 2,lwd=2)


Bvirgen=Biomasa_desovante[1,]/SPR[1,]
plot(Yrs,Bvirgen,type="l",ylim=c(0, max(Bvirgen)),xlab="Años",ylab="Biomasa",lwd=2,col="green",main="Biomasa desovante", cex.lab=1.2,cex.main=1.2)
lines(Yrs,Biomasa_desovante[1,],col="black",lwd=2)
legend("bottomleft",c("Sin pesca", "Explotada"),lty =c(1,1,1),col=c("green","black"),lwd=2,bty="n",cex=1.0)
#grid(nx = NULL, ny = NULL, lty = 2, col = "gray",lwd = 1)
abline(h = target*B0, col = "red",lty = 2,lwd=2)
abline(h = B0, col = "green",lty = 2,lwd=2)


plot(Yrs,SPR[1,],type="l",xlab="Años",ylab="SPR, B/B0",ylim=c(0, max(SPR)),lwd=2,col="black",main="Biomasa relativa", cex.lab=1.2,cex.main=1.2)


li=SPR[1,]-1.96*SPR[2,]
ls=SPR[1,]+1.96*SPR[2,]

x=c(Yrs,Yrs[seq(length(Yrs),1,-1)])
y=c(li,ls[seq(length(ls),1,-1)])

polygon(x,y,col="#DCDCDC",border="#DCDCDC")  

lines(Yrs,Biomasa_desovante[1,]/B0,col="green",lwd=2)

lines(Yrs,SPR[1,],col="black",lwd=2)
#grid(nx = NULL, ny = NULL, lty = 2, col = "gray",lwd = 1)
abline(h = target, col = "red",lty = 2,lwd=2)
legend("topright",c("SPR","B/B0"),col=c("black","green"),lty=1,lwd=2,bty="n")


#Kobe---------------------------------------------------------------------
par(mfrow = c(1, 1))

nyrs=length(Yrs)
BRMS=B0*target
FRMS=Ftar

p_low=1-pnorm((Biomasa_desovante[1,nyrs]),BRMS,(Biomasa_desovante[2,nyrs]))
p_high=pnorm((Mort_F[1,nyrs]),Ftar,(Mort_F[2,nyrs]))


X0=Biomasa_desovante[1,nyrs]/BRMS
Y0=Mort_F[1,nyrs]/FRMS


plot(Biomasa_desovante[1,]/BRMS,Mort_F[1,]/FRMS,pch = 16,ylab="F/Frms",xlab="B/Brms", xlim = c(0,max(Biomasa_desovante[1,]/BRMS)), ylim = c(0,max(Mort_F[1,]/FRMS)*1.5), 
     type="o",col="white",lty="dashed",main=paste("B/Brms=",round(X0,2)," (risk_SE=",round(p_low,2),")",
                                                  " F/Frms=",round(Y0,2)," (risk_SP=",round(p_high,2),")"))

polygon(c(0,1,1,0),c(0,0,1,1),col="yellow1") #amarillo
polygon(c(1,1.1*max(Biomasa_desovante[1,]/BRMS),1.1*max(Biomasa_desovante[1,]/BRMS),1),c(0,0,1,1),col="green") #verde
polygon(c(1,1.1*max(Biomasa_desovante[1,]/BRMS),1.1*max(Biomasa_desovante[1,]/BRMS),1),c(1,1,1.5*max(Mort_F[1,]/FRMS),1.5*max(Mort_F[1,]/FRMS)),col="yellow1") #amarillo
polygon(c(0,1,1,0),c(1,1,1.5*max(Mort_F[1,]/FRMS),1.5*max(Mort_F[1,]/FRMS)),col="tomato1") #rojo

lines(Biomasa_desovante[1,]/BRMS,Mort_F[1,]/FRMS,pch = 16, type="o",col="darkgray",lty="dashed")
text(Biomasa_desovante[1,]/BRMS*.95,Mort_F[1,]/FRMS,paste(Yrs),cex=0.8)



lines(X0,Y0,type="p",col="blue",pch = 16,cex=2)

cvF=Mort_F[2,nyrs]/Mort_F[1,nyrs];
cvB=Biomasa_desovante[2,nyrs]/Biomasa_desovante[1,nyrs];

arrows(X0,Y0-1.96*cvF*Y0,X0,Y0+1.96*cvF*Y0,
       length = 0.1, code = 3, angle = 90, lwd=1)

arrows(X0-1.96*cvB*X0,Y0,X0+1.96*cvB*X0,Y0,
       length = 0.1, code = 3, angle = 90,lwd=1)

abline(v=.5,lty=2)
box()

#CPUErms-------

R0=Reclutamientos[2,1]
riesgo=1-pnorm(Red_stock[1,],target,Red_stock[2,])
riesgo_crash=1-pnorm(Red_stock[1,],0.5*target,Red_stock[2,])
nanos=length(Yrs)
nysim=length(Bio_proy[,1])



png("Fig14.png")

par(mfrow = c(1, 2))

Brms=BPRtar*R0
RMS=YPRtar*R0
CPUErms=q_flo*BEtar[1]*Reclutamientos[1,1]


ny=length(Yrs)
ubi=which(CPUE[1,]>0)
max=max(CPUE[1,]/CPUErms)
rho=0.5*(CPUE[1,ny-1]+CPUE[1,ny])/CPUErms
phi=0.5*(Desembarques[1,ny-1]+Desembarques[1,ny])/RMS


plot(Yrs[ubi],CPUE[1,ubi]/CPUErms,ylab="CPUE/CPUErms",xlab="A?o",ylim = c(0,max*1.01),
     type="l",main=paste("CPUErms=",paste(round(CPUErms,2))," rho=",paste(round(rho,2))))
abline(h=1,col="red",lty = 2, lwd=1)


max=max(Desembarques[1,]/RMS)
plot(Yrs,Desembarques[1,]/RMS,ylab="Desembarque/RMS",xlab="A?o",ylim = c(0,max*1.01),
     type="l",main=paste("RMS=",paste(round(RMS,0)), "phi=",paste(round(phi,2))))
abline(h=1,col="red",lty = 2, lwd=1)

dev.off()
#Genera excel--------------------------------------------------------------------------------------

ubi=which(Yrs>2000)

R0=Reclutamientos[1,1]
Variables=data.frame(Año=Yrs[ubi],Biomasa=Biomasa_desovante[1,ubi],R_R0=Reclutamientos[2,ubi]/R0,
                     Fcr=Mort_F[1,ubi], F_Fmrs=Mort_F[1,ubi]/Ftar,B_Brms=Biomasa_desovante[1,ubi]/BRMS,
                     B_B0=Biomasa_desovante[1,ubi]/B0, SPR=SPR[1,ubi])

write.csv(round(Variables,2), 'Var_Pobl.csv',row.names = FALSE)

write.table(round(Variables,2), 'Var_Pobl.txt',  
          row.names = FALSE)


Variables2=data.frame(Año=Yrs,Desembarques=Desembarques[1,],CPUErel=CPUE[1,],
                     B.Acustica=Surveys[1,])

write.csv(Variables2, 'Data_usada.csv',  
          row.names = FALSE)


LL=data.frame(value=round(c(Likeval[1:5], sum(Likeval)),2))
rownames(LL)=c("L_CPUE","L_Cru","L_desemb","Lprop_f","Lprop_cru","LTotal")
write.csv(LL, 'Verosimil.csv', row.names = TRUE)








