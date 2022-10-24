
rm(list=ls()) # erasure all objects
system('./mestockt -ind datos10.dat -nox')  # for model running
#system('./mestockt -nox')  # for model running


source('read.admb.R')
source('read.admbFit.R')
source('por_recluta_r.R')


data <-read.rep('for_R.rep')
attach(data)


target=0.4
#Fig1--------------------------------------------------------------------------
barplot(Desembarques[1,]~Yrs, cex.lab=1.5, xlab="Año",ylab="Toneladas", col = "gray",ylim = c(0,max(Desembarques[1,])*1.1), 
        main="Desembarques", cex.main = 1.5, major.ticks = "years")
box()
grid(nx = NULL, ny = NULL, lty = 2, col = "gray",lwd = 1)


#Figs--------------------------------------------------------------------------

par(mfrow = c(3, 2))
sim=20
  
max=max(CPUE);CPUE[which(CPUE==0)]=NaN
plot(Yrs,CPUE[1,],ylab="Indice",xlab="Año",ylim = c(0,max*1.01),main="CPUE",
     pch=sim,lwd=2,cex.lab=1.5,cex=2,cex.main=2,cex.axis=1.5)
lines(Yrs,CPUE[2,],col="red",lwd=2)
grid(nx = NULL, ny = NULL, lty = 2, col = "gray",lwd = 1)

plot(Yrs,Desembarques[1,],ylab="Toneladas",xlab="Año",main="Desembarque",ylim = c(0,max(Desembarques)*1.01),
     pch=sim,lwd=2,cex.lab=1.5,cex=2,cex.main=2,cex.axis=1.5)
lines(Yrs,Desembarques[2,],col="red",lwd=2)
grid(nx = NULL, ny = NULL, lty = 2, col = "gray",lwd = 1)


max=max(Surveys);Surveys[which(Surveys==0)]=NaN
plot(Yrs,Surveys[1,],ylab="Indice",xlab="Año",ylim = c(0,max*1.01),main="Crucero",
     pch=sim,lwd=2,cex.lab=1.5,cex=2,cex.main=2,cex.axis=1.5)
lines(Yrs,Surveys[2,],col="red",lwd=2)
grid(nx = NULL, ny = NULL, lty = 2, col = "gray",lwd = 1)

plot(Lmed_flo[1,],Lmed_flo[2,],ylab="Talla promedio",xlab="Año",main="Talla promedio flota",
     pch=sim,lwd=2,cex.lab=1.5,cex=2,cex.main=2,cex.axis=1.5)
lines(Lmed_flo[1,],Lmed_flo[3,],lwd=2,col="red")
grid(nx = NULL, ny = NULL, lty = 2, col = "gray",lwd = 1)

plot(Lmed_srv[1,],Lmed_srv[2,],ylab="Talla promedio",xlab="Año",main="Talla promedio Crucero",
     pch=sim,lwd=2,cex.lab=1.5,cex=2,cex.main=2,cex.axis=1.5)
lines(Lmed_srv[1,],Lmed_srv[3,],lwd=2,col="red")
grid(nx = NULL, ny = NULL, lty = 2, col = "gray",lwd = 1)


#----------------------------------------------------------------------
n=length(Lmed_flo[1,])
filas=4 
cols=5 
dl=0.5*(Tallas[2]-Tallas[1])

par(mfrow = c(filas, cols))
for (i in 1:n)
{
  plot(Tallas-dl,Frecs_capt_obs[i,],main=paste(Lmed_flo[1,i]),type="s",lwd=3, col="gray",cex.main = 1.2, ylab="", xlab="Talla",cex.lab=1.2,
               ylim=c(0,max(c(Frecs_capt_obs,Frecs_capt_pred))))
  lines(Tallas,Frecs_capt_pred[i,],col="red",lwd=2)
}


#----------------------------------------------------------------------
n=length(Lmed_srv[1,])
filas=4
cols=round(n/filas)
if (cols==0)
{cols=1}

par(mfrow = c(filas, cols))

for (i in 1:n)
{
  
  plot(Tallas-dl,Frecs_srv_obs[i,],main=paste(c(Lmed_srv[1,i]),"c"),type="s",lwd=3, col="gray",cex.main = 1.2,cex.lab=1.2, 
       ylab="", xlab="Talla",ylim=c(0,max(c(Frecs_srv_obs,Frecs_srv_pred))))
  lines(Tallas,Frecs_srv_pred[i,],col="red",lwd=2)
  
}


#----------------------------------------------------------------------
par(mfrow = c(1, 2))


plot(Tallas-dl, Frec_marg_flo[1,],col="gray",type="s",lwd=5,xlab="Talla",ylab="Proporcion",main="Flota")
lines(Tallas,Frec_marg_flo[2,],col="red",lwd=2)
grid(nx = NULL, ny = NULL, lty = 2, col = "gray",lwd = 1)

plot(Tallas-dl, Frec_marg_srv[1,],col="gray",type="s",lwd=5,xlab="Talla",ylab="Proporcion",main="Cruceros")
lines(Tallas,Frec_marg_srv[2,],col="red",lwd=2)
grid(nx = NULL, ny = NULL, lty = 2, col = "gray",lwd = 1)
#-------------------------------------------------------------------------
par(mfrow = c(2, 2))

plot(Frecs_capt_obs,Frecs_capt_pred,xlab="Observada",ylab ="Predicha", main="Frecs tallas flota",pch=20,cex=1.5)
lines(Frecs_capt_obs,Frecs_capt_obs,col="red")
grid(nx = NULL, ny = NULL, lty = 2, col = "gray",lwd = 1)


plot(Frecs_srv_obs,Frecs_srv_pred,xlab="Observada",ylab ="Predicha", main="Frecs tallas cruceros",pch=20,cex=1.5)
lines(Frecs_capt_obs,Frecs_capt_obs,col="red")
grid(nx = NULL, ny = NULL, lty = 2, col = "gray",lwd = 1)

plot(CPUE[1,],CPUE[2,],xlab="Observada",ylab ="Predicha", main="CPUE",pch=20,cex=1.5)
lines(CPUE[1,],CPUE[1,],col="red")
grid(nx = NULL, ny = NULL, lty = 2, col = "gray",lwd = 1)

plot(Surveys[1,],Surveys[2,],xlab="Observada",ylab ="Predicha", main="Crucero",pch=20,cex=1.5)
lines(Surveys[1,],Surveys[1,],col="red")
grid(nx = NULL, ny = NULL, lty = 2, col = "gray",lwd = 1)
#----------------------------------------------------------------------
par(mfrow = c(1, 2))
matplot(t(Sel_f),type="l",lwd=3,lty = 1,xlab="Edad relativa",ylab="Proporcion",main="Selectividad Flota")
lines(seq(1,length(Sel_f[1,])),Madurez_edad,col="blue",lty = 2,lwd = 2)
grid(nx = NULL, ny = NULL, lty = 2, col = "gray",lwd = 1)
legend("bottomright",paste(seq(1,length(Sel_f[,1]))),lty =c(1,1,1),col=seq(1,length(Sel_f[,1])),lwd=2)


plot(seq(1,length(Sel_f[1,])),Sel_srv,lwd=3,type="l",ylim=c(0, 1),xlab="Edad relativa",ylab="Proporcion",main="Selectividad Crucero")
lines(seq(1,length(Sel_f[1,])),Madurez_edad,col="blue",lty = 2,lwd = 2)
grid(nx = NULL, ny = NULL, lty = 2, col = "gray",lwd = 1)
#-------------------------------------------------------------------------
par(mfrow = c(2, 2))
qqnorm(Lmed_flo[2,]-Lmed_flo[3,], main="QQ plot - Talla promedio flota",cex=1.5); qqline(Lmed_flo[2,]-Lmed_flo[3,], col="red", lwd=2)
grid(nx = NULL, ny = NULL, lty = 2, col = "gray",lwd = 1)

qqnorm(CPUE[1,]-CPUE[2,], main="QQ plot - CPUE flota",cex=1.5); qqline(CPUE[1,]-CPUE[2,], col="red", lwd=2)
grid(nx = NULL, ny = NULL, lty = 2, col = "gray",lwd = 1)

qqnorm(Frecs_capt_obs-Frecs_capt_pred, main="QQ plot - Frecs tallas flota",cex=1.5); qqline(Frecs_capt_obs-Frecs_capt_pred, col="red", lwd=2)
grid(nx = NULL, ny = NULL, lty = 2, col = "gray",lwd = 1)

qqnorm(Frecs_srv_obs-Frecs_srv_pred, main="QQ plot - Frecs tallas crucero",cex=1.5); qqline(Frecs_srv_obs-Frecs_srv_pred, col="red", lwd=2)
grid(nx = NULL, ny = NULL, lty = 2, col = "gray",lwd = 1)


#----------------------------------------------------------------------
par(mfrow = c(2, 2))

plot(Yrs,Biomasa_total,type="l",xlab="Años",ylab="Biomasa (miles)",ylim=c(0, max(Biomasa_total)),lwd=2,col="black",
     cex.lab=1.2,cex.axis=1.2)
lines(Yrs,Biomasa_explotable,col="red",lwd=2)
lines(Yrs,Biomasa_desovante[1,],col="green",lwd=3)
legend("topright",c("Total", "Explotable","Desovante"),lty =c(1,1,1),col=c("black","red","green"),lwd=2,cex=1.0)
grid(nx = NULL, ny = NULL, lty = 2, col = "gray",lwd = 1)

sdR=Reclutamientos[3,]
li=Reclutamientos[2,]-1.96*sdR
ls=Reclutamientos[2,]+1.96*sdR
nyrs=length(Biomasa_desovante)
plot(Yrs,Reclutamientos[2,],ylim = c(0,max(Reclutamientos[2,]+1.96*sdR)*1.01),type="l",
     ylab="Reclutamientos",xlab="Año",pch = 16,cex=1,lwd=2,cex.lab=1.2,cex.main=2,cex.axis=1.2)
x=c(Yrs,Yrs[seq(length(Yrs),1,-1)])
y=c(li,ls[seq(length(ls),1,-1)])
polygon(x,y,col="#DCDCDC",border="#DCDCDC")
lines(Yrs,Reclutamientos[2,],lwd="2")
lines(Yrs,Reclutamientos[1,],lwd="2",col="red")
grid(nx = NULL, ny = NULL, lty = 2, col = "gray",lwd = 1)

plot(Yrs,Dev_log_R,type="l",xlab="Años",ylab="Desvio log_R",lwd=2,col="black",cex.lab=1.2,cex.main=2,cex.axis=1.2)
grid(nx = NULL, ny = NULL, lty = 2, col = "gray",lwd = 1)
abline(h=0,col="red",lwd = 2)

plot(Yrs,Mort_F[1,],type="l",xlab="Años",ylab="Mortalidad por pesca",lwd=2,col="black",cex.lab=1.2,cex.axis=1.2)
grid(nx = NULL, ny = NULL, lty = 2, col = "gray",lwd = 1)

#--------------------------------------------------------------------
# Analisis por recluta
tmax=length(Sel_f[1,])
nsel=length(Sel_f[,1])
ypr_out<-por_recluta_r(tmax,Sel_f[nsel,],Madurez_edad,Peso_edad,target,h,M,dts)
attach(ypr_out)
Ftar=Ftar[1]
BPRtar=BPRtar[1]
YPRtar=YPRtar[1]

par(mfrow = c(1, 1))
plot(Fcr,Y/max(Y),type="l", col="green", lwd=2, main="Analisis por recluta", xlab="Mortalidad por pesca", ylab="BPR, YPR relativos",
     cex.lab=1.5,cex.main=2)
lines(Fcr,B/max(B), col="red", lwd=2)
lines(Ftar, BPRtar/max(B),type="p", lwd=5)
text(Ftar, 0,Ftar,cex=1.5)
grid(nx = NULL, ny = NULL, lty = 2, col = "gray",lwd = 1)
abline(h = target, lty = 2,lwd=1)
abline(v = Ftar, lty = 2,lwd=1)
#--------------------------------------------------------------------
par(mfrow = c(1, 2))

li=Biomasa_desovante[1,]-1.96*Biomasa_desovante[2,]
ls=Biomasa_desovante[1,]+1.96*Biomasa_desovante[2,]
nyrs=length(Biomasa_desovante)

plot(Yrs,Biomasa_desovante[1,],ylim = c(0,max(Biomasa_desovante[1,]+1.96*Biomasa_desovante[2,])*1.01),type="l",
     ylab="Biomasa",xlab="Año",pch = 16,cex=1,lwd=2,main="Biomasa", cex.lab=1.5,cex.main=1.5)
x=c(Yrs,Yrs[seq(length(Yrs),1,-1)])
y=c(li,ls[seq(length(ls),1,-1)])

polygon(x,y,col="#DCDCDC",border="#DCDCDC")  

lines(Yrs,Biomasa_desovante[1,],lwd=2)
B0=mean(Biomasa_desovante[1,]/SPR[1,])
abline(h = target*B0, col = "red",lty = 2,lwd=2)



li=Mort_F[1,]-1.96*Mort_F[2,]
ls=Mort_F[1,]+1.96*Mort_F[2,]
nyrs=length(Mort_F)

plot(Yrs,Mort_F[1,],ylim = c(0,max(Mort_F[1,]+1.96*Mort_F[2,])*1.01),type="l",
     ylab="F",xlab="Año",pch = 16,cex=1,lwd=2,main="Mortalidad por pesca",cex.lab=1.5,cex.main=1.5)
x=c(Yrs,Yrs[seq(length(Yrs),1,-1)])
y=c(li,ls[seq(length(ls),1,-1)])

polygon(x,y,col="#DCDCDC",border="#DCDCDC")  

lines(Yrs,Mort_F[1,],lwd=2)
abline(h = Ftar, col = "red",lty = 2,lwd=2)


#--------------------------------------------------------------------

par(mfrow = c(1, 2))
plot(Yrs,SPR[1,],type="l",xlab="Años",ylab="%B0",ylim=c(0, max(SPR)),lwd=2,col="black",main="Potencial reproductivo (B/B0)", cex.lab=1.5,cex.main=1.5)
lines(Yrs,SPR[2,],col="green",lwd=2)
legend("bottomleft",c("Largo plazo", "Dinámico"),lty =c(1,1,1),col=c("black","green"),lwd=2)
grid(nx = NULL, ny = NULL, lty = 2, col = "gray",lwd = 1)
abline(h = target, col = "red",lty = 2,lwd=2)

Bvirgen=Biomasa_desovante[1,]/SPR[2,]
plot(Yrs,Bvirgen,type="l",ylim=c(0, max(Bvirgen)),xlab="Años",ylab="Biomasa",lwd=2,col="green",main="Biomasa desovante", cex.lab=1.5,cex.main=1.5)
lines(Yrs,Biomasa_desovante[1,],col="black",lwd=2)
legend("bottomleft",c("Virgen", "Explotada"),lty =c(1,1,1),col=c("green","black"),lwd=2)
grid(nx = NULL, ny = NULL, lty = 2, col = "gray",lwd = 1)
abline(h = target*B0, col = "red",lty = 2,lwd=2)
abline(h = B0, col = "green",lty = 2,lwd=2)

#-----------------------------------------------------------------------------
par(mfrow = c(1, 1))
BRMS=B0*target
FRMS=Ftar
nyrs=length(Yrs)
  
plot(SPR[1,]/target,Mort_F[1,]/FRMS,pch = 16,ylab="F/Frms",xlab="B/Brms", xlim = c(0,max(SPR[1,]/target)), ylim = c(0,max(Mort_F[1,]/FRMS)*1.5), 
     type="o",col="black",lty="dashed",main=paste("B/Brms=",round(SPR[1,][length(Yrs)]/target,2),
                                                  " F/Frms=",round(Mort_F[1,][length(Yrs)]/FRMS,2)),cex.main=1.5,cex.lab=1.5)

polygon(c(0,1,1,0),c(0,0,1,1),col="yellow1") #amarillo
polygon(c(1,1.1*max(SPR[1,]/target),1.1*max(SPR[1,]/target),1),c(0,0,1,1),col="green") #verde
polygon(c(1,1.1*max(SPR[1,]/target),1.1*max(SPR[1,]/target),1),c(1,1,1.5*max(Mort_F[1,]/FRMS),1.5*max(Mort_F[1,]/FRMS)),col="yellow1") #amarillo
polygon(c(0,1,1,0),c(1,1,1.5*max(Mort_F[1,]/FRMS),1.5*max(Mort_F[1,]/FRMS)),col="tomato1") #rojo

lines(SPR[1,]/target,Mort_F[1,]/FRMS,pch = 16, type="o",col="black",lty="dashed")
lines(SPR[1,nyrs]/target,Mort_F[1,][length(Yrs)]/FRMS,type="p",col="blue",pch = 16,cex=2)
text(SPR[1,]/target*.95,Mort_F[1,]/FRMS,paste(Yrs),cex=0.8)


X0=Biomasa_desovante[1,nyrs]/BRMS
Y0=Mort_F[1,nyrs]/FRMS
cvF=Mort_F[2,nyrs]/Mort_F[1,nyrs];
cvB=Biomasa_desovante[2,nyrs]/Biomasa_desovante[1,nyrs];

arrows(X0,Y0-1.96*cvF*Y0,X0,Y0+1.96*cvF*Y0,
       length = 0.15, code = 3, angle = 90, lwd=2, col="blue")

arrows(X0-1.96*cvB*X0,Y0,X0+1.96*cvB*X0,Y0,
       length = 0.15, code = 3, angle = 90,lwd=2, col="blue")

box()


# Curva probabilidad normal------------------
par(mfrow = c(1, 2))

p_low=1-pnorm(SPR[1,nyrs],target,cvB*SPR[1,nyrs])
p_high=pnorm(Mort_F[1,nyrs],Ftar,Mort_F[2,nyrs])

eje=seq(0,1,by=0.005)
d1=dnorm(eje,SPR[1,nyrs],cvB*SPR[1,nyrs])
p1=pnorm(eje,SPR[1,nyrs],cvB*SPR[1,nyrs])

color=rgb(1, 0.27, 0, 0.3)
plot(eje/target,d1/max(d1),type="l", ylab="Densidad", xlab="% rms",cex.lab=1.5,
     main="Densidad de probabilidad", cex.main=1.5,col="red") 
polygon(c(0, eje/target, SPR[1,nyrs]/target), c(0, d1/max(d1), 0), col = color)
grid(nx = NULL, ny = NULL, lty = 2, col = "gray",lwd = 1)


eje2=seq(0,2*Mort_F[1,nyrs],by=0.01)
d2=dnorm(eje2,Mort_F[1,nyrs],Mort_F[2,nyrs])
p2=pnorm(eje2,Mort_F[1,nyrs],Mort_F[2,nyrs])


color=rgb(0.6, 0.96, 1, 0.3)
lines(eje2/Ftar,d2/max(d2),ylab="Densidad",col="blue")
polygon(c(0, eje2/Ftar, Mort_F[1,nyrs]/Ftar), c(0, d2/max(d2), 0), col = color)
abline(v=1,  col = "black",lty = 2,lwd=2)

text(2,0.9,"B/Brms",col="red",cex=1.2)
text(2,0.8,"F/Frms",col="blue",cex=1.2)

plot(eje/target,p1,type="l",col="red",lwd=2,ylab="Riesgo", xlab="% rms",cex.lab=1.5,
     main="Probabilidad",cex.main=1.5)
lines(eje2/Ftar,p2,col="blue",lwd=2)
abline(v=1,  col = "black",lty = 2,lwd=2)
grid(nx = NULL, ny = NULL, lty = 2, col = "gray",lwd = 1)
text(2.0,0.3,paste("p(B<Brms)=",round(p_low,3)),col="red",cex=1.2)
text(2.0,0.2,paste("p(F>Frms)=",round(p_high,3)),col="blue",cex=1.2)

#---------------------------------
par(mfrow = c(2, 2))
nanos=length(Yrs)
nysim=length(Bio_proy[,1])
yproy=seq(Yrs[length(Yrs)]+1,Yrs[length(Yrs)]+nysim)
vecto=seq(nyrs-9,nyrs)

plot(Yrs[vecto],Biomasa_desovante[1,vecto]/B0,type="l",xlim=c(min(Yrs[vecto]),max(yproy)),
     ylim=c(0,max(Bio_proy/B0)),lwd="2",lty=1,col="blue",xlab="Año", ylab="B/B0",
     main="Biomasa",cex.lab=1.5,cex.axis=1.5,cex.main=1.5)
abline(h=target,  col = "red",lty = 2,lwd=2)
matlines(yproy,Bio_proy/B0,lwd="2",lty=1)
x=yproy[nysim]
y=Bio_proy[nysim,]*0.98/B0
text(x,y,paste(round(Mult_F,2)),cex=1.1)
grid(nx = NULL, ny = NULL, lty = 2, col = "gray",lwd = 1)


R0=Reclutamientos[1,1]
RMS=YRMS[1]*R0

plot(Yrs[vecto],Desembarques[1,vecto],type="l",xlim=c(min(Yrs[vecto]),max(yproy)),
     ylim=c(0,max(Desembarques[1,vecto])),lwd="2",lty=1,col="blue",xlab="Año", ylab="Capturas",
     main="Capturas",cex.lab=1.5,cex.axis=1.5,cex.main=1.5)
abline(h=RMS,  col = "red",lty = 2,lwd=2)
matlines(yproy,Capt_proy,lwd="2",lty=1)
x=yproy[nysim]
y=Capt_proy[nysim,]*0.98
text(x,y,paste(round(Mult_F,2)),cex=1.1)
grid(nx = NULL, ny = NULL, lty = 2, col = "gray",lwd = 1)


matplot(yproy, F_proy,type="l",lty=1,lwd=2,ylim=c(0,max( F_proy)), xlab="Año", ylab="Mortalidad por pesca", 
        main="Mort por pesca",cex.lab=1.5,cex.axis=1.5,cex.main=1.5)
abline(h=FRMS,  col = "red",lty = 2,lwd=2)
y=F_proy[nysim,]*0.98
text(x,y,paste(round(Mult_F,2)),cex=1.1)
grid(nx = NULL, ny = NULL, lty = 2, col = "gray",lwd = 1)

#-------------------------------------------------------------------------------------------
par(mfrow = c(1, 2))
barplot(Red_stock[1,]~Mult_F,ylim=c(0,1.1),xlab="Mult del esfuerzo", ylab="B/B0", cex.lab=1.3, cex.axis=1.2, main="Reducción del stock", 
        cex.main=1.3)
abline(h=target,  col = "red",lty = 2,lwd=2)
box()


riesgo=1-pnorm(Red_stock[1,],target,Red_stock[2,])
riesgo_crash=1-pnorm(Red_stock[1,],0.5*target,Red_stock[2,])

plot(Mult_F,riesgo,style="p",ylim=c(0,1.1),xlab="Mult del esfuerzo", ylab="p(B<Brms)", main="Riesgo largo plazo",cex.lab=1.2, 
     cex.axis=1, cex.lab=1.3,cex.main=1.3)
lines(Mult_F,riesgo,col="red",lwd=2)
text(Mult_F*1.02,riesgo,round(riesgo,3),cex=1.3)
grid(nx = NULL, ny = NULL, lty = 2, col = "gray",lwd = 1)



#------------------------------------------------------------------------------------------------

Variables=data.frame(Mult_Eff=Mult_F,B_B0=Red_stock[1,],Desv_std=Red_stock[2,],Riesgo=riesgo,Riesgo_colapso=riesgo_crash,Captura_cp=Capt_proy[1,], 
                     Captura_lp=colMeans(Capt_proy[seq(nysim,nysim-4,-1),]))


write.csv(Variables, 'Decision.csv', append = FALSE, sep = " ", dec = ".",
          row.names = F, col.names = T)



Variables=data.frame(years=Yrs,Biomasa=Biomasa_desovante[1,],Reclutamiento=Reclutamientos[2,],
                     Fcr=Mort_F[1,], F_Fmrs=Mort_F[1,]/Ftar,B_Brms=Biomasa_desovante[1,]/BRMS)



write.csv(Variables, 'Var_Pobl.csv', append = FALSE, sep = " ", dec = ".",
          row.names = F, col.names = T)




