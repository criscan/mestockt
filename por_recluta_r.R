por_recluta_r<-function(Tmax,Sel,Madage,Wage,tar,h,M,dt){

  age=c(1:Tmax)

  N=seq(1,Tmax)
  N0=N
  Ntar=N
  Ctar=N
  
  Fcr=seq(0.001,5*M,0.01)
  Eprom=Fcr
  B=Fcr
  Y=Fcr
  R=Fcr
  
  # Calcula B0
  for (i in 2:Tmax) {
    
    N0[i]=N0[i-1]*exp(-M)
  }
  N0[i]=N0[i]/(1-exp(-M))
  B0=sum(N0*Madage*Wage*exp(-dt*M))
  alfa=4*h/(5*h-1);
  beta=(1-h)/(5*h-1)*B0;
  aux=0
  
  for (j in 1:length(Fcr)) {
    
    F=Fcr[j]*Sel
    Z=F+M
    
    
    for (i in 2:Tmax) {
      
      N[i]=N[i-1]*exp(-Z[i-1])
    }
    N[i]=N[i]/(1-exp(-Z[i]))
    
    C=N*F/Z*(1-exp(-Z))
    
    B[j]=alfa*sum(N*Madage*Wage*exp(-dt*Z))-beta
    R[j]=alfa*B[j]/(beta+B[j])
    Y[j]=R[j]*sum(C*Wage)

    
    if (B[j]/B[1]>0.99*tar){Ftar=Fcr[j]
    BPRtar=B[j]
    YPRtar=Y[j]
    }
    
    if (Y[j]>aux){
      BRMS=B[j]
      YRMS=Y[j]
      FRMS=Fcr[j]
      }

    aux=Y[j]
    
  }
  

  outputs=data.frame(Fcr, B, Y, Ftar, BPRtar, YPRtar, FRMS, BRMS, YRMS)

  return(outputs)  

  
}

