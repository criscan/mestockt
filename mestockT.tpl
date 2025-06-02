GLOBALS_SECTION
 #include <admodel.h>
 #include <stdio.h>
 #include <time.h>
 time_t start,finish;
 long hour,minute,second;
 double elapsed_time;
 ofstream mcmc_report("mcmc.csv");

TOP_OF_MAIN_SECTION
 time(&start);
 arrmblsize = 90000000; 
 gradient_structure::set_GRADSTACK_BUFFER_SIZE(1.e7); 
 gradient_structure::set_CMPDIF_BUFFER_SIZE(1.e7); 
 gradient_structure::set_MAX_NVAR_OFFSET(5000); 
 gradient_structure::set_NUM_DEPENDENT_VARIABLES(5000); 


DATA_SECTION

 init_int ymin
 init_int ymax

 int ntime

 !!ntime=ymax-ymin+1;

 init_int nedades
 init_number minedad
 init_int ntallas

 init_matrix mdatos(1,ntime,1,9)
 init_vector Tallas(1,ntallas)

 init_int N_ftc
 !!if(N_ftc>0)
 init_ivector nanos_ftc(1,N_ftc)
 init_matrix Ctot(1,N_ftc,1,ntallas)
 

 init_int N_fts
 !!if(N_ftc>0)
 init_ivector nanos_fts(1,N_fts)
 init_matrix Ccru(1,N_fts,1,ntallas)


 // !! ad_comm::change_datafile_name("mestockL.ctl");
 init_number sigmaR
 init_number Roprior

 number  log_Ro_prior
 !! log_Ro_prior = log(Roprior);

 init_number L50ms
 init_number L95ms
 init_number lnaw
 init_number bw


 init_vector dt(1,3)
 init_vector Par_bio(1,8)
 init_vector cv_par(1,8) // CV de las priors logN
 init_int    opt_Linf
 init_int    opt_k
 init_int    opt_Lo
 init_int    opt_aedad
 init_int    opt_bedad
 init_int    opt_M
 init_int    opt_h
 init_int    opt_bpow

  number log_Linf_prior
  number log_k_prior
  number log_Lo_prior
  number log_aedad_prior
  number log_bedad_prior
  number log_M_prior
  number log_h_prior
  number log_b_prior
  
  !! log_Linf_prior = log(Par_bio(1));
  !! log_k_prior = log(Par_bio(2));
  !! log_Lo_prior = log(Par_bio(3));
  !! log_aedad_prior = log(Par_bio(4)+1e-10);
  !! log_bedad_prior = log(Par_bio(5)+1e-10);
  !! log_M_prior = log(Par_bio(6));
  !! log_h_prior = log(Par_bio(7));
  !! log_b_prior = log(Par_bio(8));

 init_number L50prior
 init_number s1prior
 init_number s2prior
 init_vector cv_parsel(1,3) // CV de las priors Selectividad
 init_int fases_flo1 // fases Sel Flota
 init_int fases_flo2 // 
 init_int fases_flo3 //

 number log_L50prior
 number log_s1prior
 number log_s2prior

 !! log_L50prior = log(L50prior);
 !! log_s1prior = log(s1prior);
 !! log_s2prior = log(s2prior);


 init_number L50priorc
 init_number s1priorc
 init_number s2priorc
 init_number qpriorc
 init_vector cv_parselc(1,4) // CV de las priors Selectividad
 init_int fases_cru1 // fases Sel crucero
 init_int fases_cru2 // 
 init_int fases_cru3 // 
 init_int fases_cru4 // 


 number log_L50priorc
 number log_s1priorc
 number log_s2priorc
 number log_qpriorc

 !! log_L50priorc = log(L50priorc);
 !! log_s1priorc = log(s1priorc);
 !! log_s2priorc = log(s2priorc);
 !! log_qpriorc = log(qpriorc);



 init_int    nbloques1
 init_vector ybloques1(1,nbloques1)

 init_int    nbloques2
 init_vector ybloques2(1,nbloques2)

 init_int    nqbloques
 init_vector yqbloques(1,nqbloques)


// define otras fases de estimacion-----------------------------
 init_int    opt_qf
 init_int    opt_F
 init_int    opt_devRt
 init_int    opt_Ro
 init_number  Regla
 

 init_int    npbr
 init_vector pbr(1,npbr)
 init_int ntime_sim


 int reporte_mcmc 


INITIALIZATION_SECTION


  log_Lo         log_Lo_prior
  log_L50        log_L50prior 
  log_sigma1     log_s1prior 
  log_sigma2     log_s2prior
  log_L50c       log_L50priorc 
  log_sigma1c    log_s1priorc 
  log_sigma2c    log_s2priorc
  log_Linf       log_Linf_prior
  log_k          log_k_prior
  log_Lo         log_Lo_prior
  log_aedad      log_aedad_prior
  log_bedad      log_bedad_prior
  log_M          log_M_prior
  log_h          log_h_prior
  log_b          log_b_prior
  log_Ro         log_Ro_prior
  log_qcru       log_qpriorc
  
PARAMETER_SECTION

// selectividad paramétrica a la talla común
 
 init_vector log_L50(1,nbloques1,fases_flo1)  
 init_vector log_sigma1(1,nbloques1,fases_flo2)
 init_vector log_sigma2(1,nbloques1,fases_flo3)

 init_vector log_L50c(1,nbloques2,fases_cru1)  
 init_vector log_sigma1c(1,nbloques2,fases_cru2)
 init_vector log_sigma2c(1,nbloques2,fases_cru3)

// parametros reclutamientos y mortalidades)
 init_number log_Ro(opt_Ro)
 init_bounded_dev_vector dev_log_Ro(1,ntime,-10,10,opt_devRt)
 init_bounded_vector log_F(1,ntime,-20,1.0,opt_F) // log  mortalidad por pesca por flota

// capturabilidades
 init_vector log_qflo(1,nqbloques,opt_qf)
 init_number log_qcru(fases_cru4)
 init_number log_b(opt_bpow)

// Crecim
 init_number log_Linf(opt_Linf)
 init_number log_k(opt_k)
 init_number log_Lo(opt_Lo)
 init_number log_aedad(opt_aedad)
 init_number log_bedad(opt_bedad)
 init_number log_M(opt_M)
 init_number log_h(opt_h)

//---------------------------------------------------------------------------------
//Defino las variables de estado 
 vector BMflo(1,ntime)
 vector BMcru(1,ntime)
 vector Brec(1,ntime)
 vector pred_CPUE(1,ntime);
 vector pred_Bcru(1,ntime);
 vector pred_Desemb(1,ntime);
 vector likeval(1,7);
 vector Neq(1,nedades);

 vector Rpred(1,ntime);
 vector Unos_edad(1,nedades);
 vector Unos_anos(1,ntime);
 vector Unos_tallas(1,ntallas);
 vector mu_edad(1,nedades)
 vector sigma_edad(1,nedades)
 vector BDo(1,ntime);
 vector No(1,nedades)

 vector yrs(1,ntime)
 vector Desemb(1,ntime);
 vector CPUE(1,ntime);
 matrix cv_index(1,3,1,ntime)
 vector nm(1,ntime)
 vector Lmed_obs(1,N_ftc)
 vector Lmed_pred(1,N_ftc)
 vector Lmed_obs_cru(1,N_fts)
 vector Lmed_pred_cru(1,N_fts)
 vector edades(1,nedades)
 vector pri(1,15)
 vector ftot_obs(1,ntallas)
 vector ftot_pred(1,ntallas)
 vector msex(1,ntallas)
 vector Wmed(1,ntallas)

 vector index(1,ntime)
 vector Bcru(1,ntime)
 vector cv_cpue(1,ntime)
 vector cv_capt(1,ntime)
 vector cv_bcru(1,ntime)
 vector nm_c(1,ntime)

 vector Madage(1,nedades)
 vector Wage(1,nedades)

 matrix S1(1,nbloques1,1,nedades)
 matrix S2(1,nbloques2,1,nedades)

 matrix Sel(1,ntime,1,nedades)
 matrix Selc(1,ntime,1,nedades)
 matrix F(1,ntime,1,nedades)
 matrix Z(1,ntime,1,nedades)
 matrix S(1,ntime,1,nedades)


 matrix N(1,ntime,1,nedades)

 matrix NM(1,ntime,1,nedades)
 matrix NMD(1,ntime,1,ntallas)
 matrix NDv(1,ntime,1,ntallas)
 matrix Nrec(1,ntime,1,ntallas)
 matrix NVflo(1,ntime,1,ntallas)
 matrix NVcru(1,ntime,1,ntallas)

 matrix pred_Ctot(1,ntime,1,ntallas)
 matrix pred_Ctot_a(1,ntime,1,nedades)

 matrix pobs(1,N_ftc,1,ntallas)
 matrix ppred(1,N_ftc,1,ntallas)
 matrix pobs_cru(1,N_fts,1,ntallas)
 matrix ppred_cru(1,N_fts,1,ntallas)

 matrix Prob_talla(1,nedades,1,ntallas)

 matrix P1(1,nedades,1,ntallas)
 matrix P2(1,nedades,1,ntallas)
 matrix P3(1,nedades,1,ntallas)
 matrix Nv(1,ntime,1,nedades)
 matrix NMDv(1,ntime,1,nedades)

 number suma1
 number suma2
 number suma3
 number suma4
 number suma5
 number suma6

 number penalty

 number So
 number alfa
 number beta
 number plus


 number Linf
 number k
 number M
 number h
 number rango

 number BDp
 number Npplus
 number Bref

 number nm1;
 number cuenta1;

 vector Np(1,nedades)
 vector Zpbr(1,nedades)
 vector Fpbr(1,nedades)
 vector Sp(1,nedades)

 matrix Bp(1,npbr,1,ntime_sim)
 vector CTPp(1,nedades)
 matrix Yp(1,npbr,1,ntime_sim)
 matrix Fproy(1,npbr,1,ntime_sim)

 
 objective_function_value f
  
 sdreport_vector BD(1,ntime) // 
 sdreport_vector F_mort(1,ntime) // 
 sdreport_vector BT(1,ntime) // 
 sdreport_vector RPR(1,ntime) // 
 sdreport_vector RPRlp(1,ntime) //
 sdreport_vector Rest(1,ntime) //
 sdreport_vector Redstock(1,npbr)
 sdreport_number SSBo
// sdreport_vector RPRp(1,npbr) // RPR proyectado en la simulacion


PRELIMINARY_CALCS_SECTION


 yrs=column(mdatos,1);
 Desemb=column(mdatos,2);
 CPUE=column(mdatos,4);
 Bcru=column(mdatos,6);
 
 cv_capt=column(mdatos,3); 
 cv_cpue=column(mdatos,5); 
 cv_bcru=column(mdatos,7); 

 nm=column(mdatos,8); //nm_r
 nm_c=column(mdatos,9); //nm_r

 edades.fill_seqadd(minedad,1);

 Unos_edad=1;// lo uso en  operaciones matriciales con la edad
 Unos_anos=1;// lo uso en operaciones matriciales con el año
 Unos_tallas=1;// lo uso en operaciones matriciales con el año
 reporte_mcmc=0;


 Wmed=exp(lnaw)*pow(Tallas,bw);
 rango=L95ms-L50ms;
 msex=1/(1+exp(-log(19)*(Tallas-L50ms)/rango));

RUNTIME_SECTION
 maximum_function_evaluations 500,2000,5000
 convergence_criteria  1e-2,1e-5,1e-5

PROCEDURE_SECTION
// se listan las funciones que contienen los calculos
 Eval_prob_talla_edad();

 Eval_selectividad();
 Eval_mortalidades();
 Eval_abundancia();
 Eval_deinteres();
 Eval_biomasas();
 Eval_capturas_predichas();
 Eval_indices();
 Eval_logverosim();

 Eval_funcion_objetivo();

 Eval_mcmc();

 if(last_phase()){Eval_CTP();}

FUNCTION Eval_prob_talla_edad


 Linf=exp(log_Linf);
 k=exp(log_k);

 int i, j;

// genero una clave edad-talla para otros calculos. Se modela desde L(1)
 mu_edad(1)=exp(log_Lo);
 for (i=2;i<=nedades;i++)
  {
  mu_edad(i)=Linf*(1-exp(-k))+exp(-k)*mu_edad(i-1);
  }

  sigma_edad=exp(log_aedad)+exp(log_bedad)*mu_edad;

  Prob_talla = ALK( mu_edad, sigma_edad, Tallas);



//----------------------------------------------------------------------
FUNCTION dvar_matrix ALK(dvar_vector& mu, dvar_vector& sig, dvector& x)
	//RETURN_ARRAYS_INCREMENT();
	int i, j;
	dvariable z1;
	dvariable z2;
	int si,ni; si=mu.indexmin(); ni=mu.indexmax();
	int sj,nj; sj=x.indexmin(); nj=x.indexmax();
	dvar_matrix pdf(si,ni,sj,nj);
	pdf.initialize();
	double xs=0.5*(x[sj+1]-x[sj]);
	for(i=si;i<=ni;i++) //loop over ages
	{
		 for(j=sj;j<=nj;j++) //loop over length bins
		{
			z1=((x(j)-xs)-mu(i))/sig(i);
			z2=((x(j)+xs)-mu(i))/sig(i);
			pdf(i,j)=cumd_norm(z2)-cumd_norm(z1);
		}//end nbins
		pdf(i)/=sum(pdf(i));
	}//end nage
	//RETURN_ARRAYS_DECREMENT();
	return(pdf);


//----------------------------------------------------------------------

FUNCTION Eval_selectividad
 int i,j;

 // FLOTA

 for (j=1;j<=nbloques1;j++){

 S1(j)=exp(-0.5*square(mu_edad-exp(log_L50(j)))/square(exp(log_sigma1(j))));


    for (i=1;i<=nedades;i++){

      if(mu_edad(i)>=exp(log_L50(j))){
      S1(j,i)= exp(-0.5*square(mu_edad(i)-exp(log_L50(j)))/square(exp(log_sigma2(j))));
      }

 }}

   for (i=1;i<=ntime;i++){
      for (j=1;j<=nbloques1;j++){
              if (yrs(i)>=ybloques1(j)){
                Sel(i)=S1(j);}
       }
   }

// Crucero


 for (j=1;j<=nbloques2;j++){

 S2(j)=exp(-0.5*square(mu_edad-exp(log_L50c(j)))/square(exp(log_sigma1c(j))));


    for (i=1;i<=nedades;i++){

      if(mu_edad(i)>=exp(log_L50c(j))){
      S2(j,i)= exp(-0.5*square(mu_edad(i)-exp(log_L50c(j)))/square(exp(log_sigma2c(j))));
      }

 }}

   for (i=1;i<=ntime;i++){
      for (j=1;j<=nbloques2;j++){
              if (yrs(i)>=ybloques2(j)){
                Selc(i)=S2(j);}
       }
   }



//----------------------------------------------------------------------


 Wage=exp(lnaw)*pow(mu_edad,bw);
 Madage=1/(1+exp(-log(19)*(mu_edad-L50ms)/rango));

//----------------------------------------------------------------------



FUNCTION Eval_mortalidades

 M=exp(log_M);
 F_mort=mfexp(log_F);


 F=elem_prod(Sel,outer_prod(mfexp(log_F),Unos_edad));

 Z=F+M;

 S=mfexp(-1.0*Z);


FUNCTION Eval_abundancia
 int i, j;


 h=exp(log_h);

 // Biomasa desovante virgen de largo plazo
 No(1)=exp(log_Ro); //

 
 for (int j=2;j<=nedades;j++)
     {No(j)=No(j-1)*exp(-1.*M);}
     No(nedades)=No(nedades)/(1-exp(-1.*M));
 
     
  SSBo=sum(elem_prod(No*exp(-dt(1)*M)*Prob_talla,elem_prod(msex,Wmed)));// Biomasa virginal de LP
  alfa=4*h*mfexp(log_Ro)/(5*h-1);//
  beta=(1-h)*SSBo/(5*h-1);// Reclutamiento


// Abundancia inicial
 // Condición inicial en equilibrio en torno a Z

 No(1)=mfexp(log_Ro+dev_log_Ro(1)); 
 for (int j=2;j<=nedades;j++)
     {No(j)=No(j-1)*exp(-1.*Z(1,j-1));}
     No(nedades)=No(nedades)/(1-exp(-1.*Z(1,nedades)));

 N(1)=No; 
 BD(1)=sum(elem_prod(elem_prod(N(1),exp(-dt(1)*Z(1)))*Prob_talla,elem_prod(msex,Wmed)));
 Rpred=exp(log_Ro);//


// se estima la sobrevivencia por edad(a+1) y año(t+1)
 for (i=1;i<ntime;i++)
 {

    if(i>minedad){
     Rpred(i+1)=(alfa*BD(i-minedad)/(beta+BD(i-minedad)));
    } // 

     N(i+1,1)=Rpred(i+1)*mfexp(dev_log_Ro(i+1));  // 

     N(i+1)(2,nedades)=++elem_prod(N(i)(1,nedades-1),S(i)(1,nedades-1));
     N(i+1,nedades)+=N(i,nedades)*S(i,nedades);// grupo plus

     BD(i+1)=sum(elem_prod(elem_prod(N(i+1),exp(-dt(1)*Z(i+1)))*Prob_talla,elem_prod(msex,Wmed)));
 }

 Rest=column(N,1);



FUNCTION Eval_deinteres

// Rutina para calcular RPR
 Nv=N;// solo para empezar los calculos

// se estima la sobrevivencia por edad(a+1) y año(t+1)
 for (int i=1;i<ntime;i++)
 {
     Nv(i+1)(2,nedades)=++Nv(i)(1,nedades-1)*exp(-1.0*M);
     Nv(i+1,nedades)+=Nv(i,nedades)*exp(-1.0*M);// grupo plus
 }


 NDv=elem_prod((Nv*exp(-dt(1)*M))*Prob_talla,outer_prod(Unos_anos,msex));
 BDo=NDv*Wmed;
 RPR=elem_div(BD,BDo);


 RPRlp=BD/SSBo;


FUNCTION Eval_biomasas
 
 NMD=elem_prod(N,mfexp(-dt(1)*Z))*Prob_talla;
 NMD=elem_prod(NMD,outer_prod(Unos_anos,msex));
 BD=NMD*Wmed;

 NVflo=elem_prod(elem_prod(N,mfexp(-dt(2)*(Z))),Sel)*Prob_talla;
 BMflo=NVflo*Wmed;
 BT=(N*Prob_talla)*Wmed;

 NVcru=elem_prod(elem_prod(N,mfexp(-dt(3)*(Z))),Selc)*Prob_talla;
 BMcru=NVcru*Wmed;


FUNCTION Eval_capturas_predichas

// matrices de capturas predichas por edad y año
 pred_Ctot_a=elem_prod(elem_div(F,Z),elem_prod(1.-S,N));
 pred_Ctot=pred_Ctot_a*Prob_talla;

// vectores de desembarques predichos por año
 pred_Desemb=pred_Ctot*Wmed;

// matrices de proporcion de capturas por talla y año
 pobs=elem_div(Ctot,outer_prod(rowsum(Ctot),Unos_tallas));

 if(N_ftc>0){
 for (int i=1;i<=N_ftc;i++){

 ppred(i)=pred_Ctot(nanos_ftc(i)-ymin+1)/sum(pred_Ctot(nanos_ftc(i)-ymin+1));
 }

 Lmed_obs=Tallas*trans(pobs);
 Lmed_pred=Tallas*trans(ppred);
 }

// matrices de proporcion de capturas por talla y año CRUCEROS
 pobs_cru=elem_div(Ccru,outer_prod(rowsum(Ccru),Unos_tallas));

 if(N_fts>0){
 for (int j=1;j<=N_fts;j++){
 ppred_cru(j)=NVcru(nanos_fts(j)-ymin+1)/sum(NVcru(nanos_fts(j)-ymin+1));
 }


 Lmed_pred_cru=Tallas*trans(ppred_cru);
 Lmed_obs_cru=Tallas*trans(pobs_cru);

 }


FUNCTION Eval_indices
 

   for (int i=1;i<=ntime;i++){
      for (int j=1;j<=nqbloques;j++){
              if (yrs(i)>=yqbloques(j)){
                 pred_CPUE(i)=exp(log_qflo(j))*pow(BMflo(i),exp(log_b));}
       }
   }


 pred_Bcru=exp(log_qcru)*BMcru;


FUNCTION Eval_logverosim
// esta funcion evalua el nucleo de las -log-verosimilitudes marginales para
// series con datos 0.
  int i;

 suma1=0; suma2=0; suma3=0; penalty=0;

 for (i=1;i<=ntime;i++)
 {
  if (CPUE(i)>0){
    suma1+=square(log(CPUE(i)/pred_CPUE(i))*1/cv_cpue(i));}

  if (Bcru(i)>0){
    suma2+=square(log(Bcru(i)/pred_Bcru(i))*1/cv_bcru(i));}
 }


FUNCTION Eval_funcion_objetivo

 suma4=0;suma5=0;suma6=0; penalty=0;

 likeval(1)=0.5*suma1;//CPUE
 likeval(2)=0.5*suma2;//Bcru



 likeval(3)=0.5*norm2(elem_div(log(elem_div(Desemb,pred_Desemb)),cv_capt));// desemb

 if(N_ftc>0){
 for (int i=1;i<=N_ftc;i++){
 suma4+=-nm(nanos_ftc(i)-ymin+1)*sum(elem_prod(pobs(i),log(ppred(i)+1e-10)));
 }}

 if(N_fts>0){
 for (int j=1;j<=N_fts;j++){
 suma5+=-nm_c(nanos_fts(j)-ymin+1)*sum(elem_prod(pobs_cru(j),log(ppred_cru(j)+1e-10)));
 }}

 likeval(4)=suma4;//
 likeval(5)=suma5;//


// lognormal Ninicial y Reclutas
 if(active(dev_log_Ro)){
 likeval(6)=1./(2*square(sigmaR))*norm2(dev_log_Ro);}

 //if(active(dev_log_No)){
 //likeval(7)=1./(2*square(sigmaR))*norm2(dev_log_No);}




 if (active(log_F)){
 penalty+=1000*norm2(log_F-mean(log_F));}


 // distribuciones a priori
 
  pri(1)=0.5*square((log_Linf_prior-log_Linf)/cv_par(1));
  pri(2)=0.5*square((log_k_prior-log_k)/cv_par(2));
  
  pri(3)=0.5*square((log_Lo_prior-log_Lo)/cv_par(3));
  
  pri(4)=0.5*square((log_aedad_prior-log_aedad)/cv_par(4));
  pri(5)=0.5*square((log_bedad_prior-log_bedad)/cv_par(5));
  pri(6)=0.5*square((log_M_prior-log_M)/cv_par(6));
  pri(7)=0.5*square((log_h_prior-log_h)/cv_par(7));
  pri(8)=0.5*square((log_b_prior-log_b)/cv_par(8));

  pri(9)=0.5*norm2((log_L50prior-log_L50)/cv_parsel(1));
  pri(10)=0.5*norm2((log_s1prior-log_sigma1)/cv_parsel(2));
  pri(11)=0.5*norm2((log_s2prior-log_sigma2)/cv_parsel(3));

  pri(12)=0.5*norm2((log_L50priorc-log_L50c)/cv_parselc(1));
  pri(13)=0.5*norm2((log_s1priorc-log_sigma1c)/cv_parselc(2));
  pri(14)=0.5*norm2((log_s2priorc-log_sigma2c)/cv_parselc(3));
  pri(15)=0.5*square((log_qcru-log_qpriorc)/cv_parselc(4));


 f=(sum(likeval)+penalty);
 if(last_phase){
 f=(sum(likeval)+sum(pri));}
 


FUNCTION  Eval_CTP
//-----------------------------------------------------------------

  for (int i=1;i<=npbr;i++){ // ciclo de PBR

  Np=N(ntime);
  Sp=S(ntime);
  Bref=BD(ntime);

   for (int j=1;j<=ntime_sim;j++){ // ciclo de años

    plus=Np(nedades)*Sp(nedades);
    if(j<=minedad){
    Np(1)=alfa*BD(ntime-minedad+j)/(beta+BD(ntime-minedad+j));
    } // cuando j<minedad

    if(j>minedad){
    Np(1)=alfa*Bp(i,j-minedad)/(beta+Bp(i,j-minedad));
    } // cuando j>minedad

    Np(2,nedades)=++elem_prod(Np(1,nedades-1),Sp(1,nedades-1));
    Np(nedades)+=plus;

    Fpbr=F(ntime)*pbr(i);
    


    if(Bref/SSBo<=Regla){ // regla de control

     Fpbr=Fpbr*Bref/SSBo;
    }

    Zpbr=Fpbr+M;

    Bp(i,j)=sum(elem_prod(elem_prod(Np,mfexp(-dt(1)*Zpbr))*Prob_talla,elem_prod(msex,Wmed)));
    CTPp=elem_prod(elem_div(Fpbr,Zpbr),elem_prod(1.-exp(-1.*Zpbr),Np));
    Yp(i,j)=sum(elem_prod(CTPp*Prob_talla,Wmed));
    Fproy(i,j)=max(Fpbr);

    Sp=exp(-1.*Zpbr);
    Bref=Bp(i,j);

  }
   Redstock(i)=Bref/SSBo;
  }


 
REPORT_SECTION

 report << "Modelo Evaluación de Stock con datos de Tallas (MestockT)" << endl;
 report << "cristian.canales.r@pucv.cl" << endl;
 report << "-----------------------------------------------------------"<<endl;
 report << "Años" << endl;
 report << yrs << endl;
 report << "Desemb_obs & pred" << endl;
 report << Desemb << endl;
 report << pred_Desemb << endl;
 report << "CPUE_obs & pred" << endl;
 report << CPUE << endl;
 report << pred_CPUE << endl;
 report << "Campañas_obs & pred" << endl;
 report << Bcru << endl;
 report << pred_Bcru << endl;
 report << "Biomasa_desovante/reproductiva" << endl;
 report << BD << endl;
 report << "Biomasa_total" << endl;
 report << BT << endl;
 report << "Biomasa_explotable" << endl;
 report <<  (elem_prod(N,Sel)*Prob_talla)*Wmed << endl;
 report << "Reclutamiento predicho & Estimado" << endl;
 report << Rpred<< endl;
 report << column(N,1)<< endl;
 report << "Desvios log_R" << endl;
 report << dev_log_Ro<< endl;
 report << "Mor por pesca anual F " << endl;
 report << exp(log_F) << endl;
 report << "Reducción del stock (SPR, B/B0) de largo plazo y dinámico" << endl;
 report << RPRlp << endl;
 report << RPR << endl;
 report << "-----------------------------------------------" << endl;
 report << "Frecs tallas Capturas_obs"<< endl;
 report << pobs<< endl;
 report << "-----------------------------------------------" << endl;
 report << "Frecs tallas Capturas_predichas"<< endl;
 report << ppred<< endl;
 report << "-----------------------------------------------" << endl;
 report << "Frecs tallas campañas/cruceros"<< endl;
 report << pobs_cru<< endl;
 report << "-----------------------------------------------" << endl;
 report << "Frecs tallas campañas/cruceros_predichas"<< endl;
 report << ppred_cru<< endl;
 report << "---------------------------------------------------------------" << endl;
 report << "B0   R0    alfa(SR)   beta(SR)" << endl;
 report << SSBo <<" "<< No(1)<<" "<<alfa<<" "<<beta<< endl;
 report << "---------------------------------------------------------------" << endl;
 report << "Talla media y desv. a la edad" << endl;
 report << "---------------------------------------------------------------" << endl;
 report << edades<< endl;
 report << "---------------------------------------------------------------" << endl;
 report << mu_edad<< endl;
 report << sigma_edad<< endl;
 report << "---------------------------------------------------------------" << endl;
 report << "B0 anual (dinámica)" << endl;
 report << BDo << endl;
 report << "---------------------------------------------------------------" << endl;
 report << "Loo    k    Lo      a0        cv_edad    M    h   dt " << endl;
 report << exp(log_Linf) <<" "<<exp(log_k) <<" "<<exp(log_Lo) <<" "<<exp(log_aedad)<<" "<<exp(log_bedad)<<" "<<exp(log_M)<<" "<<exp(log_h)<<" "<<dt(1)<<endl;

 report << "---------------------------------------------------------------" << endl;
 report << "b-hiperestabilidad   q_cpue    q_camp/cru" << endl;
 report << exp(log_b) <<" "<<exp(log_qflo)<<" "<<exp(log_qcru)<<endl;
 report << "---------------------------------------------------------------" << endl;

// ESTIMA nm y CV



  suma1=0; suma2=0;nm1=1;cuenta1=0;

  for (int i=1;i<=N_ftc;i++){ //

   if (sum(pobs(i))>0){
      suma1=sum(elem_prod(ppred(i),1-ppred(i)));
      suma2=norm2(pobs(i)-ppred(i));
      nm1=nm1*suma1/suma2;
      cuenta1+=1;
   }}

 report << "Tamaño muestra ideal flota " <<endl;
 report <<pow(nm1,1/cuenta1)<< endl;

  suma1=0; suma2=0;nm1=1;cuenta1=0;

  for (int i=1;i<=N_fts;i++){ //

   if (sum(pobs_cru(i))>0){
      suma1=sum(elem_prod(ppred_cru(i),1-ppred_cru(i)));
      suma2=norm2(pobs_cru(i)-ppred_cru(i));
      nm1=nm1*suma1/suma2;
      cuenta1+=1;
   }}


 report << "Tamaño muestra ideal campaña/crucero " <<endl;
 report <<pow(nm1,1/cuenta1)<< endl;

 report << "-----------------------------------------------" << endl;
 report << "Componentes de log-verosimilitud" << endl;
 report << " CPUE    Cruceros  Desemb   Prop_f   Prop_c    dev_Rt   dev_No"<<endl;
 report << likeval << endl;
 report << "-----------------------------------------------" << endl;
 report << "Edades"<< endl;
 report << edades<< endl;
 report << "-----------------------------------------------" << endl;
 report<<  "Abundancia a la edad por año"<<endl;
 report<<N<<endl;
 report << "---------------------------------------------------------------" << endl;
 report<<  "Bloques de selectividad a la edad"<<endl;
 report<<S1<<endl;
 report << "---------------------------------------------------------------" << endl;
 report<<"Selectividad a la edad crucero"<<endl;
 report<<Selc<<endl;
 report << "---------------------------------------------------------------" << endl;
 report<<"Madurez a la talla"<<endl;
 report<<msex<<endl;
 report << "---------------------------------------------------------------" << endl;
 report<<"Mort por pesca a la edad por año"<<endl;
 report<<F<<endl;
 report << "---------------------------------------------------------------" << endl;
 report<<"Captura a la edad por año"<<endl;
 report<<pred_Ctot_a<<endl;
 report << "---------------------------------------------------------------" << endl;
 report << "Tallas"<< endl;
 report << Tallas<< endl;
 report << "---------------------------------------------------------------" << endl;
 report << "Abundancia la talla poblacional por año" << endl;
 report << N*Prob_talla<< endl;
 report << "---------------------------------------------------------------" << endl;
 report << "Probabilidad de la talla a la edad" << endl;
 report << Prob_talla << endl;



FUNCTION Eval_mcmc

 if(reporte_mcmc == 0)


 mcmc_report<<"log_L50"<< ","<<"log_sigma1"<<","<<"log_sigma2"<<","<<"log_L50c"<< ","<<"log_sigma1c"<<","<<"log_sigma2c"<<","<<"log_qflo"<<","<<"log_qcru"<<endl; // imprime las variables
 mcmc_report<<log_L50(nbloques1)<< ","<<log_sigma1(nbloques1) <<","<<log_sigma2(nbloques1)<<"," <<log_L50c(nbloques2)<< ","<<log_sigma1c(nbloques2) <<","<<log_sigma2c(nbloques2)<<","<<log_qflo(nqbloques)<<","<<log_qcru<<endl; // imprime las variables



 reporte_mcmc++;



FINAL_SECTION

 time(&finish);
 elapsed_time=difftime(finish,start);
 hour=long(elapsed_time)/3600;
 minute=long(elapsed_time)%3600/60;
 second=(long(elapsed_time)%3600)%60;
 cout<<endl<<endl<<"*********************************************"<<endl;
 cout<<"--Start time:  "<<ctime(&start)<<endl;
 cout<<"--Finish time: "<<ctime(&finish)<<endl;
 cout<<"--Runtime: ";
 cout<<hour<<" hours, "<<minute<<" minutes, "<<second<<" seconds"<<endl;
 cout<<"*********************************************"<<endl;


 //------------------------------------------------
  ofstream print_R("for_R.rep");
 print_R << "Yrs" << endl;
 print_R << yrs << endl;
 print_R << "Desembarques" << endl;
 print_R << Desemb << endl;
 print_R << pred_Desemb << endl;
 print_R << "CPUE" << endl;
 print_R << CPUE << endl;
 print_R << pred_CPUE << endl;
 print_R << "Surveys" << endl;
 print_R << Bcru << endl;
 print_R << pred_Bcru << endl;
 print_R << "Biomasa_desovante" << endl;
 print_R << BD << endl;
 print_R << BD.sd << endl;
 print_R << "Biomasa_total" << endl;
 print_R << BT << endl;
 print_R << "Biomasa_explotable" << endl;
 print_R <<  (elem_prod(N,Sel)*Prob_talla)*Wmed << endl;
 print_R << "Reclutamientos" << endl;
 print_R << Rpred<< endl;
 print_R << column(N,1)<< endl;
 print_R << Rest.sd<< endl;
 print_R << "Dev_log_R" << endl;
 print_R << dev_log_Ro<< endl;

 print_R << "N" << endl;
 print_R <<  N << endl;
 print_R << "F" << endl;
 print_R <<  F << endl;
 print_R << "Z" << endl;
 print_R <<  Z << endl;


 print_R << "Mort_F " << endl;
 print_R << F_mort << endl;
 print_R << F_mort.sd << endl;
 print_R << "SPR" << endl;
 print_R << RPR << endl;
 print_R << RPR.sd << endl;
 print_R << "Lmed_flo" << endl;
 print_R << nanos_ftc << endl; 
 print_R << Lmed_obs <<endl;
 print_R << Lmed_pred <<endl;
 print_R << "Lmed_srv" << endl;
 print_R << nanos_fts << endl; 
 print_R << Lmed_obs_cru <<endl;
 print_R << Lmed_pred_cru <<endl;
 print_R << "Tallas"<< endl;
 print_R << Tallas<< endl;
 print_R << "Frec_marg_flo"<< endl;
      ftot_obs=0;
      ftot_pred=0;
  for (int i=1;i<=N_ftc;i++){ //
      ftot_obs+=pobs(i);
      ftot_pred+=ppred(i);
   }
 print_R << ftot_obs<< endl;
 print_R << ftot_pred<< endl;
      ftot_obs=0;
      ftot_pred=0;
  for (int i=1;i<=N_fts;i++){ //
      ftot_obs+=pobs_cru(i);
      ftot_pred+=ppred_cru(i);
   }
 print_R << "Frec_marg_srv"<< endl;
 print_R << ftot_obs<< endl;
 print_R << ftot_pred<< endl;
 print_R << "Frecs_capt_obs"<< endl;
 print_R << pobs<< endl;
 print_R << "Frecs_capt_pred"<< endl;
 print_R << ppred<< endl;
 print_R << "Frecs_srv_obs"<< endl;
 print_R << pobs_cru<< endl;
 print_R << "Frecs_srv_pred"<< endl;
 print_R << ppred_cru<< endl;
 print_R << "Sel_f"<< endl;
 print_R << Sel<< endl;
 print_R << "Sel_srv"<< endl;
 print_R << Selc<< endl;
 print_R << "Madurez_edad"<< endl;
 print_R << Madage<<endl;
 print_R << "Peso_edad"<< endl;
 print_R << Wage<<endl;
 print_R << "Prob_talla"<< endl;
 print_R << Prob_talla<<endl;
 print_R << "No"<< endl;
 print_R << No<<endl;
 print_R << "Loo_k_Lo_alfa_beta_M_h_dt"<< endl;
 print_R << exp(log_Linf) <<" "<<exp(log_k) <<" "<<exp(log_Lo) <<" "<<exp(log_aedad)<<" "<<exp(log_bedad)<<" "<<exp(log_M)<<" "<<exp(log_h)<<" "<<dt(1)<<endl;

 print_R <<"dts"<<endl;
 print_R <<dt(1)<<endl;

 print_R<<"log_L50f"<<endl;
 print_R<<log_L50<<endl;
 print_R<<"log_s1"<<endl;
 print_R<<log_sigma1<<endl;
 print_R<<"log_s2"<<endl;
 print_R<<log_sigma2<<endl;
 
 print_R<<"log_L50c"<<endl;
 print_R<<log_L50c<<endl;
 print_R<<"log_s1c"<<endl;
 print_R<<log_sigma1c<<endl;
 print_R<<"log_s2c"<<endl;
 print_R<<log_sigma2c<<endl;

 print_R << "Mult_F" << endl;
 print_R << pbr << endl;
 print_R << "Bio_proy" << endl;
 print_R << trans(Bp) << endl;
 print_R << "Capt_proy" << endl;
 print_R << trans(Yp) << endl;
 print_R << "F_proy" << endl;
 print_R << trans(Fproy) << endl;
 print_R << "Red_stock" << endl;
 print_R << Redstock << endl;
 print_R << Redstock.sd << endl;
 print_R << "cv_cpue" << endl;
 print_R << cv_cpue(1) << endl;
 print_R << "cv_capt" << endl;
 print_R << cv_capt(1) << endl;
 print_R << "cv_cru" << endl;
 print_R << cv_bcru(1) << endl;
 print_R << "L_edad" << endl;
 print_R << mu_edad << endl;
 print_R << "q_flo" << endl;
 print_R <<  exp(log_qflo(nqbloques)) << endl;
 print_R << "q_cru" << endl;
 print_R <<  exp(log_qcru) << endl;
 print_R << "Likeval" << endl;
 print_R <<  likeval << endl;
 print_R << "MaxGrad" << endl;
 print_R <<objective_function_value::pobjfun->gmax<<endl;
 print_R << "FunObj" << endl;
 print_R <<  f << endl;
 print_R << "B0_R0_alfaSR_betaSR" << endl;
 print_R << SSBo <<" "<< No(1)<<" "<<alfa<<" "<<beta<< endl;


