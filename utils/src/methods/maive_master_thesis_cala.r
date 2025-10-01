# MAIVE Estimator method - Irsova et al. (2023)
# Source: http://meta-analysis.cz/maive/

# R code for MAIVE
# input as excel file:
#       estimates: bs
#       standard errors: sebs
#       number of observations: Ns
#       optional: study_id
#
# default option for MAIVE: MAIVE-PET-PEESE, unweighted, instrumented
#
# choices:
#       method= 1 FAT-PET, 2 PEESE, 3 PET-PEESE, 4 EK
#       weighting = 0 no weights, 1 standard weights, 2 adjusted weights  
#       instrumenting = 1 yes, 0 no 
#       correlation at study level: 0 none, 1 fixed effects, 2 cluster
#
# standard estimator: same option as for MAIVE but weighted by inverse variance and not instrumented     
#
# output:
#       MAIVE meta-estimate and standard error
#       Hausman type test: comparison between MAIVE and standard version
#       when instrumenting: heteroskedastic robust F-test of the first step


maive <- function(dat=dat,method=method,weight=weight,instrument=instrument,studylevel=studylevel) {

  methods <- c("PET","PEESE","PET-PEESE","EK")                              
  instrumented <- c("not instrumented","instrumented")                      
  weighted <- c("no weights","standardly weighted", "adjusted weights")     
  studylevelcorrelation <- c("none","study level dummies", "cluster")         

  if (studylevel==0){
    cluster<-0
    dummy<-0
  } else if (studylevel==1){
    cluster<-0
    dummy<-1
  } else if (studylevel==2) {
    cluster<-1
    dummy<-0
  }
  
  # extracting data from excel  
  dat = as.data.frame(dat)
  bs<-dat[,1]
  M<-length(bs)
  sebs<-dat[,2]
  Ns<-dat[,3]
  if (dim(dat)[2]==4){
    studyid<-dat[,4]
  } else {
    studyid<-(1:M)
    dummy<-0
    cluster<-0
  }  
    
  alpha_s <- 0.05 
  
  # create Dummies from studyid
    df<-data.frame(studyid)
    D <- to.dummy(df,"studyid")
    D <-D-matrix(colMeans(D),nrow=M,ncol=size(D)[2], byrow = TRUE)
    D <- D[,1:(dim(D)[2]-1)]
  
  # g=studyid if clustered and g=(1:M)' if not clustered (gives heteroskedastic robust SE)
    if (cluster==0){           
      g <- (1:M)    
    } else if (cluster==1){    
      g <- studyid           
    }

# (1) Instrumenting the variances with sample size allowing for a constant and including dummies
    invNS<- 1/Ns
    sebs2<- sebs^2
    Xiv<-matrix(c(ones(M,1)[,1],invNS),nrow=M)
    varreg1 <- lm(sebs2~ 0+Xiv) 
    dimiv<-2
  if (varreg1$coefficients[1]<0){
     Xiv<-invNS
     varreg1 <- lm(sebs2~ 0+Xiv) 
     dimiv<-1
  }                                           

  sebs2fit1 <- varreg1$fitted.values         

  # F-statistic of first step. heteroskedasticity and autocorrelation robust variance HAC
  F_hac <- (varreg1$coefficients[dimiv]^2 /vcovCL(varreg1, cluster = g)[dimiv,dimiv]) 

  #weight                                   
  if (weight==0){
      w <- ones(M,1)[,1]
  } else if (weight==1){
      w <- sebs       
  } else if (weight==2){
      w <- sebs2fit1^(1/2)
  }             
              
  #instrument
  if (instrument==0){
      x <- sebs      
      x2 <- sebs^2 
      F_hac <-"NA"
  } else if (instrument==1){
      x <- sebs2fit1^(1/2)  
      x2 <- sebs2fit1
      F_hac<-round(F_hac,3)
  }                          
  
                           
  #choose dependent variable and regressor
    y <- bs/w 
    x <- x 
    x[is.na(x)] <- median(x, na.rm=T) # Added externally - remove missing variables
    x2 <- x2
    X <- matrix(c(ones(M,1)[,1], x)/w,nrow=M)     
    X_d <- matrix(c(X, D/w), nrow=M)
    X2 <- matrix(c(ones(M,1)[,1], x2)/w,nrow=M)
    X2_d <- matrix(c(X2, D/w), nrow=M)         
  
  # baseline, i.e. chosen method, with chosen options of study-level correlation
  #  but with inverse-variance weighting and without instrumenting  
    y0 <- bs/sebs                                                               
    x0 <- sebs                                                                  
    x20 <- sebs ^2
    X0 <- matrix(c(ones(M,1)[,1], x0)/sebs, nrow=M)
    X0_d <- matrix(c(X0, D/sebs), nrow=M)          
    X20 <- matrix(c(ones(M,1)[,1], x20)/sebs, nrow=M)
    X20_d <- matrix(c(X20, D/sebs),  nrow=M)         
  
    if (dummy==0){                                            
      X <- X
      X0 <- X0
      X2 <- X2                                              
      X20 <- X20                                            
      cD <- ones(M,1)[,1]     
    } else if (dummy==1){                                          
      X <- X_d   
      X0 <- X0_d
      X2 <- X2_d                                                 
      X20 <- X20_d                                               
      cD <- matrix(c(ones(M,1)[,1],D),nrow=M)  # for EK stack constant and dummies         
    }
  
    cD<- cD/w                                                      
    cD0<- cD/sebs

    ones_w<-ones(M,1)[,1]/w
    ones_w0<-ones(M,1)[,1]/sebs                                                                   
                                                                     
  # Fixed effects (FE)                                                                 
    wis0 <- 1/(w^2)                                                      
    fe <- sum(bs*wis0)/sum(wis0)                                         
    varfe <- 1/sum(wis0) 
  # baseline
    wis00 <- 1/(sebs^2)                                                      
    fe0 <- sum(bs*wis00)/sum(wis00)                                         
    varfe0 <- 1/sum(wis00)
                                                                
  # WLS                                                              
    wlsreg <-lm(y~ 0+ cD )
    wls <- wlsreg$coefficients[1]
    wlsse <- sqrt(vcovCL(wlsreg,cluster=g)[1,1])
  # baseline 
    wlsreg0 <-lm(y0~ 0+ cD0 )
    wls0 <- wlsreg0$coefficients[1]
    wlsse0 <- sqrt(vcovCL(wlsreg0, cluster=g)[1,1])
                                                                                    
  # FAT-PET - MAIVE                                         
    fatpet <- lm(y~ 0+X) 
  # FAT-PET - baseline case
    fatpet0 <- lm(y0~ 0+X0) 

  # PEESE - MAIVE
    peese <- lm(y~0+X2)
  # PEESE - baseline case
    peese0 <- lm(y0~0+X20)
                                     
  # PET-PEESE - MAIVE                         
    if (abs(fatpet$coefficients[1]/sqrt(vcovCL(fatpet,cluster=g)[1,1]))>qt(1-alpha_s/2,M-dim(X)[2]-1)){
       petpeese <- peese                                                                             
    } else {                                                                                          
        petpeese <- fatpet                                                                            
    }                                                                                                 
  # PET-PEESE - baseline case                        
    if (abs(fatpet0$coefficients[1]/sqrt(vcovCL(fatpet0,cluster=g)[1,1]))>qt(1-alpha_s/2,M-dim(X0)[2]-1)){
        petpeese0 <- peese0
    } else {
        petpeese0 <- fatpet0
    }
                      
  # True effect variance - MAIVE
    Qfe0 <- sum(wlsreg$residuals*wlsreg$residuals)
    sigh2hat0 <- max(0,M*((Qfe0/(M-dim(wlsreg$model)[2]-1))-1)/sum(wis0))           
    sighhat0 <- sqrt(sigh2hat0)
  #True effect variance - baseline                      
    Qfe00 <- sum(wlsreg0$residuals*wlsreg0$residuals)
    sigh2hat00 <- max(0,M*((Qfe00/(M-dim(wlsreg0$model)[2]-1))-1)/sum(wis00))        
    sighhat00 <- sqrt(sigh2hat00)

  # Endogenous Kink (EK) Threshold- MAIVE
    if (petpeese$coefficients[1] > 1.96*sighhat0){    
        a0 <- (petpeese$coefficients[1]-1.96*sighhat0)*(petpeese$coefficients[1]+1.96*sighhat0)/(2*1.96*petpeese$coefficients[1])
    } else {   
        a0 <- 0
    }
  # Endogenous Kink (EK) Threshold - baseline
    if (petpeese0$coefficients[1] > 1.96*sighhat00){    
        a00 <- (petpeese0$coefficients[1]-1.96*sighhat00)*(petpeese0$coefficients[1]+1.96*sighhat00)/(2*1.96*petpeese0$coefficients[1])
    } else {   
        a00 <- 0
    }

  # EK - MAIVE                                      
    if (a0>min(x)  && a0<max(x)){ 
      xx_w=(x-a0)*(x>a0)/w
      ekreg <- lm(y~ 0+cD+xx_w)
    } else if (a0<min(x)){
      x_w=x/w
      ekreg <- lm(y~ 0+cD+x_w)
    } else if (a0>max(x)){
      ekreg <- lm(y~ 0+cD)
    }
    ek <- ekreg$coefficients[1]
    
  # EK - baseline
    if (a00>min(x0)  && a00<max(x0)){ 
      xx0_w=(x0-a00)*(x0>a00)/sebs
      ekreg0 <- lm(y0~ 0+cD0+xx0_w)
    } else if (a00<min(x0)){
      x0_w=x0/sebs
      ekreg0 <- lm(y0~ 0+cD0+x0_w )
    } else if (a00>max(x0)){
      ekreg0 <- lm(y0~ 0+cD0 )
    }
    ek0 <- ekreg0$coefficients[1]
    
    
  "RESULTS"
  if (method==1){
    "MAIVE-FAT-PET"
    beta=fatpet$coefficients[1]
    betase=sqrt(vcovCL(fatpet,cluster=g)[1,1])
    "Standard FAT-PET"
    beta0=fatpet0$coefficients[1]
    beta0se=sqrt(vcovCL(fatpet0,cluster=g)[1,1])
    "Hausman-type test"
    Hausman=(fatpet$coefficients[1]-fatpet0$coefficients[1])^2/(vcovCL(fatpet,cluster=g)[1,1])
    Chi2=qchisq(p=0.05, df=1, lower.tail=FALSE)
  } else if (method==2){       
    "MAIVE-PEESE"                  
    beta=peese$coefficients[1]    
    betase=sqrt(vcovCL(peese,cluster=g)[1,1]) 
    "Standard PEESE"
    beta0=peese0$coefficients[1]    
    beta0se=sqrt(vcovCL(peese0,cluster=g)[1,1])
    "Hausman-type test"
    Hausman=(peese$coefficients[1]-peese0$coefficients[1])^2/(vcovCL(peese,cluster=g)[1,1])
    Chi2=qchisq(p=0.05, df=1, lower.tail=FALSE)
  } else if (method==3){
    "MAIVE-PET-PEESE"                  
    beta=petpeese$coefficients[1]
    betase=sqrt(vcovCL(petpeese,cluster=g)[1,1])
    "Standard PET-PEESE"                  
    beta0=petpeese0$coefficients[1]
    beta0se=sqrt(vcovCL(petpeese0,cluster=g)[1,1])
    "Hausman-type test"
    Hausman=(petpeese$coefficients[1]-petpeese0$coefficients[1])^2/(vcovCL(petpeese,cluster=g)[1,1])
    Chi2=qchisq(p=0.05, df=1, lower.tail=FALSE)
  } else if (method==4){         
    "MAIVE-EK"                       
    beta=ekreg$coefficients[1]      
    betase=sqrt(vcovCL(ekreg,cluster=g)[1,1])   
    "Standard EK"                       
    beta0=ekreg0$coefficients[1]      
    beta0se=sqrt(vcovCL(ekreg0,cluster=g)[1,1])
    "Hausman-type test" # with variance of MAIVE in denominator (instead of the difference) hence is conservative
    Hausman=(ekreg$coefficients[1]-ekreg0$coefficients[1])^2/(vcovCL(ekreg,cluster=g)[1,1])
    Chi2=qchisq(p=0.05, df=1, lower.tail=FALSE)
  }     

my_list <- list("beta"=round(beta,3), "SE"=round(betase,3),"F-test"=F_hac,"beta_standard"=round(beta0,3),"SE_standard"=round(beta0se,3),"Hausman"=round(Hausman,3), "Chi2"=round(Chi2,3))
return(my_list)
}