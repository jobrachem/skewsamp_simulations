# Note by J. Brachem & D. Strache: In this version of the code, we fixed
# the function by adding support for the logit link.

# R function to implement sample size methods in Cundill & Alexander BMC Med Res Meth
#
# May be used freely as long as the above paper is cited.
#
# Supplied in good faith but no liability implied or accepted.
#
# To use: 'source' this file, or simply copy/paste all the contents into R
#
# The following examples should reproduce entries in Table III of
# Zhu & Lakkis Stat Med 2014, 33(3):376-387.
# To run these, examples, the '# ' at the start of each line should be omitted.
# The result is the total sample size (including both arms)
#
# nGLM(link="log",  family="NegBinomial", mu0=2, mu1=2*0.35, k0=2, k1=2, power=0.8, method=1)
# nGLM(link="log",  family="NegBinomial", mu0=2, mu1=2*0.35, k0=2, k1=2, power=0.8, method=2)
# nGLM(link="log",  family="NegBinomial", mu0=2, mu1=2*0.50, k0=2, k1=2, power=0.8, method=1)
# nGLM(link="log",  family="NegBinomial", mu0=2, mu1=2*0.50, k0=2, k1=2, power=0.8, method=2)


nGLM<-function(mu0, mu1, k0=NULL, k1=k0, d=1, kappa0=NULL, kappa1=kappa0,
   Q0=0.5, Q1=1-Q0, power, alpha=0.05, link, family,
   verbose=FALSE, method=2){

   linkSupported<-c("log", "I", "logit")
   if(!is.character(link)){stop("Error: the link argument must be supplied in the character type (in quotation marks).")}
   if(is.na(match(link, linkSupported))){
      stop(paste(
         "Error: link must be one of ",
         paste(linkSupported, collapse=', '),
         ".", sep=""))}

   familySupported<-c("binomial", "Gamma", "gaussian", "poisson", "NegBinomial")
   if(!is.character(family)){stop("Error: the family argument must be supplied in the character type (in quotation marks).")}
   if(is.na(match(family, familySupported))){
      stop(paste(
         "Error: family must be one of ",
         paste(familySupported, collapse=', '),
         ".", sep=""))}

   linkFamily<-paste(link, family, sep="")
   linkFamilySupported<-c(             "logGamma",            "logpoisson", "logNegBinomial",
                          "Ibinomial",   "IGamma", "Igaussian", "Ipoisson",   "INegBinomial",
                          "logitbinomial")
   if(is.na(match(linkFamily, linkFamilySupported))){
      stop("Error: that combination of link and family is not supported.")}

   if (link == "logit") {
      linkFn <- function(mu) log(mu / (1 - mu))
   } else {
      linkFn<-match.fun(link)
   }

   if(family=="Gamma"){
      if(is.null(kappa0)){stop("Error: for Gamma family, must supply kappa parameter.")}
      dispersion0<-kappa0
      dispersion1<-kappa1
      VarFn<-function(mu, dispersion){mu*mu/dispersion}
   }
   if(family=="NegBinomial"){
      dispersion0<-k0
      dispersion1<-k1
      VarFn<-function(mu, dispersion){mu+(mu*mu/dispersion)}
   }
   if(family=="binomial"){
      dispersion0<-dispersion1<-d
      VarFn<-function(mu, dispersion){mu*(1-mu)/dispersion}
   }
   if(family=="poisson"){
      VarFn<-function(mu, dispersion){mu}
      dispersion0<-dispersion1<-NULL
   }

   if(link=="log"){
      dMu.dEtaFn<-function(mu){mu}
      InverseLinkFn<-match.fun('exp')
   }
   if(link=="I"){
      dMu.dEtaFn<-function(mu){1}
      InverseLinkFn<-match.fun('I')
   }
   if(link=="logit"){
      dMu.dEtaFn<-function(mu){mu*(1-mu)}
      InverseLinkFn<-function(x) exp(x) / (1 + exp(x))
   }

   if(Q0+Q1!=1){stop('Error: Q0 and Q1 should add to 1.')}

   # if(muNull!='0' & muNull!='average'){
   #    stop("Error: the muNull argument should be '0' (to use mu0) or 'average' (to use the average, on the link function scale, of mu0 & mu1).")
   # }

   Z_alpha<-qnorm(1-(alpha/2))
   Z_beta <-qnorm(power)

   denom<-(linkFn(mu0)-linkFn(mu1))

   # if(muNull=='0'){
      muNullValue<-mu0
   # }else{
   #    muNullValue<-InverseLinkFn((Q0*linkFn(mu0))+(Q1*linkFn(mu1)))
   # }
   numerator1<-sqrt(   (VarFn(muNullValue, dispersion0)/((dMu.dEtaFn(muNullValue))^2))*((1/Q1)+(1/Q0))   )

   numerator2<-sqrt(  ((VarFn(mu0,         dispersion0)/((dMu.dEtaFn(mu0        ))^2))    /Q0) +
                      ((VarFn(mu1,         dispersion1)/((dMu.dEtaFn(mu1        ))^2))    /Q1)           )

   if(verbose){
      print("dMu/dEta function:")
      print(dMu.dEtaFn)
      print(unlist(list("(dMu.dEtaFn(mu0))^2"=((dMu.dEtaFn(mu0))^2))))
      print(unlist(list("(dMu.dEtaFn(mu1))^2"=((dMu.dEtaFn(mu1))^2))))

      print("variance function:")
      print(VarFn)
      print(unlist(list("VarFn(mu0, dispersion0)"=VarFn(mu0, dispersion0))))
      print(unlist(list("VarFn(mu1, dispersion1)"=VarFn(mu1, dispersion1))))

      print(unlist(list(Z_alpha=Z_alpha, Z_beta=Z_beta, mu0=mu0, dispersion0=dispersion0, mu1=mu1, dispersion1=dispersion1,
         method=method,
         denom=denom, numerator1=numerator1, numerator2=numerator2)))
   }

   if(method==2){
      n<-(  ( (Z_alpha+Z_beta) *  numerator2 )/denom)^2
   }else{
      n<-(
            (   (Z_alpha*  numerator1) +
                (Z_beta *  numerator2)
            )/denom
         )^2
   }

   # print(linkFn(testdata))
   return(n)
}
