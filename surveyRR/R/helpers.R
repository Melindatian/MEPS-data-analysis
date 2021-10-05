

logit_rr_if<-function(treatment,response,data,design,formula,...){

  fitmodel<-try(svyglm(formula,design=design, family=binomial(link = "logit")),
                silent=TRUE)


  if(any(class(fitmodel)=="try-error")){
    result_rr<-data.frame(EST=NA,SE=NA,TYPE="RR")


  }else{
    est_coef<-as.matrix(coef(fitmodel))
    X_matrix<- model.matrix(formula,data)
    Y_matrix<-as.matrix(data[,response])
    exp_vector<- exp(X_matrix%*%est_coef)
    link_vecotr<-exp_vector/(1+exp_vector)
    weight<-1/design$prob
    W_matrix<- diag(c(link_vecotr*(1-link_vecotr)*weight/sum(weight)))
    information_matirx<- solve(t(X_matrix)%*%W_matrix%*%X_matrix)

    score_function<- apply(as.matrix(c(1:nrow(X_matrix))), 1, function(r){

      t(t(X_matrix[r,,drop=FALSE])%*%(Y_matrix[r,,drop=FALSE]-link_vecotr[r,,drop=FALSE]))
    })

    influence_function<-information_matirx%*%score_function
    covaraince_matrix<-(influence_function%*%t(influence_function))
    b0 <- est_coef[1]
    b1 <- est_coef[nrow(est_coef)]
    e1 <- exp(-b0 - 1*b1)
    e2 <- exp(-b0 - 0*b1)
    p1 <- 1/(1+e1)
    p2 <- 1/(1+e2)
    dgdb0 <- -e2*p1 + (1+e2)*p1*(1-p1)
    dgdb1 <- (1+e2)*p1*(1-p1)
    nk<- ncol(X_matrix)-2
    dRR <- as.matrix(c(dgdb0, rep(0,nk), dgdb1))
    influence_RR<- t(dRR)%*%influence_function
    data$influence_RR<-as.vector(influence_RR)


    design<-update(design, influence_RR=data$influence_RR)

    logit_RR_SE<-as.data.frame(svymean(~influence_RR,design))[2]
    logit_RR_EST<-p1/p2

    result_rr<-cbind(logit_RR_EST,logit_RR_SE)
    colnames(result_rr)<-c('EST','SE')
    result_rr$TYPE<-'RR'

  }


  result_rate<- as.data.frame(svymean(as.formula(paste0("~",treatment)),design))
  colnames(result_rate)<-c('EST','SE')
  result_rate$TYPE<-'Treatment Rate'
  result<- rbind(result_rr,result_rate)
  return(result)

}

#logit_rr_if(treatment=treatment,response=response,data=data,design=design,formula=formula,control = list(maxit = 200))

logb_rr_if<-function(treatment,response,data,design,formula,...){

  fitmodel<-try(svyglm(formula,design=design, family=binomial(link='log')),
                silent=TRUE)

  if(any(class(fitmodel)=="try-error")){
    result_rr<-data.frame(EST=NA,SE=NA,TYPE="RR")


  }else{

    est_coef<-as.matrix(coef(fitmodel))
    X_matrix<- model.matrix(formula,data)
    Y_matrix<-as.matrix(data[,response])
    exp_vector<- exp(X_matrix%*%est_coef)
    link_vecotr<-exp_vector
    weight<-1/design$prob
    W_matrix<- diag(c(link_vecotr)*weight/sum(weight))
information_matirx<- solve(t(X_matrix)%*%W_matrix%*%X_matrix,tol = 1e-100)

score_function<- apply(as.matrix(c(1:nrow(X_matrix))), 1, function(r){

  t(t(X_matrix[r,,drop=FALSE])%*%(Y_matrix[r,,drop=FALSE]-link_vecotr[r,,drop=FALSE]))
})

influence_function<-information_matirx%*%score_function
#covaraince_matrix<-(influence_function%*%t(influence_function))
b1 <- est_coef[nrow(est_coef)]
logb_RR<-exp(b1)
nk<- ncol(X_matrix)-2
dRR <- as.matrix(c(0, rep(0,nk),logb_RR))
influence_RR<- t(dRR)%*%influence_function
data$influence_RR<-as.vector(influence_RR)
design<-update(design, influence_RR=data$influence_RR)

logb_RR_SE<-as.data.frame(svymean(~influence_RR,design))[2]
logb_RR_EST<-logb_RR


    result_rr<-cbind(logb_RR_EST,logb_RR_SE)
    colnames(result_rr)<-c('EST','SE')
    result_rr$TYPE<-'RR'

  }


  result_rate<- as.data.frame(svymean(as.formula(paste0("~",treatment[[1]])),design))
  colnames(result_rate)<-c('EST','SE')
  result_rate$TYPE<-'Treatment Rate'
  result<- rbind(result_rr,result_rate)

  return(result)

}


#logb_rr_if(treatment=treatment,response=response,data=data,design=design,formula=formula,control = list(maxit = 200))


probit_rr_if<-function(treatment,response,data,design,formula,...){

  fitmodel<-try(svyglm(formula,design=design, family=binomial(link='probit')),
                silent=TRUE)

  if(any(class(fitmodel)=="try-error")){
    result_rr<-data.frame(EST=NA,SE=NA,TYPE="RR")


  }else{
    est_coef<-as.matrix(coef(fitmodel))
    X_matrix<- model.matrix(formula,data)
    Y_matrix<-as.matrix(data[,response])
    multi_vector<- X_matrix%*%est_coef
    link_vecotr<-pnorm(multi_vector)
    weight<-1/design$prob
    W_matrix<- diag(c(dnorm(multi_vector))*weight/sum(weight))
    information_matirx<- solve(t(X_matrix)%*%W_matrix%*%X_matrix)

    score_function<- apply(as.matrix(c(1:nrow(X_matrix))), 1, function(r){

      t(t(X_matrix[r,,drop=FALSE])%*%(Y_matrix[r,,drop=FALSE]-link_vecotr[r,,drop=FALSE]))
    })

    influence_function<-information_matirx%*%score_function
    covaraince_matrix<-(influence_function%*%t(influence_function))
    b0 <- est_coef[1]
    b1 <- est_coef[nrow(est_coef)]
    e1 <- dnorm(b0 +1*b1)
    e2 <- dnorm(b0 +0*b1)
    p1 <- pnorm(b0 +1*b1)
    p2 <- pnorm(b0 +0*b1)
    dgdb0 <- (e1*p2-e2*p1)/((p2)**2)
    dgdb1 <- e1/p2

    nk<- ncol(X_matrix)-2
    dRR <- as.matrix(c(dgdb0, rep(0,nk), dgdb1))
    influence_RR<- t(dRR)%*%influence_function
    data$influence_RR<-as.vector(influence_RR)


    design<-update(design, influence_RR=data$influence_RR)

    probit_RR_SE<-as.data.frame(svymean(~influence_RR,design))[2]
    probit_RR_EST<-p1/p2

    result_rr<-cbind(probit_RR_EST,probit_RR_SE)
    colnames(result_rr)<-c('EST','SE')
    result_rr$TYPE<-'RR'

  }


  result_rate<- as.data.frame(svymean(as.formula(paste0("~",treatment[[1]])),design))
  colnames(result_rate)<-c('EST','SE')
  result_rate$TYPE<-'Treatment Rate'
  result<- rbind(result_rr,result_rate)
  return(result)

}

#probit_rr_if(treatment=treatment,response=response,data=data,design=design,formula=formula,control = list(maxit = 200))
