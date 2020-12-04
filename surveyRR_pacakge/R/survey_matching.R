#' Matching SoftWare for Causal Inference with Complex Survey Design
#'
#'
#'
#' @details This function estimates the RR using parametric models with complex survey design allowed
#'
#' @param treatment a binary \code{variable} where 1 indicates group of treatment and 0 indicates group of control
#' @param covariates a \code{list} of pre-treatment covariates
#' @param data a \code{data.frame}
#' @param design a \code{svydesign} object created by the svydesign function using survey package
#' @param ps.method This argument specifies a matching method. Currently, 'M'(nearest neighbor matching, the weight adjustment will do automatically), 'SW'(stablized weighting)
#' @param distance.threshold The threshold value for matched case selection. If you choose 'M', then you have to give a value for this argument.
#' @param M The maximum number of control cases

#'
#' @return
#' \describe{
#' \item{\code{balanced_data}}{a \code{data.frame} of data balanced with propensity score methods.If "M" is used, then the weights variable is either
#' original weight variable or "MA_weight" for weight adjustment method. If 'SW' is used, then "SW_weight", "IPTW_weight" is the weight for un-weighted data,
#' using stablized weight method and inverse probability weighting method, and "SW_longwt", "IPTW_longwt" is the corresponding weight for complex survey data.  }
#' }
#'
#' @author Mingmei Tian
#'
#' @references Assessing the Effectiveness of Influenza Vaccination – influence function approach for comparative studies using publicly available medical expenditure data.
#' Mingmei Tian, Jihnhee Yu
#' @references Xu S, Ross C, Raebel MA, et al. Use of stabilized inverse propensity scores as weights to directly estimate relative risk and its confidence intervals. Value in Health 2010; 13: 273-277.
#' @references Reardon SF, Cheadle JE and Robinson JP. The effect of Catholic schooling on math and reading development in kindergarten through fifth grade. Journal of Research on Educational Effectiveness 2009; 2: 45-87.
#'
#' @import survey
#' @importFrom dplyr slice_max
#'
#'
#' @examples
#'library(survey)
#'library(surveyRR)
#'data("mepsRR")
#'options(survey.lonely.psu="adjust")
#'mycovariates<-c( "AGELAST"   ,   "race"   ,      "sex"   ,  "diabetes"  ,   "asthma"    ,   "heartdisease", "health"    ,
#' "mnhlth"   ,   "BMILAST"    ,  "education"  ,  "employed"    , "insurance"  )
#'CTdsgn <- svydesign(
#'  id = ~VARPSU,
#'  strata = ~VARSTR,
#'  weights = ~LONGWT,
#'  data = mepsRR,
#'  nest = TRUE)
#'  survey_matching(treatment="binary_flushot",covariates=mycovariates,
#'  data=mepsRR,design=CTdsgn,ps.method="M",distance.threshold=10^-5,M=2)
#'
#' @export



survey_matching<-function(treatment,covariates,data,design,ps.method=c("M","SW"),distance.threshold,M...){

  h_mathing<- as.formula(paste0(treatment,"~",
                                paste(covariates ,collapse='+')))
  ps <- svyglm(h_mathing, family = binomial(),design=design, control = list(maxit = 200))
  psValues <- predict(ps,type = "response")
  psTreated <- psValues[data[,treatment] == 1]
  psUntreated <- psValues[data[,treatment] == 0]
  if(ps.method=="SW"){
    data$SW_weight=1
    data$IPTW_weight=1
    ind_treatment<- which(data[,treatment] == 1)
    ind_control<- which(data[,treatment] == 0)

    p_hat<- as.data.frame(svymean(~get(treatment), design=design))[1,1]
    data$IPTW_weight[ind_treatment]<- 1/(psTreated)
    data$IPTW_weight[ind_control]<- 1/((1-psUntreated))

    data$SW_weight[ind_treatment]<- p_hat/(psTreated)
    data$SW_weight[ind_control]<- (1-p_hat)/((1-psUntreated))


    data$SW_longwt<- data$SW_weight/design$prob
    data$IPTW_longwt<- data$IPTW_weight/design$prob

    balanced_data<- data
  }else{

    limit <- distance.threshold * sd(psValues)
    top_value= M
    matches <- matrix(NA, ncol = top_value +1, nrow = length(psTreated))
    for (i in 1:nrow(matches)) {
      #i=1
      x <- psTreated[i]
      distance <- (x - psUntreated)^2
      if (any(abs(distance) <= limit)) {
        nn <- which(abs(distance) <= limit)
        if(length(nn)>=top_value){
          temp_score<- as.data.frame(cbind(nn,abs(distance[nn])))
          nn_top<- slice_max(temp_score, n=top_value,order_by = V2)
          if(nrow(nn_top)>top_value)nn_top<-nn_top[sample(nrow(nn_top), top_value),]
          matches[i, ] <- c(i,nn_top$nn)
        }else{
          matches[i, ] <- c(i,nn,rep(NA,top_value-length(nn)))
        }

      } else {
        matches[i, ] <- c(i,rep(NA,top_value))
      }
    }
    match_class<- as.data.frame(matches[,-1])
    rownames(match_class)<- which(data[,treatment] == 1)


    match_class[is.na(match_class)]<-0
    match_class_only<-as.matrix(match_class)

    match_class_only<- match_class_only[!(rowSums(match_class_only)==0),,drop=FALSE]
    matched_control_repeat<- as.vector(as.matrix(match_class_only))
    if(length(which(matched_control_repeat==0))!=0) matched_control_repeat<-matched_control_repeat[-which(matched_control_repeat==0)]

    matched_control_repeat<-ind_control[matched_control_repeat]
    matched_control<-unique(matched_control_repeat)
    matched_treat<- rownames(match_class_only)

    # Construct the matched data
    selected_seq<-as.numeric(c(matched_control,matched_treat))
    selected_seq<-selected_seq[order(selected_seq)]

    matched_data<- data[selected_seq,,drop=FALSE]

    #@ Create a matching status matrix  @#
    match_status<- matrix(data=0,nrow = nrow(data),ncol = nrow(data))

    for(i in 1:length(matched_treat)){
      temp<-match_class_only[i,,drop=FALSE]
      status_1<-temp[!(temp==0)]
      status_1_treat<- ind_control[status_1]
      match_status[status_1_treat,as.numeric(matched_treat[i])]<-1

    }


    #@ Calculate the weights after mathcing using original weights@#

    matched_control<-matched_control[order(matched_control)]

    weight_vector<-as.matrix(1/design$prob)

    demoninato_vector<-t(match_status)%*% weight_vector

    demoninato_vector_modify<-demoninato_vector

    demoninato_vector_modify[which(demoninato_vector_modify==0)]<-1

    weight_vector_modify<- 1/demoninato_vector_modify

    balanced_weight<- (match_status* c(weight_vector)) %*% weight_vector_modify * weight_vector

    all_matched_seq<-selected_seq
    balanced_weight_keepmatchonly<-balanced_weight[all_matched_seq]

    matched_data<-matched_data%>%
      mutate(MA_weight=ifelse(balanced_weight_keepmatchonly==0,weight_vector,balanced_weight_keepmatchonly))


    balanced_data<-matched_data

  }

  return(balanced_data)

}


