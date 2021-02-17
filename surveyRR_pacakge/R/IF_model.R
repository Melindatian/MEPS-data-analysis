#' Relative Risk and Disease Rate for Parametric model
#'
#' Estimate relative risk using parametric models with complex survey design
#'
#' @details This function estimates the RR using parametric models with complex survey design allowed
#'
#' @param treatment a binary \code{variable} where 1 indicates group of treatment and 0 indicates group of control
#' @param response a binary \code{variable} where 1 indicates group of event and 0 indicates group of non-event
#' @param data a \code{data.frame}
#' @param design a \code{svydesign} object created by the svydesign function using survey package
#' @param formula a model formula
#' @param model : a description of distribution

#'
#' @return
#' \describe{
#' \item{\code{result}}{a \code{data.frame} of estimated RR and treatment rate}
#' }
#'
#' @author Mingmei Tian
#'
#' @references Assessing the Effectiveness of Influenza Vaccination – influence function approach for comparative studies using publicly available medical expenditure data.
#' Mingmei Tian, Jihnhee Yu
#'
#' @import survey
#'
#'
#' @examples
#'library(survey)
#'library(surveyRR)
#'data("mepsRR")
#'options(survey.lonely.psu="adjust")
#'CTdsgn <- svydesign(
#'  id = ~VARPSU,
#'  strata = ~VARSTR,
#'  weights = ~LONGWT,
#'  data = mepsRR,
#'  nest = TRUE)
#'  svyglm_rr_if(treatment="binary_flushot",response="disease_status",data=mepsRR,design=CTdsgn,
#'              formula=disease_status~binary_flushot)
#'
#' @export



svyglm_rr_if<- function(treatment,response,data,design,formula,model=c('logit','log-binominal','probit'),...){

  if(is.null(model)|model=='logit'){

    temp<- logit_rr_if(treatment=treatment,response=response,data=data,design=design,formula=formula,control = list(maxit = 200))
  }else if(model=='log-binominal'){
    temp<- logb_rr_if(treatment=treatment,response=response,data=data,design=design,formula=formula,control = list(maxit = 200))
  }else if(model=='probit'){
    temp<- probit_rr_if(treatment=treatment,response=response,data=data,design=design,formula=formula,control = list(maxit = 200))
  }

  result_list<-list(temp,model)
  names(result_list)<-c('Summary results','Model')
  return(result_list)
}
