#' Relative Risk and Disease Rate for Contingency Table
#'
#' Estimate relative risk using contingency table with complex survey design
#'
#' @details This function estimates the RR using contingency table method with complex survey design allowed
#'
#' @param treatment a \code{list} contains treatment variable name, treatment (exposed) group indicator and control (unexposed) group indicator, respectively.
#' @param response a \code{list} contains response variable name,  event group indicator and non-event group indicator, respectively.
#' @param data a \code{data.frame}
#' @param design a \code{svydesign} object created by the svydesign function using survey package

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
#' library(survey)
#' library(surveyRR)
#' data("mepsRR")
#'options(survey.lonely.psu="adjust")
#'CTdsgn <- svydesign(
#'  id = ~VARPSU,
#'  strata = ~VARSTR,
#'  weights = ~LONGWT,
#'  data = mepsRR,
#'  nest = TRUE)
#'
#'  treatment.list<- list("flushot_status","Having flushot","Not having flushot")
#'  response.list<-   list("disease_status",1,0)
#'  svyct_rr_if(treatment=treatment.list,response=response.list,data=mepsRR, design=CTdsgn)
#' @export


svyct_rr_if<- function(treatment,response,data, design){

  xval<-matrix(data=0,nrow = 4,ncol = nrow(data))
  xval[1,]<- ifelse(data[,treatment[[1]]]==treatment[[2]]&data[,response[[1]]]==response[[2]],1,0)
  xval[2,]<- ifelse(data[,treatment[[1]]]==treatment[[2]]&data[,response[[1]]]==response[[3]],1,0)
  xval[3,]<- ifelse(data[,treatment[[1]]]==treatment[[3]]&data[,response[[1]]]==response[[2]],1,0)
  xval[4,]<- ifelse(data[,treatment[[1]]]==treatment[[3]]&data[,response[[1]]]==response[[3]],1,0)
  A1<-xval[1,]
  B1<-xval[3,]
  A2<-xval[1,]+xval[2,]
  B2<-xval[3,]+xval[4,]

  data$A1<- A1
  data$B1<- B1
  data$A2<- A2
  data$B2<- B2

  mA1<- svymean(~A1,design)[1]
  mA2<- svymean(~A2,design)[1]
  mB1<- svymean(~B1,design)[1]
  mB2<- svymean(~B2,design)[1]

  ct_RR_EST<-mA1*mB2/(mB1*mA2)

  data$ifs_ct<- with(data,ct_RR_EST*(A1/mA1+B2/mB2-B1/mB1-A2/mA2))

  design<-update(design, ifs_ct= data$ifs_ct)


  ct_RR_SD<-as.data.frame(svymean(~ifs_ct,design))[2]

  result_rr<-cbind(ct_RR_EST,ct_RR_SD)

  colnames(result_rr)<-c('EST','SE')
  result_rate<- as.data.frame(svymean(~A2,design))

  colnames(result_rate)<-c('EST','SE')

  result<- rbind(result_rr,result_rate)
  result$TYPE<- c('RR','Treatment Rate')
  return(result)
}


