#' Subset MEPS panel 10 data for RR estimation
#'
#' Data from the Medical Expenditure Panel Survey panel 10, modified for the RR study. The data has 9959 observations with 21 variables. Note that , this data is an example data, not the one used in the published paper. If you need the whole dataset, please contact the author.
#'
#' @docType data
#'
#' @usage data(mepsRR)
#'
#' @format A \code{data.frame} with columns:
#' \describe{
#'  \item{DUPERSID}{}
#'  \item{VARSTR}{}
#'  \item{VARPSU}{}
#'  \item{LONGWT}{}
#'  \item{flushot_status}{}
#'  \item{disease_status}{}
#' }
#'
#' @keywords datasets
#'
#' @references Assessing the Effectiveness of Influenza Vaccination – influence function approach for comparative studies using publicly available medical expenditure data
#' (\href{https://www.meps.ahrq.gov/mepsweb/data_stats/download_data_files_detail.jsp?cboPufNumber=HC-106}{MEPS})
#'
#' @source \href{https://www.meps.ahrq.gov/mepsweb/data_stats/download_data_files_detail.jsp?cboPufNumber=HC-106}{MEPS}
#'
#' @examples
#' data(mepsRR)

"mepsRR"
