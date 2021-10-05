# Project Title

Influence function methods to assess the effectiveness of influenza vaccine with survey data

## Description

Basic codes for Medical Expenditure Panel Survey (MEPS) data analysis applications, as introduced by the paper 'Influence function methods to assess the effectiveness of influenza vaccine with survey data'. This paper is accepted by HSR journal (updpated:10/05/2021) and will be soon published online.

# Introdcution
## Background
It is always challenging to estimate the ever-changing influenza vaccine effectiveness (VE) in the general population. Given the lack of randomized controlled trials in the general population, national-level publicly available survey data is a feasible source to assess the VE. When utilizing complex survey data, it is important to identify robust methods for VE estimation and relevant inference with procedural discussions for handling unbalanced data. We investigate a broad range of data-balancing techniques with the implementation of influence function (IF) methods, where the IF method allows us to easily estimate the variability for the relative risk (RR) estimates in the complex survey setting. Upon implementation of IF methods and data-balancing techniques, a simple approach such as contingency-table-based RR estimation yields a comparable result to the generalized linear model approach. We demonstrate the applicability of the proposed methods for complex survey data using 2000-2016 Medical Expenditure Panel Survey (MEPS) data. 

## Keywords
Complex survey data; Imbalanced data; Influence function; Influenza vaccine effectiveness; Propensity score methods; Relative risk; Influenza-associated expenditure


# Getting Started

## Dependencies

* R (>= 3.5.0)
* required package: dplyr, survey

## Installing

* Download the R and Data package from Github

### Instructions

*  The data.R file is used for the loaded example dataset.
*  Run helper.R .
*  Run survey_matching.R if you need propensity scores matching.
*  The IF_ct.R file provides RR estimation using the contingency table method.
*  The IF_model.R file provides RR estimation using parametric models. There are three choices: logistic regression, log-binomial regression, and probit regression. The default model is the logistic regression.
```
#example
library(survey)
library(dplyr)
library(surveyRR)
data("mepsRR")
options(survey.lonely.psu="adjust")
CTdsgn <- svydesign(
  id = ~VARPSU,
 strata = ~VARSTR,
  weights = ~LONGWT,
  data = mepsRR,
  nest = TRUE)
  svyglm_rr_if(treatment="binary_flushot",response="disease_status",data=mepsRR,design=CTdsgn,
              formula=disease_status~binary_flushot)


```

## Help

Using “adjust” option for lonely PSUs in R survey package
```
options(survey.lonely.psu="adjust")
```

# Authors

Contributors names and contact info

Mingmei Tian [mingmeit@buffalo.edu]
Jihnhee Yu


# Version History
* 0.1
    * Initial Release

# License

This project is licensed under the [GPL-2] License - see the LICENSE.md file for details

# Acknowledgments

Inspiration, code snippets, etc.
* [dplyr](https://github.com/tidyverse/dplyr)
* [survey](https://cran.r-project.org/web/packages/survey/index.html)

