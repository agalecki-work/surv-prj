create_cch_weights <- function(datax, xsubcohort, xcase, n_total){
   
   dfx <- datax[, c(xsubcohort, xcase)]
   colnames(dfx) <- c("subcohort", "case")
   # print(colnames(dfx))
   ttx <- paste(sort(unique(dfx$subcohort)), collapse = "")
   # print (ttx)
   ## n_total  number of subjects in the entire cohort study
   n_subcohort = sum(dfx$subcohort)
   ## n_non_cases_overall = sum(data$case == 0)
   n_non_cases_overall <- n_total - sum(dfx$case == 1)
   # print(n_subcohort)
   # print(n_non_cases_overall)
   # print(n_total)
   # print(head(dfx))
 # Calculate various C-C weights

   dt <- dfx  %>%
   mutate(
    CCH_Self = case_when(
        subcohort == 1 ~ 1,
        subcohort == 0 & case == 1 ~ n_total / n_subcohort,
        TRUE ~ 0 # Non-cases not in the subcohort should not have weights assigned
        ),
  
    CCH_SelfPrentice = case_when(
      subcohort == 1 & case == 0 ~ 1,
      subcohort == 1 & case == 1 ~ 1 + n_subcohort / n_non_cases_overall,
      subcohort == 0 & case == 1 ~ n_subcohort / n_non_cases_overall,
      TRUE ~ 0        # Cases that are not part of the subcohort have a calculated weight
    ),
    
    CCH_BorganI = case_when(
        subcohort == 1 ~ 1,
        subcohort == 0 & case == 1 ~ n_total / n_subcohort,
        TRUE ~ 0  # Non-cases outside of the subcohort typically not assigned weights
    )
   )
    
  if (ttx != "01"){  # no subcohort
   dt$CCH_Self <-1
   dt$CCH_SelfPrentice <- 1
   dt$CCH_BorganI <- 1
  }
  dt <- dt %>% select(-c(subcohort, case))
  return(cbind(datax, dt))
 } # create_cch_weights 
