create_cch_folds <- function(data, subcohort, case, initSplitx, nfolds, seed=numeric(0)){
  # data:  non-cases outside of subcohort are omitted
  if (length(seed) ==0) set.seed(nrow(data))
  idx_subcohort <- which(subcohort ==1)
  idx_cases <- which(case==1)
  idx_extra_cases <- which(case==1 & subcohort == 0)
  
  if (length(initSplitx) == 0){
     tmp <- sample(1:nfolds, size = nrow(data), replace=TRUE)
     data <- data %>% mutate(initSplit = 1, foldid = tmp)
  } else {
     sx <- rep(NA, nrow(data))
     #print(unique(sx))
     tt <- initSplitx
     tx1 <-  sample(c(0,1), size = length(idx_subcohort), replace=TRUE, prob =c(1-tt,tt))
     sx[idx_subcohort]  <- tx1
     #print(tx1)
     #print(table(sx))
     tx2 <-  sample(c(0,1), size = length(idx_extra_cases), replace=TRUE, prob =c(1-tt,tt))
     sx[idx_extra_cases] <- tx2
     data$initSplit <- sx
     
   # 
        tmp0 <-  data$initSplit == 0
        tmp1 <- ifelse(tmp0==0 , NA, -1)
        idx1 <- which(tmp1 == -1)
        tmp2 <- tmp0[idx1]
        sx  <- sample(1:nfolds, size = length(tmp2), replace=TRUE)
        tmp1[idx1] <- sx
        data$foldid <- tmp1
   }
  return(data)
} 

#df1 <- create_cch_folds(df, df$sub_cohort, df$case, 0.8, nfolds=5)

#table(df1$initSplit)
#table(df1$foldid, useNA = "ifany")


