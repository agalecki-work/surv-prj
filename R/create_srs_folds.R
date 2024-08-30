create_srs_folds <- function(data, initSplitx, nfolds, seed=numeric(0)){
  if (length(seed) ==0) set.seed(nrow(data))
  if (length(initSplitx) == 0){
     tmp <- sample(1:nfolds, size = nrow(data), replace=TRUE)
     data <- data %>% mutate(initSplit = 1, foldid = tmp)
  } else {
     tt <- initSplitx
     tmp0 <-  sample(c(0,1),size = nrow(data), replace=TRUE,prob =c(1-tt,tt))
     data$initSplit <- tmp0
     tmp1 <- ifelse(tmp0==0 , NA, -1)
     idx1 <- which(tmp1 == -1)
     tmp2 <- tmp0[idx1]
     sx  <- sample(1:nfolds, size = length(tmp2), replace=TRUE)
     tmp1[idx1] <- sx
     data$foldid <- tmp1
  }
  return(data)
} 
