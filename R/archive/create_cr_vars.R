create_cr_vars <- function(data, tvars, dvars, outvars = paste0("cr_", tvars)){
  #-- eTIME_LOSS50 <- with(train_data, ifelse(LOSS50==0, TIME_DEATH, TIME_LOSS50))
  #-- event_LOSS50 <- with(train_data, ifelse(LOSS50==0, 2*DEAD, 1))
  #-- event_LOSS50 <- factor(event_LOSS50, 0:2, labels=c("censor", "LOSS50", "death"))
  # tvars c("TIME_LOSS50", "LOSS50")
  # dvars c("TIME_DEATH", "DEATH")
  assign("tvars1x", data[, tvars[1]]) # store TIME_LOSS50 in tvars1
  assign("tvars2x", data[, tvars[2]]) # store LOSS50 in tvars1
  assign("dvars1x", data[, dvars[1]]) # store TIME_DEATH in dvars1
  assign("dvars2x", data[, dvars[2]]) # store DEAD in dvars1
 
  etime <- with(data,  ifelse(tvars2x ==0, dvars1x, tvars2x))
  event <- with(data,  ifelse(tvars2x ==0, 2*dvars2x, 1))
  event <- factor(event, 0:2, labels=c("censor", tvars[2], dvars[2]))
  data[, outvars[1]] <- etime
  data[, outvars[2]] <- event
  return(data)
 }
  
  
 # dtx <-  create_cr_vars(cric_imputed, c("TIME_LOSS50", "LOSS50"), c("TIME_DEATH", "DEAD"))
