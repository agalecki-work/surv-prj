
create_cr_vars2 <- function(data, tvars, dvars, outvars = paste0("cr_", tvars)){

  tv1 <- tvars[1]
  tv2 <- dvars[1]
  sv1 <- tvars[2]
  sv2 <- dvars[2]
  T1  <- data[, tv1]
  T2  <- data[, tv2]
  S1  <- data[, sv1]
  S2  <- data[, sv2]

  # Step 1: Construct follow-up time T
  T <- pmin(T1, T2)

  # Step 2: Initialize S to zero (censored)
  S <- rep(0, length(T1))

  # Step 3: Update S for primary event occurring first
  primary_first <- (T1 <= T2 & S1 == 1)
  S[primary_first] <- 1
  primary0 <- (T1 <= T2 & S1 ==0 & S2 ==1)
  S[primary0] <- 2

  # Step 4: Update S for competing event occurring first
  competing_first <- (T2 < T1 & S2 == 1)
  S[competing_first] <- 2
  cmp_risk <- (T2 < T1 & S2 == 0 & S1 ==1)
  S[cmp_risk] <- 1
  
  # Step 5: Handle cases where both events occur at the same time
  # Prioritize the primary event if both happen at the same time
  # both_events <- (T1 == T2)
  # S[both_events & S1 == 1] <- 1
  # S[both_events & S1 != 1 & S2 == 1] <- 2
  S <-   factor(S, 0:2, labels=c("censor", tvars[2], dvars[2]))
  data[, outvars[1]] <- T
  data[, outvars[2]] <- S
  return(data)
 }
  
  
 # dtx <-  create_cr_vars(cric_imputed, c("TIME_LOSS50", "LOSS50"), c("TIME_DEATH", "DEAD"))
