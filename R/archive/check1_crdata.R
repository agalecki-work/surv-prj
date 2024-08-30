check1_crdata <- function(data, tvars, dvars){

  tv1 <- tvars[1]
  tv2 <- dvars[1]
  sv1 <- tvars[2]
  sv2 <- dvars[2]
  T1  <- data[, tv1]
  T2  <- data[, tv2]
  S1  <- data[, sv1]
  S2  <- data[, sv2]

  # Check for mutually exclusive events
  data <- data %>%
    mutate(
      event1_occurrence = ifelse(!is.na(T1) & S1 == 1, 1, 0),
      event2_occurrence = ifelse(!is.na(T2) & S2 == 1, 1, 0),
      both_events = event1_occurrence + event2_occurrence
  )
  return(data)
 }
  
