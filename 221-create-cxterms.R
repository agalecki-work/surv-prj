# source ("221-create-cxterms.R")  # This file
# rm(list=ls())  # to test this script
source("./R/create_cxterms_plus.R")


# create different instancea of cxterms matrix

#---- User needs to create `cxterms` character vector/mtx 
#

nx <- 21   # Number of coxph models in `cxterms_mtx`

nb <- 21   # Number of biomarkers



common_cxterms <- c(
    #  AGE      = "AGE", 
       AGE_ns3  = "ns(AGE , knots = ns_df3[,'AGE'])", # cxterm defined in `205create-aux-objects script
       BASE_UCR = "BASE_UACR",
       AGE_tt   = "AGE:{tt}"
       # strtm  = "strata(strtm)",
       # offset  = "offset(AGE/10)"
     )
common_cxterms_mtx <- matrix(rep(common_cxterms, times =nx), nrow=nx, byrow = TRUE)              
colnames(common_cxterms_mtx) <- names(common_cxterms)
rownames(common_cxterms_mtx) <- paste("M", 1:nx, sep="")


## print(head(common_cxterms_mtx))  
# 
model_nr <- tibble(nb = 1:nb)
# Vector with sequence of cterms:   ----ns(BM#   , knots = ns_df3[,'BM#'])
seq_BMns3 <- model_nr %>% str_glue_data("ns(BM{nb}, knots = ns_df3[,'BM{nb}'])")
message("---- Initial version of `cxterms` matrix/vector defined by the user")

#--- 
cxterms1 <- cbind(common_cxterms_mtx, seq_BMns3)  # matrix with named rows and columns
attr(cxterms1, "form_no") <- 15 

cxterms2 <- common_cxterms  # vector
attr(cxterms2, "form_no") <- 1 

cxterms3 <- matrix(common_cxterms, nrow=1, byrow =TRUE)  # matrix with one row
attr(cxterms3, "form_no") <- 1 

message("=== test1")
test1 <- create_cxterms_plus(cxterms1)
print(test1)

message("---- test2")
test2 <- create_cxterms_plus(cxterms2)
print(test2)

message("==== test3")
test3 <- create_cxterms_plus(cxterms3)
print(test3)



