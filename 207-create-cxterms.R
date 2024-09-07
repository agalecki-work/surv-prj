# source ("207-create-cxterms.R")  # This file


#---- User needs to create `cxterms` character vector/mtx 
#

nx <- 21   # Number of coxph models in `cxterms_mtx`

nb <- 21   # Number of biomarkers

model_nr <- tibble(nb = 1:nb)

common_cxterms <- c(
    #               AGE      = "AGE", 
                   AGE_ns3  = "ns(AGE , knots = ns_df3[,'AGE'])",
                   BASE_UCR = "BASE_UACR",
                   AGE_tt  = "AGE:{tt}"
                   # strtm   = "strata(strtm)",
                   # offset  = "offset(AGE/10)"
                   )
common_cxterms_mtx <- matrix(rep(common_cxterms, times =nx), nrow=nx, byrow = TRUE)              
colnames(common_cxterms_mtx) <- names(common_cxterms)
rownames(common_cxterms_mtx) <- paste("M", 1:nx, sep="")


## print(head(common_cxterms_mtx))  
# 

# Vector with sequence of cterms:   ----ns(BM#   , knots = ns_df3[,'BM#'])
seq_BMns3 <- model_nr %>% str_glue_data("ns(BM{nb}, knots = ns_df3[,'BM{nb}'])")
message("---- Initial version of `cxterms` matrix/vector defined by the user")

#--- 

cxterms1 <- cbind(common_cxterms_mtx, seq_BMns3)  # matrix with named rows and columns

cxterms2 <- c(AGE = "AGE", FEMALE="FEMALE", AGE_tt = "AGE:{tt}", AGE ="AGE") # vector with named elements  






