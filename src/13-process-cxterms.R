### mod_function = "coxph" or "coxnet"

# Detect {tt} columns in mod_cxterms_mtx
if (is.vector(mod_cxterms)) mod_cxterms <- mod_cxterms[!duplicated(mod_cxterms)]

mod_cxterms_mtx <- if (is.matrix(mod_cxterms)) mod_cxterms else matrix(mod_cxterms, nrow=1)
if (is.vector(mod_cxterms)){ 
 rownames(mod_cxterms_mtx) <- "M0"
 colnames(mod_cxterms_mtx) <- names(mod_cxterms)
 }
contains_tt <- apply(mod_cxterms_mtx, 2, FUN =  function(cx) any(grepl("\\{tt\\}",cx)))
#print(contains_tt)

mod_tt_data <- TRUE # Tentative 

if (mod_skip_tt) {
     mod_cxterms_mtx <- mod_cxterms_mtx[, !contains_tt]
     mod_tt_data <- FALSE # Because tt collumns in `mod_cxterms_mtx` skipped
} 

contains_tt_no <- grepl("\\{tt\\}", as.vector(mod_cxterms_mtx))
mod_tt_data <-  all(!contains_tt_no)



# mod_cxterms_mtx -> cxterms_plus
  rownames(mod_cxterms_mtx) <- paste(rownames(mod_cxterms_mtx), "_", DV_name, sep="") 
    
    if (ntvarsx == 3) rownames(mod_cxterms_mtx) <- paste(rownames(mod_cxterms_mtx), "_fg", sep="")

    tmp_nms <- paste(rownames(mod_cxterms_mtx), "_tt", sep="")
    if (mod_tt_data) rownames(mod_cxterms_mtx) <- tmp_nms
    
    cxterms_plus    <- apply(mod_cxterms_mtx, 1,  FUN = function(xi){
                 cxtrms        <- xi[which(xi != "")] 
                 paste(cxtrms, collapse ="+" )
     }) 
     
    # Replace {tt} with time variable
    cxterms_plus <- if (ntvarsx == 2){
       str_replace_all(cxterms_plus, "\\{tt\\}", paste0(tv_tnms[1],"_tt"))
       } else {
       str_replace_all(cxterms_plus, "\\{tt\\}", "fgtime_tt")
       }
 message("----   `cxterms_plus` RHS formulae to be used by coxph models ")
 print(head(cxterms_plus))
    

    
  