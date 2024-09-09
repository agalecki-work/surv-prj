### mod_function = "coxph" or "coxnet"

message("-- ntvarsx =", ntvarsx)
message("-- skip_tt = ", mod_skip_tt)
message("-- is.vector(mod_cxterms) = ", is.vector(mod_cxterms))

# Extract attributes from `mod_cxterms`

mod_alphas <- if (is.null(attr(mod_cxterms, "alphas"))) 1 else attr(mod_cxterms, "alphas")

mod_alpha_pos <- if (is.null(attr(mod_cxterms, "alpha_pos"))) NULL  else attr(mod_cxterms, "alpha_pos")

if (length(mod_alpha_pos) && mod_alpha_pos <= length(mod_alphas)) mod_alphas <- mod_alphas[mod_alpha_pos]

# Assign names to `mod_xterms` object
if (is.vector(mod_cxterms) && is.null(names(mod_cxterms))) names(mod_cxterms) <- paste0("term", 1:length(mod_cxterms), sep="")
if (is.matrix(mod_cxterms) && is.null(rownames(mod_cxterms))) rownames(mod_cxterms) <- paste0("M", 1:nrow(mod_cxterms), sep="")
if (is.matrix(mod_cxterms) && is.null(colnames(mod_cxterms))) colnames(mod_cxterms) <- paste0("col", 1:ncol(mod_cxterms), sep="")



if (is.vector(mod_cxterms)) mod_cxterms <- mod_cxterms[!duplicated(mod_cxterms)]


mod_cxterms_mtx <- if (is.matrix(mod_cxterms)) mod_cxterms else matrix(mod_cxterms, nrow=1, byrow=TRUE)
if (is.vector(mod_cxterms)){ 
 rownames(mod_cxterms_mtx) <- "M0"
 colnames(mod_cxterms_mtx) <- names(mod_cxterms)
 }

# Logical vector indicating columns with tt vars in `mod_cxterms_mtx` matrix
contains_tt <- apply(mod_cxterms_mtx, 2, FUN =  function(cx) any(grepl("\\{tt\\}",cx)))

message ("-- contains_tt")
print(head(contains_tt))

mod_tt_data <- TRUE # Tentatively `mod_cxterms_mtx` contains tt elements

# if skip_tt=TRUE then columns with tt vars removed
if (mod_skip_tt) {  # IF TRUE skip tt columns
     mod_cxterms_mtx <- mod_cxterms_mtx[, !contains_tt]  # tt columns skipped
     if (is.vector(mod_cxterms_mtx)){
         tmp <- matrix(mod_cxterms_mtx, nrow =1, byrow= TRUE)
         rownames(tmp) <- "M0"
         colnames(tmp) <- names(mod_cxterms_mtx)
         mod_cxterms_mtx <- tmp
      }
     mod_tt_data <- FALSE # Because tt collumns in `mod_cxterms_mtx` skipped
}


mod_tt_data <-  any(grepl("\\{tt\\}", as.vector(mod_cxterms_mtx))) # TRUE if any element is tt



# rownames assigned to mod_cxterms_mtx 
  rownames(mod_cxterms_mtx) <- paste(rownames(mod_cxterms_mtx), "_", DV_name, sep="") 
    
    if (ntvarsx == 3) rownames(mod_cxterms_mtx) <- paste(rownames(mod_cxterms_mtx), "_fg", sep="")

    tmp_nms <- paste(rownames(mod_cxterms_mtx), "_tt", sep="")
    if (mod_tt_data) rownames(mod_cxterms_mtx) <- tmp_nms

message("--- Processed `mod_cxterms_mtx` (it may contain unresolved tt terms)")
message("- rows/cols: ", nrow(mod_cxterms_mtx), "x" , ncol(mod_cxterms_mtx))
message("- row/col/nm1: ", rownames(mod_cxterms_mtx)[1], ":" , colnames(mod_cxterms_mtx)[1])
col_saved <-  colnames(mod_cxterms_mtx)
#print(str(mod_cxterms_mtx)) 

#----- Resolve tt terms in `mod_cxterms_mtx` matrix


if (mod_tt_data){ 


  if ( ntvarsx == 2 ){
      mod_cxterms_mtx   <- apply(mod_cxterms_mtx, 1,  FUN = function(xi){
       str_replace_all(xi, "\\{tt\\}", paste0(tv_tnms[1],"_tt"))
      })}  # if ntvarsx == 2
  
  if (ntvarsx > 2 ){
        mod_cxterms_mtx <- apply(mod_cxterms_mtx, 1,  FUN = function(xi){
         str_replace_all(xi, "\\{tt\\}", "fgtime_tt")
      })}  # if ntvarsx > 2 
  mod_cxterms_mtx <- t(mod_cxterms_mtx)
  colnames(mod_cxterms_mtx) <- col_saved

  
  message("--- `mod_cxterms_mtx` (tt terms resolved)")
  message("- rows/cols: ", nrow(mod_cxterms_mtx), "x" , ncol(mod_cxterms_mtx))
  message("- row/col/nm1: ", rownames(mod_cxterms_mtx)[1], ":" , colnames(mod_cxterms_mtx)[1])

 # print(str(mod_cxterms_mtx)) 
}




# mod_cxterms_mtx -> cxterms_plus vector

    cxterms_plus    <- apply(mod_cxterms_mtx, 1,  FUN = function(xi){
                 cxtrms        <- xi[which(xi != "")] 
                 paste(cxtrms, collapse ="+" )
     }) 
     
  if (mod_tt_data){
    cxterms_plus <- if (ntvarsx == 2){
       str_replace_all(cxterms_plus, "\\{tt\\}", paste0(tv_tnms[1],"_tt"))
       } else {
       str_replace_all(cxterms_plus, "\\{tt\\}", "fgtime_tt")
       }
   }
  
 message("----   `cxterms_plus` RHS formulae to be used by coxph models ")
 print(head(cxterms_plus))
    

    
  