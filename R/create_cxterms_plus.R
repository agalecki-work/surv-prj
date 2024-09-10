create_cxterms_plus <- function(mod_cxterms, tv_tnms1="?tv_tnms1?", tt = "TIME", skip_tt= FALSE, DV_name= "DV", ntvarsx = 2){

 mod_skip_tt <- skip_tt
 mod_alphas <- if (is.null(attr(mod_cxterms, "alphas"))) 1 else attr(mod_cxterms, "alphas")
 mod_alpha_pos <- if (is.null(attr(mod_cxterms, "alpha_no"))) NULL  else attr(mod_cxterms, "alpha_pos")
 if (length(mod_alpha_pos) && mod_alpha_pos <= length(mod_alphas)) mod_alphas <- mod_alphas[mod_alpha_pos]
 
 cxterm_row <- if (is.null(attr(mod_cxterms, "form_no"))) NULL else attr(mod_cxterms, "cxterm_select")
 message("-- cxterm_no:  ", cxterm_row)
# Assign names to `mod_xterms` object
if (is.vector(mod_cxterms) && is.null(names(mod_cxterms))) names(mod_cxterms) <- paste0("term", 1:length(mod_cxterms), sep="")
if (is.matrix(mod_cxterms) && is.null(rownames(mod_cxterms))) rownames(mod_cxterms) <- paste0("M", 1:nrow(mod_cxterms), sep="")
if (is.matrix(mod_cxterms) && is.null(colnames(mod_cxterms))) colnames(mod_cxterms) <- paste0("col", 1:ncol(mod_cxterms), sep="")

if (is.vector(mod_cxterms)) mod_cxterms <- mod_cxterms[!duplicated(mod_cxterms)]

# Convert mod_cxterms to matrix if needed

if (length(cxterm_row) == 1)  mod_cxterms <- mod_cxterms[cxterm_row , ]

mod_cxterms_mtx <- if (is.matrix(mod_cxterms)) mod_cxterms else matrix(mod_cxterms, nrow=1, byrow=TRUE)
if (is.vector(mod_cxterms)){ 
  rownames(mod_cxterms_mtx) <- "M0"
  colnames(mod_cxterms_mtx) <- names(mod_cxterms)
  }


# Logical vector indicating columns with tt vars in `mod_cxterms_mtx` matrix
  contains_tt <- apply(mod_cxterms_mtx, 2, FUN =  function(cx) any(grepl("\\{tt\\}",cx)))


# if skip_tt=TRUE then columns with tt vars removed
 if (mod_skip_tt) {  # IF TRUE skip tt columns
     mod_cxterms_mtx <- mod_cxterms_mtx[, !contains_tt]  # tt columns skipped
     if (is.vector(mod_cxterms_mtx)){
         tmp <- matrix(mod_cxterms_mtx, nrow =1, byrow= TRUE)
         rownames(tmp) <- "M0"
         colnames(tmp) <- names(mod_cxterms_mtx)
         mod_cxterms_mtx <- tmp
      }
     mod_tt_data <- FALSE # Because tt columns in `mod_cxterms_mtx` skipped
    } # if mod_skip_tt

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

if (mod_tt_data){ 

  if ( ntvarsx == 2 ){
      mod_cxterms_mtx   <- apply(mod_cxterms_mtx, 1,  FUN = function(xi){
       str_replace_all(xi, "\\{tt\\}", paste0(tv_tnms1,"_tt"))
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
  } # if (mod_tt_data)
  
# mod_cxterms_mtx -> cxterms_plus vector

    cxterms_plus    <- apply(mod_cxterms_mtx, 1,  FUN = function(xi){
                 cxtrms        <- xi[which(xi != "")] 
                 paste(cxtrms, collapse ="+" )
     })      
  return(cxterms_plus)

}
# create_cxterms_plus(cxterms1)

