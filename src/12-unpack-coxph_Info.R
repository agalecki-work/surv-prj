#--coxph_Info
# mod_mtx_spec <- coxph_Info$mtx_spec
mod_id   <- coxph_Info$id
mod_wght <- coxph_Info$wght
mod_cxterms_mtx   <- coxph_Info$cxterms_mtx     # Matrix spec, multiple models
mod_cxterms_mtx_tt<- coxph_Info$cxterms_mtx_tt  # select columns in `cxterms_mtx`. Possibly NULL
#mod_cxterms       <- unique(coxph_Info$cxterms) # Vector spec, one model 
#mod_cxterms_tt    <- coxph_Info$cxterms_tt      # Select element(s) 

# Vector with tt cxterms 
#-- cxterms_tt <- if (mod_mtx_spec) unique(mod_cxterms_mtx[, mod_cxterms_mtx_tt]) else mod_cxterms[mod_cxterms_tt]
cxterms_tt <- unique(mod_cxterms_mtx[, mod_cxterms_mtx_tt])

# Create `cpatt_nms`
  matches <- grep("^cpatt",  colnames(mod_cxterms_mtx), value = TRUE)
  tmp_mtx <- mod_cxterms_mtx[ , matches ]
  cpatt_nms <-  if (is.null(dim(tmp_mtx))) tmp_mtx else apply(tmp_mtx, 1, paste, collapse=":") 