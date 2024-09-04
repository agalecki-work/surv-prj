#--coxph_Info
# mod_mtx_spec <- coxph_Info$mtx_spec
message("-- Source ` ", srcf, "?")

mod_cxterms_mtx    <- coxph_Info$cxterms_mtx
mod_tt_data        <- coxph_Info$tt_data # TRUE/FALSE

tmp_nms <- paste(rownames(mod_cxterms_mtx), "_tt", sep="")
if (mod_tt_data) rownames(mod_cxterms_mtx) <- tmp_nms
mod_id   <- coxph_Info$id
mod_wght <- coxph_Info$wght
mod_tt_split_length <- coxph_Info$tt_split_length
print(mod_tt_split_length)
