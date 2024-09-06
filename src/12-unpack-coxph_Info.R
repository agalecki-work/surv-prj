#--coxph_Info
# mod_mtx_spec <- coxph_Info$mtx_spec
message("-- Source ` ", srcf, "?")

mod_cxterms        <- coxph_Info$cxterms

# mod_tt_data        <- coxph_Info$tt_data # TRUE/FALSE
mod_skip_tt          <- coxph_Info$skip_tt # TRUE/FALSE

mod_id   <- coxph_Info$id
mod_wght <- coxph_Info$wght
mod_tt_split_length <- coxph_Info$tt_split_length
# print(mod_tt_split_length)
mod_function <- "coxph"