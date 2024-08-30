funNms <- c(
  "SurvSplit1_truncate",
  "SurvSplit2_truncate",
  "create_cxterms_mtx", 
  "create_foldid_vars",
  "create_srs_folds",
  "create_cch_folds", 
  "list_added_components",
  "create_cch_weights",
  "create_spline_basis",
  "expand_data_finegray",
  "split_data_intervals",
  "create_time_dependent_covariates2"
)

funNmsR  <- paste0(funNms,".R") 
lapply(funNmsR, FUN= function(src) source(paste0( "./R/", src)))

rm(funNmsR)
