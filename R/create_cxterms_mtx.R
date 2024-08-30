create_cxterms_mtx <- function(cpattern0, ccommon){
  cpattern <- if (is.null(dim(cpattern0))) matrix(cpattern0, ncol=1)else cpattern0
  cdifx <- setdiff(ccommon, as.vector(cpattern))
  # print(cdifx)
  tmx <- nrow(cpattern)
  # print(tmx)
  repx <- rep(cdifx,tmx)
  repx <- matrix(repx, nrow = nrow(cpattern), byrow = TRUE)
  #print(repx)
  res <- cbind(cpattern, repx)
  #print("==")
  cpat <- paste("cpatt", 1:ncol(cpattern),sep="")
  comx <- paste("comm", 1:length(cdifx), sep="")
  colnames(res) <- c(cpat, comx) # "cpatt1", "cpatt2", comm1, comm2
  rownames(res) <- NULL; # paste("cpatt", 1: nrow(cpattern), sep ="")
  return(res)
}
# create_cxterms_mtx (t1,t2)

# create_cxterms_mtx (tx,t2)
 