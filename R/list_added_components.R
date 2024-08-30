list_added_components <- function(list0, list1){
  nms0 <- names(list0)
  nms1 <- names(list1)
  added_components <- setdiff(nms1, nms0)

if (length(added_components) == 0){
        res <- paste0("No new components created in second list : ... OK")
    } else {
        res <- paste0("Number of added components is ", 
        length(added_components),
        ". Error: It should be zero")
    } 
   return(res)
}