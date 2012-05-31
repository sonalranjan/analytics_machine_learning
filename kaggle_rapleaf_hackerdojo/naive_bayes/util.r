
# 
flta_prettify <- function( flt_a, digits=3) {
    return ( toString( format(flt_a, digits=digits)))
}


# use only if your R version does not have list2env natively
mylist2env <- function(l, envir=NULL, parent=parent.frame(), hash=FALSE) {
   n_l <- names(l)
   if (is.null(envir)) {
       envir <- new.env(parent=parent, hash=hash)
   }
   for (n in n_l) {
       assign(n, l[[n]], envir=envir)
   }
   return(envir)
}


indices.partition <- function(sz_tr,num_part)
{
    sz_by <-  as.integer(sz_tr/num_part)
    rem <- 1:sz_tr
    exgrp <- c()
    exgrp_list <- list()
    for (i in 1:(num_part-1) ) {
        s1 <- sample(rem, sz_by)
        exgrp <- union(exgrp, s1)
        rem <- setdiff(rem, exgrp)
        exgrp_list <- append(exgrp_list, list(s1))
        # print(list( length(exgrp), length(s1), sz_by))
    }
    if (length(rem) > 0) {
        exgrp_list <- append(exgrp_list, list(rem))
    }
    return (exgrp_list)
}

