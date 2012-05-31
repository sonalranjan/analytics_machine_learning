
#------------------------------------------------------
# prepare data set for final predictions. 
#------------------------------------------------------
demog_unlbl <- demog_profile[ which(demog_profile$uid %in% unlblusr$uid), ]
demog_unlbl_x_hdl <- merge(demog_unlbl, hdl, by="uid")

#------------------------------------------------------
# be reminded: we need all columns that we used in creating the model.
#              and we need them in the same order.
#------------------------------------------------------
ex_uid_col <- which(colnames(demog_unlbl_x_hdl) %in% model_exclude_x_col_names)
x <- demog_unlbl_x_hdl[, -ex_uid_col]
# now try to extract the results

#------------------------------------------------------
# prepare list of chosen models
# iterate over all the stored predictions
# collect the AUCs in a vector (maintain the order)
#------------------------------------------------------
l_m_list <- length(m_list)

v_test_uid_err = c()
v_test_err = c()
v_auc <- c()
for (i in 1:l_m_list) { 
    e <- m_list[[i]] 
    print( paste(i, flta_prettify(c(e$test.pred.auc, e$pred.auc)), flta_prettify(e$test.pred.err), flta_prettify(e$test.pred.tpr)))
    v_test_uid_err <- c(v_test_err, min(e$test.pred.uid.err))
    v_test_err <- c(v_test_err, min(e$test.pred.err))
    v_auc <- c(v_auc, e$test.pred.auc)
    rm(e)
}

# choose min few test_errs
ord_test_err_idx_array <- order( v_test_err)
chosen_min_test_err_idx <- ord_test_err_idx_array[1:min(length(ord_test_err_idx_array),5)]
tr_str <- paste(format(Sys.time(), "%H:%M:%S"), " chosen_idx: ", toString(chosen_min_test_err_idx))
tr_str <- paste(tr_str, 
                paste( "test err:", flta_prettify(v_test_err[chosen_min_test_err_idx])),
                sep="\n")
tr_str <- paste(tr_str, 
                paste( "test auc:", flta_prettify(v_auc[chosen_min_test_err_idx])),
                sep="\n")
cat( tr_str, sep="\n")
cat( tr_str, sep="\n", file=out_file, append=TRUE)

