# now try to extract the results

#------------------------------------------------------
# prepare list of chosen models
# iterate over all the stored predictions
# collect the AUCs in a vector (maintain the order)
#------------------------------------------------------
l_m_list <- length(m_list)

v_test_err = c()
v_1_err <- c()
v_0_err <- c()
for (i in 1:l_m_list) { 
    e <- m_list[[i]] 
    print( paste(i, flta_prettify(c(e$test.pred.auc, e$train.pred.auc)) ) )
    v_test_err <- c(v_test_err, min(e$test.pred.err))
    v_1_err <- c(v_1_err, summary(e$model$err.rate[,3])[3])
    v_0_err <- c(v_0_err, summary(e$model$err.rate[,2])[3])
    #rm(e)
}

print("************************")
# choose min few test_errs
ord_test_err_idx_array <- order( v_test_err)
chosen_min_test_err_idx <- ord_test_err_idx_array[1:min(length(ord_test_err_idx_array),10)]
print(chosen_min_test_err_idx)
print(v_test_err[chosen_min_test_err_idx])
print(v_1_err[chosen_min_test_err_idx])
print(v_0_err[chosen_min_test_err_idx])


print("************************")
# choose min few 1_errs
ord_1_err_idx_array <- order( v_1_err)
chosen_min_1_err_idx <- ord_1_err_idx_array[1:min(length(ord_1_err_idx_array),10)]
print(chosen_min_1_err_idx)
print(v_test_err[chosen_min_1_err_idx])
print(v_1_err[chosen_min_1_err_idx])
print(v_0_err[chosen_min_1_err_idx])


