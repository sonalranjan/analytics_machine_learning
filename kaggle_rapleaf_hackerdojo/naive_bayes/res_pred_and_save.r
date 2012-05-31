
#user can manually enter candidates_idx_array

# choose top 3 models
if ( ! exists("candidates_idx_array", envir=.GlobalEnv) ) {
    candidates_idx_array <- chosen_min_test_err_idx[order( v_auc[chosen_min_test_err_idx], decreasing=TRUE ) ]
}

for (chosen_index in candidates_idx_array[1:min(length(candidates_idx_array),3)]) { # iteration over chosen indices

    e_chosen <- m_list[[chosen_index]]
    m_chosen <- e_chosen$model

    #diagnostics
    tr_str <- paste(paste( format(Sys.time(), "%H:%M:%S"),"e_chosen:",chosen_index),
                    paste("chosen:test_err ", flta_prettify(e_chosen$test.pred.err)),
                    paste("pred:test_tpr ", flta_prettify(e_chosen$test.pred.tpr)),
                    paste("pred:test_fpr ", flta_prettify(e_chosen$test.pred.fpr)),
                    paste("pred:test_tpr_by_fpr ", flta_prettify(e_chosen$test.pred.tpr/e_chosen$test.pred.fpr)),
                    paste("pred:test_auc", flta_prettify(e_chosen$test.pred.auc)),
                    sep="\n"
                    )

    # log  the diagnostics to screen and file
    cat( tr_str, sep="\n")
    cat( tr_str, sep="\n", file=out_file, append=TRUE)

    p_df <- pred_model(m_chosen, x)

    # done with prediction
    tr_str <- paste( format(Sys.time(), "%H:%M:%S"), "done with modeling .. ")
    cat( tr_str, sep="\n")
    cat( tr_str, sep="\n", file=out_file, append=TRUE)

    # create an intermediate data-frame, aggregate by uid (on FUN=mean)
    f_df <- data.frame(uid=demog_unlbl_x_hdl$uid, pred0=p_df$pred.raw[,1], pred1=p_df$pred.raw[,2])
    final_df <- aggregate( f_df, by=list( f_df$uid), FUN="mean")

    #
    predfile <- paste("pred_", Sys.getpid(), "_", chosen_index, "_a", flta_prettify((e_chosen$test.pred.auc)*1000), ".csv", sep="")
    write.csv( final_df[,which(colnames(final_df) %in% c("uid", "pred1")) ], file=predfile, row.names=FALSE )
    tr_str <- paste("writing out predictions to file ", predfile)
    cat( tr_str, sep="\n")
    cat( tr_str, sep="\n", file=out_file, append=TRUE)

} # iteration over chosen indices
