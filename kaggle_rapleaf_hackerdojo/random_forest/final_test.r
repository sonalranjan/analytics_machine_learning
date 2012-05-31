
data_set <- use_demog_lbl_x_hdl # whole data
y_whole_lbl <- data_set[ , col_y_data_set]
x_whole_lbl <- data_set[ , col_x_data_set]

#
pred_and_print <- function(m, x=x_whole_lbl, y=y_whole_lbl, uid_x=data_set$uid, iterIdx=1) {

    # start predict
    tr_str <- paste( format(Sys.time(), "%H:%M:%S"), "start iter: ", iterIdx)
    cat( tr_str, sep="\n")
    cat( tr_str, sep="\n", file=out_file, append=TRUE)

    pred_df <- pred_model(m, x=x, y=y, uid_x=uid_x)

    # end predict. report on pred_df
    tr_str <- paste( format(Sys.time(), "%H:%M:%S"), "end iter: ", iterIdx)

    if (1) {
        for (pfx in c("train.")) {
            for (upfx in c("pred.")) {
                auc <- get(paste(pfx,upfx,"auc",sep=""),  envir=pred_df)
                fpr <- get(paste(pfx,upfx,"fpr",sep=""),  envir=pred_df)
                tpr <- get(paste(pfx,upfx,"tpr",sep=""),  envir=pred_df)
                err <- get(paste(pfx,upfx,"err",sep=""),  envir=pred_df)
                thr <- get(paste(pfx,upfx,"thr",sep=""),  envir=pred_df)
                err_thr_len <- length(thr)
                sample_err_idx <- seq(1, err_thr_len, ( err_thr_len %/% 10) )

                tr_str <- paste(tr_str,
                                paste( paste(pfx,upfx,"thr",sep=""), flta_prettify(thr[sample_err_idx])), 
                                paste( paste(pfx,upfx,"err",sep=""), flta_prettify(err[sample_err_idx])), 
                                sep="\n"
                                )

                if (exists( paste(pfx,upfx,"uid.err",sep=""), envir=pred_df)) {
                    uid.err <- get(paste(pfx,upfx,"uid.err",sep=""), envir=pred_df)
                    tr_str <- paste(tr_str,
                                    paste( paste(pfx,upfx,"uid.err",sep=""), flta_prettify(uid.err)), 
                                    sep="\n"
                                    )
                }

                tr_str <- paste(tr_str,
                                paste( paste(pfx,upfx,"tpr",sep=""), flta_prettify(tpr[sample_err_idx])), 
                                paste( paste(pfx,upfx,"fpr",sep=""), flta_prettify(fpr[sample_err_idx])), 
                                paste( paste(pfx,upfx,"auc",sep=""), flta_prettify(auc)), 
                                sep="\n"
                                )
            }
        }
    }

    cat( tr_str, sep="\n")
    cat( tr_str, sep="\n", file=out_file, append=TRUE)

    return( pred_df)
}

do_pred_loop <- function(candidates_idx_array, model_list, x, y, uid_x=NULL, maxIdx=3) {

    for (chosen_index in candidates_idx_array[1:min(length(candidates_idx_array),maxIdx)]) { # iteration over chosen indices

        e_chosen <- model_list[[chosen_index]]
        m_chosen <- e_chosen$model

        if (1) {
            #diagnostics
            err_thr_len <- length(e_chosen$test.pred.thr)
            sample_err_idx <- seq(1, err_thr_len, ( err_thr_len %/% 10) )
            tr_str <- paste(paste( format(Sys.time(), "%H:%M:%S"),"final_test:",chosen_index),
                            paste("chosen:test_err ", flta_prettify(e_chosen$test.pred.err[sample_err_idx])),
                            paste("pred:test_tpr ", flta_prettify(e_chosen$test.pred.tpr[sample_err_idx])),
                            paste("pred:test_fpr ", flta_prettify(e_chosen$test.pred.fpr[sample_err_idx])),
                            paste("pred:test_auc", flta_prettify(e_chosen$test.pred.auc)),
                            sep="\n"
                            )
        }

        # log  the diagnostics to screen and file
        cat( tr_str, sep="\n")
        cat( tr_str, sep="\n", file=out_file, append=TRUE)

        pred_and_print(m_chosen, x=x, y=y, uid_x=uid_x)

    } # iteration over chosen indices
}

if (0) {
    if (exists("candidates_idx_array", envir=environment())) {
        do_pred_loop(candidates_idx_array, m_list, x=x_whole_lbl, y=y_whole_lbl, uid_x=data_set$uid, maxIdx=1)
    }
}
