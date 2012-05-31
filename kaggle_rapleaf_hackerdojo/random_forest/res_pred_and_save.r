
#------------------------------------------------------
# prepare data set for final predictions. 
#------------------------------------------------------
demog_unlbl <- demog_profile[ which(demog_profile$uid %in% unlblusr$uid), ]
demog_unlbl_x_hdl <- merge(demog_unlbl, hdl, by="uid")
#use_demog_unlbl_x_hdl <- demog_unlbl_x_hdl
if (1) {
    demog_unlbl_rf <- demog_unlbl
    hdl_rf <- hdl
    #demog_excl_cols <- c("addr_city", "state", "country", "age", "home_market_value", "household_income", "length_of_residence")
    demog_excl_cols <- c("addr_city", "state", "country")
    demog_excl_idx <- which(colnames(demog_lbl) %in% demog_excl_cols)
    if (length(demog_excl_idx) > 0 ) { demog_unlbl_rf <- demog_unlbl[,-demog_excl_idx]; }
    hdl_excl_cols <- c("url", "url_n")
    hdl_excl_idx <- which(colnames(hdl) %in% hdl_excl_cols)
    if ( length(hdl_excl_idx) > 0 ) { hdl_rf <- hdl[,-hdl_excl_idx]; }
    demog_unlbl_rf_x_hdl_rf <- merge(demog_unlbl_rf, hdl_rf, by="uid")
}
use_demog_unlbl_x_hdl <- demog_unlbl_rf_x_hdl_rf

#------------------------------------------------------
# be reminded: we need all columns that we used in creating the model.
#              and we need them in the same order.
#------------------------------------------------------
excl_col_idx <- which(colnames(use_demog_unlbl_x_hdl) %in% model_exclude_x_col_names)
x_pred_data_set <- na.roughfix(use_demog_unlbl_x_hdl[, -excl_col_idx])
#user can manually enter candidates_idx_array

pred_and_save <- function(m, pred_data=x_pred_data_set, uid_x=use_demog_unlbl_x_hdl$uid, do_save=TRUE, save_sfx=NULL) {

    predDF <- pred_model(m_chosen, pred_data)

    # done with prediction
    tr_str <- paste( format(Sys.time(), "%H:%M:%S"), "done with modeling .. ")
    cat( tr_str, sep="\n")
    cat( tr_str, sep="\n", file=out_file, append=TRUE)

    if (do_save) {
        # create an intermediate data-frame, aggregate by uid (on FUN=mean)
        if (m_chosen$type == "classification") {
            f_df <- data.frame(uid=uid_x,  pred1=predDF$train.pred$X1)
        } else {
            f_df <- data.frame(uid=uid_x,  pred1=predDF$train.pred[[1]])
        }
        final_df <- aggregate( f_df, by=list( f_df$uid), FUN="mean")

        #
        if (is.null(save_sfx)) { 
            save_sfx=format(Sys.time(), "%H_%M_%S"); 
        } else {
            save_sfx <- paste(save_sfx, "_", format(Sys.time(), "%H_%M_%S"), sep=""); 
        }
        predfile <- paste("pred_", Sys.getpid(), "_", save_sfx, ".csv", sep="")

        write.csv( final_df[,which(colnames(final_df) %in% c("uid", "pred1")) ], file=predfile, row.names=FALSE )
        tr_str <- paste("writing out predictions to file ", predfile)
        cat( tr_str, sep="\n")
        cat( tr_str, sep="\n", file=out_file, append=TRUE)
    }

}


if (1) { # invocation

# choose top 3 models
if ( ! exists("candidates_idx_array", envir=.GlobalEnv) ) {
    candidates_idx_array <- chosen_min_test_err_idx[order( v_auc[chosen_min_test_err_idx], decreasing=TRUE )]
}

for (chosen_index in candidates_idx_array[1:min(length(candidates_idx_array),3)]) { # iteration over chosen indices

    e_chosen <- m_list[[chosen_index]]
    m_chosen <- e_chosen$model

    #diagnostics
    tr_str <- paste(paste( format(Sys.time(), "%H:%M:%S"),"e_chosen:",chosen_index),
                    paste("chosen:test_err ", flta_prettify(min(e_chosen$test.pred.err))),
                    paste("pred:test_auc", flta_prettify(e_chosen$test.pred.auc)),
                    sep="\n"
                    )

    # log  the diagnostics to screen and file
    cat( tr_str, sep="\n")
    cat( tr_str, sep="\n", file=out_file, append=TRUE)

    sfx <- paste(chosen_index, "_a", flta_prettify((e_chosen$test.pred.auc)*1000), sep="")

    pred_and_save(m=m_chosen, pred_data=x_pred_data_set, uid_x=use_demog_unlbl_x_hdl$uid, save_sfx=sfx)

} # iteration over chosen indices

} # invocation
