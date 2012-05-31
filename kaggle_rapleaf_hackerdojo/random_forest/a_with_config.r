
#------------------------------------------------------

#
library("gdata")
library("randomForest")
library(ROCR)

# utils
source("util.r")
source("model_fit.r")

#------------------------------------------------------

# set up logging files etc
out_file <- paste("out_file_", Sys.getpid(), ".txt", sep="")
print(paste("out_file is ", out_file))

# configuration file should be provided 
# e.g. cfg <- "conf3_10.csv"
#
stopifnot( exists("cfg") )
conf <- read.csv(cfg) #conf3_10.csv
cat( paste( paste(cfg, ":"), colnames(conf),conf[1,]), sep="\n", file=out_file, append=TRUE) # log the current conf

# load all data
source("load_data.r")
#use_demog_lbl_x_hdl <- demog_lbl_x_hdl
if (1) { use_demog_lbl_x_hdl <- demog_lbl_rf_x_hdl_rf; }

tr_str <- paste("data loading is done.")
cat( tr_str, sep="\n")
cat( tr_str, sep="\n", file=out_file, append=TRUE)

m_list <- c()

#------------------------------------------------------
# set up sampled train_data_set
#------------------------------------------------------

outlier_uids <- c()
if ("excl_rep_outliers" %in% colnames(conf)) {
    outliers_count <- conf$excl_rep_outliers
    #ord_tr_cls1 <- order(uid_totrep$totreps, decreasing=TRUE)
    ord_ec_cls1 <- order(uid_totrep$entry_count, decreasing=TRUE)
    outlier_uids <- uid_totrep$uid[ ord_ec_cls1[1:outliers_count] ]
}

class1_lblusr <- lblusr$uid[ which(lblusr$behavior == 1) ]
class0_lblusr <- lblusr$uid[ which(lblusr$behavior == 0) ]
idx_partitions_ <- indices.partition( length(class1_lblusr), conf$npart)

whole_data_set <- use_demog_lbl_x_hdl
conf$impute <- TRUE

for (idx_part in idx_partitions_) {

    tr_str <- paste("starting on outer loop iteration.")
    cat( tr_str, sep="\n")
    cat( tr_str, sep="\n", file=out_file, append=TRUE)

    #------------------------------------------------------
    # from the set of all labeled users:
    # - select users with class_1: 
    #   exclude the set of users at indices idx_part
    # - select users of class_0: 
    #   select a random sample of size = conf$npart_mult * sizeof(class_1 user sample)
    #------------------------------------------------------
    lblusr1_sample <- class1_lblusr[ -idx_part ]
    lblusr0_sample <- sample(class0_lblusr, length(lblusr1_sample)*conf$npart_mult)
    lblusr_sample <- setdiff( union(lblusr1_sample, lblusr0_sample), outlier_uids)

    #
    lblusr1_test_sample <- class1_lblusr[ idx_part ]
    lblusr0_test_sample <- sample(setdiff(class0_lblusr, lblusr0_sample), length(lblusr1_test_sample)*conf$npart_mult)
    lblusr_test_sample <- setdiff( union(lblusr1_test_sample, lblusr0_test_sample), outlier_uids)

    #------------------------------------------------------
    # set up sampled data_set. choose columns for model input
    #------------------------------------------------------
    train_data_set <- use_demog_lbl_x_hdl[ which(use_demog_lbl_x_hdl$uid %in% lblusr_sample), ]
    test_data_set <- use_demog_lbl_x_hdl[ which(use_demog_lbl_x_hdl$uid %in% lblusr_test_sample), ]
    #test_data_set <- use_demog_lbl_x_hdl
    #test_data_set <- whole_data_set

    tr_str <- paste("class_1 sample size: ", length(lblusr1_sample), " class_0 sample size: ", length(lblusr0_sample))
    tr_str <- paste(tr_str, paste("class_1 test sample size: ", length(lblusr1_test_sample), " class_0 test sample size: ", length(lblusr0_test_sample)), sep="\n")
    tr_str <- paste(tr_str, paste("train sample size: ", nrow(train_data_set), "test sample size: ", nrow(test_data_set)), sep="\n")
    cat( tr_str, sep="\n", file=out_file, append=TRUE)

    data_set_col_names <- colnames(train_data_set)
    model_y_col_name <- c("behavior")

    # model_x_col_names <- c("url_1", "url_2", "url_n", "repetitions")
    # if you want all but uid and behavior
    model_exclude_x_col_names <- c("uid", "behavior")
    model_x_col_names <- data_set_col_names[ which( !(data_set_col_names %in% model_exclude_x_col_names) ) ]

    # modeling columns for x,y
    col_x_data_set <- which(data_set_col_names %in% model_x_col_names) # this will get ordered by itself
    col_y_data_set <- which(data_set_col_names %in% model_y_col_name)

    # training set
    y_tr <- train_data_set[, col_y_data_set]
    x_tr <- train_data_set[, col_x_data_set]

    #
    y_test <- test_data_set[, col_y_data_set]
    x_test <- test_data_set[, col_x_data_set]

    if (TRUE == conf$impute) {
        # ...
        tr_str <- paste(
                        paste("Starting train impute:", format(Sys.time(), "%H:%M:%S")),
                        paste(
                              "train_data_set:num_rows", nrow(x_tr),
                              "train_data_set:num_cols", ncol(x_tr),
                              "train_data_set:col_x_data_set ", toString(col_x_data_set)
                              ),
                        sep="\n"
                        )
        cat( tr_str, sep="\n")
        cat( tr_str, sep="\n", file=out_file, append=TRUE)
        # ...
        tr_imptd <- rfImpute(x=x_tr, y=y_tr)
        x_tr <- tr_imptd[, -1]
        y_tr <- tr_imptd[, 1]
        # ...
        tr_str <- paste(
                        paste("Starting test impute:", format(Sys.time(), "%H:%M:%S")),
                        paste(
                              "test_data_set:num_rows", nrow(x_tr),
                              "test_data_set:num_cols", ncol(x_tr),
                              "test_data_set:col_x_data_set ", toString(col_x_data_set)
                              ),
                        sep="\n"
                        )
        cat( tr_str, sep="\n")
        cat( tr_str, sep="\n", file=out_file, append=TRUE)
        # ...
        tst_imptd <- rfImpute(x=x_test, y=y_test)
        x_test <- tst_imptd[, -1]
        y_test <- tst_imptd[, 1]
        # ...
        tr_str <- paste(
                        paste("End test impute:", format(Sys.time(), "%H:%M:%S")),
                        sep="\n"
                        )
        cat( tr_str, sep="\n")
        cat( tr_str, sep="\n", file=out_file, append=TRUE)
    }

    smpSzs <- (c(0.2, 0.3, 0.6, 0.8)[1:3])*nrow(train_data_set)
    nTreeArrays <- c(200, 600, 1000)
    nTreeIdxRanges <- list( c(2:3), c(1:2), c(1:2), c(1:2))
    sampSzConfs <- rbind( smpSzs, nTreeIdxRanges )

    for (ssConfIter in 1:ncol(sampSzConfs)) {
        sampSz <- ceiling(sampSzConfs[1,ssConfIter][[1]])

    # make partitions for cross-validation
    m_data_set <- length(col_x_data_set)
    xval_mTries <- union( c(3, 5, 7), seq( ceiling(sqrt(m_data_set)), floor(2*m_data_set/3), 5))[1:5]

    for (mTry in xval_mTries) { # cross-validation loop

    for (nTree  in nTreeArrays[sampSzConfs[2,ssConfIter][[1]]] ) {

        tr_str <- paste(
                    paste("Starting xval iter:", format(Sys.time(), "%H:%M:%S")),
                    paste(
                        "train_data_set:mTry", mTry,
                        "train_data_set:nTree", nTree,
                        "train_data_set:sampSz", sampSz
                         ),
                    paste(
                        "train_data_set:num_rows", nrow(x_tr),
                        "train_data_set:num_cols", ncol(x_tr),
                        "train_data_set:col_x_data_set ", toString(col_x_data_set)
                        ),
                    sep="\n"
                    )
         cat( tr_str, sep="\n")
         cat( tr_str, sep="\n", file=out_file, append=TRUE)

        #next # for debug

        # model
        #mRF <- make_model(x_tr, y_tr, prior_probs=c(.6, .4))
        #mRF <- make_model(x_tr, y_tr, mtry=mTry, method="class")
        mRF <- make_model(x_tr, y_tr, mtry=mTry, sampsize=sampSz, ntree=nTree, method="class")
        #mRF <- make_model(x_tr, y_tr, mtry=mTry, sampsize=sampSz, ntree=nTree, cutoff=c(0.75, 0.25), method="class")
        #mRF <- make_model(x_tr, y_tr, mtry=mTry, sampsize=sampSz, ntree=nTree, cutoff=c(0.35, 0.55))

        # stats on model-fit on training data
        pred_df <- pred_model(m=mRF, x=x_tr, y=y_tr, xtest=x_test, ytest=y_test, uid_x=train_data_set$uid, uid_xtest=test_data_set$uid)

        # 
        m_list = c(m_list, pred_df)

        if (1) {
            # report on pred_df
            tr_str <- paste( format(Sys.time(), "%H:%M:%S"), "iter:", length(m_list))

            for (pfx in c("train.","test.")) {
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

    } #nTree
    } #sampSz
    } # cross-validation loop

}



# source get_res.r here
# source("get_res.r")
