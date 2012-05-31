
#------------------------------------------------------

#
library(e1071)

# utils
source("load_data.r")
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

# choose production or test runs
prod_run <- TRUE
if ("prod_run" %in% colnames(conf)) {
    prod_run <- conf$prod_run
}

#------------------------------------------------------
# read all  the input files. load data and place variables in global env
#------------------------------------------------------
load_data(!prod_run, envir=.GlobalEnv, export_vars = vars_exportable)

m_list = c()

#------------------------------------------------------
# set up sampled data_set
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
idx_partitions_ = indices.partition( length(class1_lblusr), conf$npart)

for (idx_part in idx_partitions_) {

    #------------------------------------------------------
    # from the set of all labeled users:
    # - select users with class_1: 
    #   exclude the set of users at indices idx_part
    # - select users of class_0: 
    #   select a random sample of size = conf$npart_mult * sizeof(class_1 user sample)
    #------------------------------------------------------
    lblusr1_sample = class1_lblusr[ -idx_part ]
    lblusr0_sample = sample(class0_lblusr, length(lblusr1_sample)*conf$npart_mult)
    lblusr_sample = setdiff( union(lblusr1_sample, lblusr0_sample), outlier_uids)

    #------------------------------------------------------
    # set up sampled data_set. choose columns for model input
    #------------------------------------------------------
    data_set <- demog_lbl_x_hdl[ which(demog_lbl_x_hdl$uid %in% lblusr_sample), ]
    data_set_col_names <- colnames(data_set)
    model_y_col_name <- c("behavior")

    # model_x_col_names <- c("url_1", "url_2", "url_n", "repetitions")
    # if you want all but uid and behavior
    model_exclude_x_col_names <- c("uid", "behavior", "addr_city", "state", "country", "age", "length_of_residence","marital_status", "home_owner_status")
    model_x_col_names <- data_set_col_names[ which( !(data_set_col_names %in% model_exclude_x_col_names) ) ]

    # modeling columns for x,y
    col_x_data_set <- which(data_set_col_names %in% model_x_col_names) # this will get ordered by itself
    col_y_data_set <- which(data_set_col_names %in% model_y_col_name)


    # make partitions for cross-validation
    r_data_set <- length(data_set$uid)
    partitions_ = indices.partition( r_data_set, conf$nxval)

    for (tpart in partitions_) { # cross-validation loop

        print(paste(format(Sys.time(), "%H:%M:%S"),
                    "data_set:num_rows ", r_data_set,
                    "data_set:num_cols", length(data_set), 
                    "data_set:col_x_data_set ", toString(col_x_data_set)
                    )
        )

        # training set
        y_tr <- data_set[-tpart, col_y_data_set]
        x_tr <- data_set[-tpart, col_x_data_set]
        # test set
        y_test <- data_set[tpart, col_y_data_set]
        x_test <- data_set[tpart, col_x_data_set]

        # model
        m <- make_model(x_tr, y_tr)

        # stats on model-fit on training data
        pred_df <- pred_model(m, x=x_tr, y=y_tr, xtest=x_test, ytest=y_test, uid_x=data_set$uid[-tpart], uid_xtest=data_set$uid[tpart])

        # 
        m_list = c(m_list, pred_df)

        # report on pred_df
        tr_str <- paste( format(Sys.time(), "%H:%M:%S"), "iter:", length(m_list))
        tr_str <- paste(tr_str, 
                        paste("pred:thresh_arr ", flta_prettify(pred_df$pred.thresh_arr, digits=3)),
                        sep="\n"
                        )
        for (pfx in c("","test.")) {
            for (upfx in c("pred.","pred.uid.")) {
                err = get(paste(pfx,upfx,"err",sep=""),  envir=pred_df)
                auc = get(paste(pfx,upfx,"auc",sep=""),  envir=pred_df)
                tpr = get(paste(pfx,upfx,"tpr",sep=""),  envir=pred_df)
                fpr = get(paste(pfx,upfx,"fpr",sep=""),  envir=pred_df)

                tr_str <- paste(tr_str,
                                paste( paste(pfx,upfx,"err",sep=""), flta_prettify(err)), 
                                paste( paste(pfx,upfx,"tpr",sep=""), flta_prettify(tpr)), 
                                paste( paste(pfx,upfx,"fpr",sep=""), flta_prettify(fpr)), 
                                paste( paste(pfx,upfx,"tpr_by_fpr",sep=""), flta_prettify(tpr/fpr)), 
                                paste( paste(pfx,upfx,"auc",sep=""), flta_prettify(auc)), 
                                sep="\n"
                                )
            }
        }
        cat( tr_str, sep="\n")
        cat( tr_str, sep="\n", file=out_file, append=TRUE)

    } # cross-validation loop

}



# source get_res.r here
# source("get_res.r")
