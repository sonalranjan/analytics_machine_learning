
vars_exportable <- c("demog_profile", "hdl", "lblusr", "unlblusr", "uid_totrep",
                     "demog_lbl", "demog_lbl_rf", "hdl_rf",
                     "demog_lbl_x_hdl", "demog_lbl_rf_x_hdl_rf") 

load_data <- function(testdata=FALSE, envir=NULL, export_vars=vars_exportable) {
    if ( !testdata ) {
        demog_profile <- read.csv("2ce_demog.csv")
        #demog_profile <- read.csv("ce_demog.csv")
        #demog_profile <- read.csv("rf1_demog.csv")

        #hdl <- read.csv("b_pp_hdl.csv")
        #hdl <- read.csv("preproc_hdl.csv")
        #hdl <- read.csv("preproc_hdl_massaged1.csv")
        hdl <- read.csv("preproc_hdl_massaged2.csv")
    } else {
        #demog_profile <- read.csv("small_ce_demog.csv")
        demog_profile <- read.csv("small_rf1_demog.csv")

        #hdl <- read.csv("small_preproc_hdl.csv")
        #hdl <- read.csv("small_preproc_hdl_massaged1.csv")
        hdl <- read.csv("small_preproc_hdl_massaged2.csv")
    }

    if (0) {
    # make ordered
    for (nidx in which(colnames(demog_profile) %in% c("age", "home_market_value", "household_income", "length_of_residence"))) {
        if (is.factor(demog_profile[[nidx]])) {
            demog_profile[[nidx]] <- reorder(as.ordered(demog_profile[[nidx]]))
            print(levels(demog_profile[[nidx]]))
        }
    }
    }
    # labels - or not
    lblusr <- read.csv("training.csv")
    unlblusr <- read.csv("example_entry.csv")
    uid_totrep <- read.csv("uid_totrep.csv")

    #------------------------------------------------------
    # massage and prepare the data
    # create joins
    #------------------------------------------------------
    demog_lbl <- merge(demog_profile, lblusr, by="uid")
    demog_lbl_x_hdl <- merge(demog_lbl, hdl, by="uid")

    demog_lbl_rf <- demog_lbl
    hdl_rf <- hdl
    #demog_excl_cols <- c("addr_city", "state", "country", "age", "home_market_value", "household_income", "length_of_residence")
    demog_excl_cols <- c("addr_city", "state", "country", "home_owner_status", "home_property_type", "marital_status")
    demog_excl_idx <- which(colnames(demog_lbl) %in% demog_excl_cols)
    if (length(demog_excl_idx) > 0 ) { demog_lbl_rf <- demog_lbl[,-demog_excl_idx]; }
    hdl_excl_cols <- c("url", "url_n")
    hdl_gini_notimp <- c( "url_investing_valueinvesting", 
                         "url_personal.finance_", 
                         "url_personal.finance_download", 
                         "url_personal.finance_insurance", 
                         "url_personal.finance_ira", 
                         "url_personal.finance_shopping", 
                         "url_personal.finance_travel", 
                         "url_retirement_manageretirement", 
                         "url_retirement_retireeport",
                         "url_retirement_retirementplanning" )
    hdl_err_inc <- c(
                     "url_investing_altenergystocks",
                     "url_investing_fiercemarkets",
                     "url_investing_guides",
                     "url_personal.finance_general",
                     "url_personal.finance_home",
                     "url_personal.finance_insurance",
                     "url_retirement_annuities",
                     "url_retirement_retireeport" )

    hdl_excl_cols <- c(hdl_excl_cols, union( hdl_gini_notimp, hdl_err_inc))
    hdl_excl_idx <- which(colnames(hdl) %in% hdl_excl_cols)
    if ( length(hdl_excl_idx) > 0 ) { hdl_rf <- hdl[,-hdl_excl_idx]; }
    demog_lbl_rf_x_hdl_rf <- merge(demog_lbl_rf, hdl_rf,, by="uid")

    if ( (!is.null(envir)) & (!is.null(export_vars)))  {
        # export variables to this environment : we should get a subset of the
        var_diff_names <- setdiff(export_vars, vars_exportable)
        stopifnot( 0 == length(var_diff_names))
        e <- environment()
        for (vn in export_vars) {
            assign(vn, get(vn, envir=e), env=envir);
        }
    }

}


# configuration file should be provided 
stopifnot( exists("cfg") )

#------------------------------------------------------
# read all  the input files. load data and place variables in global env
#------------------------------------------------------
# choose production or test runs
prod_run <- TRUE
if ("prod_run" %in% colnames(conf)) {
    prod_run <- conf$prod_run
}

load_data(!prod_run, envir=.GlobalEnv, export_vars = vars_exportable)

