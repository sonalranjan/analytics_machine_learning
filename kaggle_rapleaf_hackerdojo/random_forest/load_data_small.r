
vars_exportable <- c("demog_profile", "hdl", "lblusr", "unlblusr", "demog_lbl", "demog_lbl_x_hdl")

load_data <- function(testdata=FALSE, envir=NULL, export_vars=vars_exportable) {
    if ( !testdata ) {
        demog_profile <- read.csv("ce_demog.csv")
        hdl <- read.csv("uid_totrep.csv")
    } else {
        demog_profile <- read.csv("small_ce_demog.csv")
        hdl <- read.csv("small_preproc_hdl.csv")
    }
    # labels - or not
    lblusr <- read.csv("training.csv")
    unlblusr <- read.csv("example_entry.csv")

    #------------------------------------------------------
    # massage and prepare the data
    # create joins
    #------------------------------------------------------
    demog_lbl <- merge(demog_profile, lblusr, by="uid")
    demog_lbl_x_hdl <- merge(demog_lbl, hdl, by="uid")

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
