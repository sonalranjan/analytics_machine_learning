
#source("my_rocr.r")

thr_vals <- c(0.01, 0.05, 0.1, 0.15, 0.35, 0.5, 0.65, 0.85, 0.9, 0.95, 0.99)

pred_model <- function(m, x, y=NULL, xtest=NULL, ytest=NULL, thresh_arr=thr_vals, uid_x=NULL, uid_xtest=NULL) {
    # prepare data structure
    ret_df <- new.env(mylist2env(list(model=NULL, 
                                       train.pred=NULL, train.pred.predObj=NULL, train.pred.perfObj=NULL, train.pred.auc=NULL, 
                                       test.pred=NULL, test.pred.predObj=NULL, test.pred.perfObj=NULL, test.pred.auc=NULL, 
                                       iter=NULL
                                       )))

    # assign the model
    ret_df$model <- m
    # pred for train
    if (m$type == "classification") {
        ret_df$train.pred <- data.frame(predict(m, x, type="prob"))
    } else {
        ret_df$train.pred <- data.frame(predict(m, x))
    }

    if (!is.null(y)) {
        if (m$type == "classification") {
            ret_df$train.pred.predObj <- prediction(ret_df$train.pred$X1, y)
        } else {
            ret_df$train.pred.predObj <- prediction(ret_df$train.pred, y)
        }
        ret_df$train.pred.perfObj <- performance(ret_df$train.pred.predObj, "tpr", "fpr")
        ret_df$train.pred.auc <- performance(ret_df$train.pred.predObj, "auc")@y.values[[1]]
        err <- performance(ret_df$train.pred.predObj, "err")
        ret_df$train.pred.thr <- err@x.values[[1]]
        ret_df$train.pred.err <- err@y.values[[1]]
        ret_df$train.pred.tpr <- ret_df$train.pred.perfObj@y.values[[1]]
        ret_df$train.pred.fpr <- ret_df$train.pred.perfObj@x.values[[1]]
    }

    # pred for test
    if (!is.null(xtest)) {
        tmp_df <- pred_model(m=m, x=xtest, y=ytest, thresh_arr=thresh_arr)
        ret_df$test.pred.auc <- tmp_df$train.pred.auc
        ret_df$test.pred.thr <- tmp_df$train.pred.thr 
        ret_df$test.pred.err <- tmp_df$train.pred.err
        ret_df$test.pred.tpr <- tmp_df$train.pred.tpr
        ret_df$test.pred.fpr <- tmp_df$train.pred.fpr
        rm(tmp_df)
    }

    return(ret_df)
} 

make_model <- function(x, y, prior_probs=c(1), sampsize=1000, method="regr", mtry=3, ntree=100, cutoff=c(0.5,0.5)) {

    #argl <- list(sampsize=c( min( sampsize, ceiling(nrow(x)*.9)) * prior_probs ))
    argl <- list(sampsize=c( min( sampsize, 98186) * prior_probs ))
    argl <- append(argl, list(ntree=ntree, method=method, mtry=mtry))
    argl <- append(argl, list(cutoff=cutoff))

    tr_str <- "make_model: "
    for (n in names(argl)) { tr_str <- paste(tr_str, paste(n, toString(argl[[n]]))); }
    cat( tr_str, sep="\n")
    cat( tr_str, sep="\n", file=out_file, append=TRUE)
   
    y_model <- y
    if (method == "class") { y_model <- as.factor(y); }

    m <- randomForest(x, y_model,
                      #do.trace=TRUE,
                      #strata=,
                      #replace=FALSE,
                      sampsize=argl$sampsize,
                      ntree=argl$ntree,
                      mtry=argl$mtry,
                      importance=TRUE,
                      keep.forest=TRUE, 
                      norm.votes=TRUE,
                      cutoff=argl$cutoff
                      #na.action=na.omit
                      )
    return(m)
}

