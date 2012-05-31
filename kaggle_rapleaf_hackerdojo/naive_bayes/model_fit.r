
thr_vals <- c(0.01, 0.05, 0.1, 0.15, 0.35, 0.5, 0.65, 0.85, 0.9, 0.95, 0.99)

roc_r <- function( y_preds, y, thresh_arr_len=1) {
    pred.err = c()
    pred.tpr = c()
    pred.fpr = c()
    n_ <- length(y == 0)
    p_ <- length(y == 1)
    for (th_idx in 1:thresh_arr_len) {
        yp <- y_preds[,th_idx]
        wrong_pred <- which(yp != y)
        pred.err <- c( pred.err, length(wrong_pred)/length(y))
        y_tp <- length(which((yp == 1) & (y == 1)))
        y_fp <- length(which((yp == 1) & (y == 0)))
        pred.tpr <- c( pred.tpr, y_tp/p_)
        pred.fpr <- c( pred.fpr, y_fp/n_)
    }

    fpr_order_idx <- order(pred.fpr)
    ordfpr <- pred.fpr[fpr_order_idx]
    ordtpr <- pred.tpr[fpr_order_idx]
    auc <- 0
    for (i in 1:(length(ordfpr)-1)) {
        h_d <- ordfpr[i+1]-ordfpr[i]
        v_d <- abs(ordtpr[i+1]-ordtpr[i])
        v_min <- min(ordtpr[i:i+1])
        auc <- auc + v_min*h_d + 0.5*v_d*h_d
    }
    pred.auc <- auc

    l = list(err=pred.err, tpr=pred.tpr, fpr=pred.fpr, auc=pred.auc)
    #print( list(" list_roc_r: ", l))
    return(l)
}


pred_model <- function(m, x, y=NULL, xtest=NULL, ytest=NULL, thresh_arr=thr_vals, uid_x=NULL, uid_xtest=NULL) {
    #
    if (is.null(thresh_arr)) { thresh_arr <- c(0.50); }

    # predict
    raw_pred <- predict(m, x, type="raw")

    y_preds <- c()
    for (th in thresh_arr) { y_preds <- cbind(y_preds, as.integer(raw_pred[,2] >= th)); }

    uid_y_preds = NULL
    if ( ! is.null(uid_x) ) {
        tmp_df <- data.frame(uid=uid_x, behaviour=raw_pred[,2])
        uid_raw_pred_df <- aggregate( tmp_df, by=list( tmp_df$uid), FUN="mean")
        uid_y_preds <- c()
        for (th in thresh_arr) { uid_y_preds <- cbind(uid_y_preds, as.integer(uid_raw_pred_df$behaviour >= th)); }
    }

    ret_df = mylist2env(list(model=m,
        pred.thresh_arr = thresh_arr,
        pred.raw=raw_pred, pred.bool=y_preds, pred.uid.bool=uid_y_preds,
        pred.err=NULL, pred.tpr=NULL, pred.fpr=NULL, pred.auc=NULL,
        pred.uid.err=NULL, pred.uid.tpr=NULL, pred.uid.fpr=NULL, pred.uid.auc=NULL,
        test.pred.raw=NULL, test.pred.bool=NULL, test.pred.uid.bool=NULL,
        test.pred.err=NULL, test.pred.tpr=NULL, test.pred.fpr=NULL, test.pred.auc=NULL,
        test.pred.uid.err=NULL, test.pred.uid.tpr=NULL, test.pred.uid.fpr=NULL, test.pred.uid.auc=NULL
    ))

    if (FALSE == is.null(y)) {
        #
        metrics_y <- roc_r(y_preds=y_preds, y=y, thresh_arr_len=length(thresh_arr))
        ret_df$pred.err=metrics_y$err
        ret_df$pred.auc=metrics_y$auc
        ret_df$pred.tpr=metrics_y$tpr
        ret_df$pred.fpr=metrics_y$fpr
        #
        if ( ! is.null(uid_x) ) {
            tmp_y_df <- data.frame(uid=uid_x, behaviour=y)
            uid_y_df <- aggregate( tmp_y_df, by=list( tmp_y_df$uid), FUN="mean")
            metrics_uid_y <- roc_r(y_preds=uid_y_preds, y=uid_y_df$behaviour, thresh_arr_len=length(thresh_arr))
            ret_df$pred.uid.err=metrics_uid_y$err
            ret_df$pred.uid.auc=metrics_uid_y$auc
            ret_df$pred.uid.tpr=metrics_uid_y$tpr
            ret_df$pred.uid.fpr=metrics_uid_y$fpr
        }
    }

    if (FALSE == is.null(xtest)) {
        df_test <- pred_model(m, x=xtest, y=ytest, thresh_arr=thresh_arr, uid_x=uid_xtest)
        ret_df$test.pred.raw  <- df_test$pred.raw
        ret_df$test.pred.bool <- df_test$pred.bool
        ret_df$test.pred.uid.bool <- df_test$pred.uid.bool
        ret_df$test.pred.err <- df_test$pred.err
        ret_df$test.pred.tpr <- df_test$pred.tpr
        ret_df$test.pred.fpr <- df_test$pred.fpr
        ret_df$test.pred.auc <- df_test$pred.auc
        ret_df$test.pred.uid.err <- df_test$pred.uid.err
        ret_df$test.pred.uid.tpr <- df_test$pred.uid.tpr
        ret_df$test.pred.uid.fpr <- df_test$pred.uid.fpr
        ret_df$test.pred.uid.auc <- df_test$pred.uid.auc
    }

    return(ret_df)
} 

make_model <- function(x, y) {
    m <- naiveBayes(x_tr,y_tr)
    return(m)
}

