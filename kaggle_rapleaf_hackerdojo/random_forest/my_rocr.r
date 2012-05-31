thr_vals <- c(0.01, 0.05, 0.1, 0.15, 0.35, 0.5, 0.65, 0.85, 0.9, 0.95, 0.99)

roc_r <- function( y_preds, y, thresh_arr_len=0, comment="") {

    #print( paste(comment, nrow(y_preds), ncol(y_preds), thresh_arr_len))

    pred.err = c()
    pred.tpr = c()
    pred.fpr = c()

    n_ <- length(which(y == 0))
    p_ <- length(which(y == 1))

    for (th_idx in 1:thresh_arr_len) {
        yp <- y_preds[,th_idx]
        wrong_pred <- which(yp != y)
        pred.err <- c( pred.err, length(wrong_pred)/length(y))
        #str(yp) #str(y)
        #print( paste(length(yp), length(wrong_pred), length(y), n_, p_))
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

