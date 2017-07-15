conf_mat <- function(v1,v2){
  v1F <- levels(as.factor(v1))
  v2F <- levels(as.factor(v2))
  fList<- base::union(v1F,v2F)
  cm <- table(factor(v1,levels=fList),factor(v2,levels=fList))
  return(cm)
}


calc_pop_ct <- function(ct, pop) {
  # Below uses the notation of Pontius and Millones (2011)
  nijsum <- matrix(rowSums(ct), nrow=nrow(ct), ncol=ncol(ct))
  Ni <- matrix(pop, nrow=nrow(ct), ncol=ncol(ct))
  # pop_ct is the population contigency table
  pop_ct <- (ct / nijsum) * (Ni / sum(pop))
  dimnames(pop_ct)[[1]] <- dimnames(ct)[[1]]
  dimnames(pop_ct)[[2]] <- dimnames(ct)[[2]]
  class(pop_ct) <- 'table'
  return(pop_ct)
}


calc_Q <- function(pop_ct) {
  # Calculate quantity disagreement (Pontius and Millones, 2011, eqns 2-3)
  qg_mat = abs(rowSums(pop_ct) - colSums(pop_ct))
  return(sum(qg_mat) / 2)
}

calc_A <- function(pop_ct) {
  # Calculate allocation disagreement (Pontius and Millones, 2011, eqns 4-5)
  diag_indices <- which(diag(nrow(pop_ct)) == TRUE)
  ag_mat = 2 * apply(cbind(rowSums(pop_ct) - pop_ct[diag_indices],
                           colSums(pop_ct) - pop_ct[diag_indices]), 1, min)
  return(sum(ag_mat) / 2)
}

# Adds margins to contingency table
add_ct_margins <- function(ct) {
  # For user's, producer's, and overall accuracy formulas, see Table 
  # 21.3 in Foody, G.M., Stehman, S.V., 2009. Accuracy Assessment, in: 
  # Warner, T.A., Nellis, M.D., Foody, G.M. (Eds.), The SAGE Handbook of 
  # Remote Sensing. SAGE.
  diag_indices <- which(diag(nrow(ct)) == TRUE)
  users_acc <- ct[diag_indices] / colSums(ct)
  prod_acc <- ct[diag_indices] / rowSums(ct)
  fscore <- users_acc*prod_acc*2/(users_acc+prod_acc)
  overall_acc <- sum(ct[diag_indices]) / sum(ct)
  ct <- addmargins(ct)
  dimnames(ct)[[1]][nrow(ct)] <- "Total"
  dimnames(ct)[[2]][nrow(ct)] <- "Total"
  ct <- rbind(ct, Producers=c(users_acc, NA))
  ct <- cbind(ct, Users=c(prod_acc, NA, overall_acc))
  ct <- round(ct, digits=4)
  dimnames(ct) <- list(predicted=dimnames(ct)[[1]],
                       observed=dimnames(ct)[[2]])
  class(ct) <- 'table'
  return(list(cmMargins = ct, OA =overall_acc, fscore=fscore ))
}
