mcNemar <- function(c1,c2,ref){
  #' Compute the McNemar test
  #' 
  #' This function is a warper function to compute the McNemar test between two vectors
  #' 
  #' @param c1 is a vector of a labels provided by a first classifier
  #' @param c2 is a vector of labels  provided by a second  classifier 
  #' @param ref is a vector of labels as provided by the validation data 
  #' @return the McNemar test
  #' @author Francois Waldner

  dummy1<-as.data.frame(c1,optional=TRUE)
  dummy2<-as.data.frame(c2,optional=TRUE)
  ref   <-as.data.frame(ref,optional=TRUE)
  correct.c1<-(dummy1[,] == ref[,1])
  correct.c2<-(dummy2[,] == ref[,1])
  
  #We will construct the contingency matrix by running the following commands:
  n00<-sum(!correct.linear & !correct.pol)
  n10<-sum(!correct.linear & correct.pol)
  n01<-sum(correct.linear & !correct.pol)
  n11<-sum(correct.linear & correct.pol)
  mc <- mcnemar.test(matrix(c(n00,n10,n01,n11),nrow=2))
  return(mc)
}