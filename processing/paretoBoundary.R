paretoBoundary <- function(r,nclass=100){
  t <- 1/nclass
  Ce <- rep(NA,100)
  Oe <- rep(NA,100)
  tloop <- t
  
  Ah <- base::sum(r[r>=0])
  pcinv <- r
  pcinv[]<-matrix(1,nrow=NROW(r),ncol=NCOL(r))
  pcinv <- pcinv - r
  plot(pcinv)
  
  pb <- txtProgressBar(min = 0, max = nclass, style = 3)
  
  for (i in 1:nclass){
    #find mask corresponding to threshold tloop 
    vg   <- r>=tloop
    novg <- r[r<tloop]
    
    Al_t <- raster::cellStats(vg,stat='sum')
    
    Ol_t <- base::sum(novg)
    
    Cl_t <- base::sum(pcinv[r>=tloop]) 
    
    ce <- Cl_t/Al_t
    oe <- Ol_t/Ah
    
    Oe[i] <- oe
    Ce[i] <- ce
    tloop <- tloop+t
    setTxtProgressBar(pb, i)
  }
  close(pb)
  outDf <- data.frame(Oe=Oe,Ce=Ce)
  outPol <- data.frame(Oe=Oe,Ce=Ce)
  outPol <- rbind(data.frame(Oe=c(0,0),Ce=c(0,max(Ce,na.rm=T))),outPol)
  outPol <- rbind(outPol,data.frame(Oe=c(max(Oe),0),Ce=c(0,0)))
  return(list(outPol=outPol, outDf=outDf))
}