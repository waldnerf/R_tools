Bland.Altman.diagram <-
  function(x,y,id=FALSE, alpha = .05,rep.meas = FALSE,subject,idname = FALSE, colorid=FALSE, trans=0.9 ,xname = 'x',yname = 'y',...) {
    
    library(ggplot2)
    library(RColorBrewer)
    
    #*** 1. Set a few constants
    z <- qnorm(1 - alpha / 2)  ## value of z corresponding to alpha
    d <- x - y               ## pair-wise differences
    m <- (x + y) / 2           ## pair-wise means
    
    #*** 2. Calculate mean difference
    d.mn <- mean(d,na.rm = TRUE)
    
    #*** 3. Calculate difference standard deviation
    if (rep.meas == FALSE) {
      d.sd = sqrt(var(d,na.rm = TRUE))
    } else {
      #*** 3a. Ensure subject is a factor variable
      if (!is.factor(subject))
        subject <- as.factor(subject)
      
      #*** 3b. Extract model information
      n <- length(levels(subject))      # Number of subjects
      model <- aov(d ~ subject)           # One way analysis of variance
      MSB <- anova(model)[[3]][1]       # Degrees of Freedom
      MSW <- anova(model)[[3]][2]       # Sums of Squares
      
      #*** 3c. Calculate number of complete pairs for each subject
      pairs <- NULL
      for (i in 1:length(levels(as.factor(subject)))) {
        pairs[i] <- sum(is.na(d[subject == levels(subject)[i]]) == FALSE)
      }
      Sig.dl <-
        (MSB - MSW) / ((sum(pairs) ^ 2 - sum(pairs ^ 2)) / ((n - 1) * sum(pairs)))
      d.sd <- sqrt(Sig.dl + MSW)
    }
    
    #*** 4. Calculate lower and upper confidence limits
    ucl <- d.mn + z * d.sd
    lcl <- d.mn - z * d.sd
    print(d.mn)
    print(ucl)
    print(lcl)
    
    #*** 5. Make Plot
    xlabstr <- paste(c('Mean(',xname,', ',yname,')'), sep = "",collapse = "")
    ylabstr <- paste(c(xname, ' - ', yname), sep = "",collapse = "")
    id <- ifelse(id==FALSE, as.factor(rep(1,length(m))), as.character(id))
    xy <- data.frame(m,d,id)
    xy$id <- as.factor(xy$id)
    maxm <- max(m)
    
    
    #scatterplot of x and y variables
    scatter <- ggplot(xy,aes(m, d)) 
    
    
    if(idname == FALSE){
      scatter <- scatter + geom_point(color='darkgrey', alpha=trans) + scale_color_manual(values = c("darkgrey"), guide=FALSE)
    } else {
      if( colorid[1] == FALSE) {
        scatter <- scatter + geom_point(aes(color = id), alpha=trans)+ scale_color_brewer(idname, palette="Set1") # Set1
      } else {
        scatter <- scatter + geom_point(aes(color = id), alpha=trans)+ scale_color_manual(idname, values=colorid, guide = guide_legend(direction = "horizontal", title.position = "bottom",title.hjust=0.5)) # Set1
      }
      
    }
    
    scatter <- scatter + geom_abline(intercept = d.mn,slope = 0, colour = "black",size = 1,linetype = "dashed" ) +
      geom_abline(
        intercept = ucl,slope = 0,colour = "black",size = 1,linetype = "dotted"
      ) +
      geom_abline(
        intercept = lcl,slope = 0,colour = "black",size = 1,linetype = "dotted"
      ) +
      ylim(min(lcl, -max(xy$d), min(xy$d)), max(ucl, max(xy$d), -min(xy$d)))+
      xlab(xlabstr) +
      ylab(ylabstr) +
      #geom_text(x=maxm-2.4,y=d.mn,label='Mean',colour='black',size=4)+
      #geom_text(x=maxm-2.4,y=ucl,label='+1.96 SD',colour='black',size=4)+
      #geom_text(x=maxm-2.4,y=lcl,label='-1.96 SD',colour='black',size=4)+
      theme(
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.justification = c(0,0),
        legend.position = c(0,0),
        legend.direction = "horizontal",
        legend.background = element_rect(fill="transparent"),
        axis.line = element_line(color="black", size = 1)
      )
    
    plot(scatter)
    
    values <- round(cbind(lcl,d.mn,ucl),4)
    colnames(values) <- c("LCL","Mean","UCL")
    if (rep.meas == FALSE){
      Output <- list(limits = values,Var = d.sd ^ 2)
    } else {
      Output <- list(limits = values,Var = Sig.dl,MSB = MSB,MSW = MSW)
    }
    return(Output)
  }
