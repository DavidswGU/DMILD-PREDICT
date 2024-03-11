library(scales)
pct <- function(x){
  x <- round(x,4)*100
  x <- paste0(x,"%")
  return(x)
}

is.binary <- function(x){
  table <- data.frame(table(x))
  if(nrow(table)==1) return("single") 
  if(nrow(table)==2) return("binary")
  if(nrow(table)>2) return("other")
}

binarycomp <- function(x1,x2){
  np=0
  pp=0
  pn=0
  nn=0
  Diff.rest <- as.data.frame(matrix(nrow = 1,ncol = 7))
  n = 3
  pp <- as.data.frame(table(x1))[2,2]
  pn <- as.data.frame(table(x1))[1,2]
  np <- as.data.frame(table(x2))[2,2]
  nn <- as.data.frame(table(x2))[1,2]
  if(is.na(np)==TRUE) np=0
  if(is.na(pp)==TRUE) pp=0
  if(is.na(pn)==TRUE) pn=0
  if(is.na(nn)==TRUE) nn=0
  mtx.test <- matrix(c(pp,pn,np,nn),nrow = 2)
  if(pp<=5|pn<=5|nn<=5|np<=5) {
    if((pp+pn+np+nn)>0) {
      n=0
    } 
  } else n=1
  if(n==1)
  {
    pval <- chisq.test(mtx.test,correct = FALSE)$p.value
    stat <- "Chi-Square"
  }
  if(n==0)
  {
    pval <- fisher.test(mtx.test)$p.value
    stat <- "Fisher"
  }
  Diff.rest[,1] <- paste0("'=",as.data.frame(table(x1))[2,1])
  Diff.rest[,2] <- paste0(pp+np,"(",pct((pp+np)/(pp+pn+np+nn)),")")
  Diff.rest[,3] <- paste0(pp,"(",pct(pp/(pp+pn)),")")
  Diff.rest[,4] <- paste0(np,"(",pct(np/(np+nn)),")")
  Diff.rest[,5] <- stat
  if(pval>=0.001) Diff.rest[,6] <- as.character(round(pval,3)) else Diff.rest[,6] <- "<0.001"
  Diff.rest[,7] <- ""
  if(pval<0.05) Diff.rest[,7] <- "*"
  if(pval<0.01) Diff.rest[,7] <- "**"
  if(pval<0.001) Diff.rest[,7] <- "***"
  return(Diff.rest)
}

contcomp <- function(x1,x2){
    Diff.rest <- as.data.frame(matrix(nrow = 1,ncol = 7))
    n = 0
    xnorm <- ks.test(x1,pnorm,exact = TRUE)$p.value
    ynorm <- ks.test(x2,pnorm,exact = TRUE)$p.value
    if(xnorm>0.05&ynorm>0.05) n=1
    if(n==1)
    {
      pval <- t.test(x1,x2,var.equal = TRUE,conf.level = 0.95)$p.value
      stat <- "student's t"
    }
    if(n==0)
    {
      pval <- ks.test(x1,x2)$p.value
      stat <- "K-S"
    }
    Diff.rest[,1] <- "Continuous"
    Diff.rest[,2] <- paste0(median(c(x1,x2),na.rm = TRUE),"("
                            ,quantile(c(x1,x2),probs = 0.25,na.rm = TRUE),","
                            ,quantile(c(x1,x2),probs = 0.75,na.rm = TRUE),")")
    Diff.rest[,3] <- paste0(round(median(x1,na.rm = TRUE),2),"("
                            ,quantile(x1,probs = 0.25,na.rm = TRUE),","
                            ,quantile(x1,probs = 0.75,na.rm = TRUE),")")
    Diff.rest[,4] <- paste0(median(x2,na.rm = TRUE),"("
                            ,quantile(x2,probs = 0.25,na.rm = TRUE),","
                            ,quantile(x2,probs = 0.75,na.rm = TRUE),")")
    Diff.rest[,5] <- stat
    if(pval>=0.001) Diff.rest[,6] <- as.character(round(pval,3)) else Diff.rest[,6] <- "<0.001"
    Diff.rest[,7] <- ""
    if(pval<0.05) Diff.rest[,7] <- "*"
    if(pval<0.01) Diff.rest[,7] <- "**"
    if(pval<0.001) Diff.rest[,7] <- "***"
    return(Diff.rest)
}

comp <- function(x,sep){
  sepnum <- which(colnames(x)==sep)
  table <- data.frame(table(x[,sepnum]))
  sep1 <- table[1,1]
  sep2 <- table[2,1]
  x1 <- subset(x,x[,sepnum]==sep1)[,-sepnum]
  x2 <- subset(x,x[,sepnum]==sep2)[,-sepnum]
  var.list <- c(colnames(x1))
  Diff.res <- as.data.frame(matrix(nrow = 1,ncol = 7))
  for (i in 1:ncol(x1)) {
    x1.test <- x1[,i]
    x2.test <- x2[,i]
    if (is.binary(x1[,i])=="binary"|is.binary(x2[,i])=="binary") Diff.rest <- binarycomp(x1.test,x2.test) 
    if (is.binary(x1[,i])=="other"|is.binary(x2[,i])=="other") Diff.rest <- contcomp(x1.test,x2.test)
    if (is.binary(x1[,i])=="single"&is.binary(x2[,i])=="single") Diff.rest <- as.data.frame(c("Single-value","100%","100%","100%","NA","1",""))
    Diff.res <- rbind(Diff.res,Diff.rest)
  }
  Diff.res <- Diff.res[-1,]
  Diff.res <- cbind(colnames(x1),Diff.res)
  xname <- paste0("Total"," (n=",nrow(x),")")
  x1name <- paste0(sep,"=",table[1,1]," (n=",nrow(x1),")")
  x2name <- paste0(sep,"=",table[2,1]," (n=",nrow(x2),")")
  colnames(Diff.res) <- c("Variable","Value",xname,x1name,x2name,"Method","P","significance")
  return(Diff.res)
}
comparison.d <- comp(train.data,"surv")
write.csv(comp(DM,"cohort"),file = "Output/Comparison/Comparison_dv.csv",row.names = FALSE)
