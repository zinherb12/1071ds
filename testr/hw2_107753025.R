###
# homework2   #
###


args <- commandArgs(trailingOnly = TRUE)
if(args[1] == "--target"){
  if(args[2] == "male"){
    if(args[3] == "--input"){
      counter <- 4
      a <- NULL
      while(args[counter] != "--output"){
        data <- read.csv(args[counter],header = TRUE)
        premale <- sum((data[,2]) == "male")
        prefelm <- sum((data[,2]) == "female")
        refmale <- sum((data[,3]) == "male")
        reffelm <- sum((data[,3]) == "female")
        
        malefra <- data.frame(table(ifelse(ifelse(data$prediction== "male",1,NA) == ifelse(data$reference== "male",1,NA),1,NA)))
        malemale <- as.integer(malefra[1,2]) 
        
        felmfra <- data.frame(table(ifelse(ifelse(data$prediction== "female",1,NA) == ifelse(data$reference== "female",1,NA),1,NA)))
        felmfelm <- as.integer(felmfra[1,2]) 
        
        number <- nrow(data)
        i <- 0
        j <- 0
        k <- 0
        for (i in c(1:number)) {
          for (j in c(1:number)) {
            if(data[i,3]=="male"){
              if(data[i,4]>data[j,4]&&data[j,3]=="female"){
                k=k+1
              }
            }
          }
        }
        
        x<- strsplit(args[counter],"[/.]")
        x<- unlist(x)
        x<- c(x[length(x)-1])
        method <- c(x)
        
        sensitivity <- c(malemale/refmale)
        specificity <- c(felmfelm/reffelm)
        F1 <- c(2*malemale/(reffelm+refmale+malemale-felmfelm))
        AUC <- c(k/(refmale*reffelm))
          
        if(args[counter]==4){
          a <- data.frame(method,sensitivity,specificity,F1,AUC)
        }else if(args[counter]!=4){
          b <- data.frame(method,sensitivity,specificity,F1,AUC)
          a <- rbind(a,b)
          
          counter=counter+1
        }
        
      }
      
      if(args[counter] == "--output"){
        
        method <- a$method
        sensitivity <- c(round(a[,2],digits = 2))
        specificity <- c(round(a[,3],digits = 2))
        F1 <- c(round(a[,4],digits = 2))
        AUC <- c(round(a[,5],digits = 2))        
        d <- data.frame(method,sensitivity,specificity,F1,AUC)
        
        method <- ("highest")
        sensitivity <- (a[which.max(a[,2]),1])
        specificity <- (a[which.max(a[,3]),1])
        F1 <- (a[which.max(a[,4]),1])
        AUC <- (a[which.max(a[,5]),1])
        c <- data.frame(method,sensitivity,specificity,F1,AUC)
        
        e <- rbind(d,c)
        
        counter=counter+1
      }
      write.table(e,args[counter], row.names = FALSE, quote = FALSE,sep = ",")
    }else{
      stop("USAGE3.1: Rscript hw2_yourID.R --target male|female --inputs file1 file2 ... filen --output out.csv", call. = FALSE)
    }
    
  }else if(args[2] == "female"){
    if(args[3] == "--input"){
      counter <- 4
      a <- NULL
      while(args[counter] != "--output"){
        data <- read.csv(args[counter],header = TRUE)
        premale <- sum((data[,2]) == "male")
        prefelm <- sum((data[,2]) == "female")
        refmale <- sum((data[,3]) == "male")
        reffelm <- sum((data[,3]) == "female")
        
        malefra <- data.frame(table(ifelse(ifelse(data$prediction== "male",1,NA) == ifelse(data$reference== "male",1,NA),1,NA)))
        malemale <- as.integer(malefra[1,2]) 
        
        felmfra <- data.frame(table(ifelse(ifelse(data$prediction== "female",1,NA) == ifelse(data$reference== "female",1,NA),1,NA)))
        felmfelm <- as.integer(felmfra[1,2]) 
        
        number <- nrow(data)
        i <- 0
        j <- 0
        k <- 0
        for (i in c(1:number)) {
          for (j in c(1:number)) {
            if(data[i,3]=="male"){
              if(data[i,4]>data[j,4]&&data[j,3]=="female"){
                k=k+1
              }
            }
          }
        }
        
        x<- strsplit(args[counter],"[/.]")
        x<- unlist(x)
        x<- c(x[length(x)-1])
        method <- c(x)
        
        sensitivity <- c(felmfelm/reffelm)
        specificity <- c(malemale/refmale)
        F1 <- c(2*felmfelm/(reffelm+refmale-malemale+felmfelm))
        AUC <- c(k/(refmale*reffelm))
        
        if(args[counter]==4){
          a <- data.frame(method,sensitivity,specificity,F1,AUC)
        }else if(args[counter]!=4){
          b <- data.frame(method,sensitivity,specificity,F1,AUC)
          a <- rbind(a,b)
           
          counter=counter+1
        }
      
      }
      if(args[counter] == "--output"){
        
        method <- a$method
        sensitivity <- c(round(a[,2],digits = 2))
        specificity <- c(round(a[,3],digits = 2))
        F1 <- c(round(a[,4],digits = 2))
        AUC <- c(round(a[,5],digits = 2))        
        d <- data.frame(method,sensitivity,specificity,F1,AUC)
        
        method <- ("highest")
        sensitivity <- (a[which.max(a[,2]),1])
        specificity <- (a[which.max(a[,3]),1])
        F1 <- (a[which.max(a[,4]),1])
        AUC <- (a[which.max(a[,5]),1])
        c <- data.frame(method,sensitivity,specificity,F1,AUC)
        
        e <- rbind(d,c)
        
        counter=counter+1
      }
      write.table(e,args[counter], row.names = FALSE, quote = FALSE,sep = ",") 
      
    }else{
      stop("USAGE3.2: Rscript hw2_yourID.R --target male|female --inputs file1 file2 ... filen --output out.csv", call. = FALSE)
    }
  }else{
    stop("USAGE2: Rscript hw2_yourID.R --target male|female --inputs file1 file2 ... filen --output out.csv", call. = FALSE)
  }
}else{
  stop("USAGE1: Rscript hw2_yourID.R --target male|female --inputs file1 file2 ... filen --output out.csv", call. = FALSE)
}
