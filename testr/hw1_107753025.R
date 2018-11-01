###
# homework1   #
###

args <- commandArgs(trailingOnly = TRUE)
if (length(args)!=4){
  stop("USAGE: Rcript hw1_107753025.R --input input --output output", call. = FALSE)
}else if(length(args)==4){
  if(args[1] == "--output"){
    args[5]<-args[2]
    args[2]<-args[4]
    args[4]<-args[5]
    }
}
data <- read.csv(args[2],header = TRUE)
b<- strsplit(args[2],"[/.]")
b<- unlist(b)
b<- c(b[length(b)-1])
set <- c(b)
weight <- c(round(max(data[,2]),digits = 2))
height <- c(round(max(data[,3]),digits = 2))
a <- data.frame(set,weight,height)
write.table(a,args[4], row.names = FALSE, quote = FALSE,sep = ",")
