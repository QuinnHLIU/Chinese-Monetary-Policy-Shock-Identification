############################################################
#####              PCA with missing values             #####
#####           @Haiqin Quinn Liu, Jan-1, 2023         #####
############################################################
#if (!require("devtools")) install.packages("devtools")
#library(devtools)
#install_github("husson/missMDA")
#install.packages("lubridate")
#install.packages("correlation")
#install.packages("TSstudio")

rm(list = ls()) 
setwd('E:/Dropbox/UMP_Paper/Git')
library(missMDA)  
library(xlsx)
library('writexl')
library(lubridate)
library(FactoMineR)
library(correlation)
library(TSstudio)

# five underlying daily contract price changes containing missing values
days <- c('pboc','unconv','MLF','conv','LPR','RRR','LR','control')  # 'EXR','M2','MPR'
ints <- c('broadr0075y','broadr0071y','broadirs5y','broadirs1y','broadirsr0075y','broadirsr0071y',
          'narrowr0075y','narrowr0071y','narrowirs5y','narrowirs1y','narrowirsr0075y','narrowirsr0071y') 
for (di in 1:length(days))
{
  for (ii in 1:length(ints))
  {
    day <- days[[di]]
    int <- ints[[ii]]
    tryCatch({
      
      surprise <- read.xlsx(paste0("OutPut/MDA/",day,int,".xlsx"),sheetIndex=1, 
                            sheetName=NULL,startRow=1, endRow=NULL, as.data.frame=TRUE, header=TRUE)
      # plot(surprise)
      # handle missing values by iterative (PCA + extrapolate) steps
      nb <- estim_ncpPCA(surprise,ncp.min=1,ncp.max=5, #need to optimize so time-consuming
                         method.cv="Kfold",nbsim=50)
      print(nb$ncp)  # most of the time, 1...or 4
      imputed <- imputePCA(surprise,ncp=nb$ncp)  #default=2
      res.pca <- PCA(imputed)  #$completeObs...if there are no missings
      write.xlsx(res.pca$ind$coord,row.names=FALSE, # coordinates=rotated orthogonal dimensions
                 paste0("OutPut/MDA/HighFreq/",day,int,"PCAmiss.xlsx"))
    }, error=function(e){
      print(paste0(day,int))
      message("Error: ", e$message)
    })
  }
}



# underlying daily contract price changes containing missing values
days <- c('pboc','nonpboc','conv','unconv','RRR','LR','MLF','LPR','control')  # ,'EXR','M2','MPR'
ints <- c('irs','irsr0071y','irsr0075y','r007')  #,'fwd','fut2y','fut5y'
for (di in 1:length(days))
{
  for (ii in 1:length(ints))
  {
    day <- days[[di]]
    int <- ints[[ii]]
    tryCatch({
    surprise <- read.xlsx(paste0("OutPut/MDA/",day,int,".xlsx"),sheetIndex=1, 
                          sheetName=NULL,startRow=1, endRow=NULL, as.data.frame=TRUE, header=TRUE)
    # plot(surprise)
    # handle missing values by iterative (PCA + extrapolate) steps
    nb <- estim_ncpPCA(surprise,ncp.min=1,ncp.max=5, #need to optimize so time-consuming
                       method.cv="Kfold",nbsim=50)
    print(nb$ncp)  # most of the time, 1...or 4
    imputed <- imputePCA(surprise,ncp=nb$ncp)  #default=2
    res.pca <- PCA(imputed)  #$completeObs...if there are no missings
    write.xlsx(res.pca$ind$coord,row.names=FALSE, # coordinates=rotated orthogonal dimensions
               paste0("OutPut/MDA/Daily/",day,int,"PCAmiss.xlsx"))
    }, error=function(e){
      print(paste0(day,int))
      message("Error: ", e$message)
    })
  }
}


