#Calculates exploratory analysis of NOR output
#remember to NOT convert numbers with commas, leave it as it is
NOR.StatFunction <- function(namefile, variable, ordening_vector){
  
  #read the head of the file where the categories are stored
  NOR.head <<- read.csv(file=namefile, nrows = 4, sep=";", dec=",", na.strings = "-", header = F)
  
  #compile the name of the file column
  #NOR.headPaste<<- data.frame(matrix(vector(), 0, ncol(NOR.head)))
  NOR.headPaste<<- vector(mode='character', length = (ncol(NOR.head)))
  a <- vector()
  for (j in 1:ncol(NOR.head)){
    a <- vector()
    for (i in 1:4){a <- paste(a, NOR.head[i,j])}
    NOR.headPaste[j]<<-a
  }
  
  #read the numbers of the CSV file
  NOR.Raw <<- read.csv(file=namefile, skip = 4, sep=";", dec=",", na.strings = "-", header = F)
  
  #put the names and the files together
  
  colnames(NOR.Raw) <- NOR.headPaste
  
  #find the index of "Distance moved Center-point Total cm"
  iDM <<- grep("Distance moved Center-point Total cm", colnames(NOR.Raw))
  
  
  #copy the first 7 columns where the categories and the total distance are stored 
  NOR.char <<- NOR.Raw[,c(1:iDM)]
  
  
  
  
  #find indexes for familiar (iFAM) and novel (iNOV)
  iFAM <-vector()
  iFAM <- agrep("familiar", colnames(NOR.Raw))
  iNOV <- vector()
  iNOV <- agrep("novel", colnames(NOR.Raw))
  
  #make new df with the sorted values
  NOR.FAM <<- NOR.Raw[,iFAM] 
  NOR.NOV <<- NOR.Raw[,iNOV]
  
  
  #colnames for NOR.Ratio
  NOR.RatioNames <- vector(length=length(iNOV))
  for (i in 1:length(NOR.FAM)){
  NOR.RatioNames[i]  <- paste(colnames(NOR.FAM[i]),colnames(NOR.NOV[i]), sep="/")}
  
  #make a new dataframe with all the ratios for the NOR
  NOR.ratio <<- data.frame(matrix(vector(), 0, ncol = length(NOR.RatioNames)), stringsAsFactors=F)
  colnames(NOR.ratio) <- NOR.RatioNames
  
  for (i in 1:nrow(NOR.FAM)){
    for (j in 1:ncol(NOR.FAM)){
      
      novel <- NOR.NOV[i,j]
      familiar <-NOR.FAM[i,j] 
      ratio <- novel/(familiar+novel)
      NOR.ratio[i,j]<-ratio 
    }
  }   
  
  #combine everything into our new variable NOR.Clean
  NOR.Clean <<- cbind(NOR.char, NOR.ratio)
  
  
  #make new directory with the results (and check if is there)
  mainDir <- getwd()
  subDir <- paste("NOR_Results", variable, sep="_")
  
  if (file.exists(subDir)){
    setwd(file.path(mainDir, subDir))
  } else {
    dir.create(file.path(mainDir, subDir))
    setwd(file.path(mainDir, subDir))
    
  }
  
  #find the index of the variable we want to use (called iVar)
  iVar <<- grep(variable, colnames(NOR.Raw))
  
  #make an ordered dataframe and check the ordening_vector
  if (exists("ordening_vector")) {
    try
    NOR.Clean[,iVar] <- factor(NOR.Clean[,iVar], levels=ordening_vector)
  }
  else {
    continue
  }
  
  NOR.Clean[,iVar] <- factor(NOR.Clean[,iVar], levels=ordening_vector)
  NOR.Clean <<-NOR.Clean[order(NOR.Clean[,iVar]),]
  
  #calculate ANOVA + Tukey  an put it in a doc file 
  sink("AnovaTest.doc")
  for(i in iDM:(ncol(NOR.Clean))){
    columns <- names(NOR.Clean[i])
    meanANDsd <-aggregate(NOR.Clean[,i], list(NOR.Clean[,iVar]), function (x) c(mean = mean(x), sd = sd(x)))
    
    anovaresult<- summary(aov(NOR.Clean[,i]~NOR.Clean[,iVar]))
    posthocresult <- TukeyHSD(aov(NOR.Clean[,i]~NOR.Clean[,iVar]))
    
    print(paste("your selected variable is ", colnames(NOR.Clean[iVar])))
    print(columns)
    print(meanANDsd)
    print(anovaresult)
    print(posthocresult)
  }
  sink()
  
  
  
  
  library(ggplot2)
  
  NOR.NONA <<- NOR.Clean[!is.na(NOR.Clean[i]), ]
  
  #boxplot graphs. Saves graphs as .pdf and .png
   for (i in (iDM):ncol(NOR.NONA)) {
    
    #blocks the appeareance of warnings
    con=file("temp.txt", "w")
    sink(con, type="message")
    
    options(warn=-1)
    
    #general form of graph
    p <- ggplot(NOR.NONA, aes(x=NOR.NONA[,iVar], y=NOR.NONA[, i], fill=NOR.NONA[,iVar])) +
      geom_boxplot() + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.8) + 
      scale_fill_discrete(name = as.character(colnames(NOR.Clean[iVar]))) +
      xlab("group") +
      stat_boxplot(geom ='errorbar') + 
      theme(axis.text.x=element_text(angle=45, hjust=1)) +
      theme(text = element_text(size=20))
    
    if(i == iDM){
      p = p + ggtitle(as.character(colnames(NOR.Clean[i]))) +  ylab("cm")
      suppressMessages(print(p))
      ggsave(filename=sprintf("var_%s.pdf", i), width = 12, height = 12)
      ggsave(filename=sprintf("var_%s.png", i), width = 12, height = 12)
      dev.off()
    }
    else{
      title2lanes = strsplit(colnames(NOR.Clean[i]), "/")
      p= p + ylim(0, 1) + ggtitle(paste(title2lanes[[1]][1], title2lanes[[1]][2], sep = "\n")) + ylab("Ratio")
      suppressMessages(print(p))
      ggsave(filename=sprintf("var_%s.pdf", i), plot=p, width = 12, height = 12)
      ggsave(filename=sprintf("var_%s.png", i), plot=p, width = 12, height = 12)
      dev.off()
    }
    
    
    
    graphics.off()
    options(warn=0)
    sink(type="message")
    close(con)
    
  }
  
  unlink("temp.txt", recursive=TRUE)
  #file.remove("temp.txt")
  
  #save data as csv file that can be opened in excel
  write.csv2(NOR.Clean, file = "NOR_ratioData.csv", row.names = FALSE)
  message(paste("Message: statistics from", namefile ,"has been calculated."))
  setwd(file.path(mainDir))
}






#####################################################################################################################################################
#RECALCULATE FUNCTION
#calculates the statistic once a csv file with all the ratio is available

NOR.Recalculate <- function(namefile, variable, ordening_vector){
  
  #read the head of the file where the categories are stored
  NOR.Clean <<- read.csv(file=namefile, sep=";", dec=",", na.strings = "NA", header = T, check.names=FALSE)
  
 
  #find the index of "Distance moved Center-point Total cm"
  iDM <<- grep("moved", colnames(NOR.Clean))
  
  
  
  #make new directory with the results (and check if is there)
  mainDir <- getwd()
  subDir <- paste("NOR_ResultsReloaded", variable, sep="_")
  
  if (file.exists(subDir)){
    setwd(file.path(mainDir, subDir))
  } else {
    dir.create(file.path(mainDir, subDir))
    setwd(file.path(mainDir, subDir))
    
  }
  
  #find the index of the variable we want to use (called iVar)
  iVar <<- grep(variable, colnames(NOR.Clean))
  
  #make an ordered dataframe and check the ordening_vector
  if (exists("ordening_vector")) {
    try
    NOR.Clean[,iVar] <- factor(NOR.Clean[,iVar], levels=ordening_vector)
  }
  else {
    continue
  }
  
  NOR.Clean <<-NOR.Clean[order(NOR.Clean[,iVar]),]
  
  #calculate ANOVA + Tukey  an put it in a doc file 
  sink("AnovaTest.doc")
  for(i in iDM:(ncol(NOR.Clean))){
    columns <- names(NOR.Clean[i])
    meanANDsd <-aggregate(NOR.Clean[,i], list(NOR.Clean[,iVar]), function (x) c(mean = mean(x), sd = sd(x)))
    
    anovaresult<- summary(aov(NOR.Clean[,i]~NOR.Clean[,iVar]))
    posthocresult <- TukeyHSD(aov(NOR.Clean[,i]~NOR.Clean[,iVar]))
    
    print(paste("your selected variable is ", colnames(NOR.Clean[iVar])))
    print(columns)
    print(meanANDsd)
    print(anovaresult)
    print(posthocresult)
  }
  sink()
  
  
  library(ggplot2)
  
  NOR.NONA <<- NOR.Clean[!is.na(NOR.Clean[i]), ]
  #boxplot graphs. Saves graphs as .pdf and .png
  for (i in (iDM):ncol(NOR.NONA)) {
    
    #blocks the appeareance of warnings
    con=file("temp.txt", "w")
    sink(con, type="message")
    
    options(warn=-1)
    
    #general form of graph
    p <- ggplot(NOR.NONA, aes(x=NOR.NONA[,iVar], y=NOR.NONA[, i], fill=NOR.NONA[,iVar])) +
      geom_boxplot() + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.8) + 
      scale_fill_discrete(name = as.character(colnames(NOR.Clean[iVar]))) +
      xlab("group") +
      stat_boxplot(geom ='errorbar') + 
      theme(axis.text.x=element_text(angle=45, hjust=1)) +
      theme(text = element_text(size=20))
    
    if(i == iDM){
      p = p + ggtitle(as.character(colnames(NOR.Clean[i]))) +  ylab("cm")
      suppressMessages(print(p))
      ggsave(filename=sprintf("var_%s.pdf", i), width = 12, height = 12)
      ggsave(filename=sprintf("var_%s.png", i), width = 12, height = 12)
      dev.off()
    }
    else{
      title2lanes = strsplit(colnames(NOR.Clean[i]), "/")
      p= p + ylim(0, 1) + ggtitle(paste(title2lanes[[1]][1], title2lanes[[1]][2], sep = "\n")) + ylab("Ratio")
      suppressMessages(print(p))
      ggsave(filename=sprintf("var_%s.pdf", i), plot=p, width = 12, height = 12)
      ggsave(filename=sprintf("var_%s.png", i), plot=p, width = 12, height = 12)
      dev.off()
    }
    
    
    
    graphics.off()
    options(warn=0)
    sink(type="message")
    close(con)
    
  }
  
  
  unlink("temp.txt", recursive=TRUE)
  #file.remove("temp.txt")
  
  #save data as csv file that can be opened in excel
  write.csv2(NOR.Clean, file = "NOR_ratioDataReload.csv", row.names = FALSE)
  message(paste("Message: statistics from", namefile ,"has been recalculated."))
  setwd(file.path(mainDir))
}
