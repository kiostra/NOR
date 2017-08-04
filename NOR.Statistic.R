#C:/Users/carboni_eleonora/Documents/Statistica_R/NOR_groups
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
  
  #find the index of the variable we want to use (called ii)
  ii <<- grep(variable, colnames(NOR.Raw))
  
  #make an ordered dataframe and check the ordening_vector
  if(missing(ordening_vector)) {
    ordening_vector <- vector()
  } else {
    ordening_vector <- ordening_vector
  }
  NOR.Clean[,ii] <- factor(NOR.Clean[,ii], levels=ordening_vector)
  NOR.Clean <<-NOR.Clean[order(NOR.Clean[,ii]),]
  
  #calculate ANOVA + Tukey  an put it in a doc file 
  sink("AnovaTest.doc")
  for(i in iDM:(ncol(NOR.Clean))){
    columns <- names(NOR.Clean[i])
    anovaresult<- summary(aov(NOR.Clean[,i]~NOR.Clean[,ii]))
    posthocresult <- TukeyHSD(aov(NOR.Clean[,i]~NOR.Clean[,ii]))
    
    print(paste("your selected variable is ", colnames(NOR.Clean[ii])))
    print(columns)
    print(anovaresult)
    print(posthocresult)
  }
  sink()
  
  
  library(ggplot2)
  
  NOR.NONA <<- NOR.Clean[!is.na(NOR.Clean[i]), ]
  
  
  for (i in (iDM+1):ncol(NOR.NONA)) {
    pdf(file = paste("var_", i, ".pdf", sep=""), width = 6, height = 6)
    
    # boxplot(NOR.Clean[, i] ~ NOR.Clean[ii]) + geom_dotplot(binaxis='y', stackdir='center')
    p <- ggplot(NOR.NONA, aes(x=NOR.NONA[,ii], y=NOR.NONA[, i])) + 
      geom_boxplot() + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.6) + 
      ggtitle(as.character(colnames(NOR.Clean[i]))) +
      xlab("group") +
      ylab("Ratio") +
      ylim(0, 1)
    
    print(p)
    #pdf(file = paste("var_", i, ".pdf", sep=""))
    
    dev.off()
    
  }
  on.exit(dev.off())
  
  graphics.off()
  
  
  #save files as png
  for (i in (iDM+1):ncol(NOR.NONA)) {
    png(file = paste("var_", i, ".png", sep=""), width = 500, height = 500)
    
    # boxplot(NOR.Clean[, i] ~ NOR.Clean[ii]) + geom_dotplot(binaxis='y', stackdir='center')
    p <- ggplot(NOR.NONA, aes(x=NOR.NONA[,ii], y=NOR.NONA[, i])) + 
      geom_boxplot() + geom_dotplot(binaxis='y', stackdir='center', dotsize=1) + 
      ggtitle(as.character(colnames(NOR.Clean[i]))) +
      xlab("group") +
      ylab("Ratio") +
      ylim(0, 1)
    
    print(p)
    #pdf(file = paste("var_", i, ".pdf", sep=""))
    
    dev.off()
    
  }
  on.exit(dev.off())
  
  graphics.off()
  
  setwd(file.path(mainDir))
}

#}

#"C:/Users/carboni_eleonora/Documents/Statistica_R"
#NOR_WOCalc.csv
#stringsAsFactors=FALSE