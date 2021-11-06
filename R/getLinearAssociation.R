charToFactor <- function(df) {
  df <- as.data.frame(unclass(df), stringsAsFactors = TRUE)
  return(df)
}



runCurrentRscript <- function() {
  #rstudioapi::insertText(" %in% ")
  selected_text = rstudioapi::getActiveDocumentContext()
  word <- selected_text$select[[1]]$text
  env <- globalenv()
  word<-str_split(word,',',n=3)
  dfName<-gsub("[[:space:]]", "",word[[1]][1])
  targetName<-gsub("[[:space:]]", "",word[[1]][2])
  numToPrint<-gsub("[[:space:]]", "",word[[1]][3])

  numToPrint<-strtoi(numToPrint)
  if(is.na(numToPrint) || numToPrint =="" || !is.numeric(numToPrint) ||
     !(numToPrint == round(numToPrint)) || numToPrint <= 0){
    numToPrint = 5
  }


  if (dfName %in% ls(name = env)) {
    df<-get(x = dfName, envir = env)
    df<-drop_na(df)
    df<-charToFactor(df)


    if(!(targetName %in% colnames(df))) {
      if(is.na(targetName) || targetName == ""){
        print("Target column name not given!")
      }else{
        print(paste(targetName, " not belongs to any column."))
      }
      return()
    }
    formulation<-paste(targetName,"~.")
    reg<-lm(formulation,df)
    sortedPValue <- sort(summary(reg)$coefficients[,"Pr(>|t|)"])
    sortedPValue <- sortedPValue[names(sortedPValue) %in% "(Intercept)" == FALSE]

    if(numToPrint > length(sortedPValue)){
      numToPrint = length(sortedPValue)
    }

    output <- as.data.frame(sortedPValue[1:numToPrint])
    names(output)[names(output) == "sortedPValue[1:numToPrint]"] <- "p-value"

    print(format(output, digits = 4, width=20))
  }
  else{
    if(dfName == ""){
      print("Missing data frame name!")
    }else{
      print(paste(dfName, " not in environment variable."))
    }
  }
}
