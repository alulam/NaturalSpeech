#Function accepts a valid path of a text file, returns an array of 
#characters that parse the IPA string it was given
parseIPA <- function(file){
    
  #Store File contents into data structure
  dat <- readChar(file, file.info(file)$size)
  
  if(!is.null(dat)){
    #remove new line symbol '\r\n' if it exists
    endWord <- substr(dat,nchar(dat)-1, nchar(dat))
    if(identical(endWord, "\r\n")){
      dat <- substr(dat,0,nchar(dat)-2)
    }
  
    #Convert contents to array  
    split <- strsplit(dat, ';')
    split <- split[[1]]
    finalList <- split
    
    count= 1
    
    for (let in split){
      firstChar <- substr(let,1,1) 
      if(identical(firstChar,"&")){
        finalList[count] <- substr(let,4, nchar(let))
      }
      count = count +1
    }
  }
  
 return (finalList)
}



