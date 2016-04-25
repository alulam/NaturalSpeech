library(tuneR)
library(tools)
args <- commandArgs(TRUE)

winTime <- 0.025
stepTime <- 0.01

TrimWav <- function(wav){
	firstOcc <- 1
	lastOcc <- length(wav)
	for(i in 1:length(wav)){
		if(wav[i] != 0){
			firstOcc <- i
			break
		}
	}
	for(i in length(wav):firstOcc){
		if(wav[i] != 0){
			lastOcc <- i
			break
		}
	}
	return(wav[firstOcc:lastOcc])
}

uniqueList <- NULL

getUniqueList <- function(){
  return (uniqueList)
}

addToUniqueList <- function(chr){
  if(!chr %in% uniqueList){
    uniqueList <<- c(uniqueList, chr)
  }
}

#Function accepts a valid path of a text file, returns an array of 
#characters that parse the IPA string it was given
  
ParseIPA <- function(dat){
  
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
        ipaSymbol <- substr(let,4, nchar(let))
        finalList[count] <- ipaSymbol
        addToUniqueList(ipaSymbol)
      }else{
        addToUniqueList(firstChar)
      }
      count = count +1
    }
  }
  
 return (finalList)
}

data <- vector(mode = "list", length=length(args))

## Goes through each word and preforms spectrum analysis on the wav
## Creates a vector of all the 
for(i in 1:length(args)){
	fileName <- args[i]	
	
	## Read in the wave file
	
	wavObj <- readWave(paste(fileName, ".wav", sep=""))
	wav <- wavObj@left
	sampleRate <- wavObj@samp.rate
	trimmedWav <- TrimWav(wav)
	
	pSpec <- powspec(trimmedWav, sampleRate, wintime = winTime, steptime = stepTime, dither = FALSE)
	aSpec <- audspec(pSpec, sr = sampleRate, nfilts = ceiling(hz2bark(sampleRate/2)) + 1, 
		fbtype = c("bark", "mel", "htkmel", "fcmel"), minfreq = 0, 
		maxfreq = sampleRate/2, sumpower = TRUE, bwidth = 1)$aspectrum
	
	## Read in the characters of the word
	
	fileText <- file(paste(fileName, ".txt", sep=""))
	characters <- readLines(fileText)	
	parsedCharacterArray <- ParseIPA(characters)	
	close(fileText)	
	
	dataVector <- vector(mode="list", length=2)
	dataVector[[1]] <- aSpec
	dataVector[[2]] <- parsedCharacterArray
	data[[i]] <- dataVector
}

## baseFileName <- basename(file_path_sans_ext(filePath))
fileCon <- file(paste("money.csv", sep=""), open = "a")

## Max number of features
maxRow <- 0

## Calculates the maxmimum number of features
for(i in 1:length(data)){
	aSpecOfi <- data[[i]][[1]]
	demensions <- dim(aSpecOfi)
	nRow <- demensions[1]
	if(nRow > maxRow){
		maxRow = nRow
	}
}

## Max number for each feature
maxFeatures = rep(0, maxRow)
for(i in 1:length(data)){
	aSpec <- data[[i]][[1]]
	demensions <- dim(aSpec)
	nRow <- demensions[1]
	cCol <- demensions[2]
	for(c in 1:cCol){
		for(r in 1:nRow){
			if(aSpec[r, c] > maxFeatures[r]){
				maxFeatures[r] <- aSpec[r, c]
			}
		}
	}
}

## Normalizes each feature at each time
for(i in 1:length(data)){
	aSpec <- data[[i]][[1]]
	demensions <- dim(aSpec)
	nRow <- demensions[1]
	cCol <- demensions[2]
	for(c in 1:cCol){
		for(r in 1:nRow){
			aSpec[r, c] <- aSpec[r, c] / maxFeatures[r]
		}
	}
	data[[i]][[1]] <- aSpec	
}

## Prints out data
## For checking to make sure the data is in the correct format
for(i in 1:length(data)){
	
	aSpecOfi <- data[[i]][[1]]
	demensions <- dim(aSpecOfi)
	nRow <- demensions[1]
	nCol <- demensions[2]

	#cat(sprintf("Sample Rate,%f", sampleRate), file=fileCon, append = FALSE, sep='\n')
	#cat(sprintf("Window Time,%f", winTime), file=fileCon, append = FALSE, sep='\n')
	#cat(sprintf("Step Time,%f", stepTime), file=fileCon, append = FALSE, sep='\n')
	
	for(c in 1:nCol){
		cat(sprintf("%.2f", aSpecOfi[,c]), file=fileCon, append=TRUE, sep=',')
		cat('\n', file=fileCon, append=TRUE)
	}
	
	characters <- data[[i]][[2]]	
	cat(characters, file=fileCon, append=TRUE, sep=',')
	cat('\n', file=fileCon, append=TRUE)
		
}

close(fileCon)


## A matrix where the rows are for each feature vector
## and each column is for a symbol
weights <- matrix(data=0, nrow=maxRow, ncol=3)

## Do the weight fitting
## Not currently implemented
for(i in 1:length(data)){
	
	aSpecOfi <- data[[i]][[1]]
	demensions <- dim(aSpecOfi)
	nRow <- demensions[1]
	nCol <- demensions[2]
	
	## Iterate through all the time windows
	for(j in 1:nRow){
	
	}
		
}