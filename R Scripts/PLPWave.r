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

getDataOfWords <- function(w){
	data <- vector(mode = "list", length=length(w))

	## Goes through each word and preforms spectrum analysis on the wav
	## Creates a vector of all the 
	for(i in 1:length(w)){
		fileName <- w[i]	
		
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

	return(data)
}

normalizeData <- function(data){
	#fileCon <- file(paste("money.csv", sep=""), open = "a")
	## Prints out data
	## For checking to make sure the data is in the correct format
	for(i in 1:length(data)){
		
		characters <- data[[i]][[2]]	
		#cat(characters, file=fileCon, append=TRUE, sep=',')	
		#cat('\n', file=fileCon, append=TRUE)
		
		aSpecOfi <- data[[i]][[1]]
		demensions <- dim(aSpecOfi)
		nRow <- demensions[1]
		nCol <- demensions[2]	
		
		sumAspec <- rep(0, nRow)
		
		for(r in 1:nRow){
			for(c in 1:nCol){
				sumAspec[r] <- sumAspec[r] + aSpecOfi[r, c]		
			}
		}
		
		data[[i]][[1]] <- sumAspec	
		
		#cat(sumAspec, file=fileCon, append=TRUE, sep=',')	
		#cat('\n\n', file=fileCon, append=TRUE)		
	}
	#close(fileCon)
	return(data)
}

maxFeatures <- function(data){
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
	return(maxRow)
}

fitData <- file(args[1])
words <- readLines(con=fitData)

data <- getDataOfWords(words)

fileCon <- file(paste("money.csv", sep=""), open = "a")

maxRow <- maxFeatures(data)

data <- normalizeData(data)

##cat(uniqueList, file=fileCon, append=TRUE, sep='\n')
##cat('\n', file=fileCon, append=TRUE)	

charData <- vector(mode="list", length=length(uniqueList))

for(i in 1:length(charData)){
	charData[[i]] <- vector(mode="list", length=3)
	charData[[i]][[1]] <- vector(mode = "list", length=maxRow)	
	charData[[i]][[2]] <- vector(mode = "list", length=maxRow)
}

featureVals <- vector(mode="list", length=maxRow)

for(i in 1:length(data)){
	word <- data[[i]][[2]]
	featureVector <- data[[i]][[1]]
	for(j in 1:length(uniqueList)){
		char <- uniqueList[j]			
		inArray <- charData[[j]][[1]]
		outArray <- charData[[j]][[2]]
		if(char %in% word){
			for(k in 1:length(featureVector)){			
				inArray[[k]] <- c(inArray[[k]], featureVector[[k]])
				featureVals[[k]] <- c(featureVals[[k]], featureVector[[k]])
			}
		} else {							
			for(k in 1:length(featureVector)){			
				outArray[[k]] <- c(outArray[[k]], featureVector[[k]])
				featureVals[[k]] <- c(featureVals[[k]], featureVector[[k]])
			}	
		}
		charData[[j]][[1]] <- inArray
		charData[[j]][[2]] <- outArray		
	}
}

characterData <- vector(mode="list", length=length(uniqueList))
for(i in 1:length(uniqueList)){
	characterData[[i]] <- vector(mode="list", length=maxRow)
	for(j in 1:maxRow){
		characterData[[i]][[j]] <- vector(mode="list", length=4)
	}
}

for(i in 1:length(uniqueList)){
	hasChar <- charData[[i]][[1]]
	hasNotChar <- charData[[i]][[2]]
	#cat(paste(uniqueList[i],',\n'), file=fileCon, append=TRUE, sep='')
		
	#cat(paste('feature|', uniqueList[i], ':', ',mean,'), file=fileCon, append=TRUE, sep='')	
	for(j in 1:length(hasChar)){
		hasCharVector <- hasChar[[j]]	
		if(is.null(hasCharVector)){			
			#cat('0', file=fileCon, append=TRUE, sep='')
			characterData[[i]][[j]][[1]] <- 0
		} else {		
			#cat(mean(hasCharVector), file=fileCon, append=TRUE, sep='')
			characterData[[i]][[j]][[1]] <- mean(hasCharVector)
		}			
		#cat(',', file=fileCon, append=TRUE, sep='')
	}		
	
	#cat('\n', file=fileCon, append=TRUE, sep='')
	
	#cat(paste('feature|', uniqueList[i], ':', ',variance,'), file=fileCon, append=TRUE, sep='')	
	for(j in 1:length(hasChar)){			
		hasCharVector <- hasChar[[j]]	
		if(is.null(hasCharVector)){			
			#cat('0', file=fileCon, append=TRUE, sep='')
			characterData[[i]][[j]][[2]] <- 0
		} else {									
			if(all.equal(length(hasCharVector),1) == TRUE) {
				#cat('0', file=fileCon, append=TRUE, sep='')
				characterData[[i]][[j]][[2]] <- 0			
			} else {
				#cat(var(hasCharVector), file=fileCon, append=TRUE, sep='')	
				characterData[[i]][[j]][[2]] <- var(hasCharVector)
			}	
		}			
		#cat(',', file=fileCon, append=TRUE, sep='')	
	}	
	
	#cat('\n', file=fileCon, append=TRUE,sep='')
	
	#cat(paste('feature|!', uniqueList[i], ':', ',mean,'), file=fileCon, append=TRUE, sep='')
	for(j in 1:length(hasNotChar)){
		hasCharVector <- hasNotChar[[j]]
		if(is.null(hasCharVector)){			
			#cat('0', file=fileCon, append=TRUE, sep='')
			characterData[[i]][[j]][[3]] <- 0
		} else {
			#cat(mean(hasCharVector), file=fileCon, append=TRUE, sep='')	
			characterData[[i]][[j]][[3]] <- mean(hasCharVector)
		}		
		#cat(',', file=fileCon, append=TRUE, sep='')
	}		
	
	#cat('\n', file=fileCon, append=TRUE, sep='')
	
	#cat(paste('feature|!', uniqueList[i], ':', ',variance,'), file=fileCon, append=TRUE, sep='')	
	for(j in 1:length(hasNotChar)){			
		hasCharVector <- hasNotChar[[j]]	
		if(is.null(hasCharVector)){			
			#cat('0', file=fileCon, append=TRUE, sep='')			
			characterData[[i]][[j]][[4]] <- 0
		} else {	
			if(all.equal(length(hasCharVector),1) == TRUE) {				
				#cat('0', file=fileCon, append=TRUE, sep='')				
				characterData[[i]][[j]][[4]] <- 0
			} else {
				#cat(var(hasCharVector), file=fileCon, append=TRUE, sep='')							
				characterData[[i]][[j]][[4]] <- var(hasCharVector)
			}
		}
		#cat(',', file=fileCon, append=TRUE, sep='')	
	}
	
	#cat('\n\n', file=fileCon, append=TRUE,sep='')
}

featureData <- vector(mode="list", length=maxRow)
for(i in 1:maxRow){
	featureData[[i]] <- vector(mode="list", length=2)
	featureData[[i]][[1]] <- mean(featureVals[[i]])
	featureData[[i]][[2]] <- var(featureVals[[i]])
}

for(i in 1:maxRow){
	#cat(featureData[[i]][[1]], file=fileCon, append=TRUE, sep='')
	#cat(',', file=fileCon, append=TRUE, sep='')
}
#cat('\n', file=fileCon, append=TRUE, sep='')

for(i in 1:maxRow){
	#cat(featureData[[i]][[2]], file=fileCon, append=TRUE, sep='')
	#cat(',', file=fileCon, append=TRUE, sep='')
}
#cat('\n', file=fileCon, append=TRUE, sep='')


estData <- file(args[2])
estwords <- readLines(con=estData)

est <- getDataOfWords(estwords)
est <- normalizeData(est)
for(i in 1:length(est)){
	characters <- est[[i]][[2]]
	features <- est[[i]][[1]]
	cat(characters, file=fileCon, append=TRUE, sep=',')
	cat('\n', file=fileCon, append=TRUE, sep='')
	
	for(c in 1:length(characterData)){
		featuresOfChar <- characterData[[c]]
		cat(paste(uniqueList[c], ',', sep=''), file=fileCon, append=TRUE, sep='')
		
		sumHas <- 0
		for(f in 1:length(features)){		
			featureMean <- featureData[[f]][[1]]
			featureVar <- featureData[[f]][[2]]
			meanHasChar <- featuresOfChar[[f]][[1]]
			varHasChar <- featuresOfChar[[f]][[2]]
			
			val <- features[[f]]
			
			sumHas <- sumHas + dnorm(val, mean=meanHasChar, sd=sqrt(varHasChar))
		}
		
		sumHasNot <- 0		
		for(f in 1:length(features)){
			meanHasNotChar <- featuresOfChar[[f]][[3]]
			varHasNotChar <- featuresOfChar[[f]][[4]]
			featureMean <- featureData[[f]][[1]]
			featureVar <- featureData[[f]][[2]]
			
			val <- features[[f]]
			
			sumHasNot <- sumHasNot + dnorm(val, mean=meanHasNotChar, sd=sqrt(varHasNotChar))
		}
		
		totalSum <- log10(sumHas) - log10(sumHasNot)
		if(!is.na(totalSum) && totalSum > 0){
			cat('TRUE,', file=fileCon, append=TRUE, sep='')
		} else {
			cat('FALSE,', file=fileCon, append=TRUE, sep='')
		}	
		#cat(paste(sumHas, ',', sumHasNot, sep=''), file=fileCon, append=TRUE, sep='') 
		
		cat('\n', file=fileCon, append=TRUE, sep='')
	}
	
	cat('\n\n', file=fileCon, append=TRUE, sep='')
	
}

close(fileCon)
