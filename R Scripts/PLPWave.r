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

data <- vector(mode = "list", length=length(args))


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
	close(fileText)	
	
	dataVector <- vector(mode="list", length=2)
	dataVector[[1]] <- aSpec
	dataVector[[2]] <- characters
	data[[i]] <- dataVector
}

## baseFileName <- basename(file_path_sans_ext(filePath))
fileCon <- file(paste("money.csv", sep=""), open = "a")

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
