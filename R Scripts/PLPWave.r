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

for(i in 1:length(args)){
	filePath <- args[i]
	baseFileName <- basename(file_path_sans_ext(filePath))
	fileCon <- file(paste(baseFileName, ".csv"), open = "a")
	
	wavObj <- readWave(filePath)
	wav <- wavObj@left
	sampleRate <- wavObj@samp.rate
	trimmedWav <- TrimWav(wav)
	
	pSpec <- powspec(trimmedWav, sampleRate, wintime = winTime, steptime = stepTime, dither = FALSE)
	aSpec <- audspec(pSpec, sr = sampleRate, nfilts = ceiling(hz2bark(sampleRate/2)) + 1, 
		fbtype = c("bark", "mel", "htkmel", "fcmel"), minfreq = 0, 
		maxfreq = sampleRate/2, sumpower = TRUE, bwidth = 1)$aspectrum
	dimBand <- dim(aSpec)
	nRow <- dimBand[1]
	nCol <- dimBand[2]

	cat(sprintf("Sample Rate,%f", sampleRate), file=fileCon, append = FALSE, sep='\n')
	cat(sprintf("Window Time,%f", winTime), file=fileCon, append = FALSE, sep='\n')
	cat(sprintf("Step Time,%f", stepTime), file=fileCon, append = FALSE, sep='\n')
	
	for(c in 1:nCol){
		cat(sprintf("%.2f", aSpec[,c]), file=fileCon, append=TRUE, sep=',')
		cat('\n', file=fileCon, append=TRUE)
	}
	
	close(fileCon)
}