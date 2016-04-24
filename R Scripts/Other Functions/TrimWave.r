library(tuneR)
library(tools)
args <- commandArgs(TRUE)

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
	fileCon <- file(paste(baseFileName, ".txt"), open = "a")
	
	wavObj <- readWave(filePath)
	wav <- wavObj@left
	trimmedWav <- TrimWav(wav)
	
	for(j in 1:length(trimmedWav)){
		cat(sprintf("%.2f", trimmedWav[j]), file=fileCon, append=TRUE, sep='\n')
	}
	close(fileCon)
}