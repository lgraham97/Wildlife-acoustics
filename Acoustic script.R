#R script for commands for practical example 

#Southeast Asian monkey recordings - create folder and save audio####
#Store them in subfolder called FocalRecordings to keep audio organised 
#Create folder in Rstudio project called focak recordings 
dir.create(file.path("FocalRecordings"), showWarnings = FALSE)

#Load the soundfiles that were in the behaviour package 
githubURL <- "https://github.com/DenaJGibbon/behaviouRdata/raw/master/data/FocalRecordings.rda"
FocalRecordings <- get(load(url(githubURL)))

#Now save the recordings to the new folder as (standard audio.wav format files)
for (a in 1:length(FocalRecordings)) {
  FileName <- FocalRecordings[[a]][1][[1]]
  WaveFile <- FocalRecordings[[a]][2][[1]]
  writeWave(WaveFile, paste("FocalRecordings/", FileName, sep = ""))
}
#Can now go into files and listen to the 10 second audio clips 


#Importing and displaying individual .wav file####
#Import first female gibbon sound using readWave function (tuneR package)

GibbonWaveFile <- readWave("FocalRecordings/FemaleGibbon_1.wav")
GibbonWaveFile

#Shows no of samples (572054) / duration of recording (12.97) / sample rate (44100 per second)
#Check if the sample makes sense using equation (duration * sampling rate)
12.97 * 44100 #Returns 571977 

#Sampling rate is in Hertz - one hertz the basic unit of frequency (measures as one complete cycle of sound wave per second)
#Usually work with KiloHertz 
GibbonWaveFile@samp.rate / 1000 #Returns 44.1 

#Can also check duration makes sense in terms of total number of records in the WAV file 
duration(GibbonWaveFile) * GibbonWaveFile@samp.rate
#Returns 572054

#Plot the amplitude using (oscillo) function to create oscillogram plot from sound recordings 
oscillo(GibbonWaveFile)
#Larger the amlpitude the larger the sound - amplitude reflects volume 
#Amplitude measured in dB - hard to see individual waveform across the 12+ seconds 

#Can zoom in to a fraction of a second to display waveform more clearly 
oscillo(GibbonWaveFile, from = 0.1, to = 0.2)

#Still difficult to see pattern - zoom in more clearly 
oscillo(GibbonWaveFile, from = 0.15, to = 0.2)

#Creating a spectrogram####
#Spectogram show how frequencies varies over time
#Effectively shows time on x-axis and frequencies on y-axis 
#Shows amplitude by creating a 3D plot but easier to show in 2D plot in different colour 
#Most audio processing software can create these directly from original .WAV file 
SpectrogramSingle(sound.file = "FocalRecordings/FemaleGibbon_1.wav")
#Returned (NULL)

#Default spectogram is monochrome - can see very little information above about 3kHz 
#Could be background noise - zoom in to inspect 
SpectrogramSingle(sound.file = "FocalRecordings/FemaleGibbon_1.wav", min.freq = 500, max.freq = 2500)
#Returned (NULL)

#Easier to display in colour 
SpectrogramSingle(sound.file = "FocalRecordings/FemaleGibbon_1.wav", min.freq = 500, max.freq = 2500, Colors = "Colors")
#Returned (NULL)

#Can create ggplot2 compatible with spectograms using ggspectro function 
#This operates on R object rather than .wav - slower to produce output but easier to customise 
#Recommend using default SpectogramSingle function 

#It is sometimes a case of trial and error to get the limits and spectro colors 
#at a suitable scale to see the info displayed nicely 
v <- ggspectro(GibbonWaveFile, flim = c(0,2.5)) + 
  geom_tile(aes(fill = amplitude)) +
  scale_fill_gradient2(name = "Amplitude\n(dB)\n", limits=c(-60,0),
                       na.value = "transparent",
                       low = "green",mid = "yellow",high = "red", midpoint = -30)
#Pull up graph 
v




#Display multiple spectrograms####
#Useful to display spectograms side by side to compare 
#par function controls how many graphs are plotted on single screen 

#Can tell R to print spectograms 2x2 using code below 
par(mfrow = c(2,2))

#Create the spectograms 
SpectrogramFunction(input.dir = "FocalRecordings", min.freq = 500, max.freq = 2500, 
                    Colors = "Colors")
Spec

par(mfrow = c(1,1))

#Simplifying audio data to alloww multivaritate analysis - NOTES####
#Audio datasets are complex and large - challenging to analyse 
#Two major complementary methods used to analyse datasets:
## 1: Deep learning through convolutional neural networks - Large datasets needed to train models before accurate 
## 2: Multivariate methods - principal component analysis 

#In FocalRecording folder - 12.wav files 
#Each wav file = thousands of samples (570,054 elements)
#Far too many for PCA - lots of info is redundant or noise 


#Mel-frequency cepstral coefficients (MFCC) NOTES####
#Standard techniques to simplify audio data - looking for repeated patterns in sound - undertaking feature extraction
#Feature extraction - way to simplify complex data to simpler form 
#
#Main steps:
#1: Take Fourier transform the audio signal - helps disentangle patterns in audio based on underlying sounds 
#2: Convert powers of key frequencies detected by Fourier using mel scale 
### Mel scale uses triangular overlapping windows - laid ontop of Frourier output to simplify info 
#3: Take logarithms of the values in each overlapping windows 
#4: Take the cosine of the logarithms 
#5: The coefficient are the amplitudes of the resulting spectra 

#Can compress original data from 500,000 records per wav. file to 100-200 melfrenquency coefficient using MFCCFunction command 
FeatureDataframe <- MFCCFunction(input.dir = "FocalRecordings")

dim(FeatureDataframe)
#Return (12 178 - from 500,000

#Look at the resulting data.frame and can see the individual rows and columns 
View(FeatureDataframe)

#Load in the nes r package from old coursework 
source("nes8010.R")

#Use [,-1] to keep all rows but omit first column 
acoustics_pca <- ordi_pca(FeatureDataframe[,-1],scale=TRUE)
#pull up summary 
summary(acoustics_pca)

#check what percentage variation is explained by PCA axis 1 and 2
#Not interested in the MFCC coefficient (species or columns) so dont need to visualise them so dont need to visualise 
ordi_plot(acoustics_pca, display="sites")

#There are 3 groups of plot - do they align with 3 sets of animal recordings 
# female gibbons / great argus / male solo monkeys 
#re draw the graph, extract the scores and use ggplot2 labeling 

acoustics_sco <- ordi_scores(acoustics_pca, display = "sites")
acoustics_sco <- mutate(acoustics_sco, group_code = FeatureDataframe$Class)

ggplot(acoustics_sco, aes(x=PC1, y=PC2, colour=group_code)) +
  geom_point()
#Can see the 3 group sounds are quite different to each other 


#Songbird analysis####
#The xeno-canto dataset provides a huge international dataset of Citizen science acoustic data 
#Quality and duration is variable 
#More recordings of common species 
#R package warbleR contains advanced methods for bioacoustic analysis 
#Contains a number of functions which make it very easy to bulk download 

library(warbleR)

#Search for blackbird - 2-25 seconds - song and alarm different 
#Just checking records in database - not downloading 

#Blackbird song 
blackbird_song <- query_xc(qword = 'Turdus merula cnt:"united kingdom" type:song len:5-25', download = FALSE)
#51 recordings found 

#Blackbird alarm 
blackbird_alarm <- query_xc(qword = 'Turdus merula cnt:"united kingdom" type:alarm len:5-25', download = FALSE)
#42 recordings found 

#Can add aditional types of information such as gender - many dont specify gender though 

#View 2 different dataframes i have just created 
blackbird_song
blackbird_alarm
#There are quality columns between A to E (A = best / E = poorest)
#Another columns for "other species" 
#WarblR function allows creation of leaflet maps - popup for location of each recording 
#Click popups to follow links to xeno-canto site to listen 

#Create map 
map_xc(blackbird_song, leaflet.map = TRUE)


#now download audio for analysis - into seperate folders for alarm and song 
dir.create(file.path("blackbird_songs"))
dir.create(file.path("blackbird_alarm"))

#Download mp3 files into separate subfolders 
query_xc(X = blackbird_song, path = "blackbird_songs")
query_xc(X = blackbird_alarm, path = "blackbird_alarm")


library(stringr) #part of tidyverse 

#Following code obtains list of file names in blackbird_song subfolder 
#Creates empty r object called new difiles which will contain new filenames 
#goes through loop to repeat set of commands for each file 
#Takes name of each file - uses str_split to sub divide into 3 pieces based on - 
#Concatenate the bits of filename together using str_c keeping first two components
#together to create turdusmerula / adding text saying song which ending in _ and add last component from mp3 file 
#Add this name to the new files object which gradually gets longer as you loop through each file 
#Rename all files 


old_files <- list.files("blackbird_songs", full.names=TRUE)
new_files <- NULL

for(file in 1:length(old_files)){  
  curr_file <- str_split(old_files[file], "-")  
  new_name <- str_c(c(curr_file[[1]][1:2], "-song_", curr_file[[1]][3]), collapse="")
  new_files <- c(new_files, new_name)
}

file.rename(old_files, new_files)

old_files <- list.files("blackbird_alarm", full.names=TRUE)
new_files <- NULL
for(file in 1:length(old_files)){  
  curr_file <- str_split(old_files[file], "-")  
  new_name <- str_c(c(curr_file[[1]][1:2], "-alarm_", curr_file[[1]][3]), collapse="")  
  new_files <- c(new_files, new_name)
  }

file.rename(old_files, new_files)

#Renamed all original MP3 files - now copy them to new subfolder in Rproject called blackbird_audio
dir.create(file.path("blackbird_audio"))

file.copy(from=paste0("blackbird_songs/",list.files("blackbird_songs")),          
          to="blackbird_audio")
file.copy(from=paste0("blackbird_alarm/",list.files("blackbird_alarm")),          
          to="blackbird_audio")

#Need to work with WAV files not MP3 so use mp3wav function from warbleR package 

mp32wav(path="blackbird_audio", dest.path="blackbird_audio")
unwanted_mp3 <- dir(path="blackbird_audio", pattern="*.mp3")
file.remove(paste0("blackbird_audio/", unwanted_mp3))

#Visualise and analyse song and alarm call####
blackbird_wav <- readWave("blackbird_audio/Turdusmerula-song_243908.wav")

blackbird_wav

#Plot full frequency diagram on screen 
oscillo(blackbird_wav)
#Go back to original wav file and listen to it - understand how the oscillogram changes with the sound 

#Zoom in to check the wav in more detail 
oscillo(blackbird_wav, from = 0.59, to = 0.60)


#Spectogram of single bird wav file using colour display - 
SpectrogramSingle(sound.file = "blackbird_audio/Turdusmerula-song_243908.wav",                  
                  Colors = "Colors")
SpectrogramSingle(sound.file = "blackbird_audio/Turdusmerula-song_310406.wav",                  
                  Colors = "Colors")
SpectrogramSingle(sound.file = "blackbird_audio/Turdusmerula-alarm_631603.wav",                  
                  Colors = "Colors")

#MFCC of blackbird song and alarm calls ####
#Simplify the blackbird data through feature extraction using mel-frequency 
#Default maximum frequency is 2000kHz / change to 7000Hz 
blackbird_mfcc <- MFCCFunction(input.dir = "blackbird_audio",                               
                               max.freq=7000)

dim(blackbird_mfcc)

#Carry out PCA of data - visualise the patterns in a 2 dimensional plot 
#Add scale=TRUE to call your data to ordi_pca and omit first column with [-1,]
blackbird_pca <- ordi_pca(blackbird_mfcc[, -1], scale=TRUE)
summary(blackbird_pca)

#Can see that the percentage variation explained 
blackbird_sco <- ordi_scores(blackbird_pca, display="sites")
blackbird_sco <- mutate(blackbird_sco, group_code = blackbird_mfcc$Class)

ggplot(blackbird_sco, aes(x=PC1, y=PC2, colour=group_code)) +
  geom_point() 
