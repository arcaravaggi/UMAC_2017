#setwd("G:/GDrive/Mammal Society/University Challenge 2017/Data/Splitting/")
setwd("C:/Users/Anthony caravaggi/Google Drive/Mammal Society/University Challenge 2017/Data/Splitting/")

library(reshape)
library(reshape2)
library(data.table)
library(dplyr)
library(tidyr)
library(car)

# run R scripts in subdirectories
imp.dat <- function(month){
  if(month == "jan"){
    folder.dir <- list.dirs("01 January")
  } else if(month == "feb"){
    folder.dir <- list.dirs("02 February")
  } else if(month =="mar"){
    folder.dir <- list.dirs("03 March")
  } else if(month =="apr"){
    folder.dir <- list.dirs("04 April")
  } else if(month =="may"){
    folder.dir <- list.dirs("05 May")
  } else if(month =="jun"){
    folder.dir <- list.dirs("06 June")
  }
  folder.dir <- folder.dir[-1]  # comment this out if the main folder also contains the script
  script.paths <- paste(folder.dir,"/","process.R", sep="")
  for (i in script.paths) {
    source(i)
  }
}

# remove rows with blank cells and NAs in a specified column (called in subscripts)
completeFun <- function(data, desiredCols) {
  data[data==""]<-NA
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

# read in each .csv file in file_list and rbind them into a data frame 
# fol = path to folder that holds multiple .csv files
# col = column used to limit row removal (based on NAs)
imp.csv <- function (fol, col, m){
  folder <- fol
  month <- m
  file_list <- list.files(path=folder, pattern="*.csv")
  temp.df <- do.call("rbind", 
                     lapply(file_list, function(x) 
                     {
                       tmp <- read.csv(paste(folder, x, sep=''))
                       tmp$team <- substr(x, 6, 8)
                       tmp$loc <- substr(x, 10, 13)
                       tmp$month <- month
                       return(tmp) 
                     }))
  completeFun(temp.df, col)
}


# Import data which is automatically cleaned to a certain extent
# As individual months

# imp.dat(month = "may")

# Or all months

mth <- c("jan", "feb", "mar", "apr", "may", "jun")

for(i in seq_along(mth)){
  imp.dat(month = mth[i])
}



##### Code below is superceded by joining & cleaning of dataframes
#
# Export dataframes as csvs
# Create labelled list of all active dataframes
#
# my.list <- mget(ls(pattern="*_UMAC"))
# 
# Write csvs for each dataframe
#
# setwd('../Cleaning/')
# mapply(write.csv, my.list, paste0(names(my.list), ".csv"))
#
#####



# Joining dataframes for each survey
# First tidy up column names

bats_feb_UMAC$Temperature <- NULL
bats_feb_UMAC$Time..start.end. <- NULL
names(bats_feb_UMAC)[names(bats_feb_UMAC) == 'Time.Spotted'] <- 'Time'
bats_feb_UMAC["Any.other.comments"] <- NA
bats_feb_UMAC <- bats_feb_UMAC[, c(1:12, 16, 13:15)] # re-ordering df to match others

bat.list <- mget(ls(pattern="bats*"))
bats_all_2017 <- merge_all(bat.list)

cam.list <- mget(ls(pattern="cams*"))
cams_all_2017 <- merge_all(cam.list)

field.list <- mget(ls(pattern="field*"))
field_all_2017 <- merge_all(field.list)

foot.list <- mget(ls(pattern="foot*"))
foot_all_2017 <- merge_all(foot.list)

#habitat.list <- mget(ls(pattern="habitat*"))
#habitat_all_2017 <- merge_all(habitat.list)

line_jan_UMAC["Observer"] <- NA
line_jan_UMAC["Any.other.comments"] <- NA
line_jan_UMAC <- line_jan_UMAC[, c(1:7, 11:12, 8:10)] # re-ordering df to match others
line.list <- mget(ls(pattern="line*"))
line_all_2017 <- merge_all(line.list)

live.list <- mget(ls(pattern="live*"))
live_all_2017 <- merge_all(live.list)

oppo.list <- mget(ls(pattern="oppo*"))
oppo_all_2017 <- merge_all(oppo.list)

stan.list <- mget(ls(pattern="stan*"))
stan_all_2017 <- merge_all(stan.list)




####
# Species relabelling for analyses
####


# Create list of unique species in df list, duplicate to 
Species <- unique(unlist(lapply(bat.list, function(x) unique(x[,4])))) # Extract unique species
Species <- trimws(Species) #Remove whitespace
Species <- unique(Species) #Remove duplicates
latin <- as.character(Species)

latin <- recode(latin,"c('Myotis')='Myotis sp.';
                c('C.Pip', 'Common pipistrelle', 'C. pipistrelle', 'Common', 'common pipistrelle', 'common pip', 'C. Pipistrelle', 'Common Pipistrelle', 'Common pip')='Pipistrellus pipistrellus';
                c('S.Pip', 'Soprano Pipistrelle', 'S. Pipistrelle', 'soprano pipistrelle', 'Soprano', 'Soprano pipistrelle', 'Soprano pip')='Pipistrellus pygmaeus';
                c('Noctule', 'Common noctule')='Nyctalus noctula';
                c('Nath Pip', 'Nath.Pip')='Pipistrellus nathusii';
                c('BLE', 'Brown long-eared bat')='Plecotus auritus';
                c('Myotis daubentoni', 'Daubentons', 'Daubenton')='Myotis daubentonii';
                c('Pipistrelle (exact species unknown)', 'Common/ soprano mix')='Pipistrellus sp.'
                ")
latin[[28]] <- "Myotis daubentonii"
latin


# Create df, rename column for merge & convert species lists to lookup table
bat.names <- data.frame(Species, latin)
bat.names <- setDT(bat.names)

# Merge and replace values
bats_all_2017$Species <- trimws(bats_all_2017$Species)
bats_all_2017 <- (merge(bat.names, bats_all_2017, by = 'Species'))
as.character(unique(unlist(bats_all_2017$latin))) # Check unique species

bats_all_2017<-bats_all_2017[!(bats_all_2017$latin=="Unknown"),]
as.character(unique(unlist(bats_all_2017$latin))) # Check unique species


# Camera traps

Species <- unique(unlist(lapply(cam.list, function(x) unique(x[,7])))) # Extract unique species
Species <- trimws(Species) #Remove whitespace
Species <- unique(Species) #Remove duplicates
latin <- as.character(Species)

latin <- recode(latin,"
                c('badger', 'Badger', 'Badger (Meles meles)')='Meles meles';
                c('Brown hare', 'Hare')='Lepus europaeus';
                c('fox', 'Fox', 'Red fox', 'Fox (Vulpes vulpes)', 'Red Fox')='Vulpes vulpes';
                c('rabbit', 'Rabbit')='Oryctolagus cuniculus';
                c('grey squirrel', 'Grey Squirrel?', 'Grey Squirell', 'Grey Squirrel', 'Grey squirrel')='Scirius carolinensis';
                c('red squirrel', 'Red Squirrel')='Sciurus vulgaris';
                c('Roe Deer', 'roe deer', 'Roe deer')='Capreolus capreolus';
                c('wood mouse', 'Wood Mouse', 'Wood mouse', 'Woodmouse (Apodemus sylvaticus)', 'Woodmouse')='Apodemus sylvaticus';
                c('Pine Marten')='Martes martes'; 
                c('Stoat', 'Stout')='Mustela erminea';
                c('Red Deer')='Cervus elaphus';
                c('Cat', 'Cat (Felic catus)', 'Domestic Cat', 'Domestic cat')='Felis catus';
                c('Ferret')='Mustela putorius';
                c('Mous Sp', 'Mouse Sp', 'Mouse', 'mouse')='Murinae sp.';
                c('Muntjac Deer', 'Muntjac')='Muntiacus reevesi';
                c('Rat', 'Brown Rat', 'Brown rat')='Rattus norvegicus';
                c('Small mammal (mouse?)', 'Small mammal', 'Small mammal (species unknown)')='Unknown';
                c('Hedgehog', 'hedgehog')='Erinaceus europaeus';
                c('Squirrel')='Sciurus sp.';
                c('Horse')='Equus ferus caballus';
                c('Dog')='Canis lupus familiaris';
                c('Common shrew')='Sorex araneus';
                c('Deer')='Cervidae sp.';
                c('Weasel')='Mustela nivalis'
                ")
latin

# Create df, rename column for merge & convert species lists to lookup table
cam.names <- data.frame(Species, latin)
cam.names <- setDT(cam.names)

# Merge and replace values

cams_all_2017$Species <- trimws(cams_all_2017$Species)
cams_all_2017 <- (merge(cam.names, cams_all_2017, by = 'Species'))
as.character(unique(unlist(cams_all_2017$latin))) # Check unique species

cams_all_2017<-cams_all_2017[!(cams_all_2017$latin=="Unknown"),]
as.character(unique(unlist(cams_all_2017$latin))) # Check unique species


# Field surveys

Species <- unique(unlist(lapply(field.list, function(x) unique(x[,4])))) # Extract unique species
Species <- trimws(Species) #Remove whitespace
Species <- unique(Species) #Remove duplicates
latin <- as.character(Species)

latin <- recode(latin, "c('Roe Deer', 'roe deer', 'Roe deer')='Capreolus capreolus';
                c('fox', 'Fox', 'Red fox', 'Fox (Vulpes vulpes)', 'Red Fox')='Vulpes vulpes';
                c('badger', 'Badger', 'Badger (Meles meles)')='Meles meles';
                c('Hedgehog', 'hedgehog')='Erinaceus europaeus';
                c('Mous Sp', 'Mouse Sp', 'Mouse', 'mouse')='Murinae sp.';
                c('Squirrel')='Sciurus sp.';
                c('Deer sp.', 'deer sp.')='Cervidae sp.';
                c('Cat', 'Cat (Felic catus)', 'Domestic Cat', 'Domestic cat', 'domestic cat')='Felis catus';
                c('grey squirrel', 'Grey Squirrel?', 'Grey Squirell', 'Grey Squirrel', 'Grey squirrel')='Scirius carolinensis';
                c('rabbit', 'Rabbit', 'Rabbit (Oryctolagus Cuniculus)')='Oryctolagus cuniculus';
                c('mole', 'Mole')='Talpa europaea';
                c('otter')='Lutra lutra';
                c('Field vole')='Microtus agrestis';
                c('Bank vole', 'Bank Vole')='Myodes glareolus';
                c('vole')='Arvicolinae sp.';
                c('?', 'Rabbit/Fox?', '?TBC', 'NK (most likely badger)')='Unknown'
                ")
latin

# Create df, rename column for merge & convert species lists to lookup table
field.names <- data.frame(Species, latin)
field.names <- setDT(field.names)

# Merge and replace values

field_all_2017$Species <- trimws(field_all_2017$Species)
field_all_2017 <- (merge(field.names, field_all_2017, by = 'Species'))
as.character(unique(unlist(field_all_2017$latin))) # Check unique species

field_all_2017<-field_all_2017[!(field_all_2017$latin=="Unknown"),]
as.character(unique(unlist(field_all_2017$latin))) # Check unique species


# Footprint tunnels

Species <- unique(unlist(lapply(foot.list, function(x) unique(x[,7])))) # Extract unique species
Species <- trimws(Species) #Remove whitespace
Species <- unique(Species) #Remove duplicates
latin <- as.character(Species)

# Replace separators with commas, combine to dataframe and split rows by commas

latin <- gsub(" &", ",", latin)
latin <- gsub(" and ", ",", latin)

foot.names <- data.frame(Species, latin)

# library(dplyr)
# library(tidyr)
foot.names <- foot.names %>%
  mutate(latin = strsplit(as.character(latin), ",")) %>%
  unnest(latin)

foot.names$latin <- trimws(foot.names$latin )

foot.names$latin <- recode(foot.names$latin, "c('Brown Rat', 'Rat', 'Brown rat')='Rattus norvegicus';
                           c('Cat', 'Domestic cat', 'domestic cat')='Felis catus';
                           c('badger', 'Badger', 'Badger (Meles meles)')='Meles meles';
                           c('Mice', 'Mouse', 'mouse', 'mice')='Muridae sp.';
                           c('Hedgehog')='Erinaceus europaeus';
                           c('rabbit')='Orcytolagus cuniculus';
                           c('wood mouse')='Apodemus sylvaticus'
                           ")
foot.names$latin

# Convert species lists to lookup table
foot.names <- setDT(foot.names)

# Merge and replace values

foot_all_2017$Species <- trimws(foot_all_2017$Species)
foot_all_2017 <- (merge(foot.names, foot_all_2017, by = 'Species'))
as.character(unique(unlist(foot_all_2017$latin))) # Check unique species




# Line transects

Species <- unique(unlist(lapply(line.list, function(x) unique(x[,5])))) # Extract unique species
Species <- trimws(Species) #Remove whitespace
Species <- unique(Species) #Remove duplicates
latin <- as.character(Species)

latin <- recode(latin, "c('Brown Rat', 'Rat', 'Brown rat', 'brown rat')='Rattus norvegicus';
                c('grey squirrel', 'Grey Squirrel?', 'Grey Squirrel Sciurus carolinensis', 'Grey Squirell', 'Grey Squirrel', 'Grey squirrel', 'Grey squirrel (Sciurus carolinensis)')='Scirius carolinensis';
                c('rabbit', 'Rabbit')='Orcytolagus cuniculus';
                c('fox', 'Fox', 'Red fox', 'Fox (Vulpes vulpes)', 'Red Fox')='Vulpes vulpes';
                c('Squirrel', 'squirrel', 'Squrrel', 'SquIrrel')='Sciurus sp.';
                c('Hedgehog', 'hedgehog')='Erinaceus europaeus';
                c('Brown Hare')='Lepus europaeus';
                c('Pipistrelle bat', 'Bat (unknown)', 'Bat')='Vespertilionidae sp.';
                c('Cat', 'Domestic Cat', 'Dometic Cat', 'domestic cat')='Felis catus';
                c('Sheep')='Ovis aries';
                c('Pheasent')='Phasianus colchicus';
                c('Roe Deer', 'roe deer', 'Roe deer')='Capreolus capreolus';
                c('red squirrel', 'Red Squirrel')='Sciurus vulgaris'
                ")
latin

# Create df, rename column for merge & convert species lists to lookup table
line.names <- data.frame(Species, latin)
line.names <- setDT(line.names)

# Merge and replace values

line_all_2017$Species <- trimws(line_all_2017$Species)
line_all_2017 <- (merge(line.names, line_all_2017, by = 'Species'))
as.character(unique(unlist(line_all_2017$latin))) # Check unique species



# Live trapping

Species <- unique(unlist(lapply(live.list, function(x) unique(x[,6])))) # Extract unique species
Species <- trimws(Species) #Remove whitespace
Species <- unique(Species) #Remove duplicates
latin <- as.character(Species)

latin <- recode(latin, "c('Bank vole', 'Bank Vole')='Myodes glareolus';
                c('1 Wood mouse', 'wood mouse', 'Wood Mouse', 'Wood mouse', 'Field mouse', 'Woodmouse (Apodemus sylvaticus)', 'Woodmouse')='Apodemus sylvaticus';
                c('Common shrew')='Sorex araneus';
                c('Field vole')='Microtus agrestis';
                c('Yellow-necked mouse', 'Yellow necked mouse')='Apodemus flavicollis';
                c('Pygmy shrew')='Sorex minutus'
                ")
latin

temp <- subset(live_all_2017, live_all_2017[,6] == "Woodland")
rm(temp)

# Create df, rename column for merge & convert species lists to lookup table
live.names <- data.frame(Species, latin)
live.names <- setDT(live.names)

# Merge and replace values

live_all_2017$Species <- trimws(live_all_2017$Species)
live_all_2017 <- (merge(live.names, live_all_2017, by = 'Species'))
as.character(unique(unlist(live_all_2017$latin))) # Check unique species

live_all_2017<-live_all_2017[!(live_all_2017$latin=="Human site"),]
live_all_2017<-live_all_2017[!(live_all_2017$latin=="Woodland"),]
as.character(unique(unlist(live_all_2017$latin))) # Check unique species




# Opportunistic

Species <- unique(unlist(lapply(oppo.list, function(x) unique(x[,4])))) # Extract unique species
Species <- trimws(Species) #Remove whitespace
Species <- unique(Species) #Remove duplicates
latin <- as.character(Species)

latin <- recode(latin, "c('Field vole', 'Field Vole', 'field vole')='Microtus agrestis';
                c('Cat', 'Cat (Felic catus)', 'Domestic Cat', 'Domestic cat', 'cat', 'domestic cat', 'Cat - domestic')='Felis catus';
                c('rabbit', 'Rabbit', 'Rabbit (Oryctolagus Cuniculus)', 'European Rabbit (Oryctolagus cuniculus')='Oryctolagus cuniculus';
                c('grey squirrel', 'Grey Squirrel?', 'Grey Squirell', 'Grey Squirrel', 'Grey squirrel', 'Grey squirrel - drey', 'Western Grey Squirell (Sciurus griseus)', 'Grey Squrrel')='Scirius carolinensis';
                c('Vole', 'Vole sp.')='Arvicolidae sp.';
                c('fox', 'Fox', 'Red fox', 'red fox', 'Fox (Vulpes vulpes)', 'Red Fox')='Vulpes vulpes';
                c('Squirrel', 'squiz')='Sciurus sp.';
                c('Bank vole', 'Bank Vole')='Myodes glareolus';
                c('Brown Rat', 'Rat', 'Brown rat')='Rattus norvegicus';
                c('Pipistrelles', 'Pipistrelle', 'Common pipistrelle bat')='Pipistrellus sp.';
                c('badger', 'Badger', 'Badger (Meles meles)')='Meles meles';
                c('mole', 'Mole')='Talpa europaea';
                c('Hedgehog', 'hedgehog')='Erinaceus europaeus';
                c('wood mouse', 'Wood Mouse', 'Wood mouse', 'woodmouse', 'Woodmouse (Apodemus sylvaticus)', 'Woodmouse', 'Field Mouse')='Apodemus sylvaticus';
                c('red squirrel', 'Red Squirrel')='Sciurus vulgaris';
                c('Horse')='Equus ferus caballus';
                c('Donkey')='Equus asinus';
                c('Roe Deer', 'roe deer', 'Roe deer', 'Roe deer (Capreolus capreolus)')='Capreolus capreolus';
                c('Muntjac Deer', 'Muntjac')='Muntiacus reevesi';
                c('Pygmy shrew', 'pygmy shrew')='Sorex minutus';
                c('Pheasent')='Phasianus colchicus';
                c('Deer')='Cervidae sp.';
                c('Weasel')='Mustela nivalis';
                c('Lutra sp. (otter)')='Lutra lutra';                              
                c('harbour seal')='Phoca vitulina';
                c('Pine Marten')='Martes martes'; 
                c('Pipistrelle bat', 'Bat (unknown)', 'Bat')='Vespertilionidae sp.';
                c('Mous Sp', 'Mouse Sp', 'Mouse', 'mouse', 'Unidentified mouse')='Murinae sp.';
                c('Cattle')='Bos taurus';
                c('Brown Hare')='Lepus europaeus'
                ")
latin

# Create df, rename column for merge & convert species lists to lookup table
oppo.names <- data.frame(Species, latin)
oppo.names <- setDT(oppo.names)

# Merge and replace values

oppo_all_2017$Species <- trimws(oppo_all_2017$Species)
oppo_all_2017 <- (merge(oppo.names, oppo_all_2017, by = 'Species'))
as.character(unique(unlist(oppo_all_2017$latin))) # Check unique species

oppo_all_2017<- oppo_all_2017[-1,]
as.character(unique(unlist(oppo_all_2017$latin))) # Check unique species


# Standard walk

Species <- unique(unlist(lapply(stan.list, function(x) unique(x[,4])))) # Extract unique species
Species <- trimws(Species) #Remove whitespace
Species <- unique(Species) #Remove duplicates
latin <- as.character(Species)

latin <- recode(latin, "c('Roe Deer', 'roe deer', 'Roe deer', 'Roe deer (Capreolus capreolus)')='Capreolus capreolus';
                c('rabbit', 'Rabbit', 'Rabbit (Oryctolagus Cuniculus)', 'European Rabbit (Oryctolagus cuniculus')='Oryctolagus cuniculus';
                c('Cat', 'Cat (Felic catus)', 'Domestic Cat', 'Domestic cat', 'cat', 'domestic cat', 'Cat - domestic')='Felis catus';
                c('Mouse (not sure on species)', 'Mouse', 'mouse', 'mice')='Muridae sp.';
                c('Mouse (Mus musculus)', 'House mouse')='Mus musculus';
                c('grey squirrel', 'Grey Squirrel?', 'Grey Squirell', 'grey squirell', 'Grey Squirrel', 'Grey squirrel', 'Grey squirrel - drey', 'Western Grey Squirell (Sciurus griseus)', 'Grey Squrrel')='Scirius carolinensis';
                c('Squirrel', 'squiz')='Sciurus sp.';
                c('red squirrel', 'Red Squirrel', 'Red squirrel', 'Red squirrel (Sciurus vulgaris)')='Sciurus vulgaris';
                c('mole', 'Mole')='Talpa europaea';
                c('badger', 'Badger', 'Badger (Meles meles)')='Meles meles';
                c('harbour seal', 'Seal (Phoca vitulina)')='Phoca vitulina';
                c('fox', 'Fox', 'Red fox', 'red fox', 'Fox (Vulpes vulpes)', 'Red Fox')='Vulpes vulpes';
                c('otter')='Lutra lutra'
                ")
latin

# Create df, rename column for merge & convert species lists to lookup table
stan.names <- data.frame(Species, latin)
stan.names <- setDT(stan.names)

# Merge and replace values

stan_all_2017$Species <- trimws(stan_all_2017$Species)
stan_all_2017 <- (merge(stan.names, stan_all_2017, by = 'Species'))
as.character(unique(unlist(stan_all_2017$latin)))

stan_all_2017<-stan_all_2017[!(stan_all_2017$latin=="0"),]
stan_all_2017<-stan_all_2017[!(stan_all_2017$latin=="-"),]
as.character(unique(unlist(stan_all_2017$latin)))








# Clear all individual UMAC and names dataframes

rm(list = ls(pattern = "*UMAC"))
rm(list = ls(pattern = "*names"))

# Change working directory and export data

#setwd('../Cleaning')

#s.list <- mget(ls(pattern="*_2017"))
#mapply(write.csv, s.list, paste0(names(s.list), ".csv"))

# Group data by team - all observations and across species

bats_teams.tm <- bats_all_2017 %>% group_by(team) %>% summarize(Count = sum(Count))
bats_teams.sp <- bats_all_2017 %>% group_by(team, latin) %>% summarize(Count = sum(Count))

cams_teams.tm <- cams_all_2017 %>% group_by(team) %>% summarize(Count = sum(Count))
cams_teams.sp <- cams_all_2017 %>% group_by(team, latin) %>% summarize(Count = sum(Count))

field_all_2017$Count <- 1
field_teams.tm <- field_all_2017 %>% group_by(team) %>% summarize(Count = sum(Count))
field_teams.sp <- field_all_2017 %>% group_by(team, latin) %>% summarize(Count = sum(Count))

foot_all_2017$Count <- 1
foot_teams.tm <- foot_all_2017 %>% group_by(team) %>% summarize(Count = sum(Count))
foot_teams.sp <- foot_all_2017 %>% group_by(team, latin) %>% summarize(Count = sum(Count))

line_teams.tm <- line_all_2017 %>% group_by(team) %>% summarize(Count = sum(Count))
line_teams.sp <- line_all_2017 %>% group_by(team, latin) %>% summarize(Count = sum(Count))

live_all_2017$Count <- 1
live_teams.tm <- live_all_2017 %>% group_by(team) %>% summarize(Count = sum(Count))
live_teams.sp <- live_all_2017 %>% group_by(team, latin) %>% summarize(Count = sum(Count))

oppo_all_2017$Count <- as.numeric(oppo_all_2017$Count)
oppo_all_2017$Count[is.na(oppo_all_2017$Count)] <- 1
oppo_teams.tm <- oppo_all_2017 %>% group_by(team) %>% summarize(Count = sum(Count))
oppo_teams.sp <- oppo_all_2017 %>% group_by(team, latin) %>% summarize(Count = sum(Count))

stan_all_2017$Count <- as.numeric(stan_all_2017$Count)
stan_all_2017$Count[is.na(stan_all_2017$Count)] <- 1
stan_teams.tm <- stan_all_2017 %>% group_by(team) %>% summarize(Count = sum(Count))
stan_teams.sp <- stan_all_2017 %>% group_by(team, latin) %>% summarize(Count = sum(Count))

team.list <- mget(ls(pattern="*.tm"))
team_2017 <- merge_all(team.list)

team.totals <- aggregate(team_2017$Count, by=list(Category=team_2017$team), FUN=sum)
colnames(team.totals)[1] <- "team"
colnames(team.totals)[2] <- "total"

team.totals.bats <- merge(team.totals, bats_teams.tm, all=T)
team.totals.bats$bats <- NULL
team.totals.bats$Count[is.na(team.totals.bats$Count)] <- 0
team.totals.bats$no.bats <- team.totals.bats$total - team.totals.bats$Count



sp.list <- mget(ls(pattern="*.sp"))
species_2017 <- merge_all(sp.list)

species.totals <- aggregate(species_2017$Count, by=list(Category=species_2017$latin), FUN=sum)

