#setwd("G:/GDrive/Mammal Society/University Challenge 2017/Data/Splitting/")
setwd("C:/Users/Anthony caravaggi/Google Drive/Mammal Society/University Challenge 2017/Data/Splitting/")

library(reshape)
library(reshape2)
library(data.table)
library(dplyr)
library(tidyr)
library(car)

names.all <- read.csv(file="../names.all.csv")

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

habitat.list <- mget(ls(pattern="habitat*"))
habitat_all_2017 <- merge_all(habitat.list)
habitat_all_2017 <- habitat_all_2017  [!duplicated(habitat_all_2017 [c(3,5,9,10)]),]
#setwd('../Cleaning')
#write.csv(habitat_all_2017, file = "habitat_all_2017.csv")
#setwd('../Splitting')

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



# Clear all individual UMAC dataframes

rm(list = ls(pattern = "*UMAC"))


##arrange df vars by position
# from https://stackoverflow.com/questions/5620885/how-does-one-reorder-columns-in-a-data-frame
##'vars' must be a named vector, e.g. c("var.name"=1)
# e.g.
# table <- data.frame(Time=c(1,2), In=c(2,3), Out=c(3,4), Files=c(4,5))
# arrange.vars(table, c("Out"=2))
# arrange.vars(table, c("Out"=2, "Files"=1, "Time"=4))

arrange.vars <- function(data, vars){
  ##stop if not a data.frame (but should work for matrices as well)
  stopifnot(is.data.frame(data))
  
  ##sort out inputs
  data.nms <- names(data)
  var.nr <- length(data.nms)
  var.nms <- names(vars)
  var.pos <- vars
  ##sanity checks
  stopifnot( !any(duplicated(var.nms)), 
             !any(duplicated(var.pos)) )
  stopifnot( is.character(var.nms), 
             is.numeric(var.pos) )
  stopifnot( all(var.nms %in% data.nms) )
  stopifnot( all(var.pos > 0), 
             all(var.pos <= var.nr) )
  
  ##prepare output
  out.vec <- character(var.nr)
  out.vec[var.pos] <- var.nms
  out.vec[-var.pos] <- data.nms[ !(data.nms %in% var.nms) ]
  stopifnot( length(out.vec)==var.nr )
  
  ##re-arrange vars by position
  data <- data[ , out.vec]
  return(data)
}




####
# Species relabelling for analyses
# See name_recoding.R for full replacement script
####

# Bats

bats_all_2017$Species <- trimws(bats_all_2017$Species) # Trim whitespace
bats_all_2017 <- (merge(names.all, bats_all_2017, by = 'Species'))
bats_all_2017$Species <- NULL
colnames(bats_all_2017)[2] <- "Species"
bats_all_2017 <- arrange.vars(bats_all_2017, c("latin"=3, "Species"=2))
as.character(unique(unlist(bats_all_2017$latin))) # Check unique species

bats_all_2017<-bats_all_2017[!(bats_all_2017$latin=="Unknown"),]
as.character(unique(unlist(bats_all_2017$latin))) # Check unique species


# Camera traps

cams_all_2017$Species <- trimws(cams_all_2017$Species)
cams_all_2017 <- (merge(names.all, cams_all_2017, by = 'Species'))
cams_all_2017$Species <- NULL
colnames(cams_all_2017)[2] <- "Species"
cams_all_2017 <- arrange.vars(cams_all_2017, c("latin"=7, "Species"=8))
as.character(unique(unlist(cams_all_2017$latin))) # Check unique species

cams_all_2017<-cams_all_2017[!(cams_all_2017$latin=="Unknown"),]
as.character(unique(unlist(cams_all_2017$latin))) # Check unique species


# Field surveys

field_all_2017$Species <- trimws(field_all_2017$Species)
field_all_2017 <- (merge(names.all, field_all_2017, by = 'Species'))
field_all_2017$Species <- NULL
colnames(field_all_2017)[2] <- "Species"
field_all_2017 <- arrange.vars(field_all_2017, c("latin"=3, "Species"=4))
as.character(unique(unlist(field_all_2017$latin))) # Check unique species

field_all_2017<-field_all_2017[!(field_all_2017$latin=="Unknown"),]
as.character(unique(unlist(field_all_2017$latin))) # Check unique species


# Footprint tunnels

foot_all_2017$Species <- trimws(foot_all_2017$Species)
foot_all_2017 <- (merge(names.all, foot_all_2017, by = 'Species'))
foot_all_2017$Species <- NULL
colnames(foot_all_2017)[2] <- "Species"
foot_all_2017 <- arrange.vars(foot_all_2017, c("latin"=7, "Species"=8))
as.character(unique(unlist(foot_all_2017$latin))) # Check unique species


# Line transects

line_all_2017$Species <- trimws(line_all_2017$Species)
line_all_2017 <- (merge(names.all, line_all_2017, by = 'Species'))
line_all_2017$Species <- NULL
colnames(line_all_2017)[2] <- "Species"
line_all_2017 <- arrange.vars(line_all_2017, c("latin"=3, "Species"=4))
as.character(unique(unlist(line_all_2017$latin))) # Check unique species


# Live trapping

live_all_2017$Species <- trimws(live_all_2017$Species)
live_all_2017 <- (merge(names.all, live_all_2017, by = 'Species'))
live_all_2017$Species <- NULL
colnames(live_all_2017)[2] <- "Species"
live_all_2017 <- arrange.vars(live_all_2017, c("latin"=3, "Species"=4))
as.character(unique(unlist(live_all_2017$latin))) # Check unique species

live_all_2017<-live_all_2017[!(live_all_2017$latin=="Human site"),]
live_all_2017<-live_all_2017[!(live_all_2017$latin=="Woodland"),]
as.character(unique(unlist(live_all_2017$latin))) # Check unique species


# Opportunistic

oppo_all_2017$Species <- trimws(oppo_all_2017$Species)
oppo_all_2017 <- (merge(names.all, oppo_all_2017, by = 'Species'))
oppo_all_2017$Species <- NULL
colnames(oppo_all_2017)[2] <- "Species"
oppo_all_2017 <- arrange.vars(oppo_all_2017, c("latin"=4, "Species"=5))
as.character(unique(unlist(oppo_all_2017$latin))) # Check unique species

oppo_all_2017<- oppo_all_2017[-1,]
as.character(unique(unlist(oppo_all_2017$latin))) # Check unique species


# Standard walk

stan_all_2017$Species <- trimws(stan_all_2017$Species)
stan_all_2017 <- (merge(names.all, stan_all_2017, by = 'Species'))
stan_all_2017$Species <- NULL
colnames(stan_all_2017)[2] <- "Species"
stan_all_2017 <- arrange.vars(stan_all_2017, c("latin"=4, "Species"=5))
as.character(unique(unlist(stan_all_2017$latin)))

stan_all_2017<-stan_all_2017[!(stan_all_2017$latin=="0"),]
stan_all_2017<-stan_all_2017[!(stan_all_2017$latin=="-"),]
as.character(unique(unlist(stan_all_2017$latin)))


# Change working directory and export data

# setwd('../Cleaning')
# s.list <- mget(ls(pattern="*_2017"))
# mapply(write.csv, s.list, paste0(names(s.list), ".csv"))

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

# Remove bats and live trapping to ID second-placed team

team.totals.all <- merge(team.totals, bats_teams.tm, all=T)
colnames(team.totals.all)[3] <- "bats"
team.totals.all <- merge(team.totals.all, cams_teams.tm, all=T)
colnames(team.totals.all)[4] <- "cameras"
team.totals.all <- merge(team.totals.all, field_teams.tm, all=T)
colnames(team.totals.all)[5] <- "field"
team.totals.all <- merge(team.totals.all, foot_teams.tm, all=T)
colnames(team.totals.all)[6] <- "footprint"
team.totals.all <- merge(team.totals.all, line_teams.tm, all=T)
colnames(team.totals.all)[7] <- "line"
team.totals.all <- merge(team.totals.all, live_teams.tm, all=T)
colnames(team.totals.all)[8] <- "live"
team.totals.all <- merge(team.totals.all, oppo_teams.tm, all=T)
colnames(team.totals.all)[9] <- "opportunistic"
team.totals.all <- merge(team.totals.all, stan_teams.tm, all=T)
colnames(team.totals.all)[10] <- "standard"

team.totals.all[is.na(team.totals.all)] <- 0
team.totals.all$minus.blc <- team.totals.all$total - team.totals.all$bats - team.totals.all$live - team.totals.all$cameras
#team.totals.all <- team.totals.all[order(team.totals.all$minus),] 

write.csv(team.totals.all, file = "team.totals.all.csv")

sp.list <- mget(ls(pattern="*.sp"))
species_2017 <- merge_all(sp.list)

species.totals <- aggregate(species_2017$Count, by=list(Category=species_2017$latin), FUN=sum)

write.csv(team.totals, file = "team.totals.csv")
write.csv(team.totals.bats, file = "team.totals.bats.csv")
write.csv(species.totals, file = "species.totals.csv")
