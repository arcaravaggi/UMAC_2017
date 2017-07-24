library(reshape)
library(reshape2)


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
latin[[17]] <- "Pipistrellus nathusii"
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
                c('grey squirrel', 'Grey Squirrel?', 'Grey Squirell', 'Grey Squirrel', 'Grey squirrel')='Sciurus carolinensis';
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
                c('grey squirrel', 'Grey Squirrel?', 'Grey Squirell', 'Grey Squirrel', 'Grey squirrel')='Sciurus carolinensis';
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
                           c('Mice', 'Mouse', 'mouse', 'mice')='Murinae sp.';
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
                c('grey squirrel', 'Grey Squirrel?', 'Grey Squirrel Sciurus carolinensis', 'Grey Squirell', 'Grey Squirrel', 'Grey squirrel', 'Grey squirrel (Sciurus carolinensis)')='Sciurus carolinensis';
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
                c('grey squirrel', 'Grey Squirrel?', 'Grey Squirell', 'Grey Squirrel', 'Grey squirrel', 'Grey squirrel - drey', 'Western Grey Squirell (Sciurus griseus)', 'Grey Squrrel')='Sciurus carolinensis';
                c('Vole', 'Vole sp.')='Arvicolinae sp.';
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
                c('Mouse (not sure on species)', 'Mouse', 'mouse', 'mice')='Murinae sp.';
                c('Mouse (Mus musculus)', 'House mouse')='Mus musculus';
                c('grey squirrel', 'Grey Squirrel?', 'Grey Squirell', 'grey squirell', 'Grey Squirrel', 'Grey squirrel', 'Grey squirrel - drey', 'Western Grey Squirell (Sciurus griseus)', 'Grey Squrrel')='Sciurus carolinensis';
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



name.list <- mget(ls(pattern="*names"))
names.all <- merge_all(name.list)
names.dup <- names.all[!duplicated(names.all$latin)]
names.dup <- names.dup[-c(27, 43, 45, 47, 52, 53), ]
common <-as.character(names.dup$latin)

common <- recode(common, "'Plecotus auritus'='Brown long-eared bat';
                 'Pipistrellus pipistrellus'='Common pipistrelle bat'; 
                 'Pipistrellus nathusii'='Nathusius pipistrelle bat';
                 'Pipistrellus sp.'='Pipistrelle bat (sp)';
                 'Myotis daubentonii'='Daubenton bat';     
                 'Myotis sp.'='Mouse-eared bat (sp)';
                 'Myotis daubentonii'='Daubenton bat';
                 'Nyctalus noctula'='Noctule bat';
                 'Pipistrellus pygmaeus'='Soprano pipistrelle bat';
                 'Meles meles'='Badger';
                 'Lepus europaeus'='European hare';
                 'Rattus norvegicus'='Brown rat';
                 'Felis catus'='Domestic cat';
                 'Sorex araneus'='Common shrew';
                 'Cervidae sp.'='Deer (sp)';
                 'Canis lupus familiaris'='Domestic dog';
                 'Mustela putorius'='Polecat';
                 'Vulpes vulpes'='Red fox';
                 'Sciurus carolinensis'='Grey squirrel';
                 'Erinaceus europaeus'='Hedgehog';
                 'Equus ferus caballus'='Horse';
                 'Murinae sp.'='Mouse (sp)';
                 'Muntiacus reevesi'='Muntjac';
                 'Martes martes'='Pine marten';
                 'Oryctolagus cuniculus'='Rabbit';
                 'Cervus elaphus'='Red deer';
                 'Sciurus vulgaris'='Red squirrel';
                 'Capreolus capreolus'='Roe deer';
                 'Sciurus sp.'='Squirrel (sp)';
                 'Mustela erminea'='Stoat';
                 'Mustela nivalis'='Weasel';
                 'Apodemus sylvaticus'='Wood mouse';
                 'Myodes glareolus'='Bank vole';
                 'Microtus agrestis'='Field vole';
                 'Talpa europaea'='Mole';
                 'Lutra lutra'='Otter';
                 'Arvicolinae sp.'='Vole (sp)';
                 'Vespertilionidae sp.'='Bat (sp)';
                 'Phasianus colchicus'='Pheasant';
                 'Ovis aries'='Sheep';
                 'Sorex minutus'='Pygmy shrew';
                 'Apodemus flavicollis'='Yellow-necked mouse';
                 'Bos taurus'='Cow';
                 'Equus asinus'='Donkey';
                 'Phoca vitulina'='Harbour seal';
                 'Mus musculus'='House mouse';
                 ")

common.names <- data.frame(names.dup$latin, common)
common.names <- setDT(common.names)
colnames(common.names)[1] <- "latin"

names.all <- (merge(names.all, common.names, by = 'latin'))

write.csv(names.all, file="names.all.csv", row.names=FALSE)
