library(stringr)
library(dplyr)
library(RPostgreSQL)

#read target trace fossils
ichnofossils <- read.csv("https://docs.google.com/spreadsheets/d/1LpZIVmxdmE_5yDBZPcGn-Av7C-Xj98uESEf4tfTOadE/export?format=csv")

#create a PostGres connection
#loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")

#creates a connection to the postgres database
#note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "ichnofossils",
                 host = "localhost", port = 5432,
                 user = "UNAME", password = "password123")

#select sentences with target words
target <- "Ophiomorpha"

if (target %in% c("epifauna", "surficial", "biodiffuser", "conveyor", "regenerator")) {
  target_vector <- ichnofossils$taxon_name[ichnofossils$reworking_mode == target]
  target_string <- paste(target_vector, collapse="|")
} else {target_string <- target}

target_word <- paste("%(", target_string, ")%", sep="")

target_sent <- dbGetQuery(con, paste("SELECT * FROM sentences_nlp352 WHERE words SIMILAR TO", target_word))

#read rock units from Macrostrat
rock_units <- read.csv("https://macrostrat.org/api/units?all&envclass=marine&format=csv&response=long")

rock_units$Fm <- gsub(" Conglomerate", "", rock_units$Fm)
rock_units$Fm <- gsub(" Quartzite", "", rock_units$Fm)
rock_units$Fm <- gsub(" Sandstone", "", rock_units$Fm)
rock_units$Fm <- gsub(" Siltstone", "", rock_units$Fm)
rock_units$Fm <- gsub(" Shale", "", rock_units$Fm)
rock_units$Fm <- gsub(" Dolomite", "", rock_units$Fm)
rock_units$Fm <- gsub(" Dolostone", "", rock_units$Fm)
rock_units$Fm <- gsub(" Limestone", "", rock_units$Fm)

#read time interval names
time_intervals <- read.csv("https://paleobiodb.org/data1.2/intervals/list.txt?all_records")

#functions to extract stratigraphic units
formation.extract <- function(sentence){
  
  #replace commas in words
  testwords <- gsub("\"", "", as.character(sentence[[4]]))
  testwords1 <- gsub("\\,\\,\\,", "\\,comma\\,", testwords)
  testwords1 <- gsub("\\,\\,", "\\,", testwords1)
  #remove opening brace in words
  testwords1 <- gsub("\\{", "", testwords1)
  
  #replace commas in pos
  testpos <- gsub("\"", "", as.character(sentence[[5]]))
  testpos1 <- gsub("\\,\\,\\,", "\\,comma\\,", testpos)
  testpos1 <- gsub("\\,\\,", "\\,", testpos1)
  #remove opening brace in pos
  testpos1 <- gsub("\\{", "", testpos1)
  
  #remove opening brace in parent
  testparent <- gsub("\\{", "", as.character(sentence[[9]]))
  
  #replace quotes in relation
  testrelation <- gsub("\"", "", as.character(sentence[[8]]))
  #remove opening brace in relation
  testrelation1 <- gsub("\\{", "", testrelation)
  
  #split each into vector
  words <- strsplit(testwords1, ",")
  pos <- strsplit(testpos1, ",")
  parent <- strsplit(testparent, ",")
  relation <- strsplit(testrelation1, ",")
  
  #create data frame 
  try({
    parsed_sentence <- data.frame(words, pos, parent, relation)
  colnames(parsed_sentence) <- c("words", "pos", "parent", "relation")
  
  #extract the formation names in these sentences
  type_no <- which(parsed_sentence$words %in% c("Formation", "Conglomerate", "Sandstone", "Limestone", "Quartzite", "Shale", "Siltstone", "Dolomite", "Dolostone"))
  #  target_no <- which(parsed_sentence$words %in% c("Planolites", "Areniculites", "Thalassinoides", "Skolithos", "Diplocration"))
  formation_vector <- parsed_sentence$words[parsed_sentence$pos %in% c("NNP", "NNPS") & parsed_sentence$parent %in% type_no & parsed_sentence$relation == "compound"]
  
  #make the formation vector into a string to run through macrostrat
  paste(unique(formation_vector), collapse=" ")
  })
  
}

member.extract <- function(sentence) {
  
  #replace commas in words
  testwords <- gsub("\"", "", as.character(sentence[[4]]))
  testwords1 <- gsub("\\,\\,\\,", "\\,comma\\,", testwords)
  testwords1 <- gsub("\\,\\,", "\\,", testwords1)
  #remove opening brace in words
  testwords1 <- gsub("\\{", "", testwords1)
  
  #replace commas in pos
  testpos <- gsub("\"", "", as.character(sentence[[5]]))
  testpos1 <- gsub("\\,\\,\\,", "\\,comma\\,", testpos)
  testpos1 <- gsub("\\,\\,", "\\,", testpos1)
  #remove opening brace in pos
  testpos1 <- gsub("\\{", "", testpos1)
  
  #remove opening brace in parent
  testparent <- gsub("\\{", "", as.character(sentence[[9]]))
  
  #replace quotes in relation
  testrelation <- gsub("\"", "", as.character(sentence[[8]]))
  #remove opening brace in relation
  testrelation1 <- gsub("\\{", "", testrelation)
  
  #split each into vector
  words <- strsplit(testwords1, ",")
  pos <- strsplit(testpos1, ",")
  parent <- strsplit(testparent, ",")
  relation <- strsplit(testrelation1, ",")
  
  #create data frame 
  try({
    parsed_sentence <- data.frame(words, pos, parent, relation)
  colnames(parsed_sentence) <- c("words", "pos", "parent", "relation")
  
  #extract the formation names in these sentences
  type_no <- which(parsed_sentence$words == "Member")
  #  target_no <- which(parsed_sentence$words %in% c("Planolites", "Areniculites", "Thalassinoides", "Skolithos", "Diplocration"))
  formation_vector <- parsed_sentence$words[parsed_sentence$pos %in% c("NNP", "NNPS") & parsed_sentence$parent %in% type_no & parsed_sentence$relation == "compound"]
  
  #make the formation vector into a string to run through macrostrat
  paste(unique(formation_vector), collapse=" ")
  })
  
}


#Main code to match trace fossils to units
  #clean up bad sentences
  target_sent <- filter(target_sent, pos != "" & parent != "" & relation != "")
  if (length(grep("[A-Za-z]", target_sent$parent)) > 0) {
    target_sent <- target_sent[-grep("[A-Za-z]", target_sent$parent),]
  }
  target_sent <- target_sent[grep("^\\{", target_sent$pos),]
  if (length(grep("^\\{`", target_sent$pos)) > 0) {
    target_sent <- target_sent[-grep("^\\{`", target_sent$pos),]
  }
  
  #apply the function to the sentences containing the trace fossil
  target_formations <- apply(target_sent, 1, formation.extract)
  target_members <- apply(target_sent, 1, member.extract)
  
  #remove time interval names from extracted target units
  target_formations_time_removed <- str_replace_all(as.character(target_formations), paste(time_intervals$interval_name, collapse=" |"), "")
  target_members_time_removed <- str_replace_all(as.character(target_members), paste(time_intervals$interval_name, collapse=" |"), "")
  
  #remove blanks from output
  target_formations_no_blank <- target_formations_time_removed[target_formations_time_removed != ""]
  target_members_no_blank <- target_members_time_removed[target_members_time_removed != ""]
  
  #make data frames
  if (length(target_formations_no_blank) > 0) {
    formations <- data.frame(target_formations_no_blank, "YES")
    colnames(formations) <- c("unit_name", "trace_fossil")
    
    #create table from Macrostrat    
    rock_summary1 <- rock_units %>% 
      group_by(Fm) %>% 
      summarize(t_age = mean(t_age), b_age=mean(b_age))
    colnames(rock_summary1) <- c("unit_name", "top_age", "bottom_age")
    
    #combine the dataframes
    output_formations <- formations %>% 
      select(unit_name, trace_fossil) %>% 
      right_join(rock_summary1, by = c("unit_name" = "unit_name"))
    
    output_formations$trace_fossil <- ifelse(is.na(output_formations$trace_fossil) == T, "NO", "YES")
    
  } else {
    #create table from Macrostrat    
    rock_summary1 <- rock_units %>% 
      group_by(Fm) %>% 
      summarize(t_age = mean(t_age), b_age=mean(b_age))
    colnames(rock_summary1) <- c("unit_name", "top_age", "bottom_age")
    
    output_formations <- data.frame(rock_summary1, "trace_fossil" = "NO")
  }
  
  if (length(target_members_no_blank) > 0) {
    members <- data.frame(target_members_no_blank, "YES")
    colnames(members) <- c("unit_name", "trace_fossil")  
    
    #create table from Macrostrat   
    rock_summary2 <- rock_units %>% 
      group_by(Mbr) %>% 
      summarize(t_age = mean(t_age), b_age=mean(b_age))
    colnames(rock_summary2) <- c("unit_name", "top_age", "bottom_age")
    
    #combine the dataframes
    output_members <- members %>% 
      select(unit_name, trace_fossil) %>% 
      right_join(rock_summary2, by = c("unit_name" = "unit_name"))
    
    output_members$trace_fossil <- ifelse(is.na(output_members$trace_fossil) == T, "NO", "YES")
    
  } else {
    rock_summary2 <- rock_units %>% 
      group_by(Mbr) %>% 
      summarize(t_age = mean(t_age), b_age=mean(b_age))
    colnames(rock_summary2) <- c("unit_name", "top_age", "bottom_age")
    output_members <- data.frame(rock_summary2, "trace_fossil" = "NO")
  }
  
  total_outputs <- rbind(output_formations, output_members)

  final_output <- total_outputs %>% 
    select(unit_name, top_age, bottom_age, trace_fossil) %>% 
    arrange(unit_name, desc(trace_fossil)) %>% 
    distinct(unit_name, .keep_all = TRUE)

