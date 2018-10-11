library(stringr)
library(tidyr)
library(dplyr)
library(RPostgreSQL)

#read target trace fossils
ichnofossils <- read.csv("https://docs.google.com/spreadsheets/d/1LpZIVmxdmE_5yDBZPcGn-Av7C-Xj98uESEf4tfTOadE/export?format=csv")
#read time interval names
time_intervals <- read.csv("https://paleobiodb.org/data1.2/intervals/list.txt?all_records")

#create a PostGres connection
#loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")

#creates a connection to the postgres database
#note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "ichnofossils",
                 host = "localhost", port = 5432,
                 user = "postgres", password = "password")

#create empty table to store strat results; if table exists, drop and reinitialize
dbExecute(con, "DROP TABLE if EXISTS stratresults")
dbExecute(con, "CREATE TABLE stratresults (docid TEXT, sentid INTEGER, strat_term TEXT, match_type TEXT, target_term TEXT)")

#select sentences with target words
target <- "regenerator"

#prepare text string of multiple target words for searching
if (target %in% c("epifauna", "surficial", "biodiffuser", "conveyor", "regenerator")) {
  target_vector <- ichnofossils$taxon_name[ichnofossils$reworking_mode == target]
  target_names <- paste(target_vector, collapse="|") #for grep use within R
  target_string <- paste(target_vector, collapse="','")
  target_names_postgres <- paste("['", target_string, "']", sep="") #for Postgresql queries
} else {
  target_names <- target #for grep use within R
  target_names_postgres <- paste("['", target, "']", sep="") #for Postgresql
}

#select document id from sentences file
doc_ids <- dbGetQuery(con, paste("SELECT DISTINCT docid FROM sentences WHERE ARRAY", target_names_postgres, " && words", sep=""))

#loop through all documents
for (i in 1:nrow(doc_ids)) {
  #select sentences from document
  doc_sentences <- dbGetQuery(con, paste("SELECT docid, sentid, words, poses, dep_paths, dep_parents FROM sentences WHERE docid = \'", doc_ids[i,], "\'", sep=""))
  
  #identify sentences containing stratigraphic flags
  strat_sentences <- grep("Formation|Member|Conglomerate|Sandstone|Limestone|Quartzite|Shale|Siltstone|Dolomite|Dolostone", doc_sentences$words)
  
  #if at least one sentence contains a stratigraphic flag
  if (length(strat_sentences) > 0)
  {
    
    #CLEAN UP DATA FILE
    #replace true commas in words results with word "comma"
    doc_sentences$words <- gsub(",\",\",\",\",", ",comma,comma,", doc_sentences$words) #even number of commas in a row
    doc_sentences$words <- gsub(",\",\",", ",comma,", doc_sentences$words) #single comma or odd number of commas in a row
    #replace commas that start the sentence
    doc_sentences$words <- gsub("\\{\",\",", "comma,", doc_sentences$words)
    #remove extra quote
    doc_sentences$words <- gsub(",\",", ",", doc_sentences$words)

    #replace true commas in poses results with word "comma"
    doc_sentences$poses <- gsub(",\",\",\",\",", ",comma,comma,", doc_sentences$poses) #even number of commas in a row
    doc_sentences$poses <- gsub(",\",\",", ",comma,", doc_sentences$poses) #single comma or odd number of commas in a rowa
    #replace commas that start the sentence
    doc_sentences$poses <- gsub("\\{\",\",", "comma,", doc_sentences$poses)
    
    #clean up weird number strings encased in quotes (typically distances, elevation, map scales)
    doc_sentences$words <- gsub("\"([0-9][^\"]*)\\,([^\"]*[0-9]?)\"", "numbers",  doc_sentences$words)
    
    #END DATA FILE CLEANING
    
    #find sentences with stratigraphic unit term
    fm_sentences <- doc_sentences[strat_sentences,]
    
    fm_dataframe <- data.frame(sentid = numeric(0),
                               words = character(0), 
                               poses = character(0),
                               dep_paths = character(0),
                               dep_parents = numeric(0))
    
    for (j in 1:nrow(fm_sentences)) {
      #split each into vector
      words <- strsplit(fm_sentences$words[j], ",")
      poses <- strsplit(fm_sentences$poses[j], ",")
      dep_paths <- strsplit(fm_sentences$dep_paths[j], ",")
      dep_parents <- strsplit(fm_sentences$dep_parents[j], ",")
      
      #create data frame 
      try({
        single_sentence <- data.frame(sentid=fm_sentences$sentid[j], 
                                      words=words[[1]],
                                      poses=poses[[1]],
                                      dep_paths=dep_paths[[1]],
                                      dep_parents=dep_parents[[1]])
        
        fm_dataframe <- rbind(fm_dataframe, single_sentence)
      })
      
    }

    #function to extract proper names associated with stratigraphic terms, as string
    strat.name.extract <- function(parsed_sentence) {
      type_no <- which(parsed_sentence$words %in% c("Formation", "Member", "Conglomerate", "Sandstone", "Limestone", "Quartzite", "Shale", "Siltstone", "Dolomite", "Dolostone"))
      formation_vector <- parsed_sentence$words[parsed_sentence$poses %in% c("NNP", "NNPS") & parsed_sentence$dep_parents %in% type_no & parsed_sentence$dep_paths == "compound"]
      paste(formation_vector, collapse = " ")
    }
    
    #applies stratigraphic name function to each sentence
    fm_results <- fm_dataframe %>% 
      group_by(sentid) %>% 
      do(fm_string = strat.name.extract(.)) %>% 
      unnest() %>% 
      filter(fm_string != "")
    
    #remove time interval names 
    fm_results$fm_string <- str_replace_all(as.character(fm_results$fm_string), paste(time_intervals$interval_name, collapse=" |"), "")
    
    #select sentences with target words
    target <- "regenerator"
    
    #prepare text string of multiple target words for searching
    if (target %in% c("epifauna", "surficial", "biodiffuser", "conveyor", "regenerator")) {
      target_vector <- ichnofossils$taxon_name[ichnofossils$reworking_mode == target]
      target_names <- paste(target_vector, collapse="|")
    } else {target_names <- target}
    
    
    #finds sentences containing target trace fossil
    target_sentences <- doc_sentences[grep(target_names, doc_sentences$words),]
    
    #if there are target sentences, proceed
    if(nrow(target_sentences) > 0) {
      #remove commas to create string
      target_sentences$words <- gsub(",", " ", target_sentences$words)
      
      #1. sentences that contain both target trace fossil name and strat name with strat term
      exact_matches <- fm_results[fm_results$sentid %in% target_sentences$sentid,]
      
      #add match type term
      exact_matches <- data.frame(exact_matches, match_type=rep("exact", nrow(exact_matches)))
      
      #remove exact match sentences
      target_sentences_noexact <- target_sentences[!target_sentences$sentid %in% exact_matches$sentid,]
      
      #2. search for formation names that appear in sentence without stratigraphic flag
      if (nrow(fm_results) > 0) {
        partial_match_strings <- str_extract(target_sentences_noexact$words, paste(fm_results$fm_string, collapse="|"))
        partial_matches <- data.frame(sentid = target_sentences_noexact$sentid[is.na(partial_match_strings)==F],
                                      fm_string = na.omit(partial_match_strings),
                                      match_type = rep("partial", length(na.omit(partial_match_strings))))
        
        #removes partial match sentences
        target_sentences_nopartial <- target_sentences_noexact[is.na(partial_match_strings)==T,]
      } else {
        partial_matches <- data.frame(sentid = numeric(0),
                                      fm_string = character(0),
                                      match_type = character(0))
      }
      
      
      #3. find stratigraphic names in nearby sentences
      #3A. previous sentence
      previous_matches <- fm_results[fm_results$sentid %in% (target_sentences_nopartial$sentid-1),]
      
      #add match type term
      previous_matches <- data.frame(previous_matches, match_type=rep("previous", nrow(previous_matches)))
      
      #remove any hits before checking more distant sentences
      target_sentences_noprevious <- target_sentences_nopartial[!target_sentences_nopartial$sentid %in% (previous_matches$sentid+1),]
      
      #3B. two sentences previous
      second_matches <- fm_results[fm_results$sentid %in% (target_sentences_noprevious$sentid-2),]
      
      #add match type term
      second_matches <- data.frame(second_matches, match_type=rep("second", nrow(second_matches)))
      
      #remove any hits before checking more distant sentences
      target_sentences_nosecond <- target_sentences_noprevious[!target_sentences_noprevious$sentid %in% (second_matches$sentid+2),]
      
      #3C. three sentences previous
      third_matches <- fm_results[fm_results$sentid %in% (target_sentences_nosecond$sentid-3),]
      
      #add match type term
      third_matches <- data.frame(third_matches, match_type=rep("second", nrow(third_matches)))
      
      #remove any hits before checking more distant sentences
      target_sentences_nothird <- target_sentences_nosecond[!target_sentences_nosecond$sentid %in% (third_matches$sentid+3),]
      
      #4. if a formation is superdominant in the paper, assume that the trace fossils come from it
      fm_counts <- table(fm_results$fm_string)
      fm_prop <- fm_counts/sum(fm_counts)
      
      #these are arbitrary choices (formation name in at least 20 sentences, and at least 60% of mentions)
      if (length(which(fm_counts > 20 & fm_prop > 0.6)) > 0)
      {
        dominance_matches <- data.frame(sentid = NA,
                                        fm_string = names(which(fm_counts > 20 & fm_prop > 0.6)),
                                        match_type = "dominance")
        
        fm_final <- rbind(exact_matches, partial_matches, previous_matches, second_matches, third_matches, dominance_matches)
      } else {
        fm_final <- rbind(exact_matches, partial_matches, previous_matches, second_matches, third_matches)
      }
      
      fm_final <- distinct(fm_final, sentid, .keep_all = TRUE) 
      
      if (nrow(fm_final) > 0) {
        #prepare results table for Postgres
        strat_results <- data.frame(docid = doc_ids[i,],
                                    sentid = fm_final$sentid,
                                    strat_term = fm_final$fm_string,
                                    match_type = fm_final$match_type,
                                    target_term = target)
        #Write to Postgres
        dbWriteTable(con, "stratresults", strat_results, append = TRUE, row.names = FALSE)
      }
      
    }

  }

}

#Read final results back in from Postgres
final_results <- dbGetQuery(con, "SELECT * FROM stratresults")

write.csv(final_results, "regenerators.csv", row.names=F)
