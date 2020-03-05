library(RPostgreSQL)

#read target trace fossils
sentences_to_check <- read.csv("https://docs.google.com/spreadsheets/d/10rPMGNvITCI9_ws3B9kXlwKvCF9QT0YgTUKUCDM2lik/export?format=csv")

#create a PostGres connection
#loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")

#creates a connection to the postgres database
#note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "ichnofossils",
                 host = "localhost", port = 5432,
                 user = "postgres", password = "password")

#select documents with target words
docs <- unique(sentences_to_check$docid)

#create empty data frame to store sentence strings
sentence_text <- data.frame(docid = character(0),
                            sentid = numeric(0),
                            words = character(0))

#loop through documents
for (i in 1:length(docs)) {
  
  #read document
  target_sent <- dbGetQuery(con, paste("SELECT docid, sentid, words FROM sentences WHERE docid='", docs[i], "'", sep=""))
  
  #find target sentences numbers to capture
  check_sentences <- sentences_to_check$sentid[sentences_to_check$docid == docs[i]]
  
  #heinous function to select sentence and three previous ones for each target sentence
  sentence_text_new <- sapply(check_sentences, function(x) paste(target_sent[target_sent$sentid %in% seq(x, x+3), "words"], collapse=""))
  
  #combine new results with previous ones
  sentence_text_df <- data.frame(docid = docs[i],
                              sentid = check_sentences,
                              words = sentence_text_new)
  
  sentence_text <- rbind(sentence_text, sentence_text_df)
}


write.csv(sentence_text, "check_sentences.csv")
