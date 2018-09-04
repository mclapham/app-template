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
                 user = "postgres", password = "password")

#select sentences with target words
target <- "Ophiomorpha"

#prepare text string of multiple target words for searching
if (target %in% c("epifauna", "surficial", "biodiffuser", "conveyor", "regenerator")) {
  target_vector <- ichnofossils$taxon_name[ichnofossils$reworking_mode == target]
  target_string <- paste(target_vector, collapse="','")
  target_word <- paste("['", target_string, "']", sep="")
} else {target_word <- paste("['", target_string, "']", sep="")}

#If the words are stored as an array in the postgreSQL database
target_sent <- dbGetQuery(con, paste("SELECT docid, words, poses, dep_paths, dep_parents FROM sentences WHERE ARRAY", target_word, " && words", sep=""))

write.csv(target_sent, "target_sentences.csv")
