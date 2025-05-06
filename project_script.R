##########################
### TEXT PREPROCESSING ###
##########################

# get directory
getwd()
setwd("C:/Users/maksy/Desktop/SEMESTER 5/Business Data Analytics/Project/reviews")
wd<-"C:/Users/maksy/Desktop/SEMESTER 5/Business Data Analytics/Project/reviews"

preprocessing <- function(file) {
  Data <- read.csv(file,
                   header = TRUE,
                   sep = ",", # or ";"
                   strip.white = TRUE,
                   fill = TRUE,
                   comment.char = "#",
                   stringsAsFactors = FALSE
  )
  
  # Transformation into Matrix
  MyData = as.data.frame.matrix(Data)
  
  MyData_1 <- MyData[, 2]  
  n <- length(MyData_1)
  n
  head(MyData_1)
  
  library(tm)
  corp <- VCorpus(x = VectorSource(MyData_1),
                  readerControl = list(reader = readPlain, language = "en"))
  
  corp <- tm_map(corp, PlainTextDocument)
  
  combined_text <- paste(sapply(corp, as.character), collapse = " ")
  #Split the text into words
  words <- strsplit(combined_text, "\\s+")[[1]]
  #Count the number of words
  word_count_before <- length(words)
  # Output the word count
  print(word_count_before)
  
  dtm <- DocumentTermMatrix(corp)
  
  # Convert DTM to matrix and then to data frame
  dtm_matrix <- as.matrix(dtm)
  dtm_df <- as.data.frame(dtm_matrix)
  
  # write.csv(dtm_df, file = "DTM.csv")
  
  # Text of Document 1
  # writeLines(as.character(corp[[1]]))
  
  # All comments length
  doc_length <- as.data.frame(rowSums(dtm_matrix))
  # write.csv(doc_length, file = "Doc_length.csv")
  
  # Min, max, average comments length
  max_length <- max(doc_length)
  max_length
  min_length <- min(doc_length)
  min_length
  aver_length <- mean(rowSums(dtm_matrix))
  aver_length
  
  # Pre-processing
  library(SnowballC)
  
  # Define a function to remove special characters
  remove_special_chars <- content_transformer(function(x) {
    gsub("[/@–’“”‘…()]", "", x)
  })
  
  # Preprocessing pipeline
  corp <- tm_map(corp, content_transformer(tolower))  # Convert to lowercase
  corp <- tm_map(corp, removePunctuation)              # Remove punctuation
  corp <- tm_map(corp, removeNumbers)                  # Remove numbers
  corp <- tm_map(corp, remove_special_chars)           # Remove special characters
  corp <- tm_map(corp, removeWords, stopwords("english"))  # Remove stopwords
  corp <- tm_map(corp, stripWhitespace)                # Remove extra whitespace
  corp <- tm_map(corp, stemDocument, language = "english")  # Apply stemming
  
  StW<-read.table("C:/Users/maksy/Desktop/SEMESTER 5/Business Data Analytics/Project/StopWords.txt") 
  StWW<-as.character(StW$V1) 
  corp <- tm_map(corp, removeWords, StWW)
  
  # Recreate Document-Term Matrix after preprocessing
  dtm <- DocumentTermMatrix(corp)
  dtm_matrix <- as.matrix(dtm)
  dtm_df <- as.data.frame(dtm_matrix)
  
  combined_text <- paste(sapply(corp, as.character), collapse = " ")
  #Split the text into words
  words <- strsplit(combined_text, "\\s+")[[1]]
  #Count the number of words
  word_count_after <- length(words)
  # Output the word count
  print(word_count_after)
  
  return(list(dtm = dtm_df, corp = corp))
}


bar_pod_ryba_results = preprocessing("bar pod ryba comments.csv")
billys_results = preprocessing("billys comments.csv")
eliksir_results = preprocessing("Eliksir comments.csv")
la_famiglia_results = preprocessing("La famiglia comments.csv")
whisky_in_the_jar_results = preprocessing("whisky in the jar comments.csv")

bar_pod_ryba_dtm = bar_pod_ryba_results$dtm
bar_pod_ryba_corp = bar_pod_ryba_results$corp

billys_dtm <- billys_results$dtm
billys_corp <- billys_results$corp

eliksir_dtm <- eliksir_results$dtm
eliksir_corp <- eliksir_results$corp

la_famiglia_dtm <- la_famiglia_results$dtm
la_famiglia_corp <- la_famiglia_results$corp

whisky_in_the_jar_dtm <- whisky_in_the_jar_results$dtm
whisky_in_the_jar_corp <- whisky_in_the_jar_results$corp

#######################
### TOPIC MODELLING ###
#######################


#####################################################################
### check the presence of rows with a zero's sum for bar pod ryba ###
#####################################################################
check_presence <- function(dtm){
  raw.sum=apply(dtm,1,FUN=sum) #sum by raw for each raw of the matrix
  raw.sum
  #number of rows with a zero's sum
  mmm<-nrow(dtm[raw.sum==0,])
  mmm
  # if mmm=0, only create new matrix dtm2 and NN (number of rows in DTM)
  # if mmm>0, delete the rows with zero's sum from dtm
  if (mmm==0) {
    dtm2<-dtm
    NN<-nrow(dtm)
    NN
  } else {
    dtm2<-dtm[raw.sum!=0,]
    NN<-nrow(dtm2)
  }
  #number of comments before deleting
  
  #number of comments after deleting
  NN
}

check_presence(billys_dtm)


library(topicmodels)

# Define the topic modeling function
perform_lda <- function(dtm, k, burnin = 4000, iter = 2000, thin = 500, seed = list(2003, 5, 63, 100001, 765), nstart = 5, best = TRUE) {
  
  # Perform LDA topic modeling
  ldaOut <- LDA(dtm, k, method = "Gibbs", control = list(nstart = nstart, seed = seed, best = best, burnin = burnin, iter = iter, thin = thin))
  
  # Extract the terms associated with each topic
  ldaOut_terms <- as.matrix(terms(ldaOut, 10))
  
  return(list(ldaOut = ldaOut, ldaOut_terms = ldaOut_terms))
}

# Now use the function with the DTM for each restaurant

bar_pod_ryba_lda <- perform_lda(bar_pod_ryba_dtm, k = 3)
billys_lda <- perform_lda(billys_dtm, k = 3)
eliksir_lda <- perform_lda(eliksir_dtm, k = 3)
la_famiglia_lda <- perform_lda(la_famiglia_dtm, k = 4)
whisky_in_the_jar_lda <- perform_lda(whisky_in_the_jar_dtm, k = 3)

# You can access the results like this:
bar_pod_ryba_lda$ldaOut_terms  # Terms for the bar pod ryba restaurant
billys_lda$ldaOut_terms  # Terms for Billy's restaurant
eliksir_lda$ldaOut_terms  # Terms for Eliksir restaurant
la_famiglia_lda$ldaOut_terms  # Terms for La Famiglia restaurant
whisky_in_the_jar_lda$ldaOut_terms  # Terms for Whisky in the Jar restaurant

###########################
### Topic probabilities ###
###########################

# Define the function to calculate topic probabilities and normalized sums
calculate_topic_probabilities <- function(ldaOut) {
  # Extract topic probabilities (gamma)
  topicProbabilities <- as.data.frame(ldaOut@gamma)
  
  # Calculate the column sums (sum for each topic)
  col_sum <- apply(topicProbabilities, 2, FUN = sum)  # Sum across columns (topics)
  
  # Convert column sums to a matrix
  col_sum <- as.matrix(col_sum)
  
  # Normalize the topic probabilities by dividing each column sum by the total sum
  sum_TP <- col_sum / sum(col_sum)
  print(sum_TP)
  
  return(list(topicProbabilities = topicProbabilities, normalizedTopicSums = sum_TP))
}

# Use the function with each restaurant's ldaOut object

whisky_in_the_jar_topic_probabilities <- calculate_topic_probabilities(whisky_in_the_jar_lda$ldaOut)
bar_pod_ryba_topic_probabilities <- calculate_topic_probabilities(bar_pod_ryba_lda$ldaOut)
billys_topic_probabilities <- calculate_topic_probabilities(billys_lda$ldaOut)
eliksir_topic_probabilities <- calculate_topic_probabilities(eliksir_lda$ldaOut)
la_famiglia_topic_probabilities <- calculate_topic_probabilities(la_famiglia_lda$ldaOut)

# You can access the results like this:
head(whisky_in_the_jar_topic_probabilities$topicProbabilities)  # Topic probabilities for Whisky in the Jar
whisky_in_the_jar_topic_probabilities$normalizedTopicSums  # Normalized topic sums for Whisky in the Jar

head(bar_pod_ryba_topic_probabilities$topicProbabilities)  # Topic probabilities for Bar Pod Ryba
bar_pod_ryba_topic_probabilities$normalizedTopicSums  # Normalized topic sums for Bar Pod Ryba

# Access the results for Billy's
head(billys_topic_probabilities$topicProbabilities)  # Topic probabilities for Billy's
billys_topic_probabilities$normalizedTopicSums  # Normalized topic sums for Billy's

# Access the results for Eliksir
head(eliksir_topic_probabilities$topicProbabilities)  # Topic probabilities for Eliksir
eliksir_topic_probabilities$normalizedTopicSums  # Normalized topic sums for Eliksir

# Access the results for La Famiglia
head(la_famiglia_topic_probabilities$topicProbabilities)  # Topic probabilities for La Famiglia
la_famiglia_topic_probabilities$normalizedTopicSums  # Normalized topic sums for La Famiglia

#############################
### Topics transformation ###
#############################

# Extract topics for each restaurant and save them as CSV files

# For Whisky in the Jar
whisky_in_the_jar_ldaOut_topics <- as.matrix(topics(whisky_in_the_jar_lda$ldaOut))
write.csv(whisky_in_the_jar_ldaOut_topics, file = paste("LDAGibbs_whisky_in_the_jar", 3, "DocsToTopics.csv"))

# For Bar Pod Ryba
bar_pod_ryba_ldaOut_topics <- as.matrix(topics(bar_pod_ryba_lda$ldaOut))
write.csv(bar_pod_ryba_ldaOut_topics, file = paste("LDAGibbs_bar_pod_ryba", 3, "DocsToTopics.csv"))

# For Billy's
billys_ldaOut_topics <- as.matrix(topics(billys_lda$ldaOut))
write.csv(billys_ldaOut_topics, file = paste("LDAGibbs_billys", 3, "DocsToTopics.csv"))

# For Eliksir
eliksir_ldaOut_topics <- as.matrix(topics(eliksir_lda$ldaOut))
write.csv(eliksir_ldaOut_topics, file = paste("LDAGibbs_eliksir", 3, "DocsToTopics.csv"))

# For La Famiglia
la_famiglia_ldaOut_topics <- as.matrix(topics(la_famiglia_lda$ldaOut))
write.csv(la_famiglia_ldaOut_topics, file = paste("LDAGibbs_la_famiglia", 4, "DocsToTopics.csv"))


##################################################################
### Build wf dataframe with Topics assigned to each of Comment ###
##################################################################

# Build wf dataframe with topics assigned to each comment
topic_to_comment <- function(ldaOut, corp, NN) {
  Comment <- seq(1, NN, by = 1)
  lda_topics <- as.matrix(topics(ldaOut))
  
  wf <- NULL
  for (i in 1:NN) {
    w <- data.frame(Comment = Comment[i], Topics = lda_topics[i], document = as.character(corp[[i]]))
    wf <- rbind(wf, w)
  }
  return(wf)
}

## Bar pod ryba ##
NN_bar_pod_ryba <- nrow(bar_pod_ryba_dtm)
bar_pod_ryba_wf <- topic_to_comment(bar_pod_ryba_lda$ldaOut, bar_pod_ryba_corp, NN_bar_pod_ryba)

# Save to CSV
write.csv(bar_pod_ryba_wf, file = "Comments_with_Topics_bar_pod_ryba.csv")

## Billys ##
NN_billys <- nrow(billys_dtm)
billys_wf <- topic_to_comment(billys_lda$ldaOut, billys_corp, NN_billys)

# Save to CSV
write.csv(billys_wf, file = "Comments_with_Topics_billys.csv")

## Eliksir ##
NN_eliksir <- nrow(eliksir_dtm)
eliksir_wf <- topic_to_comment(eliksir_lda$ldaOut, eliksir_corp, NN_eliksir)

# Save to CSV
write.csv(eliksir_wf, file = "Comments_with_Topics_eliksir.csv")

## La Famiglia ##
NN_la_famiglia <- nrow(la_famiglia_dtm)
la_famiglia_wf <- topic_to_comment(la_famiglia_lda$ldaOut, la_famiglia_corp, NN_la_famiglia)

# Save to CSV
write.csv(la_famiglia_wf, file = "Comments_with_Topics_la_famiglia.csv")

## Whisky in the jar ##
NN_whisky_in_the_jar <- nrow(whisky_in_the_jar_dtm)
whisky_in_the_jar_wf <- topic_to_comment(whisky_in_the_jar_lda$ldaOut, whisky_in_the_jar_corp, NN_whisky_in_the_jar)

# Save to CSV
write.csv(whisky_in_the_jar_wf, file = "Comments_with_Topics_whisky_in_the_jar.csv")


#########################################
### 2.1 CREATING TOPIC ORIENTED CORPA ###
#########################################


library(tm)
library(ggplot2)

# Function to build sub-corpus
building_sub_corpus <- function(wf, dtm, num_topics) {
  # Create a list to store results for each topic
  topics_list <- list()
  
  # Loop through each topic number
  for (topic_num in 1:num_topics) {
    # Filter the dataframe for the current topic
    topic_data <- wf[wf[2] == topic_num, ]
    
    # Number of comments with the current topic
    num_topic_comments <- nrow(topic_data)
    total_comments <- nrow(dtm)
    
    # Find the indices of comments belonging to the current topic
    topic_indices <- c()
    for (i in 1:total_comments) {
      if (wf[i, 2] == topic_num) {
        topic_indices <- c(topic_indices, i)
      }
    }
    
    # Store results for the current topic in the list
    topics_list[[topic_num]] <- list(
      topic_data = topic_data,
      topic_indices = topic_indices,
      num_topic_comments = num_topic_comments
    )
  }
  
  return(topics_list)
}

process_topic_data <- function(topics_list, dataset_name) {
  # Create a list to store the corpora for each topic
  corpora_list <- list()
  
  for (topic_num in 1:length(topics_list)) {
    topic_data <- topics_list[[topic_num]]$topic_data
    
    # Create a dataframe for the topic's comments
    topic_df <- data.frame(
      document = topic_data$document,
      text = topic_data$Comment,
      stringsAsFactors = FALSE
    )
    
    # Create a corpus from the topic's documents
    topic_corpus <- Corpus(VectorSource(as.character(topic_data$document)))
    corpora_list[[paste0("Topic_", topic_num)]] <- topic_corpus
    
    # Save the corpus content to a text file (sample)
    writeLines(as.character(topic_corpus[[1]]), 
               con = paste0(dataset_name, "_Topic_", topic_num, "_document_sample.txt"))
    
    # Save the dataframe to a CSV file
    filename <- paste0(dataset_name, "_Topic_", topic_num, "_docs.csv")
    write.csv(topic_df, filename, row.names = FALSE)
    
    cat(paste("Saved Topic", topic_num, "to", filename, "\n"))
    
    # Create a Document-Term Matrix (DTM)
    dtm_topic <- DocumentTermMatrix(topic_corpus)
    
    # Calculate word frequencies
    freqr <- colSums(as.matrix(dtm_topic))
    freq <- sort(freqr, decreasing = TRUE)
    mk <- min(head(freq, 30))
    
    # Save word frequencies to a dataframe
    wf1 <- data.frame(word = names(freq), freq = freq)
    
    # Plot Zipf's distribution
    plot_filename <- paste0(dataset_name, "_Topic_", topic_num, "_Zipf.png")
    p <- ggplot(subset(wf1, freq > mk), aes(x = reorder(word, -freq), y = freq)) +
      geom_bar(stat = "identity") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = paste("Zipf's Distribution for", dataset_name, "Topic", topic_num),
           x = "Words", y = "Frequency")
    
    # Save the plot 
    ggsave(plot_filename, plot = p, width = 12, height = 8, dpi = 300)
    cat(paste("Saved Zipf's distribution for Topic", topic_num, "to", plot_filename, "\n"))
    
    library(wordcloud)
     wordcloud(names(freq), freq, max.words = 100, scale = c(3, 0.5), random.order = FALSE, colors = brewer.pal(8, "Dark2"))
  }
  
  # Return the list of corpora for all topics
  return(corpora_list)
}

# Process Bar pod ryba dataset
bar_pod_ryba_topics_list <- building_sub_corpus(bar_pod_ryba_wf, bar_pod_ryba_dtm, 3)
process_topic_data(bar_pod_ryba_topics_list, "Bar_pod_ryba")

# Process Billy's dataset
billys_topics_list <- building_sub_corpus(billys_wf, billys_dtm, 3)
process_topic_data(billys_topics_list, "Billys")

# Process Eliksir dataset
eliksir_topics_list <- building_sub_corpus(eliksir_wf, eliksir_dtm, 3)
process_topic_data(eliksir_topics_list, "Eliksir")

# Process La Famiglia dataset
la_famiglia_topics_list <- building_sub_corpus(la_famiglia_wf, la_famiglia_dtm, 4)
process_topic_data(la_famiglia_topics_list, "La Famiglia")

# Process Whisky in the jar dataset
whisky_in_the_jar_topics_list <- building_sub_corpus(whisky_in_the_jar_wf, whisky_in_the_jar_dtm, 3)
process_topic_data(whisky_in_the_jar_topics_list, "Whisky in the jar")

bar_pod_ryba_corpora <- process_topic_data(bar_pod_ryba_topics_list, "Bar_pod_ryba")
billys_corpora <- process_topic_data(billys_topics_list, "Billys")
eliksir_corpora <- process_topic_data(eliksir_topics_list, "Eliksir")
la_famiglia_corpora <- process_topic_data(la_famiglia_topics_list, "La Famiglia")
whisky_in_the_jar_corpora <- process_topic_data(whisky_in_the_jar_topics_list, "Whisky in the jar")




##############################################################
### SENTIMENT ANALYSIS USING JEFF GENTRY’S TWITTER PACKAGE ###
##############################################################

library("plyr")
library("stringr")
library(syuzhet)
#_________________SENTIMENT_1: Jeff Gentry’s twitteR package____________________________

neg=scan("negative-words.txt", what="character", comment.char=";" )
pos=scan("positive-words.txt", what="character", comment.char=";" )

score.sentiment = function(docs, pos.words, neg.words, .progress='none')
{
  scores = laply(docs, function(docs, pos.words, neg.words) {
    
    word.list = str_split(docs, '\\s+')
    words = unlist(word.list)
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  # DATA MINING
  scores.df = data.frame(score=scores, text=docs)
  return(scores.df)
}

## Bar pod ryba ##
bar_pod_ryba_first_topic_corpus <- bar_pod_ryba_corpora[["Topic_1"]]
bar_pod_ryba_second_topic_corpus <- bar_pod_ryba_corpora[["Topic_2"]]
bar_pod_ryba_third_topic_corpus <- bar_pod_ryba_corpora[["Topic_3"]]

## Billys ##
billys_first_topic_corpus <- billys_corpora[["Topic_1"]]
billys_second_topic_corpus <- billys_corpora[["Topic_2"]]
billys_third_topic_corpus <- billys_corpora[["Topic_3"]]

## Eliksir ##
eliksir_first_topic_corpus <- eliksir_corpora[["Topic_1"]]
eliksir_second_topic_corpus <- eliksir_corpora[["Topic_2"]]
eliksir_third_topic_corpus <- eliksir_corpora[["Topic_3"]]

## La Famiglia ##
la_famiglia_first_topic_corpus <- la_famiglia_corpora[["Topic_1"]]
la_famiglia_second_topic_corpus <- la_famiglia_corpora[["Topic_2"]]
la_famiglia_third_topic_corpus <- la_famiglia_corpora[["Topic_3"]]
la_famiglia_fourth_topic_corpus <- la_famiglia_corpora[["Topic_4"]]

## Whisky in the jar ##
whisky_in_the_jar_first_topic_corpus <- whisky_in_the_jar_corpora[["Topic_1"]]
whisky_in_the_jar_second_topic_corpus <- whisky_in_the_jar_corpora[["Topic_2"]]
whisky_in_the_jar_third_topic_corpus <- whisky_in_the_jar_corpora[["Topic_3"]]



#################################
### Calculate Sentiment Score ###
#################################

calculate_sentiment_score <- function(topic_docs) {
  result=c()
  docs <- topic_docs
  m1=c()
  for (j in seq(docs)) {
    docs_s=as.character(docs[[j]])
    #print(docs_s)
    result = score.sentiment(docs_s, pos, neg)
    newRow1 <- data.frame(Doc=j,Score = result$score, Documents = result$text)
    #print(newRow1)
    m1<- rbind(m1,newRow1)
  #print(m1)
  }
  #m1[1:3,]
  return(m1)
}

## Bar pod ryba ##
bar_pod_ryba_m1 <- calculate_sentiment_score(bar_pod_ryba_first_topic_corpus)
bar_pod_ryba_m2 <- calculate_sentiment_score(bar_pod_ryba_second_topic_corpus)
bar_pod_ryba_m3 <- calculate_sentiment_score(bar_pod_ryba_third_topic_corpus)

## Billys ##
billys_m1 <- calculate_sentiment_score(billys_first_topic_corpus)
billys_m2 <- calculate_sentiment_score(billys_second_topic_corpus)
billys_m3 <- calculate_sentiment_score(billys_third_topic_corpus)

## Eliksir ##
eliksir_m1 <- calculate_sentiment_score(eliksir_first_topic_corpus)
eliksir_m2 <- calculate_sentiment_score(eliksir_second_topic_corpus)
eliksir_m3 <- calculate_sentiment_score(eliksir_third_topic_corpus)

## La Famiglia ##
la_famiglia_m1 <- calculate_sentiment_score(la_famiglia_first_topic_corpus)
la_famiglia_m2 <- calculate_sentiment_score(la_famiglia_second_topic_corpus)
la_famiglia_m3 <- calculate_sentiment_score(la_famiglia_third_topic_corpus)
la_famiglia_m4 <- calculate_sentiment_score(la_famiglia_fourth_topic_corpus)

## Whisky in the jar ##
whisky_in_the_jar_m1 <- calculate_sentiment_score(whisky_in_the_jar_first_topic_corpus)
whisky_in_the_jar_m2 <- calculate_sentiment_score(whisky_in_the_jar_second_topic_corpus)
whisky_in_the_jar_m3 <- calculate_sentiment_score(whisky_in_the_jar_third_topic_corpus)


###############################################
###  main statistics about Sentiment Score. ###
###############################################

calculate_sentiment_statistics <- function(sentiment_data) {
  # Check if the input data has the required column
  if (!"Score" %in% colnames(sentiment_data)) {
    stop("The input data must have a 'Score' column.")
  }
  
  # Main statistics
  summary_stats <- summary(sentiment_data$Score)
  minn <- min(sentiment_data$Score)
  maxx <- max(sentiment_data$Score)
  mmm <- maxx - minn
  
  # Create a list to store the statistics
  stats <- list(
    Summary = summary_stats,
    Min = minn,
    Max = maxx,
    Range = mmm
  )
  
  return(stats)
}


## Bar pod ryba ##
bar_pod_ryba_stats_m1 <- calculate_sentiment_statistics(bar_pod_ryba_m1)
bar_pod_ryba_stats_m2 <- calculate_sentiment_statistics(bar_pod_ryba_m2)
bar_pod_ryba_stats_m3 <- calculate_sentiment_statistics(bar_pod_ryba_m3)

# Print statistics
bar_pod_ryba_stats_m1
bar_pod_ryba_stats_m2
bar_pod_ryba_stats_m3


## Billys ##
billys_stats_m1 <- calculate_sentiment_statistics(billys_m1)
billys_stats_m2 <- calculate_sentiment_statistics(billys_m2)
billys_stats_m3 <- calculate_sentiment_statistics(billys_m3)

billys_stats_m1
billys_stats_m2
billys_stats_m3

## Eliksir ##
eliksir_stats_m1 <- calculate_sentiment_statistics(eliksir_m1)
eliksir_stats_m2 <- calculate_sentiment_statistics(eliksir_m2)
eliksir_stats_m3 <- calculate_sentiment_statistics(eliksir_m3)

eliksir_stats_m1
eliksir_stats_m2
eliksir_stats_m3

## La Famiglia ##
la_famiglia_stats_m1 <- calculate_sentiment_statistics(la_famiglia_m1)
la_famiglia_stats_m2 <- calculate_sentiment_statistics(la_famiglia_m2)
la_famiglia_stats_m3 <- calculate_sentiment_statistics(la_famiglia_m3)
la_famiglia_stats_m4 <- calculate_sentiment_statistics(la_famiglia_m4)

la_famiglia_stats_m1
la_famiglia_stats_m2
la_famiglia_stats_m3
la_famiglia_stats_m4

## Whisky in the jar ##
whisky_in_the_jar_stats_m1 <- calculate_sentiment_statistics(whisky_in_the_jar_m1)
whisky_in_the_jar_stats_m2 <- calculate_sentiment_statistics(whisky_in_the_jar_m2)
whisky_in_the_jar_stats_m3 <- calculate_sentiment_statistics(whisky_in_the_jar_m3)

whisky_in_the_jar_stats_m1
whisky_in_the_jar_stats_m2
whisky_in_the_jar_stats_m3

create_histogram_1 <- function(minn, maxx, mmm, topic_number, m1) {
  
  # Setting graphical parameters to scale down
  par(mar = c(5, 4, 4, 2) + 0.1) 
  par(cex = 0.8)                 
  
  main_title <- paste("Histogram for the Sentiment by Topic ", topic_number)
  
  h <- hist(m1$Score,
            main = main_title,
            xlab = "Scores",
            ylab = "Number of Opinions",
            right = FALSE,
            border = "blue",
            col = "green",
            freq = TRUE,
            las = 1,
            xlim = c(minn, maxx),
            breaks = mmm
  )
  
  # Adjust text size for counts
  text(h$mids, h$counts, labels = h$counts, adj = c(0.5, -0.5), cex = 0.7, pos = 1)
  
  m1$Score
  h$count
}

## Bar pod ryba ##
create_histogram_1(bar_pod_ryba_stats_m1$Min, bar_pod_ryba_stats_m1$Max, bar_pod_ryba_stats_m1$Range, 1, bar_pod_ryba_m1)
create_histogram_1(bar_pod_ryba_stats_m2$Min, bar_pod_ryba_stats_m1$Max, bar_pod_ryba_stats_m1$Range, 2, bar_pod_ryba_m2)
create_histogram_1(bar_pod_ryba_stats_m3$Min, bar_pod_ryba_stats_m1$Max, bar_pod_ryba_stats_m1$Range, 3, bar_pod_ryba_m3)

## Billys ##
create_histogram_1(billys_stats_m1$Min, billys_stats_m1$Max, billys_stats_m1$Range, 1, billys_m1)
create_histogram_1(billys_stats_m2$Min, billys_stats_m2$Max, billys_stats_m2$Range, 2, billys_m2)
create_histogram_1(billys_stats_m3$Min, billys_stats_m3$Max, billys_stats_m3$Range, 3, billys_m3)

## Eliksir ##
create_histogram_1(eliksir_stats_m1$Min, eliksir_stats_m1$Max, eliksir_stats_m1$Range, 1, eliksir_m1)
create_histogram_1(eliksir_stats_m2$Min, eliksir_stats_m2$Max, eliksir_stats_m2$Range, 2, eliksir_m2)
create_histogram_1(eliksir_stats_m3$Min, eliksir_stats_m3$Max, eliksir_stats_m3$Range, 3, eliksir_m3)

## La Famiglia ##
create_histogram_1(la_famiglia_stats_m1$Min, la_famiglia_stats_m1$Max, la_famiglia_stats_m1$Range, 1, la_famiglia_m1)
create_histogram_1(la_famiglia_stats_m2$Min, la_famiglia_stats_m2$Max, la_famiglia_stats_m2$Range, 2, la_famiglia_m2)
create_histogram_1(la_famiglia_stats_m3$Min, la_famiglia_stats_m3$Max, la_famiglia_stats_m3$Range, 3, la_famiglia_m3)
create_histogram_1(la_famiglia_stats_m4$Min, la_famiglia_stats_m4$Max, la_famiglia_stats_m4$Range, 4, la_famiglia_m4)

## Whisky in the jar ##
create_histogram_1(whisky_in_the_jar_stats_m1$Min, whisky_in_the_jar_stats_m1$Max, whisky_in_the_jar_stats_m1$Range, 1, whisky_in_the_jar_m1)
create_histogram_1(whisky_in_the_jar_stats_m2$Min, whisky_in_the_jar_stats_m2$Max, whisky_in_the_jar_stats_m2$Range, 2, whisky_in_the_jar_m2)
create_histogram_1(whisky_in_the_jar_stats_m3$Min, whisky_in_the_jar_stats_m3$Max, whisky_in_the_jar_stats_m3$Range, 3, whisky_in_the_jar_m3)


create_histogram_2 <- function(minn, maxx, mmm, topic_number, m1) {
  
  main_title <- paste("Histogram for the Sentiment by Topic ", topic_number)
  file_save_title <- paste("Sent_", topic_number, ".csv")
  hist(m1$Score,
       # DATA MINING
       main=main_title,
       xlab="Scores",
       ylab="Probability",
       border="blue",
       col="green",
       prob = TRUE,
       right=FALSE,
       xlim=c(minn,maxx),
       breaks=mmm
  )
  lines(density(m1$Score))
  m11<-as.matrix(m1)
  m11
  write.csv(m11, file=file_save_title)
}

## Bar pod ryba ##
create_histogram_2(bar_pod_ryba_stats_m1$Min, bar_pod_ryba_stats_m1$Max, bar_pod_ryba_stats_m1$Range, 1, bar_pod_ryba_m1)
create_histogram_2(bar_pod_ryba_stats_m2$Min, bar_pod_ryba_stats_m1$Max, bar_pod_ryba_stats_m1$Range, 2, bar_pod_ryba_m2)
create_histogram_2(bar_pod_ryba_stats_m3$Min, bar_pod_ryba_stats_m1$Max, bar_pod_ryba_stats_m1$Range, 3, bar_pod_ryba_m3)

## Billys ##
create_histogram_2(billys_stats_m1$Min, billys_stats_m1$Max, billys_stats_m1$Range, 1, billys_m1)
create_histogram_2(billys_stats_m2$Min, billys_stats_m2$Max, billys_stats_m2$Range, 2, billys_m2)
create_histogram_2(billys_stats_m3$Min, billys_stats_m3$Max, billys_stats_m3$Range, 3, billys_m3)

## Eliksir ##
create_histogram_2(eliksir_stats_m1$Min, eliksir_stats_m1$Max, eliksir_stats_m1$Range, 1, eliksir_m1)
create_histogram_2(eliksir_stats_m2$Min, eliksir_stats_m2$Max, eliksir_stats_m2$Range, 2, eliksir_m2)
create_histogram_2(eliksir_stats_m3$Min, eliksir_stats_m3$Max, eliksir_stats_m3$Range, 3, eliksir_m3)

## La Famiglia ##
create_histogram_2(la_famiglia_stats_m1$Min, la_famiglia_stats_m1$Max, la_famiglia_stats_m1$Range, 1, la_famiglia_m1)
create_histogram_2(la_famiglia_stats_m2$Min, la_famiglia_stats_m2$Max, la_famiglia_stats_m2$Range, 2, la_famiglia_m2)
create_histogram_2(la_famiglia_stats_m3$Min, la_famiglia_stats_m3$Max, la_famiglia_stats_m3$Range, 3, la_famiglia_m3)
create_histogram_2(la_famiglia_stats_m4$Min, la_famiglia_stats_m4$Max, la_famiglia_stats_m4$Range, 4, la_famiglia_m4)

## Whisky in the jar ##
create_histogram_2(whisky_in_the_jar_stats_m1$Min, whisky_in_the_jar_stats_m1$Max, whisky_in_the_jar_stats_m1$Range, 1, whisky_in_the_jar_m1)
create_histogram_2(whisky_in_the_jar_stats_m2$Min, whisky_in_the_jar_stats_m2$Max, whisky_in_the_jar_stats_m2$Range, 2, whisky_in_the_jar_m2)
create_histogram_2(whisky_in_the_jar_stats_m3$Min, whisky_in_the_jar_stats_m3$Max, whisky_in_the_jar_stats_m3$Range, 3, whisky_in_the_jar_m3)

#############################################################################################
### Divide Topic_1_docs corpus into 3 parts with Negative, Neutral and Positive Comments. ###
#############################################################################################

divide_topic_into_3_parts <- function(m1) {
  pos1<-m1[m1$Score>=1,]
  neu1<-m1[(m1$Score<1)&(m1$Score>=0),]
  neg1<-m1[m1$Score<0,]
  
  return(list(pos1 = pos1, neu1 = neu1, neg1 = neg1))
}

## Bar pod ryba ##
bar_pod_ryba_3_parts_m1 <- divide_topic_into_3_parts(bar_pod_ryba_m1)
bar_pod_ryba_3_parts_m2 <- divide_topic_into_3_parts(bar_pod_ryba_m2)
bar_pod_ryba_3_parts_m3 <- divide_topic_into_3_parts(bar_pod_ryba_m3)

pos1_data_m1 <- bar_pod_ryba_3_parts_m1$pos1
neu1_data_m1 <- bar_pod_ryba_3_parts_m1$neu1
neg1_data_m1 <- bar_pod_ryba_3_parts_m1$neg1

pos1_data_m2 <- bar_pod_ryba_3_parts_m2$pos1
neu1_data_m2 <- bar_pod_ryba_3_parts_m2$neu1
neg1_data_m2 <- bar_pod_ryba_3_parts_m2$neg1

pos1_data_m3 <- bar_pod_ryba_3_parts_m3$pos1
neu1_data_m3 <- bar_pod_ryba_3_parts_m3$neu1
neg1_data_m3 <- bar_pod_ryba_3_parts_m3$neg1

## Billys ##
billys_3_parts_m1 <- divide_topic_into_3_parts(billys_m1)
billys_3_parts_m2 <- divide_topic_into_3_parts(billys_m2)
billys_3_parts_m3 <- divide_topic_into_3_parts(billys_m3)

billys_pos1_data_m1 <- billys_3_parts_m1$pos1
billys_neu1_data_m1 <- billys_3_parts_m1$neu1
billys_neg1_data_m1 <- billys_3_parts_m1$neg1

billys_pos1_data_m2 <- billys_3_parts_m2$pos1
billys_neu1_data_m2 <- billys_3_parts_m2$neu1
billys_neg1_data_m2 <- billys_3_parts_m2$neg1

billys_pos1_data_m3 <- billys_3_parts_m3$pos1
billys_neu1_data_m3 <- billys_3_parts_m3$neu1
billys_neg1_data_m3 <- billys_3_parts_m3$neg1

## Eliksir ##
eliksir_3_parts_m1 <- divide_topic_into_3_parts(eliksir_m1)
eliksir_3_parts_m2 <- divide_topic_into_3_parts(eliksir_m2)
eliksir_3_parts_m3 <- divide_topic_into_3_parts(eliksir_m3)

eliksir_pos1_data_m1 <- eliksir_3_parts_m1$pos1
eliksir_neu1_data_m1 <- eliksir_3_parts_m1$neu1
eliksir_neg1_data_m1 <- eliksir_3_parts_m1$neg1

eliksir_pos1_data_m2 <- eliksir_3_parts_m2$pos1
eliksir_neu1_data_m2 <- eliksir_3_parts_m2$neu1
eliksir_neg1_data_m2 <- eliksir_3_parts_m2$neg1

eliksir_pos1_data_m3 <- eliksir_3_parts_m3$pos1
eliksir_neu1_data_m3 <- eliksir_3_parts_m3$neu1
eliksir_neg1_data_m3 <- eliksir_3_parts_m3$neg1

## La Famiglia ##
la_famiglia_3_parts_m1 <- divide_topic_into_3_parts(la_famiglia_m1)
la_famiglia_3_parts_m2 <- divide_topic_into_3_parts(la_famiglia_m2)
la_famiglia_3_parts_m3 <- divide_topic_into_3_parts(la_famiglia_m3)
la_famiglia_3_parts_m4 <- divide_topic_into_3_parts(la_famiglia_m4)

la_famiglia_pos1_data_m1 <- la_famiglia_3_parts_m1$pos1
la_famiglia_neu1_data_m1 <- la_famiglia_3_parts_m1$neu1
la_famiglia_neg1_data_m1 <- la_famiglia_3_parts_m1$neg1

la_famiglia_pos1_data_m2 <- la_famiglia_3_parts_m2$pos1
la_famiglia_neu1_data_m2 <- la_famiglia_3_parts_m2$neu1
la_famiglia_neg1_data_m2 <- la_famiglia_3_parts_m2$neg1

la_famiglia_pos1_data_m3 <- la_famiglia_3_parts_m3$pos1
la_famiglia_neu1_data_m3 <- la_famiglia_3_parts_m3$neu1
la_famiglia_neg1_data_m3 <- la_famiglia_3_parts_m3$neg1

la_famiglia_pos1_data_m4 <- la_famiglia_3_parts_m4$pos1
la_famiglia_neu1_data_m4 <- la_famiglia_3_parts_m4$neu1
la_famiglia_neg1_data_m4 <- la_famiglia_3_parts_m4$neg1

## Whisky in the jar ##
whisky_in_the_jar_3_parts_m1 <- divide_topic_into_3_parts(whisky_in_the_jar_m1)
whisky_in_the_jar_3_parts_m2 <- divide_topic_into_3_parts(whisky_in_the_jar_m2)
whisky_in_the_jar_3_parts_m3 <- divide_topic_into_3_parts(whisky_in_the_jar_m3)

whisky_in_the_jar_pos1_data_m1 <- whisky_in_the_jar_3_parts_m1$pos1
whisky_in_the_jar_neu1_data_m1 <- whisky_in_the_jar_3_parts_m1$neu1
whisky_in_the_jar_neg1_data_m1 <- whisky_in_the_jar_3_parts_m1$neg1

whisky_in_the_jar_pos1_data_m2 <- whisky_in_the_jar_3_parts_m2$pos1
whisky_in_the_jar_neu1_data_m2 <- whisky_in_the_jar_3_parts_m2$neu1
whisky_in_the_jar_neg1_data_m2 <- whisky_in_the_jar_3_parts_m2$neg1

whisky_in_the_jar_pos1_data_m3 <- whisky_in_the_jar_3_parts_m3$pos1
whisky_in_the_jar_neu1_data_m3 <- whisky_in_the_jar_3_parts_m3$neu1
whisky_in_the_jar_neg1_data_m3 <- whisky_in_the_jar_3_parts_m3$neg1

##############################################################################################
### Building three 3 Corpora – with Negative, Neutral and Positive Comments of the Topic 1 ###
##############################################################################################

## Bar pod ryba ##
pos_docs_1_m1 <- Corpus(VectorSource(pos1_data_m1$Documents))
pos_docs_1_m2 <- Corpus(VectorSource(pos1_data_m2$Documents))
pos_docs_1_m3 <- Corpus(VectorSource(pos1_data_m3$Documents))

neu_docs_1_m1 <- Corpus(VectorSource(neu1_data_m1$Documents))
neu_docs_1_m2 <- Corpus(VectorSource(neu1_data_m2$Documents))
neu_docs_1_m3 <- Corpus(VectorSource(neu1_data_m3$Documents))

neg_docs_1_m1 <- Corpus(VectorSource(neg1_data_m1$Documents))
neg_docs_1_m2 <- Corpus(VectorSource(neg1_data_m2$Documents))
neg_docs_1_m3 <- Corpus(VectorSource(neg1_data_m3$Documents))

## Billys ##
billys_pos_docs_1_m1 <- Corpus(VectorSource(billys_pos1_data_m1$Documents))
billys_pos_docs_1_m2 <- Corpus(VectorSource(billys_pos1_data_m2$Documents))
billys_pos_docs_1_m3 <- Corpus(VectorSource(billys_pos1_data_m3$Documents))

billys_neu_docs_1_m1 <- Corpus(VectorSource(billys_neu1_data_m1$Documents))
billys_neu_docs_1_m2 <- Corpus(VectorSource(billys_neu1_data_m2$Documents))
billys_neu_docs_1_m3 <- Corpus(VectorSource(billys_neu1_data_m3$Documents))

billys_neg_docs_1_m1 <- Corpus(VectorSource(billys_neg1_data_m1$Documents))
billys_neg_docs_1_m2 <- Corpus(VectorSource(billys_neg1_data_m2$Documents))
billys_neg_docs_1_m3 <- Corpus(VectorSource(billys_neg1_data_m3$Documents))

## Eliksir ##
eliksir_pos_docs_1_m1 <- Corpus(VectorSource(eliksir_pos1_data_m1$Documents))
eliksir_pos_docs_1_m2 <- Corpus(VectorSource(eliksir_pos1_data_m2$Documents))
eliksir_pos_docs_1_m3 <- Corpus(VectorSource(eliksir_pos1_data_m3$Documents))

eliksir_neu_docs_1_m1 <- Corpus(VectorSource(eliksir_neu1_data_m1$Documents))
eliksir_neu_docs_1_m2 <- Corpus(VectorSource(eliksir_neu1_data_m2$Documents))
eliksir_neu_docs_1_m3 <- Corpus(VectorSource(eliksir_neu1_data_m3$Documents))

eliksir_neg_docs_1_m1 <- Corpus(VectorSource(eliksir_neg1_data_m1$Documents))
eliksir_neg_docs_1_m2 <- Corpus(VectorSource(eliksir_neg1_data_m2$Documents))
eliksir_neg_docs_1_m3 <- Corpus(VectorSource(eliksir_neg1_data_m3$Documents))

## La Famiglia ##
la_famiglia_pos_docs_1_m1 <- Corpus(VectorSource(la_famiglia_pos1_data_m1$Documents))
la_famiglia_pos_docs_1_m2 <- Corpus(VectorSource(la_famiglia_pos1_data_m2$Documents))
la_famiglia_pos_docs_1_m3 <- Corpus(VectorSource(la_famiglia_pos1_data_m3$Documents))
la_famiglia_pos_docs_1_m4 <- Corpus(VectorSource(la_famiglia_pos1_data_m4$Documents))

la_famiglia_neu_docs_1_m1 <- Corpus(VectorSource(la_famiglia_neu1_data_m1$Documents))
la_famiglia_neu_docs_1_m2 <- Corpus(VectorSource(la_famiglia_neu1_data_m2$Documents))
la_famiglia_neu_docs_1_m3 <- Corpus(VectorSource(la_famiglia_neu1_data_m3$Documents))
la_famiglia_neu_docs_1_m4 <- Corpus(VectorSource(la_famiglia_neu1_data_m4$Documents))

la_famiglia_neg_docs_1_m1 <- Corpus(VectorSource(la_famiglia_neg1_data_m1$Documents))
la_famiglia_neg_docs_1_m2 <- Corpus(VectorSource(la_famiglia_neg1_data_m2$Documents))
la_famiglia_neg_docs_1_m3 <- Corpus(VectorSource(la_famiglia_neg1_data_m3$Documents))
la_famiglia_neg_docs_1_m4 <- Corpus(VectorSource(la_famiglia_neg1_data_m4$Documents))

## Whisky in the jar ##
whisky_in_the_jar_pos_docs_1_m1 <- Corpus(VectorSource(whisky_in_the_jar_pos1_data_m1$Documents))
whisky_in_the_jar_pos_docs_1_m2 <- Corpus(VectorSource(whisky_in_the_jar_pos1_data_m2$Documents))
whisky_in_the_jar_pos_docs_1_m3 <- Corpus(VectorSource(whisky_in_the_jar_pos1_data_m3$Documents))

whisky_in_the_jar_neu_docs_1_m1 <- Corpus(VectorSource(whisky_in_the_jar_neu1_data_m1$Documents))
whisky_in_the_jar_neu_docs_1_m2 <- Corpus(VectorSource(whisky_in_the_jar_neu1_data_m2$Documents))
whisky_in_the_jar_neu_docs_1_m3 <- Corpus(VectorSource(whisky_in_the_jar_neu1_data_m3$Documents))

whisky_in_the_jar_neg_docs_1_m1 <- Corpus(VectorSource(whisky_in_the_jar_neg1_data_m1$Documents))
whisky_in_the_jar_neg_docs_1_m2 <- Corpus(VectorSource(whisky_in_the_jar_neg1_data_m2$Documents))
whisky_in_the_jar_neg_docs_1_m3 <- Corpus(VectorSource(whisky_in_the_jar_neg1_data_m3$Documents))


####################################################################################################################################
### Build Zipf's distribution and Wordclouds for each of the created sub-corpus (positive, negative and neutral) for each topic. ###
####################################################################################################################################

# Load necessary libraries
library(ggplot2)
library(tm)           
library(wordcloud)    
library(RColorBrewer) 

# Function to plot Zipf's distribution and generate a word cloud
create_zipf_and_wordcloud <- function(corpus, dataset_name, topic_num, mk = 0, dataset) {
  
  # Clean the text data
  corpus_clean <- tm_map(corpus, content_transformer(tolower))   # Convert to lower case
  corpus_clean <- tm_map(corpus_clean, removePunctuation)        # Remove punctuation
  corpus_clean <- tm_map(corpus_clean, removeNumbers)            # Remove numbers
  corpus_clean <- tm_map(corpus_clean, removeWords, stopwords("en")) # Remove stop words
  corpus_clean <- tm_map(corpus_clean, stripWhitespace)          # Remove extra white spaces
  
  # Create a term-document matrix
  tdm <- TermDocumentMatrix(corpus_clean)
  m <- as.matrix(tdm)
  word_freqs <- sort(rowSums(m), decreasing = TRUE)
  word_freq_table <- data.frame(word = names(word_freqs), freq = word_freqs)
  
  # Filter words by frequency (optional, based on 'mk')
  word_freq_table <- subset(word_freq_table, freq > mk)
  
  # Plot Zipf's distribution (word frequencies)
  plot_filename <- paste0(dataset,"_",dataset_name, "_Topic_", topic_num, "_Zipf.png")
  p <- ggplot(word_freq_table, aes(x = reorder(word, -freq), y = freq)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = paste("Zipf's Distribution for", dataset_name, "Topic", topic_num),
         x = "Words", y = "Frequency")
  
  # Save the Zipf's distribution plot
  ggsave(plot_filename, plot = p, width = 12, height = 8, dpi = 300)
  cat(paste(dataset, " Saved Zipf's distribution for Topic", topic_num, "to", plot_filename, "\n"))
  
  # Generate a colorful word cloud
  wordcloud(words = word_freq_table$word, 
            freq = word_freq_table$freq, 
            min.freq = 1, 
            max.words = 100, 
            scale = c(3, 0.5), 
            colors = brewer.pal(8, "Dark2"),
            random.order = FALSE, 
            rot.per = 0.25)
}

## Bar pod ryba ##
create_zipf_and_wordcloud(pos_docs_1_m1, "Positive topic 1", 1, mk = 30, "Bar pod ryba")
create_zipf_and_wordcloud(pos_docs_1_m2, "Positive topic 2", 2, mk = 30, "Bar pod ryba")
create_zipf_and_wordcloud(pos_docs_1_m3, "Positive topic 3", 3, mk = 20, "Bar pod ryba")

create_zipf_and_wordcloud(neu_docs_1_m1, "Neutral topic 1", 1, mk = 3, "Bar pod ryba")
create_zipf_and_wordcloud(neu_docs_1_m2, "Neutral topic 2", 2, mk = 5, "Bar pod ryba")
create_zipf_and_wordcloud(neu_docs_1_m3, "Neutral topic 3", 3, mk = 3, "Bar pod ryba")

create_zipf_and_wordcloud(neg_docs_1_m1, "Negative topic 1", 1, mk = 3, "Bar pod ryba")
create_zipf_and_wordcloud(neg_docs_1_m2, "Negative topic 2", 2, mk = 5, "Bar pod ryba")
create_zipf_and_wordcloud(neg_docs_1_m3, "Negative topic 3", 3, mk = 1, "Bar pod ryba")

## Billys ##
create_zipf_and_wordcloud(billys_pos_docs_1_m1, "Positive topic 1", 1, mk = 30, "Billys")
create_zipf_and_wordcloud(billys_pos_docs_1_m2, "Positive topic 2", 2, mk = 30, "Billys")
create_zipf_and_wordcloud(billys_pos_docs_1_m3, "Positive topic 3", 3, mk = 30, "Billys")

create_zipf_and_wordcloud(billys_neu_docs_1_m1, "Neutral topic 1", 1, mk = 1, "Billys")
create_zipf_and_wordcloud(billys_neu_docs_1_m2, "Neutral topic 2", 2, mk = 1, "Billys")
create_zipf_and_wordcloud(billys_neu_docs_1_m3, "Neutral topic 3", 3, mk = 0, "Billys")

create_zipf_and_wordcloud(billys_neg_docs_1_m1, "Negative topic 1", 1, mk = 1, "Billys")
create_zipf_and_wordcloud(billys_neg_docs_1_m2, "Negative topic 2", 2, mk = 2, "Billys")
create_zipf_and_wordcloud(billys_neg_docs_1_m3, "Negative topic 3", 3, mk = 0, "Billys")

## Eliksir ##
create_zipf_and_wordcloud(eliksir_pos_docs_1_m1, "Positive topic 1", 1, mk = 30, "Eliksir")
create_zipf_and_wordcloud(eliksir_pos_docs_1_m2, "Positive topic 2", 2, mk = 30, "Eliksir")
create_zipf_and_wordcloud(eliksir_pos_docs_1_m3, "Positive topic 3", 3, mk = 30, "Eliksir")

create_zipf_and_wordcloud(eliksir_neu_docs_1_m1, "Neutral topic 1", 1, mk = 1, "Eliksir")
create_zipf_and_wordcloud(eliksir_neu_docs_1_m2, "Neutral topic 2", 2, mk = 1, "Eliksir")
create_zipf_and_wordcloud(eliksir_neu_docs_1_m3, "Neutral topic 3", 3, mk = 1, "Eliksir")

create_zipf_and_wordcloud(eliksir_neg_docs_1_m1, "Negative topic 1", 1, mk = 0, "Eliksir")
create_zipf_and_wordcloud(eliksir_neg_docs_1_m2, "Negative topic 2", 2, mk = 0, "Eliksir")
create_zipf_and_wordcloud(eliksir_neg_docs_1_m3, "Negative topic 3", 3, mk = 1, "Eliksir")

## La Famiglia ##
create_zipf_and_wordcloud(la_famiglia_pos_docs_1_m1, "Positive topic 1", 1, mk = 25, "La Famiglia")
create_zipf_and_wordcloud(la_famiglia_pos_docs_1_m2, "Positive topic 2", 2, mk = 25, "La Famiglia")
create_zipf_and_wordcloud(la_famiglia_pos_docs_1_m3, "Positive topic 3", 3, mk = 10, "La Famiglia")
create_zipf_and_wordcloud(la_famiglia_pos_docs_1_m4, "Positive topic 4", 4, mk = 10, "La Famiglia")

create_zipf_and_wordcloud(la_famiglia_neu_docs_1_m1, "Neutral topic 1", 1, mk = 1, "La Famiglia")
create_zipf_and_wordcloud(la_famiglia_neu_docs_1_m2, "Neutral topic 2", 2, mk = 1, "La Famiglia")
create_zipf_and_wordcloud(la_famiglia_neu_docs_1_m3, "Neutral topic 3", 3, mk = 1, "La Famiglia")
create_zipf_and_wordcloud(la_famiglia_neu_docs_1_m4, "Neutral topic 4", 4, mk = 1, "La Famiglia")

create_zipf_and_wordcloud(la_famiglia_neg_docs_1_m1, "Negative topic 1", 1, mk = 0, "La Famiglia")
create_zipf_and_wordcloud(la_famiglia_neg_docs_1_m2, "Negative topic 2", 2, mk = 1, "La Famiglia")
create_zipf_and_wordcloud(la_famiglia_neg_docs_1_m3, "Negative topic 3", 3, mk = 1, "La Famiglia")
create_zipf_and_wordcloud(la_famiglia_neg_docs_1_m4, "Negative topic 4", 4, mk = 1, "La Famiglia")

## Whisky in the jar ##
create_zipf_and_wordcloud(whisky_in_the_jar_pos_docs_1_m1, "Positive topic 1", 1, mk = 30, "Whisky in the jar")
create_zipf_and_wordcloud(whisky_in_the_jar_pos_docs_1_m2, "Positive topic 2", 2, mk = 30, "Whisky in the jar")
create_zipf_and_wordcloud(whisky_in_the_jar_pos_docs_1_m3, "Positive topic 3", 3, mk = 30, "Whisky in the jar")

create_zipf_and_wordcloud(whisky_in_the_jar_neu_docs_1_m1, "Neutral topic 1", 1, mk = 0, "Whisky in the jar")
create_zipf_and_wordcloud(whisky_in_the_jar_neu_docs_1_m2, "Neutral topic 2", 2, mk = 1, "Whisky in the jar")
create_zipf_and_wordcloud(whisky_in_the_jar_neu_docs_1_m3, "Neutral topic 3", 3, mk = 5, "Whisky in the jar")

create_zipf_and_wordcloud(whisky_in_the_jar_neg_docs_1_m1, "Negative topic 1", 1, mk = 0, "Whisky in the jar")
create_zipf_and_wordcloud(whisky_in_the_jar_neg_docs_1_m2, "Negative topic 2", 2, mk = 0, "Whisky in the jar")
create_zipf_and_wordcloud(whisky_in_the_jar_neg_docs_1_m3, "Negative topic 3", 3, mk = 3, "Whisky in the jar")

#####################################################################
### SENTIMENT ANALYSIS USING NRC WORD-EMOTION ASSOCIATION LEXICON ###
#####################################################################

#____SENTIMENT_2: NRC Word-Emotion Association Lexicon __________________________________

# Function to read CSV, process it into a corpus, and return the dataframe
process_topic_corpus <- function(file_path) {
  
  # Read the CSV file
  topic_data <- read.csv(file_path,
                         header = TRUE,
                         sep = ",", # or ";" depending on your data
                         strip.white = TRUE,
                         fill = TRUE,
                         comment.char = "#",
                         stringsAsFactors = FALSE)
  
  print(head(topic_data))
  
  # Convert the data frame into a matrix format
  topic_data_matrix <- as.data.frame.matrix(topic_data)
  
  # Create a corpus dataframe
  corpus_dataframe <- data.frame(text = topic_data_matrix, stringsAsFactors = FALSE)
  
  # Return the corpus dataframe
  return(corpus_dataframe)
}


## Bar pod ryba ##
processed_corpus_bar_pod_ryba_topic_1 <- process_topic_corpus("Bar pod ryba Topic_1_docs.csv")
processed_corpus_bar_pod_ryba_topic_2 <- process_topic_corpus("Bar pod ryba Topic_2_docs.csv")
processed_corpus_bar_pod_ryba_topic_3 <- process_topic_corpus("Bar pod ryba Topic_3_docs.csv")

## Billys ##
processed_corpus_billys_topic_1 <- process_topic_corpus("Billys Topic_1_docs.csv")
processed_corpus_billys_topic_2 <- process_topic_corpus("Billys Topic_2_docs.csv")
processed_corpus_billys_topic_3 <- process_topic_corpus("Billys Topic_3_docs.csv")

## Eliksir ##
processed_corpus_eliksir_topic_1 <- process_topic_corpus("Eliksir_Topic_1_docs.csv")
processed_corpus_eliksir_topic_2 <- process_topic_corpus("Eliksir_Topic_2_docs.csv")
processed_corpus_eliksir_topic_3 <- process_topic_corpus("Eliksir_Topic_3_docs.csv")

## La Famiglia ##
processed_corpus_la_famiglia_topic_1 <- process_topic_corpus("La Famiglia_Topic_1_docs.csv")
processed_corpus_la_famiglia_topic_2 <- process_topic_corpus("La Famiglia_Topic_2_docs.csv")
processed_corpus_la_famiglia_topic_3 <- process_topic_corpus("La Famiglia_Topic_3_docs.csv")
processed_corpus_la_famiglia_topic_4 <- process_topic_corpus("La Famiglia_Topic_4_docs.csv")

## Whisky in the jar ##
processed_corpus_whisky_in_the_jar_topic_1 <- process_topic_corpus("Whisky in the jar_Topic_1_docs.csv")
processed_corpus_whisky_in_the_jar_topic_2 <- process_topic_corpus("Whisky in the jar_Topic_2_docs.csv")
processed_corpus_whisky_in_the_jar_topic_3 <- process_topic_corpus("Whisky in the jar_Topic_3_docs.csv")

sentimental_analysis_nrc_word_emotion <- function(mycorpus_dataframe1, topic_number, dataset) {
  chart_title <- paste("Opinion sentiments for Topic ", topic_number)
  
  # Remove all non-graphical characters 
  usableText <- str_replace_all(mycorpus_dataframe1$text.document, "[^[:graph:]]", " ")
  
  # Get NRC sentiment analysis results
  d <- get_nrc_sentiment(usableText)
  
  # Check if 'd' is valid
  if (is.null(d) || nrow(d) == 0) {
    stop("No sentiment data returned from NRC sentiment analysis.")
  }
  print(head(d))
  
  # Transpose the sentiment dataframe
  td <- data.frame(t(d))
  
  # Check if we have enough rows in 'td' to proceed
  if (nrow(td) == 0) {
    stop("Transposed sentiment data is empty.")
  }
  
  # Sum the sentiment counts
  td_new <- data.frame(rowSums(td))
  names(td_new)[1] <- "count"
  
  # Add sentiment labels as rownames
  td_new <- cbind("sentiment" = rownames(td_new), td_new)
  rownames(td_new) <- NULL
  
  # View top 10 sentiments
  td_new2 <- td_new[1:10, ]
  print(td_new2)
  
  # Create a bar plot of the top 10 sentiments
  sentiment_plot <- qplot(sentiment, data = td_new2, weight = count, geom = "bar", fill = sentiment) +
    ggtitle(chart_title)
  
  # Define the filename for the plot
  plot_filename <- paste("Sentiment_analysis_topic", topic_number, "_", dataset, ".png")
  
  # Save the plot to the file
  ggsave(plot_filename, plot = sentiment_plot, width = 12, height = 8, dpi = 300)
  
  cat("Sentiment analysis chart saved to", plot_filename, "\n")
}

## Bar pod ryba ##
sentimental_analysis_nrc_word_emotion(processed_corpus_bar_pod_ryba_topic_1, 1, "bar_pod_ryba")
sentimental_analysis_nrc_word_emotion(processed_corpus_bar_pod_ryba_topic_2, 2, "bar_pod_ryba")
sentimental_analysis_nrc_word_emotion(processed_corpus_bar_pod_ryba_topic_3, 3, "bar_pod_ryba")

## Billys ##
sentimental_analysis_nrc_word_emotion(processed_corpus_billys_topic_1, 1, "billys")
sentimental_analysis_nrc_word_emotion(processed_corpus_billys_topic_2, 2, "billys")
sentimental_analysis_nrc_word_emotion(processed_corpus_billys_topic_3, 3, "billys")

## Eliksir ##
sentimental_analysis_nrc_word_emotion(processed_corpus_eliksir_topic_1, 1, "eliksir")
sentimental_analysis_nrc_word_emotion(processed_corpus_eliksir_topic_2, 2, "eliksir")
sentimental_analysis_nrc_word_emotion(processed_corpus_eliksir_topic_3, 3, "eliksir")

## La Famiglia ##
sentimental_analysis_nrc_word_emotion(processed_corpus_la_famiglia_topic_1, 1, "la_famiglia")
sentimental_analysis_nrc_word_emotion(processed_corpus_la_famiglia_topic_2, 2, "la_famiglia")
sentimental_analysis_nrc_word_emotion(processed_corpus_la_famiglia_topic_3, 3, "la_famiglia")
sentimental_analysis_nrc_word_emotion(processed_corpus_la_famiglia_topic_4, 4, "la_famiglia")

## Whisky in the jar ##
sentimental_analysis_nrc_word_emotion(processed_corpus_whisky_in_the_jar_topic_1, 1, "whisky_in_the_jar")
sentimental_analysis_nrc_word_emotion(processed_corpus_whisky_in_the_jar_topic_2, 2, "whisky_in_the_jar")
sentimental_analysis_nrc_word_emotion(processed_corpus_whisky_in_the_jar_topic_3, 3, "whisky_in_the_jar")
