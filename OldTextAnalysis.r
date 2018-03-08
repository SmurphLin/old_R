options(warn = -1)
print("Ttest 1")
print("Ttest 2")
print("Ttest 3")



# removed directory,filename frim function call
Sentiment_Analysis <- function(){
  library(RODBC)                                                    # open the RODBC package
  library(reshape2)
  # run encryption on unsername password database server
  dbhandle <- odbcDriverConnect('driver={SQL Server};server=XXX;database=XXX;uid=XXX;pwd=XXX',case="nochange")
  # read in the data from the database                                              
  review_data <- sqlQuery(dbhandle,'select  CaseNumber,Brand__c,Complaint_Type__c,Category__c,Case_Description__c from DailyFormulationAndPackagingComplaints
                                    where SentimentScore is NULL')  
  
  # tmp <- sqlColumns(dbhandle,"SentimentScore")
  
  # add back the leading 0's in the case number
  for(i in 1:length(review_data$Brand__c)){
    if(nchar(review_data$CaseNumber[i]) == 7){
      review_data$CaseNumber[i] <- paste0("0",review_data$CaseNumber[i])
    }
    if(nchar(review_data$CaseNumber[i] )== 6){
      review_data$CaseNumber[i] <- paste0("00",review_data$CaseNumber[i])
    }
  }
  
  
  
  
  reviews <- as.vector(review_data$Case_Description__c)             # set the reviews as its own data set
  
  
  complaint <- c(0)
  length(complaint) <- length(review_data$Brand__c)                 # set the length of the complaint data
  
  
  
  complaint_type <- review_data$Complaint_Type__c                   # complaint type subset
  complaint_category <- review_data$Category__c                     # complaint category subset
  
  
  
  # NOTE: Both corpus lists should be continually updated as new reviews are ingested
  # the larger the corpus, the more accurate the sentiment score will be 
  
  # corpus of negative words 
  neg_words <- c("acid","ache","hard","odor","smell","bad","expired","smells","cloudy","harmful","horrible","never","old","aweful",
                 "break","burned","cant","cartilage","difficult","dissappointed","dull","dumped","fermented",
                 "fishy","globby","hates","hard","headache","headaches","humiliate","ill","irregular","irregularly",
                 "issues","losing","lumps","metallic","missing","mold","musty","obscene","odd","oily","opaque",
                 "problems","quality","refund","repulsive","return","returned","ridges","rubbery","skunky",
                 "smelled","spill","spilled","stale","stick","sticking","sticky","stop","stopped","stuck","sugary",
                 "suspicious","tainted","terrible","tiredness","tough","unconfortable","urine","warning","wasnt",
                 "worry", "like","doesn't","oily","acidic","addicts","adverse","afraid","aftertaste","avoid","barrier",
                 "bitter","bland","blister","bulging","burning","burps","causing","chalk","chalky","chemicals",
                 "choked","clammy","clumped","clumpy","complain","complaints","concerned","cracked","cramps",
                 "deceptive","devastated","didnt","dislike","disoriented","disrupts","dissatisfied","dont","dreadful",
                 "dried","dull","expire","expired","garbage","horrible","ineffective","issues","jaggad","kill","old",
                 "rotten","rotting","rough","salty","sand","sick","size","smell","smells","stool","stop","struggling",
                 "stuck","swallow","swelling","tolerate","unhappy","unpleasant","unsatisfied","unusable","upset",
                 "waste","wasteful","wierd","worst","don't")
  
  
  # corpus of positive words
  pos_words <- c("amazing","appealing","best","better","beyond","good","great","happy","heal","healthy","helpful",
                 "helping","like","likes","liking","love","loved","loves","loving","recommend","recommended",
                 "thank","thanks","wonderful","affordable","appetizing","appreciated")
  
  # corpus of keywords
  # corpus currently inactive
  #key_words <- c()                                                  # placeholder for key words corpus
  
  
  the_review_list <- list(NA)                                       # create an empty list that will store each word per comment
  length(the_review_list) <- length(reviews)                        # set the length of the_review_list to the length of the file
  
  # updated(5/26/2016)
  consumerComplaints <- tolower(gsub(pattern = "[[:punct:]]",
                                     review_data$Case_Description__c,
                                     replacement = " "))                    # remove all punctuation from the complaints data
  # converts all comments to lower case
  
  

  for(i in 1:length(reviews)){
    the_review_list[[i]] <- unlist(strsplit(consumerComplaints[i]," "))   # break up the reviews into a list of lists 
  }

  # the_review_list <- the_review_list[!is.na(the_review_list)]
 
  complaint_type <- complaint_type[!is.na(complaint_type)]
  complaint_type <- as.list(complaint_type)
  comment_list <- list(0)                                           # create an empty comment list 
  length(comment_list) <- length(the_review_list)                   # set the length of the list to the length of the_review_list
  
  for(i in 1:length(comment_list)){                                 # set all values of the comment list to 0
    comment_list[[i]] <- 0
  }
  
  positive_comment_list <- list(0)                                  # create an empty positive comment list
  length(positive_comment_list) <- length(the_review_list)          # set the length of the positive comment list to the length of the_review_list
  
  for(i in 1:length(positive_comment_list)){
    positive_comment_list[[i]] <- 0                                 # set all values of the positive comment list to 0
  } 
  
  combined_comment_list <- list(0)                                  # set up a list that contains the sum of the positive & negative scores
  length(combined_comment_list) <- length(the_review_list)          # set the length of combined_comment_list to review list
  
  for(i in 1:length(combined_comment_list)){
    combined_comment_list[[i]] <- 0                                 # set all values of the combined comment list to 0
  }
  
  
  
  # neg/pos word count algorithm
  for(i in 1:length(the_review_list)){                              # this algorithm looks through the list of negative words
    the_list <- unlist(the_review_list[i])                          # it than compares the negative words to each word in each comment
    the_count <- comment_list[[i]]                                  # if there is a match in the negative words corpus in the comment
    pos_the_count <- positive_comment_list[[i]]
    for(j in 1:length(the_list)){                                   # the count for that comment is increased by 1.  The value is then
      for(k in 1:length(neg_words)){                                # stored in the comment list in its position to the comment
        if(the_list[j] == neg_words[k] & !is.na(the_list[j])){                            # the greater the number of negative words per comment
          the_count <- the_count - 1                                # the higher the probability of it being a negative comment
          comment_list[[i]] <- the_count
        }
      }
      for(l in 1:length(pos_words)){                                # this is used to count up the number of positive words
        if(the_list[j] == pos_words[l] & !is.na(the_list[j])){                            # in a comment. We would like to know what the positive
          pos_the_count <- pos_the_count + 1                        
          positive_comment_list[[i]] <- pos_the_count
        }
      }
    }
  }

  for(i in 1:length(comment_list)){
    combined_comment_list[[i]] <- comment_list[[i]] + positive_comment_list[[i]]  # sum the two values together
    if(combined_comment_list[[i]] < -5 | complaint_type[i] == "Class II Illness")
    {
      combined_comment_list[[i]] <- -5                                            # set the min value the sentiment score can take
    }
    else if(combined_comment_list[[i]] > 5)
    {
      combined_comment_list[[i]] <- 5                                             # set the max value the sentiment score can take
    }
  }
  
  ## Uncomment only if more data is needed in the excel spreadsheet
  # caseDate <- c("02/27/92")
  # length(caseDate) <- length(combined_comment_list)
  # for(i in 1:length(combined_comment_list)){
  #   caseDate[i] <- c("02/27/92")
  # }
  
  newlist <- cbind(                                                              # create a new list that will be used in the final output                               # for the excel/csv/DBMS system
    review_data$CaseNumber,
    combined_comment_list
  )
  

  
  colnames(newlist) <- c("CaseNumber","SentimentScore")
  newlist <- as.data.frame(newlist,StringsAsFactors = FALSE)                            # need stringsAsFactors =  FALSE so I can update the values in the factors
  
 
## this code writes back to the database
## the print is there to check the output of what is being written to the database
for(i in 1:length(review_data$CaseNumber)){
  queries <- paste0(
    "INSERT INTO SentimentScore  VALUES (","'", newlist$CaseNumber[[i]], "'",",","'", newlist$SentimentScore[[i]],"');"
  )
  sqlQuery(dbhandle,queries)
  print(queries)
}
  
  # uncomment if deletion is needed
  # for(i in 1:length(review_data$CaseNumber)){
  #   delete_queries <- paste0(
  #     "DELETE FROM SentimentScore WHERE CaseNumber = ","'",newlist$CaseNumber[[i]],"'"
  #   )
  #   print(delete_queries)
  #   sqlQuery(dbhandle,delete_queries)
  # }
  
  # uncomment if writing to a csv file is needed
  # write.csv(newlist,"test_new_list_today.csv")                   # write to an excel file
  # # Updated on(6/3/2016)
  # write.csv(newlist,file = paste0(Sys.Date(),"Sentiment_Score.csv"))                                
}

# keep this in the code so batch file can 
Sentiment_Analysis()
