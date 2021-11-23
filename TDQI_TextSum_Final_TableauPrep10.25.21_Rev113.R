#This script is used within Tableau Prep to analyze qualitative responses from the TDQI for youth, families and stakeholders. It uses Lexrankr to summarize the text into categories of effective practices and areas of needed support for each group. 
#This script will only work within the tableau prep flow. If you want to run it stand alone, df, which is the data called within tableau prep, needs to be an actual data file named "df". This is useful for testing if something is wrong. 

#load packages
library(lexRankr)
library(tidyverse)
library(Rserve)

#function that handles all of the analyses
TDQI_Qual_sum <- function(df) { 
  
  #Begin cleaning and summarizing written responses by families related to effective practices.
  if(sum(df$Fam_Not_Null_QI1_EFCT,na.rm = TRUE) >= 5 |
     sum(df$Fam_Not_Null_QI2_EFCT,na.rm = TRUE) >= 5 |
     sum(df$Fam_Not_Null_QI3_EFCT,na.rm = TRUE) >= 5 |
     sum(df$Fam_Not_Null_QI4_EFCT,na.rm = TRUE) >= 5 |
     sum(df$Fam_Not_Null_QI5_EFCT,na.rm = TRUE) >= 5 |
     sum(df$Fam_Not_Null_QI6_EFCT,na.rm = TRUE) >= 5 |
     sum(df$Fam_Not_Null_QI7_EFCT,na.rm = TRUE) >= 5 |
     sum(df$Fam_Not_Null_QI8_EFCT,na.rm = TRUE) >= 5 |
     sum(df$Fam_Not_Null_QI9_EFCT,na.rm = TRUE) >= 5){
    Fam <- filter(df, Role == "Parent / Legal Guardian")
    Fam1 <- Fam %>% select(QI1_WRITTEN1,QI2_WRITTEN1,QI3_WRITTEN1,QI4_WRITTEN1,QI5_WRITTEN1,QI6_WRITTEN1,QI7_WRITTEN1,QI8_WRITTEN1,QI9_WRITTEN1,Identifiers)
    Fam1 <- Fam1 %>% pivot_longer(!Identifiers,names_to="QIs",values_to ="Responses")
    Fam1 <- select(Fam1,-c(Identifiers,QIs))
    Fam1 <- lexRankr::lexRank(Fam1$Responses, docId = "create", threshold = FALSE, n = 5,
                              returnTies = FALSE, usePageRank = TRUE, damping = 0.85,
                              continuous = TRUE, sentencesAsDocs = TRUE, removePunc = TRUE,
                              removeNum = TRUE, toLower = TRUE, stemWords = TRUE,
                              rmStopWords = TRUE, Verbose = TRUE)
    Fam1 <- select(Fam1, -c(docId,sentenceId,value))
    Fam1 <- rename(Fam1, Fam_Effective = sentence)
    Fam1 <- Fam1 %>% add_column(Role="Parent / Legal Guardian")
    Fam1 <- Fam1 %>% add_column(ID=1:5)
    Fam1 <- as_tibble(Fam1)
  } else {
    Fam <- filter(df, Role == "Parent / Legal Guardian")
    Fam1 <- data.frame(Role = c("Parent / Legal Guardian"),
                       Fam_Effective = c("No Response"),
                       ID = c(1:5),
                       stringsAsFactors = FALSE)
    Fam1 <- as_tibble(Fam1)
  }
  
  #Begin cleaning and summarizing written responses by families related to areas for improvement.
  if (sum(df$Fam_Not_Null_QI1_IMPV,na.rm = TRUE) >= 5 |
      sum(df$Fam_Not_Null_QI2_IMPV,na.rm = TRUE) >= 5 |
      sum(df$Fam_Not_Null_QI3_IMPV,na.rm = TRUE) >= 5 |
      sum(df$Fam_Not_Null_QI4_IMPV,na.rm = TRUE) >= 5 |     
      sum(df$Fam_Not_Null_QI5_IMPV,na.rm = TRUE) >= 5 |
      sum(df$Fam_Not_Null_QI6_IMPV,na.rm = TRUE) >= 5 |
      sum(df$Fam_Not_Null_QI7_IMPV,na.rm = TRUE) >= 5 |
      sum(df$Fam_Not_Null_QI8_IMPV,na.rm = TRUE) >= 5 |
      sum(df$Fam_Not_Null_QI9_IMPV,na.rm = TRUE) >= 5){
    Fam2 <- Fam %>% select(QI1_WRITTEN2,QI2_WRITTEN2,QI3_WRITTEN2,QI4_WRITTEN2,QI5_WRITTEN2,QI6_WRITTEN2,QI7_WRITTEN2,QI8_WRITTEN2,QI9_WRITTEN2,Identifiers)
    Fam2 <- Fam2 %>% pivot_longer(!Identifiers,names_to="QIs",values_to ="Responses")
    Fam2 <- select(Fam2,-c(Identifiers,QIs))
    Fam2 <- lexRankr::lexRank(Fam2$Responses, docId = "create", threshold = FALSE, n = 5,
                              returnTies = FALSE, usePageRank = TRUE, damping = 0.85,
                              continuous = TRUE, sentencesAsDocs = TRUE, removePunc = TRUE,
                              removeNum = TRUE, toLower = TRUE, stemWords = TRUE,
                              rmStopWords = TRUE, Verbose = TRUE)
    Fam2 <- select(Fam2, -c(docId,sentenceId,value))
    Fam2 <- rename(Fam2, Fam_Improvement = sentence)
    Fam2 <- Fam2 %>% add_column(Role="Parent / Legal Guardian")
    Fam2 <- Fam2 %>% add_column(ID=6:10)
    Fam2 <- as_tibble(Fam2)
  } else {
    Fam2 <- data.frame(Role = c("Parent / Legal Guardian"),
                       Fam_Improvement = c("No Response"),
                       ID = c(6:10),
                       stringsAsFactors = FALSE)
    Fam2 <- as_tibble(Fam2)
  }
  
  #Begin cleaning and summarizing written responses by stakeholders for effective practices. 
  if(sum(df$STK_NOTNULL_QI1_EFCT,na.rm = TRUE) >= 5 |
     sum(df$STK_NOTNULL_QI2_EFCT,na.rm = TRUE) >= 5 |
     sum(df$STK_NOTNULL_QI3_EFCT,na.rm = TRUE) >= 5 |
     sum(df$STK_NOTNULL_QI4_EFCT,na.rm = TRUE) >= 5 |
     sum(df$STK_NOTNULL_QI5_EFCT,na.rm = TRUE) >= 5 |
     sum(df$STK_NOTNULL_QI6_EFCT,na.rm = TRUE) >= 5 |
     sum(df$STK_NOTNULL_QI7_EFCT,na.rm = TRUE) >= 5 |
     sum(df$STK_NOTNULL_QI8_EFCT,na.rm = TRUE) >= 5 |
     sum(df$STK_NOTNULL_QI9_EFCT,na.rm = TRUE) >= 5) {
    Stk <- filter(df,Role =="Transition Stakeholder")
    Stk1 <- Stk %>% select(QI1_WRITTEN1,QI2_WRITTEN1,QI3_WRITTEN1,QI4_WRITTEN1,QI5_WRITTEN1,QI6_WRITTEN1,QI7_WRITTEN1,QI8_WRITTEN1,QI9_WRITTEN1,Identifiers)
    Stk1 <- Stk1 %>% pivot_longer(!Identifiers,names_to="QIs",values_to ="Responses")
    Stk1 <- select(Stk1,-c(Identifiers,QIs))
    Stk1 <- lexRankr::lexRank(Stk1$Responses, docId = "create", threshold = FALSE, n = 5,
                              returnTies = FALSE, usePageRank = TRUE, damping = 0.85,
                              continuous = TRUE, sentencesAsDocs = TRUE, removePunc = TRUE,
                              removeNum = TRUE, toLower = TRUE, stemWords = TRUE,
                              rmStopWords = TRUE, Verbose = TRUE)
    Stk1 <- select(Stk1, -c(docId,sentenceId,value))
    Stk1 <- rename(Stk1, Stk_Effective = sentence)
    Stk1 <- Stk1 %>% add_column(Role="Transition Stakeholder")
    Stk1 <- Stk1 %>% add_column(ID=11:15)
    Stk1 <- as_tibble(Stk1)
  } else {
    Stk <- filter(df,Role =="Transition Stakeholder")
    Stk1 <- data.frame(Role = c("Transition Stakeholder"),
                       Stk_Effective = c("No Response"),
                       ID = c(11:15),
                       stringsAsFactors = FALSE)
    Stk1 <- as_tibble(Stk1)
  }  
  
  #Begin cleaning and summarizing written responses by stakeholders for areas for improvement. 
  if(sum(df$STK_NOT_NULL_QI1_IMPV,na.rm = TRUE) >= 5 |
     sum(df$STK_NOT_NULL_QI2_IMPV,na.rm = TRUE) >= 5 |
     sum(df$STK_NOT_NULL_QI3_IMPV,na.rm = TRUE) >= 5 |
     sum(df$STK_NOT_NULL_QI4_IMPV,na.rm = TRUE) >= 5 |
     sum(df$STK_NOT_NULL_QI5_IMPV,na.rm = TRUE) >= 5 |
     sum(df$STK_NOT_NULL_QI6_IMPV,na.rm = TRUE) >= 5 |
     sum(df$STK_NOT_NULL_QI7_IMPV,na.rm = TRUE) >= 5 |
     sum(df$STK_NOT_NULL_QI8_IMPV,na.rm = TRUE) >= 5 |
     sum(df$STK_NOT_NULL_QI9_IMPV,na.rm = TRUE) >= 5) {
    Stk2 <- Stk %>% select(QI1_WRITTEN2,QI2_WRITTEN2,QI3_WRITTEN2,QI4_WRITTEN2,QI5_WRITTEN2,QI6_WRITTEN2,QI7_WRITTEN2,QI8_WRITTEN2,QI9_WRITTEN2,Identifiers)
    Stk2 <- Stk2 %>% pivot_longer(!Identifiers,names_to="QIs",values_to ="Responses")
    Stk2 <- select(Stk2,-c(Identifiers,QIs))
    #identify and extract 5 sentences that best represent the topics addressed across all responses.
    Stk2 <- lexRankr::lexRank(Stk2$Responses, docId = "create", threshold = FALSE, n = 5,
                              returnTies = FALSE, usePageRank = TRUE, damping = 0.85,
                              continuous = TRUE, sentencesAsDocs = TRUE, removePunc = TRUE,
                              removeNum = TRUE, toLower = TRUE, stemWords = TRUE,
                              rmStopWords = TRUE, Verbose = TRUE)
    Stk2 <- select(Stk2, -c(docId,sentenceId,value))
    Stk2 <- rename(Stk2, Stk_Improvement = sentence)
    Stk2 <- Stk2 %>% add_column(Role="Transition Stakeholder")
    Stk2 <- Stk2 %>% add_column(ID=16:20)
    Stk2 <- as_tibble(Stk2)  
  } else {
    Stk2 <- data.frame(Role = c("Transition Stakeholder"),
                       Stk_Improvement = c("No Response"),
                       ID = c(16:20),
                       stringsAsFactors = FALSE)
    Stk2 <- as_tibble(Stk2)    
  }
  
  #Begin cleaning and summarizing written responses by youth for effective practices
  #n is larger for youth because their responses tend to be less cohesive across participants and lexrank fails to construct a similarity matrix if there are under 10.
  if (sum(df$YouthWW_Not_Null,na.rm = TRUE) >= 10){
    #subset of only youth responses
    Yth <- filter(df,Role =="Young Person")
    #Youth responses for effective practices
    Yth1 <- Yth %>% select(What.Works)
    #identify and extract 5 sentences that best represent the topics addressed across all responses.
    Yth1 <- lexRankr::lexRank(Yth1$What.Works, docId = "create", threshold = FALSE, n = 5,
                              returnTies = FALSE, usePageRank = TRUE, damping = 0.85,
                              continuous = TRUE, sentencesAsDocs = TRUE, removePunc = TRUE,
                              removeNum = TRUE, toLower = TRUE, stemWords = TRUE,
                              rmStopWords = TRUE, Verbose = TRUE)
    Yth1 <- select(Yth1, -c(docId,sentenceId,value))
    Yth1 <- rename(Yth1, Yth_Effective = sentence)
    Yth1 <- Yth1 %>% add_column(Role="Young Person")
    Yth1 <- Yth1 %>% add_column(ID=21:25)
    Yth1 <- as_tibble(Yth1)
  } else if (sum(df$YouthWW_Not_Null,na.rm = TRUE) >= 5){
    Yth <- filter(df,Role =="Young Person")
    Yth1 <- Yth %>% select(What.Works)
    Yth1 <- head(Yth1,5)
    Yth1 <- Yth1 %>% add_column(Role = "Young Person", ID = 21:25)
    Yth1 <- Yth1 %>% 
      rename(Yth_Effective = What.Works)
    Yth1 <- as_tibble(Yth1)
  } else {
    Yth1 <- data.frame(Role = c("Young Person"),
                       Yth_Effective = c("No Response"),
                       ID = c(21:25),
                       stringsAsFactors = FALSE)
    Yth1 <- as_tibble(Yth1)
  }
  
  #Begin cleaning and summarizing written responses by youth for needed supports. 
  if (sum(df$YouthNS_Not_Null,na.rm = TRUE) >= 10){
    Yth2 <- Yth %>% select(Needed.Support)
    #identify and extract 5 sentences that best represent the topics addressed across all responses.
    Yth2 <- lexRankr::lexRank(Yth2$Needed.Support, docId = "create", threshold = FALSE, n = 5,
                              returnTies = FALSE, usePageRank = TRUE, damping = 0.85,
                              continuous = TRUE, sentencesAsDocs = TRUE, removePunc = TRUE,
                              removeNum = TRUE, toLower = TRUE, stemWords = TRUE,
                              rmStopWords = TRUE, Verbose = TRUE)
    Yth2 <- select(Yth2, -c(docId,sentenceId,value))
    Yth2 <- rename(Yth2, Yth_Improvement = sentence)
    Yth2 <- Yth2 %>% add_column(Role="Young Person")
    Yth2 <- Yth2 %>% add_column(ID=26:30)
    Yth2 <- as_tibble(Yth2)
    #Take the first 5 responses and create a new data table that matches the output above.
  } else if (sum(df$YouthNS_Not_Null,na.rm = TRUE) >= 5){
    Yth <- filter(df,Role =="Young Person")
    Yth2 <- Yth %>% select(Needed.Support)
    Yth2 <- head(Yth2,5)
    Yth2 <- Yth2 %>% add_column(Role = "Young Person", ID = 26:30)
    Yth2 <- Yth2 %>% 
      rename(Yth_Improvement = Needed.Support)
    Yth2 <- as_tibble(Yth2)  
  } else {
    #Make a table that matches the output above but with no written responses so that Tableau will accept the incoming data frame.
    Yth2 <- data.frame(Role = c("Young Person"),
                       Yth_Improvement = c("No Response"),
                       ID = c(26:30),
                       stringsAsFactors = FALSE)
    Yth2 <- as_tibble(Yth2)        
  }
  
  #Full join the tibbles into data frame.
  QualFam <- full_join(Fam1,Fam2,by=NULL,copy=FALSE)
  QualStk <- full_join(Stk1,Stk2,by=NULL,copy=FALSE)
  QualYouth <- full_join(Yth1,Yth2,by=NULL,copy=FALSE)
  QualFull <- full_join(QualFam,QualStk,by=NULL,copy=FALSE)
  QualFull <- full_join(QualFull,QualYouth,by=NULL,copy=FALSE)
  QualFull <- as.data.frame(QualFull)
  df <- QualFull
  return(df)
}

#tell tableau how the incoming data frame is structured.
getOutputSchema <- function(df) {
  return(data.frame(
    Role = prep_string(),
    Fam_Effective = prep_string(),
    Fam_Improvement = prep_string(),
    Stk_Effective = prep_string(),
    Stk_Improvement = prep_string(),
    Yth_Effective = prep_string(),
    Yth_Improvement = prep_string(),
    ID = prep_int()
  ))
}
