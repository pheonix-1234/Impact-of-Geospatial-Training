library(dplyr)
library(stringr)
library(openxlsx)
library(tidyverse)

#-------------------- LOAD AND FILTER DATA --------------------
setwd() # Set to your prefered Working directory

#Load CSV file for data and score
predata <- read.csv("./csv files/Original/UTF-8anonym_pre_survey_bothTerms.csv") %>% 
  # organize ascending hash_id
  arrange(hash_id) %>%
  # Q1 = consent; if survey is finished
  filter(Q1 != "" & Finished == TRUE) %>%
  select(hash_id, Q1,Q9, Q10, Q11, Q12, Q13, Q14, Q15, Q16, Q17, Q17.1,Q21,Q18,Q29,Q20,Q23_1,Q23_2,
         Q23_3,Q23_4,Q23_5,Q23_6,Q23_7,Q23_8,Q23_9,Q23_10,Q23_11,Q23_12,Q24,Q25)

postdata <- read.csv("./csv files/Original/UTF-8anonym_post_survey_bothTerms.csv") %>% 
  # organize ascending hash_id
  arrange(hash_id) %>%
  # Q1 = consent; if survey is finished
  filter(Q1 != "" & Finished == TRUE) %>%
  select(hash_id, Q1,Q9,Q10,Q11,Q17,Q21,Q18,Q29,Q20,Q21.1, Q23_1,Q23_2,Q23_3,Q23_4,
         Q23_5,Q23_6,Q23_7,Q23_8,Q23_9,Q23_10,Q23_11,Q23_12,Q22_1,Q22_2,Q22_3,Q22_4,Q24,Q25)

# What HashIDs are found in both surveys
common_participants <- intersect(predata$hash_id, postdata$hash_id)

filtered_predata <- predata %>% filter(hash_id %in% common_participants)
filtered_postdata <- postdata %>% filter(hash_id %in% common_participants)


#-----------------------------------------RENAME RELEVANT COLUMNS TO BETTER INTERPRET--------------------------------------------------------
pre_rename_columns <- read.csv("./csv files/Original/Manual Processing/pre_rename_columns.csv") 
post_rename_columns <- read.csv("./csv files/Original/Manual Processing/post_rename_columns.csv")

for(i in 1:nrow(pre_rename_columns)) {
  # Check if the current column name exists in the filtered_predata dataframe
  if (pre_rename_columns$current_colname[i] %in% colnames(filtered_predata)) {
    # Rename the column
    colnames(filtered_predata)[colnames(filtered_predata) == pre_rename_columns$current_colname[i]] <- pre_rename_columns$mod_colname[i]
  }
}

for(i in 1:nrow(post_rename_columns)) {
  # Check if the current column name exists in the filtered_postdata dataframe
  if (post_rename_columns$current_colname[i] %in% colnames(filtered_postdata)) {
    # Rename the column
    colnames(filtered_postdata)[colnames(filtered_postdata) == post_rename_columns$current_colname[i]] <- post_rename_columns$mod_colname[i]
  }
}

#-----------------------------------------CONVERT TO NUMERIC VALUE--------------------------------------------------------
pre_columns_to_process <- read.csv("./csv files/Manual Processing/pre_columns_to_process.csv")
post_columns_to_process <- read.csv("./csv files/Manual Processing/post_columns_to_process.csv")

for (col_name in pre_columns_to_process$columns_to_process) {
  filtered_predata[[col_name]] <- sapply(filtered_predata[[col_name]], function(x) {
    # Extract the first number in each string, if present
    match <- str_extract(as.character(x), "\\d+")
    # Replace the original value with the extracted number (if found)
    return(ifelse(!is.na(match), as.numeric(match), NA))  # NA if no number is found
  })
}

for (col_name in post_columns_to_process$columns_to_process) {
  filtered_postdata[[col_name]] <- sapply(filtered_postdata[[col_name]], function(x) {
    # Extract the first number in each string, if present
    match <- str_extract(as.character(x), "\\d+")
    # Replace the original value with the extracted number (if found)
    return(ifelse(!is.na(match), as.numeric(match), NA))  # 1 if no number is found
  })
}

#-----------------------------------------SAVE FILTERED DATASETS--------------------------------------------------------
# Write the new data to a new CSV file
write.csv(filtered_predata, "./csv files/Filtered/filtered_predata.csv", row.names = FALSE)
write.csv(filtered_postdata, "./csv files/Filtered/filtered_postdata.csv", row.names = FALSE)
