library(ggplot2)
library(dplyr)
library(tidyverse)
library(openxlsx)
library(purrr)

#-----------------------------------------SET WORKING DIRECTORY AND LOAD DATASETS-------------------------------
setwd() # Set to your prefered Working directory

# Load and filter datasets
predata <- read.csv("./csv files/Filtered/_Majors/filtered_predata_majors.csv")
postdata <- read.csv("./csv files/Filtered/_Majors/filtered_postdata_majors.csv")

score_q1 <- read.csv("./csv files/Filtered/_Score/_Rubric_Score_Tracker_Isha_Q1.csv") %>%
  left_join(select(predata, hash_id, Gender), by = "hash_id")
score_q2 <- read.csv("./csv files/Filtered/_Score/_Rubric_Score_Tracker_Isha_Q2.csv") %>%
  left_join(select(predata, hash_id, Gender), by = "hash_id")


# Find common participants and filter data
common_participants <- intersect(predata$hash_id, postdata$hash_id)
filtered_predata <- predata %>% filter(hash_id %in% common_participants)
filtered_postdata <- postdata %>% filter(hash_id %in% common_participants) %>%
  left_join(select(filtered_predata, hash_id, Gender), by = "hash_id")

# Define columns to test
columns_to_test_pre <- c("Section2Q1", "Section2Q2", "Section2Q3", "Section2Q4", "Section2Q5",
                         "Section3_StrategicThinkingi.", "Section3_StrategicThinkingii.", "Section3_StrategicThinkingiii.",
                         "Section3_SystemsThinkingi.", "Section3_SystemsThinkingii.", "Section3_SystemsThinkingiii.",
                         "Section3_FutureThinkingi.", "Section3_FutureThinkingii.", "Section3_FutureThinkingiii.",
                         "Section3_ValuesThinkingi.", "Section3_ValuesThinkingii.", "Section3_ValuesThinkingiii.")

columns_to_test_post <- c("Section2Q1", "Section2Q2", "Section2Q3", "Section2Q4", "Section2Q5",
                          "Section_3_Q1_overall_self_confidence", "Section3_StrategicThinkingi.", "Section3_StrategicThinkingii.", "Section3_StrategicThinkingiii.",
                          "Section3_SystemsThinkingi.", "Section3_SystemsThinkingii.", "Section3_SystemsThinkingiii.",
                          "Section3_FutureThinkingi.", "Section3_FutureThinkingii.", "Section3_FutureThinkingiii.",
                          "Section3_ValuesThinkingi.", "Section3_ValuesThinkingii.", "Section3_ValuesThinkingiii.",
                          "Section_3_Q3_Training_Impact_Systems_Thinking", "Section_3_Q3_Training_Impact_Future_Thinking",
                          "Section_3_Q3_Training_Impact_Strategic_Thinking", "Section_3_Q3_Training_Impact_Values_Thinking")

columns_to_test_score <- c("Pre_Sys","Pre_Fut","Pre_Str","Pre_Val","Post_Sys","Post_Fut","Post_Str","Post_Val")

#-----------------------------------------MAIN FUNCTION---------------------------------------------
perform_t_test <- function(data, columns_to_test) {
  results <- list()  # Use a list to store results efficiently
  
  for (col in columns_to_test) {
    # Extract male and female data
    male_data <- data %>% filter(Gender == "Male") %>% pull(col)
    female_data <- data %>% filter(Gender == "Female") %>% pull(col)
    
    # Perform Mann-Whitney U test
    test_result <- wilcox.test(male_data, female_data, alternative = "two.sided")

    # Store results in the list
    results[[col]] <- data.frame(
      Column = col,
      IQR_male = round(IQR(male_data, na.rm = TRUE), 2),
      IQR_female = round(IQR(female_data, na.rm = TRUE), 2),
      
      W_statistic = test_result$statistic,
      p.value = round(test_result$p.value, 4),
      Rank_Biserial = round(rank_biserial(male_data, female_data)$r_rank_biserial, 2),
      
      Mean_male = round(mean(male_data, na.rm = TRUE), 2),
      Sample_size_male = length(male_data),
                                
      Mean_female = round(mean(female_data, na.rm = TRUE), 2),
      Sample_size_female = length(female_data)
    )
  }
  
  # Convert the list of results into a data frame
  results_df <- bind_rows(results)
}

#---------------------------------------------------SAVE ALL RESULTS------------------------------------------------------------------------
results_list <- list(
  perform_t_test(filtered_predata, columns_to_test_pre),
  perform_t_test(filtered_postdata, columns_to_test_post),
  perform_t_test(score_q1, columns_to_test_score),
  perform_t_test(score_q2, columns_to_test_score)
)

# Sheet names
sheet_names <- c('Pre Gender Analysis', 'Post Gender Analysis', 'Score Q1 Gender Analysis', 'Score Q2 Gender Analysis')

# Create a new workbook
wb <- createWorkbook()

# Add worksheets and write data
walk2(results_list, sheet_names, ~ {
  addWorksheet(wb, .y)
  writeData(wb, .y, .x)
})

# Save the workbook to a single Excel file
saveWorkbook(wb, "./Results/gender_analysis_results.xlsx", overwrite = TRUE)
