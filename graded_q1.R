library(dplyr)
library(openxlsx)
library(tidyverse)
library(effectsize) 
library(purrr)

setwd()

# Load and filter datasets
predata <- read.csv("./csv files/Filtered/_Majors/filtered_predata_majors.csv")


score <- read.csv("./csv files/Filtered/_Score/_Rubric_Score_Tracker_Isha_Q1.csv") %>%
  left_join(select(predata, hash_id, Gender, Major, Year, Ethnicity, 
                   Number_of_Sustainability_Courses_Taken,
                   Sustainability_related_Work, UN_SDGs), by = "hash_id")


#-------------------------------------------------ALL FUCTIONS------------------------------------------------------
# Define competencies and column names
competencies <- c("Systems-Thinking", "Future-Thinking", "Strategic-Thinking", "Values-Thinking")
pre_columns <- paste0("Pre_", c("Sys", "Fut", "Str", "Val"))
post_columns <- paste0("Post_", c("Sys", "Fut", "Str", "Val"))

# Simplified General Statistics Calculation
calculate_stats <- function(score_data, pre_columns, post_columns) {
  map_dfr(seq_along(competencies), function(i) {
    data.frame(
      Competency = competencies[i],
      Q_diff = round(mean(score_data[[post_columns[i]]]) - mean(score_data[[pre_columns[i]]]), 2),
      mean_pre = round(mean(score_data[[pre_columns[i]]]), 2),
      median_pre = median(score_data[[pre_columns[i]]]),
      sd_pre = round(sd(score_data[[pre_columns[i]]]), 2),
      mean_post = round(mean(score_data[[post_columns[i]]]), 2),
      median_post = median(score_data[[post_columns[i]]]),
      sd_post = round(sd(score_data[[post_columns[i]]]), 2)
      
    )
  })
}

# Normality Test
test_normality <- function(score_data, pre_columns, post_columns) {
  map_dfr(seq_along(competencies), ~ data.frame(
    Competency = competencies[.x],
    Pre_pvalue = round(shapiro.test(score_data[[pre_columns[.x]]])$p.value, 4),
    Post_pvalue = round(shapiro.test(score_data[[post_columns[.x]]])$p.value, 4)
  ))
}

# Paired t-test/Wilcoxon test and Effect Size Calculation
t_test_function <- function(pre, post, normality_pre, normality_post, competency) {
  # Check if normality conditions are met
  is_normal <- normality_pre >= 0.05 & normality_post >= 0.05
  
  # Perform t-test or Wilcoxon test based on normality
  test_result <- if (is_normal) t.test(pre, post, paired = TRUE) else wilcox.test(pre, post, paired = TRUE, exact = FALSE)
  
  # Calculate Effect Size based on normality
  effect_size <- if (is_normal) round(cohen.d(pre, post, paired = TRUE)$estimate, 2) else round(rank_biserial(pre, post, paired = TRUE)$r_rank_biserial, 2)
  
  # Prepare result data frame
  data.frame(
    Competency = competency,
    Test_Type = ifelse(is_normal, "Paired T-test", "Wilcoxon Signed-Rank Test"),
    IQR_Pre = round(IQR(pre, na.rm = TRUE), 2),
    IQR_Post = round(IQR(post, na.rm = TRUE), 2),
    W_Statistic = ifelse(is_normal, NA, round(test_result$statistic, 2)),
    P_Value = round(test_result$p.value, 4),
    Effect_Size = effect_size
  )
}


# Function for ANOVA and Non-Parametric Equivalent
perform_anova <- function(data, columns, pvalues) {
  factors <- c("Year", 'Ethnicity', 
               'Number_of_Sustainability_Courses_Taken',
               'Sustainability_related_Work', 'UN_SDGs')
  
  map_dfr(seq_along(competencies), function(i) {
    test_type <- if (pvalues[i] >= 0.05) "ANOVA" else "Kruskal-Wallis"
    
    test_pvalues <- if (test_type == "ANOVA") {
      test_result <- aov(data[[columns[i]]] ~ Gender + Major + Year + Ethnicity + 
                           Number_of_Sustainability_Courses_Taken + Sustainability_related_Work + UN_SDGs, 
                         data = data)
      summary(test_result)[[1]][["Pr(>F)"]][1:5]
    } else {
      map_dbl(factors, function(factor) round(kruskal.test(data[[columns[i]]] ~ data[[factor]])$p.value, 4))
    }
    
    data.frame(
      Competency = competencies[i],
      Test_Type = test_type,
      P_Value.Year = round(test_pvalues[1], 4),
      P_Value.Ethnicity = round(test_pvalues[2], 4),
      P_Value.Number_of_Sustainability_Courses_Taken = round(test_pvalues[3], 4),
      P_Value.Sustainability_related_Work = round(test_pvalues[4], 4),
      P_Value.UN_SDGs = round(test_pvalues[5], 4)
    )
  })
}

perform_anova_majors <- function(score_data, normality_df, columns, time_label) {
  map_dfr(seq_along(competencies), function(i) {
    comp <- competencies[i]
    column <- columns[i]
    
    # Normality check
    norm_pval <- if (grepl("^Pre_", column)) {
      normality_df$Pre_pvalue[i]
    } else {
      normality_df$Post_pvalue[i]
    }
    
    # Test selection
    test_type <- if (norm_pval >= 0.05) "One-Way ANOVA" else "Kruskal-Wallis"
    
    scores <- score_data[[column]]
    major <- score_data$Major
    
    p_value <- if (test_type == "One-Way ANOVA") {
      round(summary(aov(scores ~ major))[[1]]$`Pr(>F)`[1], 4)
    } else {
      round(kruskal.test(scores ~ major)$p.value, 4)
    }
    
    tibble(
      Time = time_label,
      Competency = comp,
      Test_Type = test_type,
      P_Value.Major = p_value
    )
  })
}



#-----------------------------------------------CALL FUNCTIONS AND STORE DATAFRAMES----------------------------------------------
gen_stats <- calculate_stats(score, pre_columns, post_columns)

normality <- test_normality(score, pre_columns, post_columns)

t_test_results <- map_dfr(seq_along(competencies), function(i) {
  t_test_function(score[[pre_columns[i]]], score[[post_columns[i]]], 
                  normality$Pre_pvalue[i], normality$Post_pvalue[i], competencies[i])
})

anova_pre_results <- perform_anova(score, pre_columns, normality$Pre_pvalue)
anova_post_results <- perform_anova(score, post_columns, normality$Post_pvalue)

anova_major_pre_results <- perform_anova_majors(score, normality, pre_columns, "Pre-Q1")
anova_major_post_results <- perform_anova_majors(score, normality, post_columns, "Post-Q1")

#---------------------------------------------------SAVE ALL RESULTS------------------------------------------------------------------------
wb <- createWorkbook()
results <- list(gen_stats, normality, t_test_results, anova_pre_results, anova_post_results, 
                anova_major_pre_results, anova_major_post_results)

names(results) <- c("gen_stats", "normality", "t_test_results", "anova_pre_results", 
                    "anova_post_results", "anova_major_pre_results", 'anova_major_post_results')

walk2(names(results), results, ~ { addWorksheet(wb, .x); writeData(wb, .x, .y) })
saveWorkbook(wb, "./Results/graded_Q1_stats.xlsx", overwrite = TRUE)

