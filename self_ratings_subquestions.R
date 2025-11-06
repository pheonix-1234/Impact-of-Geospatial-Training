library(dplyr)
library(stringr)
library(openxlsx)
library(tidyverse)
library(effsize)
library(psych)
library(effectsize) # for rank biserial
library(purrr)

#-----------------------------------------Load datasets, find common intersection, and filter out blanks-----------------------------------------
setwd()

# Load and filter datasets
predata <- read.csv("./csv files/Filtered/_Majors/filtered_predata_majors.csv")
postdata <- read.csv("./csv files/Filtered/_Majors/filtered_postdata_majors.csv")
common_participants <- intersect(predata$hash_id, postdata$hash_id)

filtered_predata <- filter(predata, hash_id %in% common_participants)
filtered_postdata <- filter(postdata, hash_id %in% common_participants) %>%
  left_join(select(filtered_predata, hash_id, Gender, Major, Year, Ethnicity, 
                   Number_of_Sustainability_Courses_Taken,
                   Sustainability_related_Work, UN_SDGs), by = "hash_id")

subquestions <- list("Section3_StrategicThinkingi.", "Section3_StrategicThinkingii.", "Section3_StrategicThinkingiii.",
                     "Section3_SystemsThinkingi.", "Section3_SystemsThinkingii.", "Section3_SystemsThinkingiii.",
                     "Section3_FutureThinkingi.", "Section3_FutureThinkingii.", "Section3_FutureThinkingiii.",
                     "Section3_ValuesThinkingi.", "Section3_ValuesThinkingii.", "Section3_ValuesThinkingiii.")

#---------------------------------------------------------ALL FUNCTIONS----------------------------------------------------------------
# Compute general statistics
compute_general_stats <- function(data) {
  map_dfr(subquestions, function(subques) {
    pre_scores <- data$pre[[subques]]
    post_scores <- data$post[[subques]]
    
    data.frame(
      subquestion = subques,
      pre_mean = round(mean(pre_scores, na.rm = TRUE), 2),
      post_mean = round(mean(post_scores, na.rm = TRUE), 2),
      pre_median = median(pre_scores, na.rm = TRUE),
      post_median = median(post_scores, na.rm = TRUE),
      pre_sd = round(sd(pre_scores, na.rm = TRUE), 2),
      post_sd = round(sd(post_scores, na.rm = TRUE), 2),
      overall_difference = round(mean(post_scores, na.rm = TRUE) - mean(pre_scores, na.rm = TRUE), 2)
    )
  })
}

# Normality tests
compute_normality <- function(data) {
  map_dfr(subquestions, function(subques) {
    pre_scores <- data$pre[[subques]]
    post_scores <- data$post[[subques]]
    
    data.frame(
      subquestion = subques,
      pre_pvalue = round(shapiro.test(pre_scores)$p.value, 5),
      post_pvalue = round(shapiro.test(post_scores)$p.value, 5)
    )
  })
}

# T-tests and Wilcoxon tests
perform_t_tests <- function(data, normality) {
  map_dfr(subquestions, function(subques) {
    pre_scores <- data$pre[[subques]]
    post_scores <- data$post[[subques]]
    
    normality_row <- normality %>% filter(subquestion == subques)
    pre_pvalue <- normality_row$pre_pvalue
    post_pvalue <- normality_row$post_pvalue
    
    # Choose test type and effect size calculation based on normality
    is_normal <- pre_pvalue >= 0.05 & post_pvalue >= 0.05
    
    # Apply the if-else structure for t-test vs Wilcoxon
    test <- if (is_normal) t.test(pre_scores, post_scores, paired = TRUE) else wilcox.test(pre_scores, post_scores, paired = TRUE, exact = FALSE)
    effect_size <- if (is_normal) round(cohen.d(pre_scores, post_scores)$estimate, 2) else round(rank_biserial(pre_scores, post_scores, paired = TRUE)$r_rank_biserial, 2)
    
    # Return results in a data frame
    data.frame(
      subquestion = subques,
      test_type = ifelse(is_normal, "Paired T-test", "Wilcoxon Signed-Rank Test"),
      IQR_Pre = round(IQR(pre_scores, na.rm = TRUE), 2),
      IQR_Post = round(IQR(post_scores, na.rm = TRUE), 2),
      W_Statistic = ifelse(is_normal, NA, round(test$statistic, 2)),  
      test_pvalue = round(test$p.value, 5),
      effect_size = effect_size,
      effect_size_type = ifelse(is_normal, "Cohen's D", "Rank-Biserial Correlation")
    )
    
  })
}

# Function for ANOVA and Non-Parametric Equivalent
perform_anova <- function(data, normality, pre_or_post) {
  map_dfr(subquestions, function(subques) {
    scores <- data[[pre_or_post]][[subques]]  # Accessing the subquestion directly
    
    run_test <- function(scores, group, data, normal) {
      if (normal >= 0.05) {
        round(summary(aov(scores ~ data[[group]]))[[1]]$`Pr(>F)`[1], 4)
      } else {
        round(kruskal.test(scores ~ data[[group]])$p.value, 4)
      }
    }
    
    groups <- c("Year", 'Ethnicity', 
                'Number_of_Sustainability_Courses_Taken',
                'Sustainability_related_Work', 'UN_SDGs')
    
    pvalues <- map_dbl(groups, ~ run_test(scores, .x, data[[pre_or_post]], normality[[paste0(pre_or_post, "_pvalue")]][normality$subquestion == subques]))
    
    data.frame(
      Question = subques,
      Test_Type = ifelse(normality[[paste0(pre_or_post, "_pvalue")]][normality$subquestion == subques] >= 0.05, "ANOVA", "Kruskal-Wallis"),
      P_Value.Year = pvalues[1],
      P_Value.Ethnicity = pvalues[2],
      P_Value.Sustainability = pvalues[3],
      P_Value.Sustainability_related_Work = pvalues[4],
      P_Value.UN_SDGs = pvalues[5]
    )
  })
}

# Function for ANOVA by Majors
perform_anova_majors <- function(predata, postdata, normality, items, pre_col = "pre_pvalue", post_col = "post_pvalue") {
  expand_and_test <- function(data, label, norm_col) {
    # Check if Major2 exists and pivot if necessary
    if ("Major2" %in% colnames(data)) {
      data_long <- data %>%
        mutate(Major2 = na_if(Major2, "")) %>%
        pivot_longer(c(Major, Major2), values_to = "Major_All", values_drop_na = TRUE)
    } else {
      data_long <- data %>%
        mutate(Major_All = Major)
    }
    
    map_dfr(items, ~ {
      pval_row <- filter(normality, subquestion == .x)
      
      # Defensive check: skip if no match in normality
      if (nrow(pval_row) == 0) {
        warning(paste("Subquestion", .x, "not found in normality data. Skipping."))
        return(tibble(Time = label, Subquestion = .x, Test_Type = NA, P_Value.Major = NA))
      }
      
      pval <- pval_row[[norm_col]]
      scores <- data_long[[.x]]  # Get the scores directly from subquestion column
      
      test_type <- if (pval >= 0.05) "One-Way ANOVA" else "Kruskal-Wallis"
      
      p_value <- if (pval >= 0.05) {
        round(summary(aov(scores ~ Major_All, data = data_long))[[1]]$`Pr(>F)`[1], 4)
      } else {
        round(kruskal.test(scores ~ data_long$Major_All)$p.value, 4)
      }
      
      tibble(Time = label, Subquestion = .x, Test_Type = test_type, P_Value.Major = p_value)
    })
  }
  
  # Call for both predata and postdata
  bind_rows(
    expand_and_test(predata, "Pre", pre_col),
    expand_and_test(postdata, "Post", post_col)
  )
}


#---------------------------------------------------CALL FUNCTIONS---------------------------------------------------------------------
filtered_data <- list(pre = filtered_predata, post = filtered_postdata)
gen_stats <- compute_general_stats(filtered_data)
normality <- compute_normality(filtered_data)
t_test_results <- perform_t_tests(filtered_data, normality)
anova_pre_results <- perform_anova(filtered_data, normality, pre_or_post = "pre")
anova_post_results <- perform_anova(filtered_data, normality, pre_or_post = "post")
anova_majors_results <- perform_anova_majors(filtered_predata, filtered_postdata, normality, subquestions)

#---------------------------------------------------SAVE ALL RESULTS------------------------------------------------------------------------
wb <- createWorkbook()
list(gen_stats, normality, t_test_results, anova_pre_results, anova_post_results, anova_majors_results) %>%
  setNames(c('gen_stats', 'normality', 't_test_results', 'anova_pre_results', 'anova_post_results', 'anova_majors_results')) %>%
  walk2(names(.), ~ { addWorksheet(wb, .y); writeData(wb, .y, .x) })

saveWorkbook(wb, "./Results/sub_self_ratings_stats.xlsx", overwrite = TRUE)
