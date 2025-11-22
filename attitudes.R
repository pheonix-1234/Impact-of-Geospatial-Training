library(dplyr)
library(openxlsx)
library(effsize)
library(effectsize)
library(tidyverse)
library(purrr)
library(tidyr)

#-------------------- LOAD AND FILTER DATA --------------------
setwd() # Set to your preferred Working directory

predata <- read.csv("./csv files/Filtered/_Majors/filtered_predata_majors.csv")
postdata <- read.csv("./csv files/Filtered/_Majors/filtered_postdata_majors.csv")
common_ids <- intersect(predata$hash_id, postdata$hash_id)

filtered_predata <- predata %>% filter(hash_id %in% common_ids)
filtered_postdata <- postdata %>%
  filter(hash_id %in% common_ids) %>%
  left_join(select(filtered_predata, hash_id, Gender, Major, Major2, Year, Ethnicity, 
                   Number_of_Sustainability_Courses_Taken, 
                   Sustainability_related_Work, UN_SDGs), by = "hash_id")

items <- paste0("Section2Q", 1:5)

#-----------------------------------------ALL FUNCTIONS--------------------------------------------------------
# Function for General Statistics
generate_general_stats <- function(pre, post, items) {
  map_dfr(items, ~{
    p <- as.numeric(pre[[.x]])
    q <- as.numeric(post[[.x]])
    tibble(
      question = .x,
      pre_mean = round(mean(p, na.rm = TRUE), 2),
      post_mean = round(mean(q, na.rm = TRUE), 2),
      mean_diff = round(mean(q - p, na.rm = TRUE), 2),
      pre_median = median(p, na.rm = TRUE),
      post_median = median(q, na.rm = TRUE),
      pre_sd = round(sd(p, na.rm = TRUE), 2),
      post_sd = round(sd(q, na.rm = TRUE), 2)
    )
  })
}

# Function for Normality Test
test_normality <- function(pre, post, items) {
  map_dfr(items, ~{
    pval <- function(x) if (sum(!is.na(x)) >= 3) round(shapiro.test(x)$p.value, 4) else NA
    tibble(
      question = .x,
      pre_pvalue = pval(pre[[.x]]),
      post_pvalue = pval(post[[.x]])
    )
  })
}

# Paired t-test/Wilcoxon test and Effect Size Calculation
perform_t_tests <- function(pre, post, norm, items) {
  map_dfr(items, ~{
    # Convert to numeric (pre, post) and filters p-values from normality function by 'items' (.x)
    p <- as.numeric(pre[[.x]])
    q <- as.numeric(post[[.x]])
    row <- filter(norm, question == .x)
    
    if (anyNA(row)) return(tibble(Question = .x, Test_Type = NA, W_Statistic = NA, P_Value = NA, Effect_Size = NA))
    
    # p-value from shapiro tests (above 0.05 means data exhibits normality)
    is_normal <- row$pre_pvalue >= 0.05 & row$post_pvalue >= 0.05
    
    test <- if (is_normal) t.test(p, q, paired = TRUE) else wilcox.test(p, q, paired = TRUE, exact = FALSE)
    
    # Extracting W statistic and effect size from the Wilcoxon/t-test result
    effect_size <- if (is_normal) cohen.d(p, q, paired = TRUE) else rank_biserial(p, q, paired = TRUE)
    cliffdelta <- cliff.delta(p, q)
    W_statistic <- if (is_normal) NA else test$statistic
    
    tibble(
      Question = .x,
      Test_Type = ifelse(is_normal, "Paired T-test", "Wilcoxon Signed-Rank Test"),
      IQR_Pre = IQR(p, na.rm = TRUE),
      IQR_Post = IQR(q, na.rm = TRUE),
      W_Statistic = round(W_statistic, 2),  
      P_Value = round(test$p.value, 4),
      eff_size = ifelse(is_normal, effect_size$estimate, effect_size$r_rank_biserial),
      eff_CI = ifelse(is_normal, effect_size$conf.int, effect_size$CI),
      eff_CI_upper = ifelse(is_normal, effect_size$conf.int$upper, effect_size$CI_high),
      eff_CI_lower = ifelse(is_normal, effect_size$conf.int$lower, effect_size$CI_low),
      effect_size_type = ifelse(is_normal, "Cohen's D", "Rank-Biserial Correlation"),
      cliff_delta = cliffdelta$estimate,
      cliff_delta_magnitude = cliffdelta$magnitude
    )
  })
}

# Function for ANOVA and Non-Parametric Equivalent
perform_anova <- function(data, norm, items, col) {
  map_dfr(items, ~{
    pval <- filter(norm, question == .x)[[col]]
    if (is.na(pval)) {
      return(tibble(
        Question = .x, Test_Type = NA,
        P_Value.Year = NA,
        P_Value.Ethnicity = NA,
        P_Value.Sustainability = NA,
        P_Value.Sustainability_related_Work = NA,
        P_Value.UN_SDGs = NA
      ))
    }
    
    normal <- pval >= 0.05
    test_type <- ifelse(normal, "One-Way ANOVA", "Kruskal-Wallis")
    
    run_test <- function(var) {
      formula <- as.formula(paste(.x, "~", var))
      tryCatch({
        if (normal) summary(aov(formula, data))[[1]]$`Pr(>F)`[1]
        else kruskal.test(formula, data)$p.value
      }, error = function(e) NA)
    }
    
    tibble(
      Question = .x,
      Test_Type = test_type,
      P_Value.Year = round(run_test("Year"), 4),
      P_Value.Ethnicity = round(run_test("Ethnicity"), 4),
      P_Value.Sustainability = round(run_test("Number_of_Sustainability_Courses_Taken"), 4),
      P_Value.Sustainability_related_Work = round(run_test("Sustainability_related_Work"), 4),
      P_Value.UN_SDGs = round(run_test("UN_SDGs"), 4)
    )
  })
}


# Function for ANOVA and Non-Parametric Equivalent for Majors
perform_anova_majors <- function(pre, post, norm, items, pre_col = "pre_pvalue", post_col = "post_pvalue") {
  test_by_major <- function(data, time, colname) {
    data_long <- data %>%
      pivot_longer(cols = c(Major, Major2), values_to = "Major_All", values_drop_na = TRUE)
    
    map_dfr(items, ~{
      pval <- filter(norm, question == .x)[[colname]]
      if (is.na(pval)) {
        return(tibble(Time = time, Question = .x, Test_Type = NA, P_Value.Major = NA))
      }
      
      normal <- pval >= 0.05
      formula <- as.formula(paste(.x, "~ Major_All"))
      p_value <- tryCatch({
        if (normal) summary(aov(formula, data_long))[[1]]$`Pr(>F)`[1]
        else kruskal.test(formula, data_long)$p.value
      }, error = function(e) NA)
      
      tibble(Time = time, Question = .x, Test_Type = ifelse(normal, "One-Way ANOVA", "Kruskal-Wallis"), P_Value.Major = round(p_value, 4))
    })
  }
  
  bind_rows(
    test_by_major(pre, "Pre", pre_col),
    test_by_major(post, "Post", post_col)
  )
}


#---------------------------------------------------CALL FUNCTIONS---------------------------------------------------------------------
# Generate statistics
gen_stats <- generate_general_stats(filtered_predata, filtered_postdata, items)
# Test normality
normality <- test_normality(filtered_predata, filtered_postdata, items)
# Perform t-tests
t_test_results <- perform_t_tests(filtered_predata, filtered_postdata, normality, items)
# Perform ANOVA
anova_pre_results <- perform_anova(filtered_predata, normality, items, "pre_pvalue")
anova_post_results <- perform_anova(filtered_postdata, normality, items, "post_pvalue")
anova_results_majors <- perform_anova_majors(filtered_predata, filtered_postdata, normality, items)

#---------------------------------------------------SAVE ALL RESULTS------------------------------------------------------------------------
wb <- createWorkbook()
results <- list(gen_stats, normality, t_test_results, anova_pre_results, anova_post_results, anova_results_majors)
names(results) <- c("gen_stats", "normality", "t_test_results", "anova_pre_results", "anova_post_results", "anova_results_majors")
walk2(names(results), results, ~ { addWorksheet(wb, .x); writeData(wb, .x, .y) })
saveWorkbook(wb, "./Results/attitudes_stats.xlsx", overwrite = TRUE)
