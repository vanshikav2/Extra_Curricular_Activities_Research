#Loading the necessary packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(car)
library(knitr)
library(kableExtra)
#Loading the data
data <- read_csv("D:/UTM/STA304H5/Group Project/STA304_TheOutliers_CleanedData.csv")

#Deleting the studentID column and replacing NA values
data[is.na(data)] <- "NA"
data <- data %>% select(-studentID, -lectureSection)

#Split `activityType` and `major` into individual categories
data <- data %>%
  separate_rows(activityType, sep = ",") %>%
  separate_rows(major, sep = ",")

data <- data %>% mutate(activityType = trimws(data$activityType), major = trimws(data$major))



# Descriptive Analysis (What is the most preferred Extra Curricular Activity)
data <- data %>% mutate(activityType = as.factor(activityType))
activity_counts <- data %>%
  count(activityType, name = "Frequency") %>%
  arrange(desc(Frequency))


# Plot distribution of activity types
activity_labels <- c(
  "AAS" = "Athletics and Sports",
  "NEC" = "No Extracurricular Activities",
  "CLU" = "Clubs",
  "LSG" = "Leadership/Student Governments, Councils & Unions",
  "ACS" = "Academic Societies"
)

#Graph Showing the Activity Counts
ggplot(data = activity_counts, aes(x = activityType, y = Frequency, fill = activityType)) +
  geom_bar(stat = "identity", color = "black") +
  labs(
    x = "Activity Type",
    y = "Frequency",
    title = "Distribution of Extracurricular Activity Types"
  ) +
  theme_light() +
  scale_fill_brewer(
    name = "Activity Options:",
    labels = activity_labels,  
    palette = "Set2"  
  ) +
  geom_text(aes(label = Frequency), vjust = -0.3, size = 4)


# -----------------------------
# 2. ANOVA for Major
# -----------------------------
#Convert ActivityType to NumericValues
data <- data %>% mutate(activityTypeNumeric = as.numeric(factor(activityType)))

anova_major <- aov(activityTypeNumeric ~ major, data = data)
#summary(anova_major)

#Q-Q plot of Residuals
residuals <- residuals(anova_major)

qqnorm(residuals, col = "purple", main = "Q-Q Plot of Residuals for ANOVA Model
       for Major")

qqline(residuals, col = "black", lwd = 1)

#Testing for Homogenity of Variances
data <- data %>%
  mutate(major = as.factor(major))
levene_test_major <- leveneTest(activityTypeNumeric ~ major, data = data)
levene_df <- as.data.frame(levene_test_major)

levene_df[is.na(levene_df)] <- ""

kable(levene_df, caption = "Levene Test on The Activity Preference by Major")
#Displaying the Anova Results
anova_summary <- summary(anova_major)[[1]]
anova_df <- data.frame(anova_summary)
anova_df[is.na(anova_df)] <- ""
kable(anova_df, caption = "Anova Test on The Activity Preference by Major")


#Creating labels for the boxplot
activity_lbls <- c(
  "1" = "ACS",  
  "2" = "AAS",
  "3" = "CLU",
  "4" = "LSG",  
  "5" = "NEC"   
)

# Boxplot for Major Influence
ggplot(data, aes(x = major, y = activityTypeNumeric, fill = major)) +
  geom_boxplot() +
  labs(title = "Boxplot of Activity Preferences by Major",
       x = "Major",
       y = "Activity Type") +
  scale_y_continuous(
    breaks = 1:5,
    labels = activity_lbls
  )+
  theme_minimal()


###################
#2. GenderIdentity
##################
gender_data_F <- data %>% filter(genderIdentity == "F") %>% pull(activityTypeNumeric)
gender_data_M <- data %>% filter(genderIdentity == "M") %>% pull(activityTypeNumeric)

data <- data %>%
  mutate(genderIdentity = as.factor(genderIdentity))
shapiro_F <- shapiro.test(gender_data_F)
shapiro_M <- shapiro.test(gender_data_M)

#Shapiro-Test for each Gender and Table Display
shapiro_M_df <- as.data.frame(t(c(shapiro_M$statistic, shapiro_M$p.value)))
colnames(shapiro_M_df) <- c("W statistic", "p-value")
shapiro_F_df <- as.data.frame(t(c(shapiro_F$statistic, shapiro_F$p.value)))
colnames(shapiro_F_df) <- c("W statistic", "p-value")


shapiro_results <- data.frame(
  Gender = c("Males", "Females"),
  `W Statistic` = c(shapiro_F_df$`W statistic`, shapiro_M_df$`W statistic`),
  `p-value` = c(shapiro_F_df$`p-value`, shapiro_M_df$`p-value`)
)

kable(shapiro_results, col.names = c("Gender", "W Statistic", "p-value"),
      caption = "Shapiro-Wilk Test Results for Activity Preferences by Gender Groups")

#Levene Test for Gender and Table Display
levene_gender <- leveneTest(activityTypeNumeric ~ genderIdentity, data = data)

levene_gender_df <- as.data.frame(levene_gender)
levene_gender_df[is.na(levene_gender_df)] <- ""

kable(levene_gender_df, caption = "Levene Test for Equality of Variances in
      Activity Preferences by Gender Identity")

#T-test on Gender
t_test_gender <- t.test(activityTypeNumeric ~ genderIdentity, data = data, var.equal = FALSE)


t_test_result_df <- data.frame(T_Statistic = t_test_gender$statistic, DF = t_test_gender$parameter, p_value = t_test_gender$p.value, MEAN_FEMALE = t_test_gender$estimate[1], MEAN_MALE = t_test_gender$estimate[2])

kable(t_test_result_df, caption = "Welch t-test on the Activity Proportion by Gender")


# Boxplot for Gender Influence
ggplot(data, aes(x = genderIdentity, y = activityTypeNumeric, fill = genderIdentity)) +
  geom_boxplot() +
  labs(title = "Boxplot of Activity Preferences by Gender",
       x = "Gender Identity",
       y = "Activity Type (Numeric)") +
  scale_y_continuous(
    breaks = 1:5,
    labels = activity_lbls)+
  theme_minimal()

###############
##StudentStatus
###############
status_data_D <- data %>% filter(studentStatus == "D") %>% pull(activityTypeNumeric)
status_data_I <- data %>% filter(studentStatus == "I") %>% pull(activityTypeNumeric)

#Shapiro-test on studentstatus and tabledisplay
shapiro_D <- shapiro.test(status_data_D)
shapiro_I <- shapiro.test(status_data_I)

shapiro_results <- data.frame(
  StudentStatus = c("D", "I"),
  W_Statistic = c(shapiro_D$statistic, shapiro_I$statistic),
  P_Value = c(shapiro_D$p.value, shapiro_I$p.value)
)
kable(shapiro_results, caption = "Shapiro Test on studentStatus")


#Levene Test on studentstatus and table display
data <- data %>%
  mutate(studentStatus = as.factor(studentStatus))
levene_status <- leveneTest(activityTypeNumeric ~ studentStatus, data = data)

levene_status_df <- as.data.frame(levene_status)
levene_status_df[is.na(levene_status_df)] <- ""

kable(levene_status_df, caption = "Levene Test on studentStatus")


#Wilcox Test
wilcox_test <- wilcox.test(activityTypeNumeric ~ studentStatus, data = data)

wilcox_test_df <- data.frame(
  Statistic = wilcox_test$statistic,
  P_Value = wilcox_test$p.value)

kable(
  wilcox_test_df,
  col.names = c("W Statistic", "P-Value"),
  caption = "Wilcoxon Rank-Sum Test Results for Activity Preferences by Student Status"
)

#Boxplot on Activity Preference and studentStatus
ggplot(data, aes(x = studentStatus, y = activityTypeNumeric, fill = studentStatus)) +
  geom_boxplot() +
  labs(title = "Boxplot of Activity Preferences by Student Status",
       x = "Student Status",
       y = "Activity Type (Numeric)") +
  scale_y_continuous(
    breaks = 1:5,
    labels = activity_lbls)+
  theme_minimal()



