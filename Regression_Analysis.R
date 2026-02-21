
# Student Perfrormance Analysis - Final Project
# R script
# Jonathan Mishayel
# COADDS232F-012
# NIBM | NIC



getwd()

#load dataset
df <- read.csv(file.choose(), header =T)
df

View(df)

#structure of the dataset
str(df)

#checking missing values
sum(is.na(df))

#checking the categories of guradian education variable
table(df$guardian_education)

# Convert the necessary variables to factor
df$guardian_education <- as.factor(df$guardian_education)
df$family_relationship <- as.factor(df$family_relationship)
df$family_size <- as.factor(df$family_size)
df$parent_status <- as.factor(df$parent_status)
df$family_support <- as.factor(df$family_support)

# Regression
# Fit multiple linear regression

objective1 <- lm(final_grade ~ 
                   family_relationship + 
                   guardian_education + 
                   family_size+
                   parent_status+
                   family_support+
                   absences,
                 
                 data = df)

summary(objective1)


#Distribution of Final Grade - Histogram
library(ggplot2)
ggplot(df, aes(x = final_grade)) +
  geom_histogram(bins = 10) +
  labs(title = "Distribution of Final Grade",
       x = "Final Grade",
       y = "Count") +
  theme_minimal()


#Distribution of Final Grade - Density plot
ggplot(df, aes(x = final_grade)) +
  geom_density() +
  labs(title = "Density Plot of Final Grade",
       x = "Final Grade",
       y = "Density") +
  theme_minimal()


#Scatter plot - final grade vs abtenseism
library(ggplot2)
ggplot(df , aes(final_grade,absences))+
  geom_point(color = 'blue')+
  labs(title ='Scatter Plot')


#Distribution of Absences - Histogram
ggplot(df, aes(x = absences)) +
  geom_histogram(bins = 15) +
  labs(title = "Distribution of Absences",
       x = "Number of Absences",
       y = "Count") +
  theme_minimal()


#Final Grade by Guardian Education -Bar Plot
ggplot(df, aes(x = guardian_education, y = final_grade)) +
  geom_boxplot() +
  labs(title = "Final Grade by Guardian Education",
       x = "Guardian Education",
       y = "Final Grade") +
  theme_minimal()










































