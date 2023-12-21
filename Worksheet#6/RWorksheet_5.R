

#Basic Statistics
#1. 

Stud <- c(1,2,3,4,5,6,7,8,9,10)
Pre_Test <- c(55,54,47,57,51,61,57,54,63,58)
Post_Test <-c(61,60,56,63,56,63,59,56,62,61)

Student_Score <- data.frame(
  Student = Stud, 
  Pre_Test = Pre_Test, 
  Post_Test =Post_Test
)

Student_Score


#a. Compute the descriptive statistics using different packages (Hmisc and pastecs).
#Write the codes and its result.


library(Hmisc)
summary(Student_Score)
#RESULT
##   Student         Pre_Test       Post_Test    
#Min.   : 1.00   Min.   :47.00   Min.   :56.00  
#1st Qu.: 3.25   1st Qu.:54.00   1st Qu.:56.75  
#Median : 5.50   Median :56.00   Median :60.50  
#Mean   : 5.50   Mean   :55.70   Mean   :59.70  
#3rd Qu.: 7.75   3rd Qu.:57.75   3rd Qu.:61.75  
#Max.   :10.00   Max.   :63.00   Max.   :63.00 




library(pastecs)
stat.desc(Student_Score)

#RESULT


#Student     Pre_Test    Post_Test
#nbr.val      10.0000000  10.00000000  10.00000000
#nbr.null      0.0000000   0.00000000   0.00000000
#nbr.na        0.0000000   0.00000000   0.00000000
#min           1.0000000  47.00000000  56.00000000
#max          10.0000000  63.00000000  63.00000000
#range         9.0000000  16.00000000   7.00000000
#sum          55.0000000 557.00000000 597.00000000
#median        5.5000000  56.00000000  60.50000000
#mean          5.5000000  55.70000000  59.70000000
#SE.mean       0.9574271   1.46855938   0.89504811
#CI.mean.0.95  2.1658506   3.32211213   2.02473948
#var           9.1666667  21.56666667   8.01111111
#std.dev       3.0276504   4.64399254   2.83039063
#coef.var      0.5504819   0.08337509   0.04741023


#2. 


fertilizer_levels <- c(10, 10, 10, 20, 20, 50, 10, 20, 10, 50, 20, 50, 20, 10)
ordered_factor <- ordered(fertilizer_levels)
ordered_factor


#The ordered_factor will have the levels ordered as 10, 20, 50, reflecting the specified order.



#3. 

#a. What is the best way to represent this in R?
  
subjects <- c("l", "n", "n", "i", "l", "l", "n", "n", "i", "l")

subjects_factor <- factor(subjects, levels=c("n", "l", "i"))
subjects_factor





#4. 


state <- c("tas", "sa", "qld", "nsw", "nsw", "nt", "wa", "wa", "qld",
           "vic", "nsw", "vic", "qld", "qld", "sa", "tas", "sa", "nt",
           "wa", "vic", "qld", "nsw", "nsw", "wa", "sa", "act", "nsw",
           "vic", "vic", "act")


#a. Apply the factor function and factor level. Describe the results.


state_factor <- factor(state)

summary(state_factor)
#We can see how many times each state appears in the given sample.



`
state_levels <- c("nsw", "vic", "qld", "wa", "sa", "tas", "nt", "act")

state <- factor(state, levels = state_levels)

summary(state)
#Provide a simple count of occurrences for each level in the order you specified. 


#5. From #4 - continuation:

incomes <- c(60, 49, 40, 61, 64, 60, 59, 54,
             62, 69, 70, 42, 56, 61, 61, 61, 58, 51, 48,
             65, 49, 49, 41, 48, 52, 46, 59, 46, 58, 43)
state <- c("tas", "sa", "qld", "nsw", "nsw", "nt", "wa", "wa", "qld",
           "vic", "nsw", "vic", "qld", "qld", "sa", "tas", "sa", "nt",
           "wa", "vic", "qld", "nsw", "nsw", "wa", "sa", "act", "nsw",
           "vic", "vic", "act")
incmeans <- tapply(incomes, state, mean)
incmeans


#b. Copy the results and interpret.

#act      nsw       nt      qld       sa      tas      vic       wa 
#44.50000 57.33333 55.50000 53.60000 55.00000 60.50000 56.00000 52.25000

#The median income values across different states and territories in Australia are as follows: 
#In the ACT, the median income is $44,500, while in  NSW accountants have a median income of
#$57,333.33. In the NT, the median income is $55,500, and in QLD, accountants have a median income 
#of $53,600. SA reports a median income of $55,000, while in TAS, accountants have a median income 
#of $60,500. In VIC, the median income is $56,000, and accountants in WA earn a median income of $52,250.
#These figures offer insights into the central income tendencies for accountants in each region.



#6. 

stdError <- function(x) sqrt(var(x)/length(x))

incster <- tapply(incomes, state, stdError)

print(incster)


#a. What is the standard error? Write the codes.

mean_incomes <- tapply(incomes, state, mean)


std_incomes <- tapply(incomes, state, sd)

n_incomes <- tapply(incomes, state, length)

stdError <- function(x) sqrt(var(x)/length(x))
incster <- tapply(incomes, state, stdError)

print(incster)



#b. Interpret the result.

#ACT boasts stability at 1.5, while NSW stands strong at 4.31. NT and QLD follow with scores of 4.5
#and 4.11, indicating robust economic activity. SA scores 2.74, TAS registers 0.5, 
#and VIC leads at 5.24, showcasing economic strength. WA maintains a solid standing at 2.66. 
#These scores provide a succinct overview, aiding in targeted interventions and policy considerations.


#7. 


data("Titanic")

print(Titanic)


#a. subset the titatic dataset of those who survived and not survived. Show the codes and its result.

data("Titanic")

no_survived_adult <- as.vector(Titanic[, , "Adult", "No"])
no_survived_child <- as.vector(Titanic[, , "Child", "No"])
yes_survived_adult <- as.vector(Titanic[, , "Adult", "Yes"])
yes_survived_child <- as.vector(Titanic[, , "Child", "Yes"])

cat("Number of Adults who did not survive:", sum(no_survived_adult), "\n")
cat("Number of Children who did not survive:", sum(no_survived_child), "\n")
cat("Number of Adults who survived:", sum(yes_survived_adult), "\n")
cat("Number of Children who survived:", sum(yes_survived_child), "\n")

#RESULT
#Number of Adults who did not survive: 1438 
#Number of Children who did not survive: 52 
#Number of Adults who survived: 654 
#Number of Children who survived: 57


#8. The data sets are about the breast cancer Wisconsin. The samples arrive periodically
#as Dr. Wolberg reports his clinical cases. The database therefore reflects this

#a. describe what is the dataset all about.

#The dataset focuses on women facing breast cancer and involves a survey scale ranging from 1 to 10. 
#This scale is used to assess various characteristics of cell nuclei present in breast cancer, such as clump thickness, size uniformity, shape uniformity, 
#marginal adhesion, epithelial size, bare nucleoli, bland chromatin, normal nucleoli, and mitoses. Each score on the scale reflects the severity or abnormality of the respective characteristic. The dataset aims to capture and analyze these features to gain insights into the nature of breast cancer in the surveyed women.



#d. Compute the descriptive statistics using different packages. Find the values of:
 # d.1 Standard error of the mean for clump thickness.
library(readr)

breastcancer_wisconsin <- read_csv("/cloud/project/Worksheet#6/breastcancer_wisconsin.csv")
View(breastcancer_wisconsin)

clump_thickness_column <- breastcancer_wisconsin$clump_thickness
std_error <- sd(clump_thickness_column) / sqrt(length(clump_thickness_column))

print(std_error)



#d.2 Coefficient of variability for Marginal Adhesion.


marginal_adhesion_column <- breastcancer_wisconsin$marginal_adhesion
coefficient_of_variability <- sd(marginal_adhesion_column) / mean(marginal_adhesion_column) * 100

print(coefficient_of_variability)



#d.3 Number of null values of Bare Nuclei.



bare_nucleoli_column <- breastcancer_wisconsin$bare_nucleoli
null_values_count <- sum(is.na(bare_nucleoli_column))


print(null_values_count)



#d.4 Mean and standard deviation for Bland Chromatin

mean_bland_chromatin <- mean(breastcancer_wisconsin$bland_chromatin, )
sd_bland_chromatin <- sd(breastcancer_wisconsin$bland_chromatin, )

print(paste("Mean:", mean_bland_chromatin))
print(paste("Standard deviation:", sd_bland_chromatin))


#d.5 Confidence interval of the mean for Uniformity of Cell Shape

shape_uniformity <- breastcancer_wisconsin$shape_uniformity

result <- t.test(shape_uniformity)

cat("Mean:", result$estimate, "\n")
cat("95% confidence interval:", result$conf.int[1], result$conf.int[2], "\n")






#d. How many attributes?
  ```{r}

num_attributes <- length(names(breastcancer_wisconsin))
print(num_attributes)




#e. Find the percentage of respondents who are malignant. Interpret the results.



malignant_count <- sum(breastcancer_wisconsin$class == "malignant")
total_count <- nrow(breastcancer_wisconsin)

percentage_malignant <- (malignant_count / total_count) * 100
print(percentage_malignant)



#9. 

install.packages("AppliedPredictiveModeling")
library("AppliedPredictiveModeling")

data("abalone")

head(abalone)
summary(abalone)


library(openxlsx)  

write.xlsx(abalone, file = "abalone.xlsx") 

