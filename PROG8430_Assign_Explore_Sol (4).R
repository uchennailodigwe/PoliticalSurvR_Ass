##################################################
### PROG8430                                    ##
### Demonstrates some summarization             ## 
##################################################
#                                               ##
##################################################
# Written by David Marsh
# ID: 123456789
#
##################################################
### Basic Set Up                                ##
##################################################

# Clear plots
if(!is.null(dev.list())) dev.off()

# Clear console
cat("\014") 

# Clean workspace
rm(list=ls())

#Set work directory to an appropriate location
setwd("C:/Users/Uchenna/Desktop/Conestoga/DataAnalysis/WorkDirectory")

#options(scipen=12)

##################################################
### Remove Packages Installed                   ##
##################################################

##################################################
### Install Libraries                           ##
##################################################
if(!require(lattice)){install.packages("lattice")}
library("lattice")           #"attaches" package 

if(!require(HSAUR)){install.packages("HSAUR")}
library("HSAUR")           #"attaches" package 
if(!require(pastecs)){install.packages("pastecs")}
library("pastecs")           #"attaches" package 
if(!require(ggplot2)){install.packages("ggplot2")}
library("ggplot2")           #"attaches" package 
#If the library is not already downloaded, download it

  
##################################################
### Read in Data                                ##
##################################################

#Two Ways to read in Data

#Comma Seperated Values

Elect2008 <- read.csv("Elect2008.csv", header = TRUE, sep = ",")

#R dataset

load("PROG8430_Assign_Explore-21F.Rdata")
AssignExplore_UI <-get(load("PROG8430_Assign_Explore-21F.Rdata"))

str(AssignExplore_UI)

head(AssignExplore_UI,10)

##################################################
### Measures of Central Tendency                ##
##################################################

mean(Elect08$Obama)

mean(Elect08$Obama, trim=0.1)

median(Elect08$Obama)

weighted.mean(Elect08$Obama, w=Elect08$Population)

##################################################
### Measures of Dispersion                      ##
##################################################

var(Elect08$Obama)

sd(Elect08$Obama)

mad(Elect08$Obama)

IQR(Elect08$Obama)

range(Elect08$Obama)

quantile(Elect08$Obama) 

quantile(Elect08$Obama, c(.32, .57, .98)) 

summary(Elect08$Obama)

##################################################
### Create Tables                               ##
##################################################

#Create a Variable to see if Obama won a State

Elect08$Win <- "M"
Elect08$Win <- with(Elect08, ifelse(Obama > McCain, "O", Win))
str(Elect08)
Elect08$Win <- as.factor(Elect08$Win)
str(Elect08)



#Frequency Table

Table2 <- table(Elect08$Region, Elect08$Win)
Table2
margin.table(Table2, 1) # A frequencies (summed over B)
margin.table(Table2, 2) # B frequencies (summed over A)

prop.table(Table2) # cell percentages
prop.table(Table2, 1) # row percentages
prop.table(Table2, 2) # column percentages

#States Obama won above a certain amount

Elect08[Elect08$Obama > 65, c("State", "Unemployment", "Income", "Obama")]

#Aggregate Percentage Won by Region

Table1 <- aggregate(Elect08[,3:4], by=list(Elect08$Region), FUN=mean, na.rm=TRUE)
print (Table1, digits = 4)

A1 <- aggregate(Elect08[,3:5], by=list(Elect08$Region), FUN=mean, na.rm=TRUE)
A1




############################################################################################
-

##############################QUESTION 1#################################################################3
colnames(AssignExplore_UI)[7] <-"m.status_UI" 
colnames(AssignExplore_UI)[10] <-"income_UI" 
income_Sum_UI <- aggregate(AssignExplore_UI$income_UI, by=list(AssignExplore_UI$m.status_UI), FUN=sum, na.rm=TRUE)
print(income_Sum_UI)
str(income_Sum_UI)

###############################QUESTION1B###########################################
colnames(income_Sum_UI) <- c("Marital_Status_UI", "Income_UI")
max(income_Sum_UI$Income_UI)
###############################QUESTION1.2.A##############################################
colnames(AssignExplore_UI)[4] <-"nation_UI" 
colnames(AssignExplore_UI)[6] <-"age_UI" 
Asiadf_UI <- AssignExplore_UI[(AssignExplore_UI$nation_UI %in% c("Asia")), ]
str(Asiadf_UI)

meanAge_UI <- mean(Asiadf_UI$age_UI)
print(meanAge_UI,digits = 4)

#########################################

Mean_Age_By_Nation_UI <- aggregate(AssignExplore_UI$age_UI, by=list(AssignExplore_UI$nation_UI), FUN=mean, na.rm=TRUE)
print(Mean_Age_By_Nation_UI, digits = 4)
################################# QUESTION1.2.B#########################################################
colnames(Asiadf_UI)[9] <-"n.child_UI"
weighted_mean_UI <-weighted.mean(Asiadf_UI$age_UI, w=Asiadf_UI$n.child_UI) #############40.61
print(weighted_mean_UI,digits = 4)



########################################QUESTION1.3 #######################################3333

colnames(AssignExplore_UI)[5] <-"gender_UI" 
colnames(AssignExplore_UI)[14] <-"score_UI" 
mean_Score_By_Gender_UI <- aggregate(AssignExplore_UI$score_UI, by=list(AssignExplore_UI$gender_UI), FUN=mean, na.rm=TRUE)
print(mean_Score_By_Gender_UI, digits = 4)



########################################QUESTION1.3b #######################################3333
colnames(mean_Score_By_Gender_UI)[2] <-"x_UI" 

max(mean_Score_By_Gender_UI$x_UI)

#############################################QUESTION1.4########################################

colnames(AssignExplore_UI)[16] <-"time_UI" 

quantile(AssignExplore_UI$time_UI, c(.34, .63))




############################################QUESTION 2.1######################################



colnames(AssignExplore_UI)[8] <-"political_UI"

library(plyr)
pie_set_UI <- data.frame(table(AssignExplore_UI$political_UI))
colnames(pie_set_UI)[1] <-"political_UI"
colnames(pie_set_UI)[2] <-"frequency_UI"
PopSum_UI <- aggregate(pie_set_UI$frequency_UI, by=list(pie_set_UI$political_UI), FUN=sum, na.rm=TRUE)

colnames(PopSum_UI)[1] <-"political_UI"
colnames(PopSum_UI)[2]<- "frequency_UI"
PopSum_UI
Pop_UI <- PopSum_UI$frequency_UI
names(Pop_UI) <- PopSum_UI$political_UI
pie(Pop_UI, main="Total Respondents in Each Political Affiliation")

############################################QUESTION2.1.B################################

max(PopSum_UI$frequency_UI)
 
############################################QUESTION2.1.C################################
min(PopSum_UI$frequency_UI)

############################################QUESTION2.2.A################################

colnames(AssignExplore_UI)[2] <-"group_UI" 
df_UI <- AssignExplore_UI[(AssignExplore_UI$group_UI %in% c("treat")), ]
str(df_UI)
table_UI <- data.frame(table(AssignExplore_UI$nation_UI))
table_UI
colnames(table_UI)[1] <-"nation_UI"
colnames(table_UI)[2] <-"frequency_UI"
table_UI$percentage_UI <- c(0.2349057,0.1443396,0.5155660,0.1051887)
table_UI <- table_UI[-c(2)]      # drop column 2


############################################QUESTION2.2.b################################
max(table_UI$percentage_UI)




############################################QUESTION2.2.c################################
min(table_UI$percentage_UI)


# Load the dplyr library
library("dplyr")


# Count the total number of students.
total_students <- sum(table_UI$frequency_UI)

table_UI$percentage_UI <- table_UI$frequency_UI / total_students




Elect08$Win <- "M"
Elect08$Win <- with(AssignExplore_UI, ifelse(nation_UI.contains("Asia"), "O", Win))
str(AssignExplore_UI)
Elect08$Win <- as.factor(Elect08$Win)
str(AssignExplore_UI)


###################################33. Bar Chart##############################################
colnames(AssignExplore_UI)[15] <-"scr_UI"

Relig_UI <- as.matrix(aggregate(AssignExplore_UI$scr_UI, by=list(AssignExplore_UI$nation_UI), FUN=mean))
head(Relig_UI)
rownames(Relig_UI) <- Relig_UI[,1] 
Relig_UI <- Relig_UI[,-1]
head(Relig_UI)

barplot(t(Relig_UI),
        legend=TRUE,
        main="Mean Standardised test Score by Region", 
        ylab="mean Of Standardised test scores")




###############################3.3.b Bar Chart##############################################

Relig_UI <- aggregate(AssignExplore_UI$scr_UI, by=list(AssignExplore_UI$nation_UI), FUN=mean)
colnames(Relig_UI)[2] <-"x_UI"
min(Relig_UI$x_UI)


###############################3.3.c Bar Chart##############################################

Relig_UI <- aggregate(AssignExplore_UI$scr_UI, by=list(AssignExplore_UI$nation_UI), FUN=mean)
colnames(Relig_UI)[2] <-"x_UI"
max(Relig_UI$x_UI)

################################################3.4 histogram##############################3
colnames(AssignExplore_UI)[12] <-"food_UI"
hist(AssignExplore_UI$food_UI, breaks=5, xlab= "Pct of income to Food" ,main="Percentage of household income for food")

################################################3.4.b histogram##############################3
0 -0.2 has the highest frequency from observationof the histogram bin with the highest frequency.

################################################3.5##############################3


boxplot(income_UI ~ m.status_UI, data=AssignExplore_UI, 
        main="Distribution of income by marital status",
        xlab="m.status_UI",  pch = 20)

################################################3.5b##############################3
b. According to the charts, the married has the highest average income because the box plot representing married martial has the highest top

################################################3.5c##############################3
  c. The divorced marital status has the lowest average income because the bottom of the boxplot can be traced to the lowest income on the y axis
  d. The married marital status has the greatest variability in income because it has the largest difference between the top and bottom of the box plot
  ################################################3.6##############################3
  # Clear plots
  if(!is.null(dev.list())) dev.off()
  hist(AssignExplore_UI$income_UI, dat = AssignExplore_UI, type = "density", xlab = "Income",breaks=5, main="distribution of  household income")
  
  ################################################3.6.b##############################3

  hist(AssignExplore_UI$scr_UI,dat = AssignExplore_UI, type = "density", xlab = "Standard Score", main="distribution of  standardized score")
 
  
  ################################################3.6.c##############################3
  
plot(scr_UI ~ income_UI, data=AssignExplore_UI, col=2, pch=20,
     main="relationship between Income and Standardized Score 2008")

################################################3.6.d##############################3
d. in conclusion, there is a relationship between score and income, because the x and the y co-ordinates seem to be increasing at about the same pace.


################################################3.6.e##############################3
cor.test(AssignExplore_UI$scr_UI, AssignExplore_UI$income_UI)
#Pearson defaults, but assumes normality
#Using Pearson's correlation , cor = 0.4568418 "
  


#######################################4################################################

descwater <-stat.desc(AssignExplore_UI[15:16])
summary(AssignExplore_UI)



Inference
1. Normality
a. Create a QQ Normal plot of the Political Awareness Test Score.
b. Conduct a statistical test for normality on the Political Awareness Test Score.
c. Are the Political Awareness Test Scores normally distributed? What led you to this conclusion?
str(Elect08)

PopSum <- aggregate(Elect08$Population, by=list(Elect08$Region), FUN=sum, na.rm=TRUE)
PopSum
Pop <- PopSum$x
names(Pop) <- PopSum$Group.1 
pie(Pop, main="Total Population in Each Region")

##################################################
### Bar Charts                                  ##
##################################################

#Alaska and Hawaii lack detailed info so remove them



#Percent Catholic by State

#Calculate the summaries
Cath <- tapply(Elect$Catholic, Elect$State, mean)

#Create the bar plot
barplot(Elect$Catholic, 
        main="Pct Catholic by State", 
        ylab="Pct Catholic")

#Religious Affliation by Region

str(Elect)

Relig <- as.matrix(aggregate(Elect[,9:12], by=list(Elect$Region), FUN=mean))
head(Relig)
rownames(Relig) <- Relig[,1] 
Relig <- Relig[,-1]
head(Relig)

barplot(t(Relig),
        legend=TRUE,
        ylim=c(0,130),
        main="Religious Affiliation by Region", 
        ylab="Pct of each Religious Affiliation")

##################################################
### Histograms                                  ##
##################################################

#Histogram of Percent Support 

hist(Elect$Obama, main="Obama Support")

#Forcing Breaks

hist(Elect08$Obama, breaks=10, main="Obama Support")

#Style type = (count, density, percent)

hist(Elect08$Obama, breaks=12, prob=TRUE,
     main="Obama Percent of Support")

##################################################
### Box Plots                                   ##
##################################################

#Box Plot of McCain Support 

boxplot(Elect08$McCain, data=Elect08, main="Distribution of McCain Support",
        xlab="Percent of McCain Support", horizontal=TRUE, pch=20)

#Comparing by Regions

boxplot(McCain ~ Region, data=Elect08, 
        main="Distribution of McCain Support by Region",
        xlab="Regions",  pch = 20)

# Correlation Coefficient

cor.test(Elect$Turnout, Elect$Income)
#Pearson defaults, but assumes normality

cor.test(Elect$Turnout, Elect$Income, method="spearman")

##################################################
### Scatter Plots                               ##
##################################################

# Basic chart
plot(Turnout ~ Income, data=Elect08, main="Turnout by Income, 2008")

#You can change some formating

plot(Turnout ~ Income, data=Elect08, col=2, pch=20,
     main="Turnout by Income, 2008")

