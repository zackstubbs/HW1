---
output:
  pdf_document: default
  html_document: default
---
```{r}
#loading library


    library(Hmisc) # Contains many functions useful for data analysis
    library(checkmate) # Fast and Versatile Argument Checks
    library(corrr) # Correlations in R
    library(conflicted) # Makes it easier to handle same named functions that are in different packages
    library(readxl) # reading in Excel files
    library(dplyr) # data manipulation
    library(tidyr) # Tidy Messy Data
    library(ggplot2) # data visualization
    library(knitr) # knitting data into HTML, Word, or PDF
    library(evaluate) # Parsing and Evaluation Tools that Provide More Details than the Default
    library(iopsych) # Methods for Industrial/Organizational Psychology
    library(psych) # Procedures for Psychological, Psychometric, and Personality Research
    library(quantreg) # Quantile Regression
    library(lavaan) # confirmatory factor analysis (CFA) and structural equation modeling (SEM)
    library(xtable) # Export Tables to LaTeX or HTML
    library(reshape2) # transforming data between wide and long (tall)
    library(GPArotation) # GPA Factor Rotation
    library(Amelia) # A Program for Missing Data
    # library(esquisse) # Explore and Visualize Your Data Interactively
    library(expss) # Tables, Labels and Some Useful Functions from Spreadsheets and 'SPSS' Statistics
    library(multilevel) # Multilevel Functions
    library(janitor) # 	Simple Tools for Examining and Cleaning Dirty Data
    library(mice) # Multivariate Imputation by Chained Equations
    library(skimr) # Exploratory Data Analysis
    library(lmtest) # A collection of tests, data sets, and examples for diagnostic checking in linear regression models    
    library(tidylog) # Creates a log to tell you what your tidyverse commands are doing to the data. NOTE: MAKE SURE TO ALWAYS LOAD LAST!!!
```


```{r}
#' <!-- #Loading from GitHub -->
#' <!-- #pacman::p_load_current_gh("trinker/lexicon", "trinker/sentimentr") -->
```


## Uploading SAQ.sav file 
```{r}
#loading data 
#install haven for importing and export SPSS, stata, and SAS files 
library(haven)
dataset<-read_sav("SAQ.sav")
View(dataset)
str(dataset) #return list of objects to their structure 
```

```{r}
glimpse(dataset) #from dply 
```
```{r}
#Column names 
colnames(dataset)
```
```{r}
#Wrap column names 
dput(colnames(dataset))
```

##Checking for missing data 
```{r}
#use Amelia for examine missing data and it was loaded in first chunk

missmap(dataset)
```

##Double checking for missing data
```{r}
percentmissing = function (x){ sum(is.na(x))/length(x) * 100}

missing <- apply(dataset, 1, percentmissing) 
# we will use an apply function to loop it. 1 indicates rows and 2 indicates columns

table(round(missing, 1))
```
-After analyzing for missing data there is no missing data. 

##Checking columns 
```{r}
#reminder 2=columns 

apply(dataset, 2, percentmissing)
```

-After conducting missing data analysis using multiple functions, there is no missing data. 

##Using Mahalonobis Distance to detect outliers in the data

```{r}
datasetmahal<-dataset[,1:23] #created new dataset for mahalanobis while examining the 23 relevant questions in the dataset 
cutoff=qchisq(1-0.001, ncol(datasetmahal[,1:23]))
mahal=mahalanobis(datasetmahal[,1:23], 
colMeans(datasetmahal[,1:23]),
cov(datasetmahal[,1:23]))

#cutoff score 
cutoff
ncol(datasetmahal)
summary(mahal<cutoff)
#False is not good. 
```
##Arranging Outliers in dataset

```{r,error=TRUE}
#error=TRUE is allowing me to overcome errors in knitting. 
datasetmahal<-datasetmahal %>% bind_cols(mahal)%>%rename(mahal23= `...24`)

#names new column mahal
#received a conflict error for renaming. I selected tidylog to "win" any renaming conflicts 
```

##Examining the 97 labeled False 
```{r,error=TRUE}
mahal_outliers <-datasetmahal %>%filter(mahal>cutoff)%>%
  arrange(desc(mahal23)) #most to least 
```
-It appears that these 97 outliers consist of answering extreme values or certain patterns.

-I created a new data set consisting only of the 23 questions. 
```{r}
#created a new dataset with only the 23 variables. The items beyond question 23 did not seem like they were part of the scale. 
dataset23<-dataset[,1:23]
```

#Removing Outliers 
## I removed the 97 outliers. 
```{r,error=TRUE}
#created a new dataset with outliers removed 
nooutliers23<-data.frame(dataset23) %>%
  filter(mahal<cutoff)
```

##Checking additivity 
```{r}
correl=cor(nooutliers23,use='pairwise.complete.obs')
symnum(correl)
correl
#looking for ones outside of the diaganol. 
```
-I do not see any "1s" off of the diagnal, therefore additivity is adequate. 

#Looking at the rest of the assumptions 
```{r}
#assumption set up, chisq
#We must use a fake regression analysis because, while EFA is regression to the extreme, you still have to screen it with a regular regression analysis. 
random=rchisq(nrow(nooutliers23),7)
fake=lm(random~.,data=nooutliers23)
standardized=rstudent(fake)
fitted=scale(fake$fitted.values)
#the chisq value can be anything larger than 2
```

-Check residual with histogram 
```{r}
#normality 
hist(standardized)
```
-This histogram looks pretty normal. It's not perfect as it skews left but it's not extreme. I think it indicates the data is adequate. 

#Testing for Heteroscedscity using Breusch pagan test 
```{r}
#load lmtest 
library(lmtest)

#Breusch-pagan test
bptest(fake)
```
-The BP=23.369 and the p-value=0.4394, which is not less than 0.05. We fail to reject the null hypothesis. There is not evidence of heteroscedascity. 

##Use Q-Q plot to test linearity 
```{r}
#linearity 
qqnorm(standardized)
abline(0,1)
```
-The data appears slightly u shaped but it's not extreme. I do think it's adequate to move forward. 

## Test for homogeneity 

```{r}
#homogeneity 
plot(fitted,standardized)
abline(0,0)
abline(v=0)
```
-While there are some issues. Overall, it appears normal-ish. I do think we can move forward.

## Bartlett's test to examine correlation adequacy 
```{r}
cortest.bartlett(correl, n=nrow(nooutliers23))
```
Significant so we will run a KMO test 
## Kaiser, Meyer, Olkin Measure of Sampling Adequacy (KMO) Test
```{r}
#sampling adqueacy using kmo 
KMO(correl[,1:23])
```
The overall score is 0.93, which indicates we have adequate sampling. 

##HW Question 1: 
-I do believe the data is adequate to conduct EFA. There were 97 outliers that were removed based on the cutoff for Mahalonobis Distance. Many of these items were extrememe responses of specific patterns. According to the Q-Q plot the data has a slight U-shape, but it does not appear extreme. I believe it is linear. It is important to note than we tested for homogeneity it appears the data is not evenly distributed which could be problematic. This bartlett's test revealed a significant chisq=19534, so a KMO test was conducted. The overall MSA=0.93 is adequate and significant. So using the above analysis the data was adequate for an EFA. 

#Exploratory Factor Analysis 
Set your seed to maintain random constant 

```{r}
#setseed 
set.seed(2023)
```

##Created ID variables  
```{r}
nooutliers23<-nooutliers23 %>% 
  mutate(ID=row_number())

nooutliers23<-nooutliers23 %>%
  dplyr::select(ID, everything())
```

#Splitting data 50/50 

```{r}
#Use training data going forwarward.
training <- sample(nooutliers23$ID, length(nooutliers23$ID)*0.5)

nooutliers23_training <- subset(nooutliers23, ID %in% training)
nooutliers23_test <- subset(nooutliers23, !(ID %in% training))
```
-The data set is large, therefor I chose to split the data 50/50 which provided a robust sample from which to conduct the EFA

#2. Histograms 
-used splitdata 

```{r}
#use splitdaa, nooutliers23_training
hist(nooutliers23_training$Question_01,breaks=6)
hist(nooutliers23_training$Question_02,breaks=6)
hist(nooutliers23_training$Question_03,breaks=6)
hist(nooutliers23_training$Question_04,breaks=6)
hist(nooutliers23_training$Question_05,breaks=6)
hist(nooutliers23_training$Question_06,breaks=6)
hist(nooutliers23_training$Question_07,breaks=6)
hist(nooutliers23_training$Question_08,breaks=6)
hist(nooutliers23_training$Question_09,breaks=6)
hist(nooutliers23_training$Question_10,breaks=6)
hist(nooutliers23_training$Question_11,breaks=6)
hist(nooutliers23_training$Question_12,breaks=6)
hist(nooutliers23_training$Question_13,breaks=6)
hist(nooutliers23_training$Question_14,breaks=6)
hist(nooutliers23_training$Question_15,breaks=6)
hist(nooutliers23_training$Question_16,breaks=6)
hist(nooutliers23_training$Question_17,breaks=6)
hist(nooutliers23_training$Question_18,breaks=6)
hist(nooutliers23_training$Question_19,breaks=6)
hist(nooutliers23_training$Question_20,breaks=6)
hist(nooutliers23_training$Question_21,breaks=6)
hist(nooutliers23_training$Question_22,breaks = 6)
hist(nooutliers23_training$Question_23,breaks=6)

```
#Correlation Matrix
```{r}
#use corrr package 
library(corrr)

Cor_Mat <- nooutliers23_training %>%
    correlate() %>% 
    shave() %>% # Remove upper triangle
    fashion() # Print in nice format

Flat_Cor_Mat<-nooutliers23_training %>%
    correlate() %>% 
    shave() %>% # Remove upper triangle
    fashion()

print(Flat_Cor_Mat)
```
#Simplified Matrix
```{r}
#flattercorrmatrix function
flattenCorrMatrix <- function(cormat, pmat, nmat) {
    ut <- upper.tri(cormat)
    data.frame(
        row = rownames(cormat)[row(cormat)[ut]],
        column = rownames(cormat)[col(cormat)[ut]],
        cor  =(cormat)[ut],
        p = pmat[ut],
        n = nmat[ut]
    )
}
```
-Use Hmisc package 
```{r}
library(Hmisc)
```
-Convert nooutliers23_training into matrix. 
```{r}
nooutliers23_training_MAT<-as.matrix(nooutliers23_training)
```

```{r}
library(checkmate) #activate checkmate
res <- rcorr(nooutliers23_training_MAT)
print(res)
```


```{r}
library(dplyr)
nooutliers23_Flat_Cor_Mat <- flattenCorrMatrix(res$r, res$P, res$n) #these p values match SPSS

nooutliers23_Flat_Cor_Mat[,3:5] <- round(nooutliers23_Flat_Cor_Mat[,3:5], 3)

#Highligting any significant correlation, p<0.05
nooutliers23_Flat_Cor_Mat <- nooutliers23_Flat_Cor_Mat %>%
    mutate(Sig = ifelse(p < 0.05, paste0(p, "*"),
           p))

nooutliers23_Flat_Cor_Mat
```


#EFA 
-parallel anaylysis 

```{r}
#don't use id column 
#activate psych package if needed 
fa.parallel(nooutliers23_training[c(2:24)])
```

-The initial parallel analysis recommends 6 factors. I will move forward using maximum likelihood and starting with 4 factors. Here is the key for creating a new variable for the factor analysis. 
fa=Factor Analysis 
ml=Maximum Likelihood
4=number of factors
trn=training data 

```{r}
fa_ml_4_trn<-fa(nooutliers23_training[c(2:24)],nfactors=4, fm="ml")
print(fa_ml_4_trn)
```

-Removing items that don't meet cutoff of .3
```{r}
print(fa_ml_4_trn$loadings, cutoff=.3)
```
-Rotate using promax 
```{r}
fa_ml_4_trn <- fa(nooutliers23_training[c(2:24)], nfactors = 4, fm="ml", rotate="promax")

print(fa_ml_4_trn)

print(fa_ml_4_trn$loadings, cutoff = .3)
```
-This first model has multiple crossloadings.

-As we aim for parsimony I do want to try a 5 factor model 
```{r}
fa_ml_5_trn<-fa(nooutliers23_training[c(2:24)],nfactors=5, fm="ml", rotate = "promax")
print(fa_ml_5_trn)
print(fa_ml_5_trn$loadings, cutoff=.3)
```
-Dropped question 3 and then returned to the models 

```{r}
nooutliers23_training_MOD <- nooutliers23_training %>%
    dplyr::select(-c(Question_03))
```
-returned to the 4 factor model without question 3 
```{r}
fa_ml_4_trn_MOD <- fa(nooutliers23_training_MOD[c(2:23)], nfactors = 4, fm="ml", rotate="promax") # make sure the [2:XX] reflects the correct columns after removing items

print(fa_ml_4_trn_MOD)

print(fa_ml_4_trn_MOD$loadings, cutoff = .3)
```
-Problems now arrive with Question 6 crossloading and question 19 with a negative value, so tried the 5 factor model without question 3
```{r}
fa_ml_5_trn_MOD <- fa(nooutliers23_training_MOD[c(2:23)], nfactors = 5, fm="ml", rotate="promax") # make sure the [2:XX] reflects the correct columns after removing items

print(fa_ml_5_trn_MOD)

print(fa_ml_5_trn_MOD$loadings, cutoff = .3)
```
-This is problematic as factor ML3 only has 2 items.
-Removed 2 more items, Q6 and Q19 and return back to a 4 factor model. 
```{r}
nooutliers23_training_MOD <-nooutliers23_training %>%
    dplyr::select(-c(Question_03,Question_06, Question_19))
```
-4 factor model without question 3, 6, and 19
```{r}
fa_ml_4_trn_MOD1 <- fa(nooutliers23_training_MOD[c(2:21)], nfactors = 4, fm="ml", rotate="promax") # make sure the [2:XX] reflects the correct columns after removing items

print(fa_ml_4_trn_MOD1)

print(fa_ml_4_trn_MOD1$loadings, cutoff = .3)
```
-This model is problematic as Factor ML4 only has two items loading

-5 factor model without question 3,6,and 19
```{r}
fa_ml_5_trn_MOD1 <- fa(nooutliers23_training_MOD[c(2:21)], nfactors = 5, fm="ml", rotate="promax") # make sure the [2:XX] reflects the correct columns after removing items

print(fa_ml_5_trn_MOD1)

print(fa_ml_5_trn_MOD1$loadings, cutoff = .3)
```
-This model is problematic as Factor ML 4 still has 2 items
-I am going to remove another item, question 15, as it seems redundant to question 14 and has a lower loading than 14. 
```{r}
nooutliers23_training_MOD <-nooutliers23_training %>%
    dplyr::select(-c(Question_03,Question_06, Question_19, Question_15))
```
-I'm returning to a 4 factor model now withouth Questions 3, 6, 19, and 15. 
```{r}
fa_ml_4_trn_MOD2 <- fa(nooutliers23_training_MOD[c(2:19)], nfactors = 4, fm="ml", rotate="promax") # make sure the [2:XX] reflects the correct columns after removing items

print(fa_ml_4_trn_MOD2)

print(fa_ml_4_trn_MOD2$loadings, cutoff = .3)
```
-Question 4 is also crossloading. I'm going to remove that item. 

```{r}
nooutliers23_training_MOD <-nooutliers23_training %>%
    dplyr::select(-c(Question_03,Question_06, Question_19, Question_15, Question_04))
```
-4 factor model without question 3,4,6,15,and19

```{r}
fa_ml_4_trn_MOD3 <- fa(nooutliers23_training_MOD[c(2:18)], nfactors = 4, fm="ml", rotate="promax") # make sure the [2:XX] reflects the correct columns after removing items

print(fa_ml_4_trn_MOD3)

print(fa_ml_4_trn_MOD3$loadings, cutoff = .3)
```

```{r}
fa_ml_5_trn_MOD3 <- fa(nooutliers23_training_MOD[c(2:18)], nfactors = 5, fm="ml", rotate="promax") # make sure the [2:XX] reflects the correct columns after removing items

print(fa_ml_5_trn_MOD3)

print(fa_ml_5_trn_MOD3$loadings, cutoff = .3)
```
I am not satisfied with my results with 4 or 5 factor models. I'm going to try a 3 factor model, starting with nooutliers23training data set. 
```{r}
fa_ml_3_trn<-fa(nooutliers23_training[c(2:24)],nfactors=3, rotate="promax", fm="ml")
print(fa_ml_3_trn)
print(fa_ml_3_trn$loadings, cutoff = .3)
```
-Question 12 is crossloading, so what if we remove that item? 

```{r}
nooutliers23_training_3f <-nooutliers23_training %>%
    dplyr::select(-c(Question_12))
```

```{r}
fa_ml_3_trn1<-fa(nooutliers23_training_3f[c(2:23)],nfactors=3, rotate = "promax", fm="ml")
print(fa_ml_3_trn1)
print(fa_ml_3_trn1$loadings, cutoff = .3)
```
-So far I am pleased with a 3 factor model, but items 22 and 23 are not loading in any factor so I am going to remove them from the data set and run the analysis again. 
```{r}
nooutliers23_training_3f <-nooutliers23_training %>%
    dplyr::select(-c(Question_12,Question_22,Question_23))
```

Now let's run the analysis again withouth questions 12,22,23
```{r}
fa_ml_3_trn2<-fa(nooutliers23_training_3f[c(2:21)],nfactors=3, rotate = "promax", fm="ml")
print(fa_ml_3_trn2)
print(fa_ml_3_trn2$loadings, cutoff = .3)
```
Now, Question 1 is crossloading in 2 factors so I am going to remove question 1, and questions 4 and 5  aren't loading so they are removed.
```{r}
nooutliers23_training_3f <-nooutliers23_training %>%
    dplyr::select(-c(Question_01, Question_04, Question_05, Question_12,Question_22,Question_23))
```
-3 factor model withouth questions,1,4,5,12,15
```{r}
fa_ml_3_trn3<-fa(nooutliers23_training_3f[c(2:17)],nfactors=3, rotate = "promax", fm="ml")
print(fa_ml_3_trn3)
print(fa_ml_3_trn3$loadings, cutoff = .3)
```

#I believe I have reached a simple and adequate model with 3 factors and removing questions 1,4,5,12,22,and 23. 
#HWQuestion 4: 
##I used maximum likelihood for all the EFAs conducted attempting 3,4, and 5 factor models. I chose maximum likelihood because the data was relatively normally distributed as was discovered in the initial data analysis. Further, I discovered an article that argued that "maximum likelihood is the best choice because “it allows for the computation of a wide range of indexes of the goodness of fit of the model [and] permits statistical significance testing of factor loadings and correlations among factors and the computation of confidence intervals.” (Fabrigar, et al. 1999).
#HW Question 5: 
## I rotated the factors using promax. I settled on promax as the dataset was large and promax is more interpretible than other roation options. 
#HW Question 6: 
## After conducting a number of analysis attempting 4 and 5 factor models that were continually problematic with crossloadings, I conducted and came to the conclusion that a 3 factor model was sufficient and adequate. I removed items that wer probelmatic which included: Questions 1,4,5,12,22,23. These items were either crossloading or not loading in any factor. Factor 1 is named: Confidence With Computers. Factor 2 is named: Confidence in Math. Factor 3 is named: How others view your statistics skills. 
#HW Question 7:
## The final TLI=0.94. This is adequate as it is above 0.9. 
#HW Question 8: 
## The RMSEA=0.05. This is adequate, as a good RMSEA is less than 0.06. 


#Scale Building 
I am creating dataframe with only the items of interest for scale building and analysis. 
```{r}
#this "cleans" data to include the items of interest, so the items not of interest are removed.
library(dplyr)
antistats_items<-nooutliers23_training %>%
  dplyr::select(-c(ID,Question_01, Question_04, Question_05, Question_12,Question_22,Question_23))
```

-Now we will make keys using the psych package, and prior to doing this we will check for items reverse scoring 

```{r}
#checking for reverse scoring
library(skimr)
skim(antistats_items)
```
-It appears from the histograms and EFA that items 16, 20, and 21 need to be reversed scored. 

-We are only scoring items that made the "cut" of the EFA. Items 1,4,5,12,22&23, will not be included 

```{r}
#use the dataframe created in the skim line of code to build the keys list. These numbers indicate a question/item. F1=Confidence with computers. F2=Confidence in math. F3=how others view your statistic skills. 
# a negative sign indicates reverse scoring items. 
antistats_keys_list<-list(F1=c(3,4,7,9,10,11,14),F2=c(5,8,13),
                    F3=c(1,2,6,-12,15,-16,-17))
```

```{r}
antistats_keys<-make.keys(antistats_items, antistats_keys_list,item.labels = colnames(antistats_items))
#From R help: When scoring items by forming composite scales either from the raw data using scoreItems or from the correlation matrix using cluster.cor, it used to be necessary to create a keys matrix. This is no longer necessary as most of the scoring functions will directly use a keys list. make.keys is just a short cut for creating a keys matrix. The keys matrix is a nvar x nscales matrix of -1,0, 1 and defines the membership for each scale. Items can be specified by location or by name.

```
##Scoring items 
```{r}
#besure your min and max align with scale. 
scores <- scoreItems(antistats_keys, antistats_items, impute = "none", 
                         min = 1, max = 5, digits = 3)

head(scores$scores)

scores_df <- as.data.frame(scores$scores)
# I used the $ designator for a variable within a df the scores variable within the scores df, wrapped it with `as.data.frame` and then made it into a new df called scores_df
```
##Creating Individual Scales for each factor 
```{r}
#F1=Confidence with Computers. F2=Confidence in Math. F3=How others perceive your statistics skills. 
#This allows for more efficient analysis. Double check you have the right questions in the right factor. 
F1<-antistats_items%>%
  dplyr::select(c(Question_06, Question_07, Question_10,Question_13, Question_14, Question_15,Question_18 ))

F2<-antistats_items%>%
  dplyr::select(c(Question_08,Question_11,Question_17))

F3<-antistats_items%>%
  dplyr::select(c(Question_02,Question_03,Question_09,Question_16,Question_19,Question_20, Question_21))
```
## Determining Reliability for each scale
```{r}
#besure to use psych::alpha when trying to discover chronbach's alpha. I found that this code worked and provided the information I needed for all 3 scales. 
psych::alpha(F1)
```

```{r}
psych::alpha(F2)
```

```{r}
psych::alpha(F3, check.keys = "TRUE")
#this automatically reverse scored items that needed to be reversed scored. 
```
##Question 9&10: 
#Confidence with Computers
-Question 6
-Question 7
-Question 10
-Question 13
-Question 14
-Question 15
-Question 18
-Chronbach's alpha=0.83. Yes this is considered adequat for reliability. 

#Confidence in Math 
-Question 8
-Question 11
-Question 17
-Chronbach's alpha=0.83. Yes this is considered adequate for reliability. 
#How other view your statistics skills 
-Question 2
-Question 3
-Question 9
-Question 16
-Question 19
-Question 20
-Question 21
-Chronbach's alpha=0.74. Those this is a weaker score compared to the previous 2 scales, it is adequate and indicated reliability. 





