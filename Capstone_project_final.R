  library(rpart)
  library(rpart.plot)
  library(kernlab)
  library(readr)
  library(ROSE)
  library(DMwR)
  library(ggthemes)
  library(ROCR)
  library(grid)
  library(dplyr)
  library(scales)
  library(gridExtra)
  library(data.table)
  library(tidyr)
  library(broom)
  library(Hmisc)
  library(ROCR)
  library(caret)
  library(caTools)
  library(dummies)
  library(MASS)
  library(mlbench)
  library(cowplot)
  library(ggplot2)
  
#  setwd("E:/Big data Projects/Project 9 - Capstone Project/Data Provided")
  
  
  ## Loading Credit bureau and Demographic data and Credit Data
  demograpic_data <- read.csv("Demographic data.csv",header = TRUE, stringsAsFactors = FALSE,na.strings = c(""," ","NA",NA))
  credit_data <- read.csv("Credit Bureau data.csv", header = TRUE, stringsAsFactors = FALSE,na.strings = c(""," ","NA",NA))
  
  ## Checking the summary of both datasets
  demograpic_data
  
  ## Checking structure of dataset
  str(demograpic_data)
  
  ## Checking missing values count in each column of the bothe dataframes
  sapply(demograpic_data, function(x) sum(is.na(x)))
  
  
  ## Checking the Application ID is Unique in the 2 dataframes
  dup_credit_data <- credit_data[which(duplicated(credit_data$Application.ID)), ] 
  dup_demograpic_data <- demograpic_data[which(duplicated(demograpic_data$Application.ID)),]
  
  
  # Found 3 records as such and there duplicate applicant id's are 765011468,653287861,671989187
  
  ## Removing the duplicates in the bot the dataframes.
  credit_data <- credit_data[-which(duplicated(credit_data$Application.ID)), ] 
  demograpic_data <- demograpic_data[-which(duplicated(demograpic_data$Application.ID)),]
  
  
  
  ## before merging the data sets need to check whether all the applicants id presents in both the datasets or not
  setdiff(credit_data$Application.ID,demograpic_data$Application.ID) #integer(0)
  
  ## Found 1425 has the NA values in the Performance.Tag and will treat this set as the Real data set for prediction by treating as the 
  ## validating data set 2.Here i am treating the the validattion set 1 for hyper tuning and cross-validation of the model and validating data set2
  ## as the real time data set
  
  demograpic_data_validation <- demograpic_data[which(is.na(demograpic_data$Performance.Tag)), ]
  credit_data_validation <- credit_data[which(is.na(credit_data$Performance.Tag)), ]
  demograpic_data <- demograpic_data[-which(is.na(demograpic_data$Performance.Tag)), ]
  credit_data <- credit_data[-which(is.na(credit_data$Performance.Tag)), ]
  validation_dataset2 <- merge(demograpic_data_validation,credit_data_validation,by.x = "Application.ID",by.y = "Application.ID", all=FALSE)
  
  
  ## CONVERTING THE CHR TO FACTORS
  demograpic_data$Gender <- as.factor(demograpic_data$Gender)
  demograpic_data$Marital.Status..at.the.time.of.application. <- as.factor(demograpic_data$Marital.Status..at.the.time.of.application. )
  demograpic_data$Education <- as.factor(demograpic_data$Education)
  demograpic_data$Profession <- as.factor(demograpic_data$Profession)
  demograpic_data$Type.of.residence <- as.factor(demograpic_data$Type.of.residence)
  
  
  ##performing the KNN IMPUTAtion to fill the missing values
  #library(DMwR)
  #demograpic_data <- knnImputation(demograpic_data)
  
  
  ## Checking missing values count in each column after doing the knn imputation
  sapply(demograpic_data, function(x) sum(is.na(x)))
  
  demo_credit_data <- merge(demograpic_data,credit_data, by.x = "Application.ID",by.y = "Application.ID", all=FALSE)
  
  ## ABLE TO SEE that 2 PERFORMANCE TAGS AND WILL KEEP ONLY ONE BUT BEFORE DOING THAT LETS CHECK DOES THE PERFORMANE TAGS SHOWS DIFFERENT IN THE TWO DATASETS
  demo_credit_data$PERFOMANCE_DIF <- demo_credit_data$Performance.Tag.x - demo_credit_data$Performance.Tag.y
  levels(factor(demo_credit_data$PERFOMANCE_DIF)) # "0"
  
  
  ## FROM THE ABOVE THERE IS NO DIFFERENCE IN THE PERFORMANCE TAGS AND HENCE WE CAN KEEP ANY ONE PERFORMANCE TAG
  demo_credit_data <- demo_credit_data[, -which(names(demo_credit_data) %in% c("Performance.Tag.x","PERFOMANCE_DIF"))]
  
  ##performing the KNN IMPUTAtion to fill the missing values
  
  demo_credit_data <- knnImputation(demo_credit_data)
  
  ## Checking missing values count in each column after doing the knn imputation
  sapply(demo_credit_data, function(x) sum(is.na(x)))
  str(demo_credit_data)
  
  
  master_data <- demo_credit_data
  str(demo_credit_data)
  demo_credit_data <- demo_credit_data[,-c(1)]
  demo_credit_data_y_n <-demo_credit_data
  demo_credit_data_y_n$y_n <-ifelse(demo_credit_data_y_n$Performance.Tag.y==0,"NO","YES")
  
  
  
  bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                     legend.position="none")
  
  plot_grid(ggplot(demo_credit_data_y_n, aes(x=Gender,fill=y_n))+ geom_bar(),
            ggplot(demo_credit_data_y_n, aes(x=Marital.Status..at.the.time.of.application.,fill=y_n))+ geom_bar()+bar_theme1,
            ggplot(demo_credit_data_y_n, aes(x=Education,fill=y_n))+ geom_bar()+bar_theme1,
            ggplot(demo_credit_data_y_n, aes(x=Profession,fill=y_n))+ geom_bar()+bar_theme1,
            ggplot(demo_credit_data_y_n, aes(x=Type.of.residence,fill=y_n))+ geom_bar()+bar_theme1,
            ggplot(demo_credit_data_y_n, aes(x=No.of.dependents,fill=y_n))+ geom_bar()+bar_theme1,
            align = "h") 
  str(demo_credit_data)
  
  #next we will check the outliers based on boxplots and remove the outliers based on them
  plot_grid(ggplot(demo_credit_data_y_n, aes(x="count",y=Age))+ geom_boxplot(width=0.1)+coord_flip() +bar_theme1,#outliers
            ggplot(demo_credit_data_y_n, aes(x="",y=Income))+ geom_boxplot(width=0.1)+coord_flip()+bar_theme1,#no outliers
            ggplot(demo_credit_data_y_n, aes(x="",y=No.of.months.in.current.residence))+ geom_boxplot(width=0.1)+coord_flip()+bar_theme1, #no outliers
            ggplot(demo_credit_data_y_n, aes(x="",y=No.of.months.in.current.company))+ geom_boxplot(width=.5)+coord_flip()+bar_theme1, #outliers
            align = "h") #no outliers
  
  
  #next we will check the outliers based on boxplots and remove the outliers by analysing them
  plot_grid(ggplot(demo_credit_data, aes(x="count",y=No.of.times.90.DPD.or.worse.in.last.6.months))+ geom_boxplot(width=0.1)+coord_flip() +bar_theme1,#no outliers
            ggplot(demo_credit_data, aes(x="",y=No.of.times.60.DPD.or.worse.in.last.6.months))+ geom_boxplot(width=0.1)+coord_flip()+bar_theme1,#no outliers
            ggplot(demo_credit_data, aes(x="",y=No.of.times.30.DPD.or.worse.in.last.6.months))+ geom_boxplot(width=0.1)+coord_flip()+bar_theme1, #no outliers
            ggplot(demo_credit_data, aes(x="",y=No.of.times.90.DPD.or.worse.in.last.12.months))+ geom_boxplot(width=.5)+coord_flip()+bar_theme1,
            ggplot(demo_credit_data, aes(x="",y=No.of.times.60.DPD.or.worse.in.last.12.months))+ geom_boxplot(width=.5)+coord_flip()+bar_theme1,
            ggplot(demo_credit_data, aes(x="",y=No.of.times.30.DPD.or.worse.in.last.12.months))+ geom_boxplot(width=.5)+coord_flip()+bar_theme1,
            align = "h") #no outliers
  
  plot_grid(ggplot(demo_credit_data, aes(x="count",y=Avgas.CC.Utilization.in.last.12.months))+ geom_boxplot(width=0.1)+coord_flip() +bar_theme1,#no outliers
            ggplot(demo_credit_data, aes(x="",y=No.of.trades.opened.in.last.6.months))+ geom_boxplot(width=0.1)+coord_flip()+bar_theme1,#no outliers
            ggplot(demo_credit_data, aes(x="",y=No.of.trades.opened.in.last.12.months))+ geom_boxplot(width=0.1)+coord_flip()+bar_theme1, #no outliers
            ggplot(demo_credit_data, aes(x="",y=No.of.PL.trades.opened.in.last.6.months))+ geom_boxplot(width=.5)+coord_flip()+bar_theme1,
            ggplot(demo_credit_data, aes(x="",y=No.of.PL.trades.opened.in.last.12.months))+ geom_boxplot(width=.5)+coord_flip()+bar_theme1,
            ggplot(demo_credit_data, aes(x="",y=No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.))+ geom_boxplot(width=.5)+coord_flip()+bar_theme1,
            align = "h") #no outliers
  
  plot_grid(ggplot(demo_credit_data, aes(x="count",y=No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.))+ geom_boxplot(width=0.1)+coord_flip() +bar_theme1,#no outliers
            ggplot(demo_credit_data, aes(x="",y=Presence.of.open.home.loan))+ geom_boxplot(width=0.1)+coord_flip()+bar_theme1,#no outliers
            ggplot(demo_credit_data, aes(x="",y=Outstanding.Balance))+ geom_boxplot(width=0.1)+coord_flip()+bar_theme1, #no outliers
            ggplot(demo_credit_data, aes(x="",y=Total.No.of.Trades))+ geom_boxplot(width=.5)+coord_flip()+bar_theme1,
            ggplot(demo_credit_data, aes(x="",y=Presence.of.open.auto.loan))+ geom_boxplot(width=.5)+coord_flip()+bar_theme1,
            align = "h") #no outliers
  
  ##capping the outliers
  
  ###Age
  
  quantile(demo_credit_data$Age,seq(0,1,0.01))
  demo_credit_data$Age <- ifelse(demo_credit_data$Age <15,15,demo_credit_data$Age)
  
  ###Income
  #Even outloiers are not showing in the graph,there are negative values and hence will make -ve value to zeros
  quantile(demo_credit_data$Income,seq(0,1,0.01))
  demo_credit_data$Income <- ifelse(demo_credit_data$Income <0,0,demo_credit_data$Income)
  
  ###No.of.months.in.current.residence
  quantile(demo_credit_data$No.of.months.in.current.residence,seq(0,1,0.01))
  # demograpic_data$No.of.months.in.current.residence <- ifelse(demograpic_data$No.of.months.in.current.residence <0,20,demograpic_data$No.of.months.in.current.residence)
  
  ###No.of.months.in.current.company
  quantile(demo_credit_data$No.of.months.in.current.company,seq(0,1,0.01))
  demo_credit_data$No.of.months.in.current.company <- ifelse(demo_credit_data$No.of.months.in.current.company >74,74,demo_credit_data$No.of.months.in.current.company)
  
  ###No.of.dependents
  quantile(demo_credit_data$No.of.dependents,seq(0,1,0.01))
  # demograpic_data$Age <- ifelse(demograpic_data$Age <27,27,demograpic_data$Age)
  
  ###No.of.trades.opened.in.last.6.months has NA and hence filling with a value
  demo_credit_data$No.of.trades.opened.in.last.6.months <- ifelse(is.na(demo_credit_data$No.of.trades.opened.in.last.6.months),0,demo_credit_data$No.of.trades.opened.in.last.6.months)
  
  #checking the ouliers if any available even after capping them
  plot_grid(ggplot(demo_credit_data, aes(x="count",y=Age))+ geom_boxplot(width=0.1)+coord_flip() +bar_theme1,#outliers
            ggplot(demo_credit_data, aes(x="",y=Income))+ geom_boxplot(width=0.1)+coord_flip()+bar_theme1,#no outliers
            ggplot(demo_credit_data, aes(x="",y=No.of.months.in.current.residence))+ geom_boxplot(width=0.1)+coord_flip()+bar_theme1, #no outliers
            ggplot(demo_credit_data, aes(x="",y=No.of.months.in.current.company))+ geom_boxplot(width=.5)+coord_flip()+bar_theme1,
            align = "h") #no outliers
  
  
  
  #now we will create a Information value table using create_infotables command
  library(Information)
  Inf_Values <- create_infotables(data = demo_credit_data,y = "Performance.Tag.y",parallel = TRUE)
  
  #now we will look at the most important variables 
  
  head(Inf_Values$Summary,10)
  
  #                                                     Variable        IV
  # 18                          Avgas.CC.Utilization.in.last.12.months 0.3228233
  # 20                           No.of.trades.opened.in.last.12.months 0.2979723
  # 22                        No.of.PL.trades.opened.in.last.12.months 0.2958971
  # 24 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. 0.2954176
  # 26                                             Outstanding.Balance 0.2462796
  # 14                    No.of.times.30.DPD.or.worse.in.last.6.months 0.2415512
  # 27                                              Total.No.of.Trades 0.2366296
  # 21                         No.of.PL.trades.opened.in.last.6.months 0.2197272
  # 15                   No.of.times.90.DPD.or.worse.in.last.12.months 0.2138633
  # 13                    No.of.times.60.DPD.or.worse.in.last.6.months 0.2058259
  
  
  
  ##--------------------------------------------------------------------------------------------------------------------------------------##
  ##---------------------------------------------EDA by Imputing the NA Values with WOE and Trating the Outliers -------------------------##
  ##--------------------------------------------------------------------------------------------------------------------------------------##
  
  #Display all the values related to Particular variable] 
  
  print(Inf_Values$Tables$No.of.times.90.DPD.or.worse.in.last.6.months)
  
  # No.of.times.90.DPD.or.worse.in.last.6.months     N   Percent        WOE         IV
  # 1                                        [0,0] 54664 0.7824008 -0.2606781 0.04725916
  # 2                                        [1,3] 15203 0.2175992  0.6224550 0.16010599
  
  print(Inf_Values$Tables$No.of.times.60.DPD.or.worse.in.last.6.months)
  
  # No.of.times.60.DPD.or.worse.in.last.6.months     N   Percent        WOE         IV
  # 1                                        [0,0] 51870 0.7424106 -0.3363664 0.07220016
  # 2                                        [1,5] 17997 0.2575894  0.6225361 0.20582586
  
  
  print(Inf_Values$Tables$No.of.times.30.DPD.or.worse.in.last.6.months)
  
  # No.of.times.30.DPD.or.worse.in.last.6.months     N   Percent        WOE         IV
  # 1                                        [0,0] 50098 0.7170481 -0.3867918 0.09018455
  # 2                                        [1,1]  9500 0.1359726  0.4643187 0.12658538
  # 3                                        [2,7] 10269 0.1469793  0.7428448 0.24155115
  
  
  print(Inf_Values$Tables$No.of.times.90.DPD.or.worse.in.last.12.months)
  
  # No.of.times.90.DPD.or.worse.in.last.12.months     N   Percent        WOE         IV
  # 1                                         [0,0] 50492 0.7226874 -0.3566331 0.07830347
  # 2                                         [1,1] 11663 0.1669315  0.5088234 0.13311253
  # 3                                         [2,5]  7712 0.1103812  0.7219824 0.21386327
  
  
  print(Inf_Values$Tables$No.of.times.60.DPD.or.worse.in.last.12.months)
  
  # No.of.times.60.DPD.or.worse.in.last.12.months     N   Percent        WOE         IV
  # 1                                         [0,0] 45868 0.6565045 -0.3519211 0.06940922
  # 2                                         [1,1] 12816 0.1834342  0.2141538 0.07869700
  # 3                                         [2,7] 11183 0.1600613  0.6940858 0.18548895
  
  
  print(Inf_Values$Tables$No.of.times.30.DPD.or.worse.in.last.12.months)
  
  # No.of.times.30.DPD.or.worse.in.last.12.months     N   Percent        WOE         IV
  # 1                                         [0,0] 44857 0.6420342 -0.3763960 0.07681744
  # 2                                         [1,2] 17590 0.2517641  0.2805525 0.09938446
  # 3                                         [3,9]  7420 0.1062018  0.7994935 0.19824100
  
  
  print(Inf_Values$Tables$Avgas.CC.Utilization.in.last.12.months)
  
  # Avgas.CC.Utilization.in.last.12.months    N    Percent         WOE           IV
  # 1                                      NA 1023 0.01464211  0.11147371 0.0001915246
  # 2                                   [0,4] 5524 0.07906451 -0.80175843 0.0359814516
  # 3                                   [5,6] 5471 0.07830592 -0.80150239 0.0714091162
  # 4                                   [7,8] 6869 0.09831537 -0.79452312 0.1152450134
  # 5                                  [9,11] 9597 0.13736099 -0.67240619 0.1614116074
  # 6                                 [12,14] 6595 0.09439363 -0.46800192 0.1781868927
  # 7                                 [15,21] 6854 0.09810068 -0.07900431 0.1787775352
  # 8                                 [22,37] 7122 0.10193654  0.47504577 0.2074875063
  # 9                                 [38,51] 6746 0.09655488  0.58458900 0.2508658288
  # 10                                [52,71] 7017 0.10043368  0.56373098 0.2924106675
  # 11                               [72,113] 7049 0.10089169  0.38134147 0.3099291653
  
  
  print(Inf_Values$Tables$No.of.trades.opened.in.last.6.months)
  
  # No.of.trades.opened.in.last.6.months     N      Percent        WOE        IV
  # 1                                   NA     1 1.431291e-05  0.0000000 0.0000000
  # 2                                [0,0] 12194 1.745316e-01 -0.6576285 0.0564612
  # 3                                [1,1] 20121 2.879900e-01 -0.4795153 0.1099231
  # 4                                [2,2] 12116 1.734152e-01  0.2328610 0.1203956
  # 5                                [3,3]  9402 1.345700e-01  0.4351239 0.1515994
  # 6                                [4,4]  6297 9.012839e-02  0.5242769 0.1832473
  # 7                               [5,12]  9736 1.393505e-01  0.1368556 0.1860271
  
  
  print(Inf_Values$Tables$No.of.trades.opened.in.last.12.months)
  
  # No.of.trades.opened.in.last.12.months     N    Percent          WOE         IV
  # 1                                 [0,0]  4956 0.07093478 -0.653462151 0.02269765
  # 2                                 [1,1] 11377 0.16283796 -1.019086045 0.13168755
  # 3                                 [2,2]  9323 0.13343925 -0.816468838 0.19394762
  # 4                                 [3,3]  4678 0.06695579  0.003598878 0.19394849
  # 5                                 [4,5]  9397 0.13449840  0.109294271 0.19563796
  # 6                                 [6,7]  8297 0.11875420  0.447981607 0.22500374
  # 7                                 [8,9]  7175 0.10269512  0.571340073 0.26879653
  # 8                               [10,12]  6699 0.09588218  0.491781025 0.29796776
  # 9                               [13,28]  7965 0.11400232  0.006306206 0.29797230
  
  
  print(Inf_Values$Tables$No.of.PL.trades.opened.in.last.6.months)
  
  # No.of.PL.trades.opened.in.last.6.months     N   Percent        WOE        IV
  # 1                                   [0,0] 31080 0.4448452 -0.6492118 0.1407488
  # 2                                   [1,1] 13546 0.1938827  0.1993619 0.1491979
  # 3                                   [2,2] 12565 0.1798417  0.4384356 0.1916027
  # 4                                   [3,6] 12676 0.1814304  0.3619618 0.2197272
  
  
  print(Inf_Values$Tables$No.of.PL.trades.opened.in.last.12.months)
  
  # No.of.PL.trades.opened.in.last.12.months     N    Percent        WOE        IV
  # 1                                    [0,0] 25824 0.36961656 -0.8938108 0.2002061
  # 2                                    [1,1]  6641 0.09505203 -0.1310168 0.2017433
  # 3                                    [2,2]  6830 0.09775717  0.2513399 0.2086806
  # 4                                    [3,3]  8130 0.11636395  0.4122959 0.2326462
  # 5                                    [4,4]  7903 0.11311492  0.5000753 0.2683711
  # 6                                    [5,5]  6189 0.08858259  0.4261494 0.2879895
  # 7                                   [6,12]  8350 0.11951279  0.2431575 0.2958971
  
  
  
  print(Inf_Values$Tables$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)
  
  # No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.     N   Percent         WOE        IV
  # 1                                                          [0,0] 25069 0.3588103 -0.71823049 0.1349630
  # 2                                                          [1,1] 13175 0.1885726  0.17707210 0.1413790
  # 3                                                          [2,2] 12831 0.1836489  0.21609676 0.1508557
  # 4                                                          [3,4] 11506 0.1646843  0.50980053 0.2051600
  # 5                                                         [5,10]  7286 0.1042839  0.01241548 0.2051762
  
  
  
  print(Inf_Values$Tables$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)
  
  # No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.     N    Percent         WOE        IV
  # 1                                                           [0,0] 20581 0.29457398 -1.06753664 0.2122103
  # 2                                                           [1,1]  3899 0.05580603 -0.06177455 0.2124173
  # 3                                                           [2,2]  7907 0.11317217  0.14214469 0.2148588
  # 4                                                           [3,3]  8978 0.12850130  0.16434931 0.2186030
  # 5                                                           [4,4]  7113 0.10180772  0.24810534 0.2256323
  # 6                                                           [5,5]  4927 0.07051970  0.58818059 0.2577593
  # 7                                                           [6,8]  8951 0.12811485  0.48413154 0.2953973
  # 8                                                          [9,20]  7511 0.10750426  0.01370484 0.2954176
  
  
  print(Inf_Values$Tables$Presence.of.open.home.loan)
  
  # Presence.of.open.home.loan     N     Percent         WOE           IV
  # 1                         NA   272 0.003893111 -0.37379739 0.0004599143
  # 2                      [0,0] 51524 0.737458314  0.07370543 0.0046041149
  # 3                      [1,1] 18071 0.258648575 -0.23665793 0.0176193891
  
  
  print(Inf_Values$Tables$Outstanding.Balance)
  
  # Outstanding.Balance    N     Percent        WOE           IV
  # 1                   NA  272 0.003893111 -0.3737974 0.0004599143
  # 2             [0,6843] 6958 0.099589220 -0.7702840 0.0426184656
  # 3         [6847,25509] 6960 0.099617845 -0.9203411 0.0992122830
  # 4       [25522,386813] 6959 0.099603532 -0.1343423 0.1009033611
  # 5      [386815,585402] 6960 0.099617845  0.2542645 0.1081480438
  # 6      [585423,774228] 6960 0.099617845  0.4532364 0.1334257608
  # 7      [774241,972455] 6959 0.099603532  0.4342640 0.1564210664
  # 8     [972456,1357300] 6959 0.099603532  0.4049624 0.1761431837
  # 9    [1357399,2960998] 6960 0.099617845 -0.3824181 0.1884141015
  # 10   [2961005,3282314] 6960 0.099617845 -0.8310260 0.2362772416
  # 11   [3282409,5218801] 6960 0.099617845  0.2958682 0.2462795906
  
  
  
  print(Inf_Values$Tables$Total.No.of.Trades)
  
  # Total.No.of.Trades    N    Percent         WOE         IV
  # 1               [0,1] 3914 0.05602073 -0.67304028 0.01885887
  # 2               [2,2] 6766 0.09684114 -1.01772550 0.08353841
  # 3               [3,3] 8615 0.12330571 -0.70202474 0.12815199
  # 4               [4,4] 7490 0.10720369 -0.44785257 0.14575218
  # 5               [5,5] 5714 0.08178396 -0.04880056 0.14594265
  # 6               [6,6] 4966 0.07107791  0.12930127 0.14720390
  # 7               [7,8] 9361 0.13398314  0.37936559 0.17020640
  # 8              [9,10] 7133 0.10209398  0.54394026 0.20915717
  # 9             [11,19] 8476 0.12131622  0.42717578 0.23616781
  # 10            [20,44] 7432 0.10637354 -0.06689796 0.23662955
  
  
  
  print(Inf_Values$Tables$Presence.of.open.auto.loan)
  
  # Presence.of.open.auto.loan     N    Percent         WOE           IV
  # 1                      [0,0] 63937 0.91512445  0.01198467 0.0001321651
  # 2                      [1,1]  5930 0.08487555 -0.13836752 0.0016580606
  
  
  
  print(Inf_Values$Tables$Age)
  
  # Age    N    Percent          WOE           IV
  # 1  [15,30] 5948 0.08513318 -0.041941665 0.0001469155
  # 2  [31,35] 6927 0.09914552  0.034531539 0.0002670262
  # 3  [36,38] 6924 0.09910258  0.069071901 0.0007550768
  # 4  [39,41] 7129 0.10203673  0.068297625 0.0012461993
  # 5  [42,44] 7007 0.10029055 -0.037941656 0.0013880928
  # 6  [45,47] 6830 0.09775717 -0.003958667 0.0013896219
  # 7  [48,50] 6743 0.09651194 -0.012629305 0.0014049268
  # 8  [51,53] 6841 0.09791461 -0.136905430 0.0031293724
  # 9  [54,57] 7619 0.10905005  0.043405263 0.0033389564
  # 10 [58,65] 7899 0.11305767 -0.010013410 0.0033502407
  
  
  print(Inf_Values$Tables$Gender)
  
  # Gender     N   Percent        WOE           IV
  # 1      F 16506 0.2362489  0.0321743 0.0002481960
  # 2      M 53361 0.7637511 -0.0101473 0.0003264734
  
  
  
  print(Inf_Values$Tables$Marital.Status..at.the.time.of.application.)
  
  # Marital.Status..at.the.time.of.application.     N   Percent          WOE           IV
  # 1                                     Married 59550 0.8523337 -0.004092434 1.424819e-05
  # 2                                      Single 10317 0.1476663  0.023326708 9.546226e-05
  
  
  print(Inf_Values$Tables$No.of.dependents)
  
  # No.of.dependents     N   Percent         WOE           IV
  # 1            [1,1] 15218 0.2178138  0.04008522 0.0003564831
  # 2            [2,3] 15129 0.2165400 -0.08529040 0.0018716125
  # 3          [3,3.4] 15647 0.2239541  0.05402167 0.0025415940
  # 4            [4,4] 11998 0.1717263 -0.02520439 0.0026494350
  # 5            [5,5] 11875 0.1699658  0.00439087 0.0026527185
  
  
  print(Inf_Values$Tables$Income)
  
  # Income    N    Percent         WOE          IV
  # 1  [-0.5,5] 6330 0.09060071  0.30246890 0.009536858
  # 2    [6,10] 6510 0.09317704  0.27575091 0.017587277
  # 3   [11,16] 7923 0.11340118  0.06608894 0.018097848
  # 4   [17,21] 6803 0.09737072  0.08080252 0.018757634
  # 5   [22,26] 6828 0.09772854  0.02506399 0.018819737
  # 6   [27,31] 6817 0.09757110  0.07864867 0.019445483
  # 7   [32,36] 6830 0.09775717 -0.15595501 0.021660491
  # 8   [37,41] 6723 0.09622569 -0.26368117 0.027599526
  # 9   [42,48] 7784 0.11141168 -0.17686352 0.030815758
  # 10  [49,60] 7319 0.10475618 -0.36078566 0.042410776
  
  
  print(Inf_Values$Tables$Education)
  
  # Education     N     Percent          WOE           IV
  # 1     Bachelor 17333 0.248085648  0.016850834 7.099005e-05
  # 2      Masters 23525 0.336711180  0.007039474 8.772938e-05
  # 3       Others   142 0.002032433  0.429585621 5.458848e-04
  # 4          Phd  4483 0.064164770 -0.022839080 5.790069e-04
  # 5 Professional 24384 0.349005968 -0.017931398 6.903078e-04
  
  
  print(Inf_Values$Tables$Profession)
  
  # Profession     N   Percent         WOE           IV
  # 1        SAL 39683 0.5679792 -0.02832970 0.0004499779
  # 2         SE 13927 0.1993359  0.09127350 0.0021817800
  # 3    SE_PROF 16257 0.2326850 -0.01336187 0.0022230703
  
  
  print(Inf_Values$Tables$Type.of.residence)
  
  # Type.of.residence     N     Percent          WOE           IV
  # 1    Company provided  1603 0.022943593  0.080146599 0.0001529064
  # 2 Living with Parents  1784 0.025534229  0.064003100 0.0002606243
  # 3              Others   199 0.002848269 -0.535710074 0.0009047316
  # 4               Owned 14004 0.200437975  0.004074027 0.0009080647
  # 5              Rented 52277 0.748235934 -0.004293999 0.0009218339
  
  
  print(Inf_Values$Tables$No.of.months.in.current.residence)
  
  # No.of.months.in.current.residence     N    Percent         WOE         IV
  # 1                             [6,9] 34694 0.49657206 -0.27219153 0.03253516
  # 2                           [10,28]  6922 0.09907395  0.49872310 0.06363656
  # 3                           [29,49]  7210 0.10319607  0.30118432 0.07440068
  # 4                           [50,72]  6988 0.10001861  0.13401754 0.07631146
  # 5                           [73,97]  6931 0.09920277  0.13948089 0.07836953
  # 6                          [98,126]  7122 0.10193654 -0.07705956 0.07895394
  
  
  
  print(Inf_Values$Tables$No.of.months.in.current.company)
  
  # No.of.months.in.current.company    N    Percent         WOE           IV
  # 1                            [3,5] 6689 0.09573905  0.09851585 0.0009722477
  # 2                           [6,12] 6798 0.09729915  0.17548049 0.0042210945
  # 3                          [13,19] 6933 0.09923140  0.20630691 0.0088669784
  # 4                          [20,26] 6919 0.09903102  0.03919674 0.0090218882
  # 5                          [27,33] 7104 0.10167890 -0.08567605 0.0097396575
  # 6                          [34,40] 7182 0.10279531  0.03079397 0.0098385211
  # 7                          [41,47] 7217 0.10329626 -0.17614850 0.0127973671
  # 8                          [48,53] 6169 0.08829633 -0.21792183 0.0165964951
  # 9                          [54,61] 7824 0.11198420 -0.21640008 0.0213510210
  # 10                        [62,133] 7032 0.10064837  0.06288591 0.0217607089
  
  ##Plotting all the WOE Values for all the variables
  
  plot_grid(plot_infotables(Inf_Values,"No.of.times.90.DPD.or.worse.in.last.6.months")+bar_theme1,
            plot_infotables(Inf_Values,"No.of.times.60.DPD.or.worse.in.last.6.months")+bar_theme1,
            plot_infotables(Inf_Values,"No.of.times.30.DPD.or.worse.in.last.6.months")+bar_theme1,
            plot_infotables(Inf_Values,"Avgas.CC.Utilization.in.last.12.months")+bar_theme1,
            align = "h")
  
  
  plot_grid(plot_infotables(Inf_Values,"No.of.times.90.DPD.or.worse.in.last.12.months")+bar_theme1,
            plot_infotables(Inf_Values,"No.of.times.60.DPD.or.worse.in.last.12.months")+bar_theme1,
            plot_infotables(Inf_Values,"No.of.times.30.DPD.or.worse.in.last.12.months")+bar_theme1,
            plot_infotables(Inf_Values,"Presence.of.open.home.loan")+bar_theme1,
            align = "h")
  
  
  plot_grid(plot_infotables(Inf_Values,"No.of.trades.opened.in.last.6.months")+bar_theme1,
            plot_infotables(Inf_Values,"No.of.trades.opened.in.last.12.months")+bar_theme1,
            plot_infotables(Inf_Values,"No.of.PL.trades.opened.in.last.6.months")+bar_theme1,
            plot_infotables(Inf_Values,"No.of.PL.trades.opened.in.last.12.months")+bar_theme1,
            align = "h")
  
  
  plot_grid(plot_infotables(Inf_Values,"No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.")+bar_theme1,
            plot_infotables(Inf_Values,"No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.")+bar_theme1,
            plot_infotables(Inf_Values,"Outstanding.Balance")+bar_theme1,
            plot_infotables(Inf_Values,"Total.No.of.Trades")+bar_theme1,
            plot_infotables(Inf_Values,"Presence.of.open.auto.loan")+bar_theme1,
            align = "h")
  
  
  plot_grid(plot_infotables(Inf_Values,"Age")+bar_theme1,
            plot_infotables(Inf_Values,"Gender")+bar_theme1,
            plot_infotables(Inf_Values,"Marital.Status..at.the.time.of.application.")+bar_theme1,
            plot_infotables(Inf_Values,"No.of.dependents")+bar_theme1,
            plot_infotables(Inf_Values,"Income")+bar_theme1,
            plot_infotables(Inf_Values,"Education")+bar_theme1,
            align = "h")
  
  plot_grid(plot_infotables(Inf_Values,"Profession")+bar_theme1,
            plot_infotables(Inf_Values,"Type.of.residence")+bar_theme1,
            plot_infotables(Inf_Values,"No.of.months.in.current.residence")+bar_theme1,
            plot_infotables(Inf_Values,"No.of.months.in.current.company")+bar_theme1,
            align = "h")
  
  
  ############### Model building using only demographic data
  demographic_data_model <- demograpic_data
  str(demographic_data_model)
  summary(demographic_data_model)
  sapply(demographic_data_model,function(x) sum(is.na(x)))
  
  demographic_data_model$Gender[which(is.na(demographic_data_model$Gender))]<- 'M'
  demographic_data_model$Marital.Status..at.the.time.of.application.[which(is.na(demographic_data_model$Marital.Status..at.the.time.of.application))]<- 'Married'
  demographic_data_model$Education[which(is.na(demographic_data_model$Education))]<- 'Professional'
  demographic_data_model$Profession[which(is.na(demographic_data_model$Profession))]<- 'SAL'
  demographic_data_model$Type.of.residence[which(is.na(demographic_data_model$Type.of.residence))]<- 'Rented'
  demographic_data_model$No.of.dependents[which(is.na(demographic_data_model$No.of.dependents))]<- 2
  
  
  
  sapply(demographic_data_model,function(x) sum(is.na(x)))
  
  demograpic_data_base <- demographic_data_model
  demograpic_data_f<- demographic_data_model
  
  
  ## Covert categorical Variables into dummy variables in single shot 
  
  table(demograpic_data_f$Performance.Tag)
  prop.table(table(demograpic_data_f$Performance.Tag))
  
  # data before scaling 
  table(demograpic_data_f$Performance.Tag)
  
  str(demograpic_data_f)
  
  cat_columns <- c("Gender","Marital.Status..at.the.time.of.application.","Education","Profession","Type.of.residence")
  continuos_columns <- c("Age","No.of.dependents","Income","No.of.months.in.current.residence","No.of.months.in.current.company")
  
  
  scale_continuois_columns <- data.frame(sapply(demograpic_data_f[continuos_columns], scale))
  
  dummies <- demograpic_data_f[cat_columns] 
  
  
  
  str(dummies)  
  # dummy variables for Continuios attributes
  dummies<- data.frame(sapply(dummies,function(x) data.frame(model.matrix(~x-1,data =dummies))[,-1])) 
  # combine all relevant columns to build final training data
  final_df<- cbind(scale_continuois_columns,dummies)
  
  final_df$Performance.Tag <- demograpic_data_f$Performance.Tag
  
  str(final_df)
  
  
  
  set.seed(100)
  
  split_indices <- sample.split(final_df, SplitRatio = 7/10)
  
  train_dm<- final_df[split_indices, ]
  
  test_dm<- final_df[!split_indices, ]
  
  
  Model_1_dm = glm(Performance.Tag ~ ., family = binomial(), data = train_dm)
  
  Model_2_dm <- stepAIC(Model_1_dm, direction = "both")
  
  summary(Model_2_dm)
  vif(Model_2_dm)
  
  #Removing Profession.xSE due to less significance
  Model_3_dm <- glm(formula = Performance.Tag ~ Income + No.of.months.in.current.residence + 
                      No.of.months.in.current.company , family = binomial(), 
                    data = train_dm)
  summary(Model_3_dm)
  vif(Model_3_dm)
  
  #Removing No.of.months.in.current.residence due to less significance
  Model_4_dm <- glm(formula = Performance.Tag ~ Income  + 
                      No.of.months.in.current.company , family = binomial(), 
                    data = train_dm)
  summary(Model_4_dm)
  vif(Model_4_dm)
  
  final_dm_model <- Model_4_dm
  
  #predicted probabilities of default for test data
  test_pred_dm = predict(final_dm_model, type = "response",newdata = test_dm)
  
  
  # Let's see the summary 
  summary(test_pred_dm)
  test_dm$prob <- test_pred_dm
  View(test_dm)
  
  test_actual_def_dm <- factor(ifelse(test_dm$Performance.Tag==1,"Yes","No"))
  
  #######################################################################
  
  perform_fn <- function(cutoff) 
  {
    predicted_def <- factor(ifelse(test_pred_dm >= cutoff, "Yes", "No"))
    conf <- confusionMatrix(predicted_def, test_actual_def_dm, positive = "Yes")
    acc <- conf$overall[1]
    sens <- conf$byClass[1]
    spec <- conf$byClass[2]
    out <- t(as.matrix(c(sens, spec, acc))) 
    colnames(out) <- c("sensitivity", "specificity", "accuracy")
    return(out)
  }
  
  s = seq(.01,.90,length=100)
  
  OUT_dm = matrix(0,100,3)
  
  for(i in 1:100)
  {
    OUT_dm[i,] = perform_fn(s[i])
  } 
  
  
  plot(s, OUT_dm[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
  axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
  axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
  lines(s,OUT_dm[,2],col="darkgreen",lwd=2)
  lines(s,OUT_dm[,3],col=4,lwd=2)
  box()
  legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))
  
  
  cutoff_dm <- s[which(abs(OUT_dm[,1]-OUT_dm[,2])==min(abs(OUT_dm[,1]-OUT_dm[,2])))]
  cutoff_dm
  
  # the optimal cutoff value is 0.0459596
  
  test_cutoff_def_dm <- factor(ifelse(test_pred_dm >=cutoff_dm, "Yes", "No"))
  
  conf_final_dm <- confusionMatrix(test_cutoff_def_dm, test_actual_def_dm, positive = "Yes")
  
  acc <- conf_final_dm$overall[1]
  
  sens <- conf_final_dm$byClass[1]
  
  spec <- conf_final_dm$byClass[2]
  conf_final_dm
  
  # Accuracy : 68.57%
  #Sensitivity : 38.43%        
  #Specificity : 69.93% 
  
  # Replace WOE function
  DF.Replace.WOE <-
    function(X,
             y,
             Dependent = NULL) {
      z <- 0
      cz <- 0
      D <- X[, Dependent]
      x <- X[,-which(names(X) == Dependent)]
      cn <- names(x)
      if (class(y) == "Information") {
        for (i in 1:ncol(x)) {
          if (class(x[, i]) == "factor") {
            for (j in 1:length(y[[1]][i][[1]][1][[1]])) {
              x[, i] <- as.character(x[, i])
              if (is.na(y[[1]][i][[1]][1][[1]][j])) {
                x[which(is.na(x[, i])), paste(colnames(x)[i], "WOE", sep = ":")] <-
                  y[[1]][i][[1]][4][[1]][which(is.na(y[[1]][i][[1]][1][[1]]))]
              }
              else {
                x[which(x[, i] == y[[1]][i][[1]][1][[1]][j]), paste(colnames(x)[i], "WOE", sep = ":")] <-
                  y[[1]][i][[1]][4][[1]][j]
              }
            }
          }
          else {
            for (j in 1:length(y[[1]][i][[1]][1][[1]])) {
              cz <-
                as.vector(strsplit(gsub(
                  "[]]", "", gsub("[[]", "", y[[1]][i][[1]][1][[1]])
                ), ","))
              if (y[[1]][i][[1]][1][[1]][j] == "NA") {
                x[which(is.na(x[, i])), paste(colnames(x)[i], "WOE", sep = ":")] <-
                  y[[1]][i][[1]][4][[1]][which(y[[1]][i][[1]][1][[1]][j] == "NA")]
              }
              else {
                x[which(x[, i] >= as.double(cz[[j]][1]) &
                          x[, i] <= as.double(cz[[j]][2])), paste(colnames(x)[i], "WOE", sep = ":")] <-
                  y[[1]][i][[1]][4][[1]][j]
              }
            }
          }
        }
      }
      z <- cbind(x, D)
      colnames(z)[which(names(z) == "D")] <- Dependent
      z <- z[, -which(names(x) == cn)]
      return(z)
    }
  
  
  avg_cc_woe <- demo_credit_data
  avg_cc_woe<-DF.Replace.WOE(avg_cc_woe,Inf_Values,"Performance.Tag.y")
  master_data_wth_woe <- avg_cc_woe
  
  
  str(master_data_wth_woe)
  
  colnames(master_data_wth_woe) = gsub(":WOE", ".WOE", colnames(master_data_wth_woe))
  
  library("car")
  set.seed(100)
  # Set the fractions of the dataframe you want to split into training, validation, and test.
  fractionTraining   <- 0.70
  fractionTest       <- 0.30
  
  # Compute DF sizes.
  sampleSizeTraining      <- floor(fractionTraining   * nrow(master_data_wth_woe))
  sampleSizeTest          <- floor(fractionTest       * nrow(master_data_wth_woe))
  
  # Create the randomly-sampled indices for the dataframe. Use setdiff() to
  # avoid overlapping subsets of indices.
  indicesTraining    <- sort(sample(seq_len(nrow(master_data_wth_woe)), size=sampleSizeTraining))
  indicesNotTraining <- setdiff(seq_len(nrow(master_data_wth_woe)), indicesTraining)
  indicesTest        <- setdiff(indicesNotTraining, indicesTraining)
  
  # Finally, output the three dataframes for training, validation and test.
  master_data_wth_woe_train   <- master_data_wth_woe[indicesTraining, ]
  master_data_wth_woe_test    <- master_data_wth_woe[indicesTest, ]
  
  #Logistic Regression
  
  #Initial model
  model_1 <- glm(Performance.Tag.y~., data=master_data_wth_woe_train,family = "binomial")
  summary(model_1)
  
  # using STEPAIC to find to remove insignificant features
  model_2 <- stepAIC(model_1,direction = "both")
  summary(model_2)
  sort(vif(model_2))
  
  #removing Total.No.of.Trades.WOE based on high vif and less significant p-value 
  model_3 <- glm(Performance.Tag.y ~ No.of.months.in.current.company.WOE + No.of.times.30.DPD.or.worse.in.last.6.months.WOE + 
                   No.of.times.90.DPD.or.worse.in.last.12.months.WOE + Avgas.CC.Utilization.in.last.12.months.WOE + 
                   No.of.trades.opened.in.last.12.months.WOE + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans..WOE + 
                   Outstanding.Balance.WOE, 
                 family = "binomial", data = master_data_wth_woe_train)
  summary(model_3)
  sort(vif(model_3))
  
  
  #removing No.of.times.30.DPD.or.worse.in.last.6.months.WOE based on high vif and less significant p-value 
  model_4 <- glm(Performance.Tag.y ~ No.of.months.in.current.company.WOE +  
                   No.of.times.90.DPD.or.worse.in.last.12.months.WOE + Avgas.CC.Utilization.in.last.12.months.WOE + 
                   No.of.trades.opened.in.last.12.months.WOE + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans..WOE + 
                   Outstanding.Balance.WOE, 
                 family = "binomial", data = master_data_wth_woe_train)
  summary(model_4)
  sort(vif(model_4))
  
  #Outstanding.Balance.WOE
  model_5 <- glm(Performance.Tag.y ~ No.of.months.in.current.company.WOE +  
                   No.of.times.90.DPD.or.worse.in.last.12.months.WOE + Avgas.CC.Utilization.in.last.12.months.WOE + 
                   No.of.trades.opened.in.last.12.months.WOE + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans..WOE,
                 family = "binomial", data = master_data_wth_woe_train)
  summary(model_5)
  sort(vif(model_5))
  
  ##No.of.months.in.current.company.WOE
  model_6 <- glm(Performance.Tag.y ~ No.of.times.90.DPD.or.worse.in.last.12.months.WOE + Avgas.CC.Utilization.in.last.12.months.WOE + 
                   No.of.trades.opened.in.last.12.months.WOE + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans..WOE,
                 family = "binomial", data = master_data_wth_woe_train)
  summary(model_6)
  sort(vif(model_6))
  
  final_model_logistic <- model_6
  
  
  # following are the main variables predicted by our model 
  #"No.of.times.90.DPD.or.worse.in.last.12.months.WOE","Avgas.CC.Utilization.in.last.12.months.WOE",
  #"No.of.trades.opened.in.last.12.months.WOE","No.of.Inquiries.in.last.12.months..excluding.home...auto.loans..WOE""
  
  # predicted probabilities of default for test data
  test_pred = predict(final_model_logistic, type = "response",newdata = master_data_wth_woe_test)
  
  # Let's see the summary 
  summary(test_pred)
  master_data_wth_woe_test$prob <- test_pred
  
  #View(master_data_wth_woe_test)
  
  test_actual_def <- factor(ifelse(master_data_wth_woe_test$Performance.Tag==1,"Yes","No"))
  
  table(test_actual_def)
  
  library(caret)
  #######################################################################
  # finding the optimal cutoff value
  perform_fn <- function(cutoff) 
  {
    predicted_def <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
    conf <- confusionMatrix(predicted_def, test_actual_def, positive = "Yes")
    acc <- conf$overall[1]
    sens <- conf$byClass[1]
    spec <- conf$byClass[2]
    out <- t(as.matrix(c(sens, spec, acc))) 
    colnames(out) <- c("sensitivity", "specificity", "accuracy")
    return(out)
  }
  
  s = seq(.01,.80,length=100)
  
  OUT = matrix(0,100,3)
  
  for(i in 1:100)
  {
    OUT[i,] = perform_fn(s[i])
  } 
  
  plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
  axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
  axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
  lines(s,OUT[,2],col="darkgreen",lwd=2)
  lines(s,OUT[,3],col=4,lwd=2)
  box()
  legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))
  
  cutoff <- s[which(abs(OUT[,1]-OUT[,2])==min(abs(OUT[,1]-OUT[,2])))]
  cutoff
  # optimal cutoff obtained by tuning is 0.04989899
  
  test_cutoff_def <- factor(ifelse(test_pred >=cutoff, "Yes", "No"))
  
  conf_final <- confusionMatrix(test_cutoff_def, test_actual_def, positive = "Yes")
  
  acc <- conf_final$overall[1]
  
  sens <- conf_final$byClass[1]
  
  spec <- conf_final$byClass[2]
  conf_final
  #               Reference
  # Prediction    No   Yes
  #         No  12741   304
  #         Yes  7355   559
  
  # Accuracy : 0.6346
  # Sensitivity : 0.64774         
  # Specificity : 0.63401
  
  
  ############################################################################################
  ################################# Model Evaluation##################################
  ########################## KS -statistic - Test Data ###############################
  library(ROCR)
  test_cutoff_def <- ifelse(test_cutoff_def=="Yes",1,0)
  test_actual_def <- ifelse(test_actual_def=="Yes",1,0)
  
  #on testing  data
  pred_object_test<- prediction(test_cutoff_def, test_actual_def)
  
  performance_measures_test<- performance(pred_object_test, "tpr", "fpr")
  
  ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
    (attr(performance_measures_test, "x.values")[[1]])
  
  max(ks_table_test)
  #  0.2817472
  # KS- Stat value is 26%
  
  # finding the area under the ROC curve
  ROC <- performance(pred_object_test, measure = "auc")
  area <- ROC@y.values[[1]]
  area 
  # 0.6408736
  
  
  # plotting the ROC curve
  tpr_fpr_table <- data.frame(fpr=unlist(performance_measures_test@x.values), tpr=unlist(performance_measures_test@y.values))
  
  ggplot(tpr_fpr_table ,aes(x=fpr, y=tpr)) + geom_line(colour="red") +
    geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1))) +
    labs(x="False Positive Rate",y="True Positive Rate",title="ROC Curve for Logistic Regression Model") +
    theme(axis.text.x=element_text(hjust=1))
  
  
  ####################################################################
  # Lift & Gain Chart 
  library(dplyr)
  lift <- function(labels , predicted_prob,groups=10) {
    
    if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
    if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
    helper = data.frame(cbind(labels , predicted_prob))
    helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
    gaintable = helper %>% group_by(bucket)  %>%
      summarise_at(vars(labels ), funs(total = n(),
                                       totalresp=sum(., na.rm = TRUE))) %>%
      
      mutate(Cumresp = cumsum(totalresp),
             Gain=Cumresp/sum(totalresp)*100,
             Cumlift=Gain/(bucket*(100/groups))) 
    return(gaintable)
  }
  
  default_decile = lift(test_actual_def, test_pred, groups = 10)
  default_decile
  
  # Plotting Gain Chart
  ggplot(default_decile, aes(x = bucket)) +
    labs(x = "Decile", y="Gain (%)")+
    geom_line(data=default_decile,aes(x=bucket,y=Gain),color='red',size=1, group = 1)+
    scale_x_continuous(breaks = seq(1, 10, 1))+
    scale_y_continuous(breaks = seq(20, 100, 10),labels=function(x) paste0(x,"%"))+
    ggtitle("Logistic Model's Gain Chart")
  
  
  # Plotting Lift Chart
  ggplot(default_decile, aes(x = bucket)) +
    labs(x = "Decile", y="Lift")+ geom_line(data=default_decile,aes(x=bucket,y=Cumlift),color='red',size=1, group = 1)+
    scale_x_continuous(breaks = seq(1, 10, 1))+
    scale_y_continuous(breaks = seq(0.4, 4, 0.4))+
    ggtitle("Logistic Model's Lift Chart")
  
  
  
  #########################################################################################
  #########################################################################################
  # 
  ################# Logistic regression using the SMOTE training data #####################
  set.seed(100)
  trainindices_smote= sample(1:nrow(master_data_wth_woe), 0.7*nrow(master_data_wth_woe))
  train_smote = master_data_wth_woe[trainindices_smote,]
  test_smote = master_data_wth_woe[-trainindices_smote,]
  train_smote$Performance.Tag.y <- as.factor(train_smote$Performance.Tag.y)
  train_smote_model <- SMOTE(Performance.Tag.y ~ ., train_smote, perc.over = 200, perc.under=200)
  
  ##checking to see the train dataset is biased or not
  
  table(train_smote$Performance.Tag.y)
  # 0     1
  # 46822  2084 
  
  table(train_smote_model$Performance.Tag.y)
  # 0    1 
  # 8336 6252 
  
  #########################################################################################
  # Building the Logistic regression model with smote dataset
  #########################################################################################
  
  model_smote_1 <- glm(Performance.Tag.y~.,data=train_smote_model,family = "binomial")
  summary(model_smote_1)
  
  # using STEPAIC to find to remove insignificant features
  model_smote_2 <- stepAIC(model_smote_1,direction = "both")
  summary(model_smote_2)
  sort(vif(model_smote_2))
  
  
  #Removing the No.of.times.60.DPD.or.worse.in.last.6.months.WOE based on high VIF and less significant p-value
  model_smote_3 <- glm(formula = Performance.Tag.y ~ Age.WOE + Marital.Status..at.the.time.of.application..WOE + 
                         No.of.dependents.WOE + Income.WOE + Type.of.residence.WOE + 
                         No.of.months.in.current.company.WOE + 
                         No.of.times.30.DPD.or.worse.in.last.6.months.WOE + No.of.times.30.DPD.or.worse.in.last.12.months.WOE + 
                         Avgas.CC.Utilization.in.last.12.months.WOE + No.of.trades.opened.in.last.12.months.WOE + 
                         No.of.PL.trades.opened.in.last.6.months.WOE + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans..WOE + 
                         No.of.Inquiries.in.last.12.months..excluding.home...auto.loans..WOE + 
                         Presence.of.open.home.loan.WOE + Outstanding.Balance.WOE + 
                         Total.No.of.Trades.WOE + Presence.of.open.auto.loan.WOE, 
                       family = "binomial", data = train_smote_model)
  
  summary(model_smote_3)
  sort(vif(model_smote_3))
  
  #Removing the No.of.trades.opened.in.last.12.months.WOE based on high VIF and less significant p-value
  model_smote_4 <- glm(formula = Performance.Tag.y ~ Age.WOE + Marital.Status..at.the.time.of.application..WOE + 
                         No.of.dependents.WOE + Income.WOE + Type.of.residence.WOE + 
                         No.of.months.in.current.company.WOE + 
                         No.of.times.30.DPD.or.worse.in.last.6.months.WOE + No.of.times.30.DPD.or.worse.in.last.12.months.WOE + 
                         Avgas.CC.Utilization.in.last.12.months.WOE + 
                         No.of.PL.trades.opened.in.last.6.months.WOE + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans..WOE + 
                         No.of.Inquiries.in.last.12.months..excluding.home...auto.loans..WOE + 
                         Presence.of.open.home.loan.WOE + Outstanding.Balance.WOE + 
                         Total.No.of.Trades.WOE + Presence.of.open.auto.loan.WOE, 
                       family = "binomial", data = train_smote_model)
  summary(model_smote_4)
  sort(vif(model_smote_4))
  
  #Removing the No.of.Inquiries.in.last.6.months..excluding.home...auto.loans..WOE based on high VIF and less significant p-value
  model_smote_5 <- glm(formula = Performance.Tag.y ~ Age.WOE + Marital.Status..at.the.time.of.application..WOE + 
                         No.of.dependents.WOE + Income.WOE + Type.of.residence.WOE + 
                         No.of.months.in.current.company.WOE + 
                         No.of.times.30.DPD.or.worse.in.last.6.months.WOE + No.of.times.30.DPD.or.worse.in.last.12.months.WOE + 
                         Avgas.CC.Utilization.in.last.12.months.WOE + 
                         No.of.PL.trades.opened.in.last.6.months.WOE +  
                         No.of.Inquiries.in.last.12.months..excluding.home...auto.loans..WOE + 
                         Presence.of.open.home.loan.WOE + Outstanding.Balance.WOE + 
                         Total.No.of.Trades.WOE + Presence.of.open.auto.loan.WOE, 
                       family = "binomial", data = train_smote_model)
  summary(model_smote_5)
  sort(vif(model_smote_5))
  
  #Removing the No.of.times.30.DPD.or.worse.in.last.12.months.WOE based on high VIF and less significant p-value
  model_smote_6 <- glm(formula = Performance.Tag.y ~ Age.WOE + Marital.Status..at.the.time.of.application..WOE + 
                         No.of.dependents.WOE + Income.WOE + Type.of.residence.WOE + 
                         No.of.months.in.current.company.WOE + 
                         No.of.times.30.DPD.or.worse.in.last.12.months.WOE + 
                         Avgas.CC.Utilization.in.last.12.months.WOE + 
                         No.of.PL.trades.opened.in.last.6.months.WOE +  
                         No.of.Inquiries.in.last.12.months..excluding.home...auto.loans..WOE + 
                         Presence.of.open.home.loan.WOE + Outstanding.Balance.WOE + 
                         Total.No.of.Trades.WOE + Presence.of.open.auto.loan.WOE, 
                       family = "binomial", data = train_smote_model)
  summary(model_smote_6)
  sort(vif(model_smote_6))
  
  #Removing the Total.No.of.Trades.WOE based on high VIF and less significant p-value
  model_smote_7 <- glm(formula = Performance.Tag.y ~ Age.WOE + Marital.Status..at.the.time.of.application..WOE + 
                         No.of.dependents.WOE + Income.WOE + Type.of.residence.WOE + 
                         No.of.months.in.current.company.WOE + 
                         No.of.times.30.DPD.or.worse.in.last.12.months.WOE + 
                         Avgas.CC.Utilization.in.last.12.months.WOE + 
                         No.of.PL.trades.opened.in.last.6.months.WOE +  
                         No.of.Inquiries.in.last.12.months..excluding.home...auto.loans..WOE + 
                         Presence.of.open.home.loan.WOE + Outstanding.Balance.WOE + 
                         Presence.of.open.auto.loan.WOE, 
                       family = "binomial", data = train_smote_model)
  summary(model_smote_7)
  sort(vif(model_smote_7))
  
  #Removing the Type.of.residence.WOE based on less significant p-value
  model_smote_8 <- glm(formula = Performance.Tag.y ~ Age.WOE + Marital.Status..at.the.time.of.application..WOE + 
                         No.of.dependents.WOE + Income.WOE + 
                         No.of.months.in.current.company.WOE + 
                         No.of.times.30.DPD.or.worse.in.last.12.months.WOE + 
                         Avgas.CC.Utilization.in.last.12.months.WOE + 
                         No.of.PL.trades.opened.in.last.6.months.WOE +  
                         No.of.Inquiries.in.last.12.months..excluding.home...auto.loans..WOE + 
                         Presence.of.open.home.loan.WOE + Outstanding.Balance.WOE + 
                         Presence.of.open.auto.loan.WOE, 
                       family = "binomial", data = train_smote_model)
  summary(model_smote_8)
  sort(vif(model_smote_8))
  
  #Removing the Income.WOE based on less significant p-value
  model_smote_9 <- glm(formula = Performance.Tag.y ~ Age.WOE + Marital.Status..at.the.time.of.application..WOE + 
                         No.of.dependents.WOE +  
                         No.of.months.in.current.company.WOE + 
                         No.of.times.30.DPD.or.worse.in.last.12.months.WOE + 
                         Avgas.CC.Utilization.in.last.12.months.WOE + 
                         No.of.PL.trades.opened.in.last.6.months.WOE +  
                         No.of.Inquiries.in.last.12.months..excluding.home...auto.loans..WOE + 
                         Presence.of.open.home.loan.WOE + Outstanding.Balance.WOE + 
                         Presence.of.open.auto.loan.WOE, 
                       family = "binomial", data = train_smote_model)
  summary(model_smote_9)
  sort(vif(model_smote_9))
  
  #Removing the Marital.Status..at.the.time.of.application..WOE based on less significant p-value
  model_smote_10 <- glm(formula = Performance.Tag.y ~ Age.WOE +  
                          No.of.dependents.WOE +  
                          No.of.months.in.current.company.WOE + 
                          No.of.times.30.DPD.or.worse.in.last.12.months.WOE + 
                          Avgas.CC.Utilization.in.last.12.months.WOE + 
                          No.of.PL.trades.opened.in.last.6.months.WOE +  
                          No.of.Inquiries.in.last.12.months..excluding.home...auto.loans..WOE + 
                          Presence.of.open.home.loan.WOE + Outstanding.Balance.WOE + 
                          Presence.of.open.auto.loan.WOE, 
                        family = "binomial", data = train_smote_model)
  summary(model_smote_10)
  sort(vif(model_smote_10))
  
  #Removing the No.of.PL.trades.opened.in.last.6.months.WOE  based on less significant p-value
  model_smote_11 <- glm(formula = Performance.Tag.y ~ Age.WOE +  
                          No.of.dependents.WOE +  
                          No.of.months.in.current.company.WOE + 
                          No.of.times.30.DPD.or.worse.in.last.12.months.WOE + 
                          Avgas.CC.Utilization.in.last.12.months.WOE + 
                          No.of.Inquiries.in.last.12.months..excluding.home...auto.loans..WOE + 
                          Presence.of.open.home.loan.WOE + Outstanding.Balance.WOE + 
                          Presence.of.open.auto.loan.WOE, 
                        family = "binomial", data = train_smote_model)
  summary(model_smote_11)
  sort(vif(model_smote_11))
  
  #Removing the No.of.dependents.WOE  based on less significant p-value
  model_smote_12 <- glm(formula = Performance.Tag.y ~ Age.WOE + 
                          No.of.months.in.current.company.WOE + 
                          No.of.times.30.DPD.or.worse.in.last.12.months.WOE + 
                          Avgas.CC.Utilization.in.last.12.months.WOE + 
                          No.of.Inquiries.in.last.12.months..excluding.home...auto.loans..WOE + 
                          Presence.of.open.home.loan.WOE + Outstanding.Balance.WOE , 
                        family = "binomial", data = train_smote_model)
  summary(model_smote_12)
  sort(vif(model_smote_12))
  
  #Removing the No.of.dependents.WOE  based on less significant p-value
  model_smote_13 <- glm(formula = Performance.Tag.y ~ Age.WOE + 
                          No.of.months.in.current.company.WOE + 
                          No.of.times.30.DPD.or.worse.in.last.12.months.WOE + 
                          Avgas.CC.Utilization.in.last.12.months.WOE + 
                          No.of.Inquiries.in.last.12.months..excluding.home...auto.loans..WOE + 
                          Outstanding.Balance.WOE , 
                        family = "binomial", data = train_smote_model)
  summary(model_smote_13)
  sort(vif(model_smote_13))
  
  final_smote_model_logistic<- model_smote_13
  
  #predicted probabilities of default for test data
  test_pred_smote = predict(final_smote_model_logistic, type = "response",newdata = test_smote)
  
  
  # Let's see the summary 
  summary(test_pred_smote)
  test_smote$prob <- test_pred_smote
  # View(test_smote)
  
  test_actual_def_smote <- factor(ifelse(test_smote$Performance.Tag==1,"Yes","No"))
  
  #######################################################################
  
  perform_fn <- function(cutoff) 
  {
    predicted_def <- factor(ifelse(test_pred_smote >= cutoff, "Yes", "No"))
    conf <- confusionMatrix(predicted_def, test_actual_def_smote, positive = "Yes")
    acc <- conf$overall[1]
    sens <- conf$byClass[1]
    spec <- conf$byClass[2]
    out <- t(as.matrix(c(sens, spec, acc))) 
    colnames(out) <- c("sensitivity", "specificity", "accuracy")
    return(out)
  }
  
  s = seq(.01,.90,length=100)
  
  OUT_smote = matrix(0,100,3)
  
  for(i in 1:100)
  {
    OUT_smote[i,] = perform_fn(s[i])
  } 
  
  
  plot(s, OUT_smote[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
  axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
  axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
  lines(s,OUT_smote[,2],col="darkgreen",lwd=2)
  lines(s,OUT_smote[,3],col=4,lwd=2)
  box()
  legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))
  
  
  cutoff_smote <- s[which(abs(OUT_smote[,1]-OUT_smote[,2])==min(abs(OUT_smote[,1]-OUT_smote[,2])))]
  cutoff_smote
  
  # the optimal cutoff value is 0.4774747
  
  test_cutoff_def_smote <- factor(ifelse(test_pred_smote >=cutoff_smote, "Yes", "No"))
  
  conf_final_smote <- confusionMatrix(test_cutoff_def_smote, test_actual_def_smote, positive = "Yes")
  
  acc <- conf_final_smote$overall[1]
  
  sens <- conf_final_smote$byClass[1]
  
  spec <- conf_final_smote$byClass[2]
  conf_final_smote
  
  # Accuracy = 64.11%
  # Sensitivity = 63.96%
  # Specificity = 64.11%
  
  
  ############################################################################################
  ############# Model Evaluation##################
  ### KS -statistic - Test Data ######
  
  test_cutoff_def_smote <- ifelse(test_cutoff_def_smote=="Yes",1,0)
  test_actual_def_smote <- ifelse(test_actual_def_smote=="Yes",1,0)
  
  #on testing  data
  pred_object_test<- prediction(test_cutoff_def_smote, test_actual_def_smote)
  
  performance_measures_test<- performance(pred_object_test, "tpr", "fpr")
  
  ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
    (attr(performance_measures_test, "x.values")[[1]])
  
  max(ks_table_test)
  # 0.28
  
  # finding the area under the ROC curve
  ROC <- performance(pred_object_test, measure = "auc")
  area <- ROC@y.values[[1]]
  area 
  #0.64
  
  
  # plotting the ROC curve
  tpr_fpr_table <- data.frame(fpr=unlist(performance_measures_test@x.values), tpr=unlist(performance_measures_test@y.values))
  
  ggplot(tpr_fpr_table ,aes(x=fpr, y=tpr)) + geom_line(colour="red") +
    geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1))) +
    labs(x="False Positive Rate",y="True Positive Rate",title="ROC Curve for Logistic Regression Model using SMOTE") +
    theme(axis.text.x=element_text(hjust=1))
  
  
  # Lift & Gain Chart 
  
  lift <- function(labels , predicted_prob,groups=10) {
    
    if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
    if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
    helper = data.frame(cbind(labels , predicted_prob))
    helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
    gaintable = helper %>% group_by(bucket)  %>%
      summarise_at(vars(labels ), funs(total = n(),
                                       totalresp=sum(., na.rm = TRUE))) %>%
      
      mutate(Cumresp = cumsum(totalresp),
             Gain=Cumresp/sum(totalresp)*100,
             Cumlift=Gain/(bucket*(100/groups))) 
    return(gaintable)
  }
  
  default_decile_smote = lift(test_actual_def_smote, test_cutoff_def_smote, groups = 10)
  default_decile_smote
  
  # Plotting Gain Chart
  ggplot(default_decile_smote, aes(x = bucket)) +
    labs(x = "Decile", y="Gain (%)")+
    geom_line(data=default_decile_smote,aes(x=bucket,y=Gain),color='red',size=1, group = 1)+
    scale_x_continuous(breaks = seq(1, 10, 1))+
    scale_y_continuous(breaks = seq(20, 100, 10),labels=function(x) paste0(x,"%"))+
    ggtitle("Logistic Model using SMOTE - Gain Chart")
  
  
  # Plotting Lift Chart
  ggplot(default_decile_smote, aes(x = bucket)) +
    labs(x = "Decile", y="Lift")+ geom_line(data=default_decile_smote,aes(x=bucket,y=Cumlift),color='red',size=1, group = 1)+
    scale_x_continuous(breaks = seq(1, 10, 1))+
    scale_y_continuous(breaks = seq(0.4, 4, 0.4))+
    ggtitle("Logistic Model using SMOTE - Lift Chart")
  
  
  ##############################################################################################
  # Performing the cross validation on the final model
  
  # Load data
  
  cross_data_smote <- master_data_wth_woe
  #-------------------------------------------------------------------------------
  # False positive rate
  fpr <- NULL
  
  # False negative rate
  fnr <- NULL
  
  # Number of iterations
  k <- 500
  
  
  # Accuracy
  acc <- NULL
  sens <- NULL
  spec <- NULL
  set.seed(100)
  
  for(i in 1:k)
  {
    # Train-test splitting
    # 80% of samples -> fitting
    # 20% of samples -> testing
    smp_size <- floor(0.80 * nrow(cross_data_smote))
    index <- sample(seq_len(nrow(cross_data_smote)),size=smp_size)
    train_smote <- cross_data_smote[index, ]
    test_smote <- cross_data_smote[-index, ]
    
    # Predict results
    results_prob <- predict(final_smote_model_logistic,newdata = test_smote,type='response')
    
    # If prob > 0.5 then 1, else 0
    results <- factor(ifelse(results_prob > 0.5,1,0),levels = 0:1)
    
    # Actual answers
    answers <- factor(test_smote$Performance.Tag,levels = 0:1)
    
    # Confusion matrix
    cm <- confusionMatrix(data=results, reference=answers)
    acc[i] <- cm$overall[1]
    sens[i] <- cm$byClass[1]
    spec[i] <- cm$byClass[2]
    fpr[i] <- cm$table[2]/(nrow(cross_data_smote)-smp_size)
    fnr[i] <- cm$table[3]/(nrow(cross_data_smote)-smp_size)
    
  }
  
  # Average accuracy,sensitivity and specificity of the model
  mean(acc) # 68%
  mean(sens) # 69%
  mean(spec) # 55%
  
  par(mfcol=c(1,2))
  
  # Histogram of accuracy
  hist(acc,xlab='Accuracy',ylab='Freq',
       col='orange',border='red',density=30)
  
  # Boxplot of accuracy
  boxplot(acc,col='orange',border='red',horizontal=T,xlab='Accuracy',
          main='boxplot-Accuracy')
  
  # plots of fpr and fnr
  mean(fpr) #0.2965993
  mean(fnr) #0.01888751 
  hist(fpr,xlab='% of fnr',ylab='Freq',main='FPR',
       col='cyan',border='blue',density=30)
  hist(fnr,xlab='% of fnr',ylab='Freq',main='FNR',
       col='cyan',border='blue',density=30)
  
  
  ####################################################################################
  ############################### Random Forest ##################################### 
  set.seed(100)
  master_data_wth_woe_base <- na.omit(master_data_wth_woe)
  master_data_wth_woe_rf <- master_data_wth_woe_base
  trainindices_rf= sample(1:nrow(master_data_wth_woe_rf), 0.7*nrow(master_data_wth_woe_rf))
  train_rf = master_data_wth_woe_rf[trainindices_rf,]
  test_rf = master_data_wth_woe_rf[-trainindices_rf,]
  
  
  library(randomForest)
  model_rf <- randomForest(Performance.Tag.y ~.,data = train_rf,proximity = F,do.trace = T,
                           mtry = 4,ntree=500,importance = TRUE)
  
  model_rf 
  # Number of trees: 500
  # No. of variables tried at each split: 4
  # 
  # Mean of squared residuals: 0.04152475
  # % Var explained: -0.22
  
  summary(model_rf)
  
  testPred_rf <- predict(model_rf, newdata=test_rf)
  testactual_rf <- factor(ifelse(test_rf$Performance.Tag.y==1,"yes","no"))
  
  
  #finding the optimal cutoff value for probalility
  performfn_rf <- function(cutoff) 
  {
    predicted_response <- as.factor(ifelse(testPred_rf >= cutoff, "yes", "no"))
    conf <- confusionMatrix(predicted_response, testactual_rf, positive = "yes")
    acc <- conf$overall[1]
    sens <- conf$byClass[1]
    spec <- conf$byClass[2]
    OUT_rf <- t(as.matrix(c(sens, spec, acc))) 
    colnames(OUT_rf) <- c("sensitivity", "specificity", "accuracy")
    return(OUT_rf)
  }
  
  # creating cutoff values from 0.01 to 0.99 for plotting
  s = seq(.01,.99,length=100)
  
  OUT_rf = matrix(0,100,3)
  
  for(i in 1:100)
  {
    OUT_rf[i,] = performfn_rf(s[i])
  } 
  
  
  # plotting cutoffs
  plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
  axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
  axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
  lines(s,OUT_rf[,2],col="darkgreen",lwd=2)
  lines(s,OUT_rf[,3],col=4,lwd=2)
  box()
  legend(0,.50,col=c(1,"darkgreen",2,"darkred"),lwd=c(1,1,1,1),c("Sensitivity","Specificity","Accuracy"))
  
  cutoff_rf <- s[which(abs(OUT_rf[,1]-OUT_rf[,2]) < 0.15)]
 # cutoff_rf <- s[which(abs(OUT[,1]-OUT[,2])==min(abs(OUT[,1]-OUT[,2])))]
 # cutoff_rf #0.05949495
  # cutoff_rf <- 0.05949495
  # head(testPred_rf[,1],5)
  # optimal cutoff value is 0.059
  testpred_optimal_rf<- factor(ifelse(testPred_rf >= cutoff_rf, "yes", "no"))
  conf_rf <- confusionMatrix(testpred_optimal_rf, testactual_rf, positive = "yes")
  conf_rf
  # Reference
  #     Prediction   no      yes
  #          no      13156   359
  #          yes     6976    468
  
  # Accuracy : 61.43%
  # Sensitivity : 60.70%       
  # Specificity : 61.46%
  
  ####################### KS - statistic -Random Forest - Test Data######################## #######################
  
  testactual_rf<-ifelse(testactual_rf == "yes", 1,0)
  testpred_optimal_rf<-ifelse(testpred_optimal_rf == "yes", 1,0)
  
  pred_object_test<- prediction(testpred_optimal_rf, testactual_rf)
  
  performance_measures_test<- performance(pred_object_test, "tpr", "fpr")
  
  ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
    (attr(performance_measures_test, "x.values")[[1]])
  
  max(ks_table_test)  
  # 0.2216567
  #KS-statistic is 22.16%
  
  
  # find area under the roc curve
  roc <- performance(pred_object_test, measure = "auc")
  area <- roc@y.values[[1]]
  area 
  
  # 0.6108283
  
  
  tpr_fpr_table <- data.frame(fpr=unlist(performance_measures_test@x.values), tpr=unlist(performance_measures_test@y.values))
  
  ggplot(tpr_fpr_table ,aes(x=fpr, y=tpr)) +
    geom_line(colour="red") +
    geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1))) +
    labs(x="False Positive Rate",
         y="True Positive Rate",
         title="ROC Curve for Random Forest") +
    theme(axis.text.x=element_text(hjust=1)) 
  
  lift <- function(labels , predicted_prob,groups=10) {
    
    if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
    if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
    helper = data.frame(cbind(labels , predicted_prob))
    helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
    gaintable = helper %>% group_by(bucket)  %>%
      summarise_at(vars(labels ), funs(total = n(),
                                       totalresp=sum(., na.rm = TRUE))) %>%
      mutate(Cumresp = cumsum(totalresp),
             Gain=Cumresp/sum(totalresp)*100,
             Cumlift=Gain/(bucket*(100/groups))) 
    return(gaintable)
  }
  
  Default_decile = lift(testactual_rf, testpred_optimal_rf, groups = 10)
  Default_decile
  
  # Plotting Gain Chart
  ggplot(Default_decile, aes(x = bucket)) +
    labs(x = "Decile", y="Gain (%)")+
    geom_line(data=Default_decile,aes(x=bucket,y=Gain),color='red',size=1, group = 1)+
    scale_x_continuous(breaks = seq(1, 10, 1))+
    scale_y_continuous(breaks = seq(20, 100, 10),labels=function(x) paste0(x,"%"))+
    ggtitle("Gain Chart")
  
  
  # Plotting Lift Chart
  ggplot(Default_decile, aes(x = bucket)) +
    labs(x = "Decile", y="Lift")+
    geom_line(data=Default_decile,aes(x=bucket,y=Cumlift),color='red',size=1, group = 1)+
    scale_x_continuous(breaks = seq(1, 10, 1))+
    scale_y_continuous(breaks = seq(0.4, 4, 0.4))+
    ggtitle("Lift Chart")
  
  
  ############################### Random Forest with Smote ##################################### 
  
  
  set.seed(100)
  trainindices_smote_rf = sample(1:nrow(master_data_wth_woe), 0.7*nrow(master_data_wth_woe))
  train_smote_rf = master_data_wth_woe[trainindices_smote_rf,]
  test_smote_rf = master_data_wth_woe[-trainindices_smote_rf,]
  train_smote_rf$Performance.Tag.y <- as.factor(train_smote_rf$Performance.Tag.y)
  
  train_smote_rf_model <- SMOTE(Performance.Tag.y ~ ., train_smote_rf, perc.over = 200, perc.under=200)
  
  
  library(randomForest)
  model_rf_smote <- randomForest(Performance.Tag.y ~.,data = train_smote_rf_model,proximity = F,do.trace = T,
                           mtry = 4,ntree=500,importance = TRUE)
  
  ##RandomForestClassifier(n_estimators=10, criterion='gini', max_depth=None,min_samples_split=2, min_samples_leaf=1, min_weight_fraction_leaf=0.0, max_features='auto', max_leaf_nodes=None,bootstrap=True, oob_score=False, n_jobs=1, random_state=None, verbose=0, warm_start=False,class_weight=None)
  
  model_rf_smote 
 
  summary(model_rf_smote)
  
  
  testPred_rf_smote <- predict(model_rf_smote, newdata=test_smote_rf[,-28],type = "prob")
  
  testactual_rf_smote <- factor(ifelse(test_smote_rf$Performance.Tag.y==1,"yes","no"))
  
  #finding the optimal cutoff value for probalility
  performfn_rf_smote <- function(cutoff) 
  {
    predicted_response <- as.factor(ifelse(testPred_rf_smote[, 2] >= cutoff, "yes", "no"))
    conf <- confusionMatrix(predicted_response, testactual_rf_smote, positive = "yes")
    acc <- conf$overall[1]
    sens <- conf$byClass[1]
    spec <- conf$byClass[2]
    OUT_rf_smote <- t(as.matrix(c(sens, spec, acc))) 
    colnames(OUT_rf_smote) <- c("sensitivity", "specificity", "accuracy")
    return(OUT_rf_smote)
  }
  
  # creating cutoff values from 0.01 to 0.99 for plotting
  s = seq(.01,.99,length=100)
  
  OUT_rf_smote = matrix(0,100,3)
  
  for(i in 1:100)
  {
    OUT_rf_smote[i,] = performfn_rf_smote(s[i])
  } 
  
  
  # plotting cutoffs
  plot(s, OUT_rf_smote[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
  axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
  axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
  lines(s,OUT_rf_smote[,2],col="darkgreen",lwd=2)
  lines(s,OUT_rf_smote[,3],col=4,lwd=2)
  box()
  
  
  legend(0,.50,col=c(1,"darkgreen",2,"darkred"),lwd=c(1,1,1,1),c("Sensitivity","Specificity","Accuracy"))
  
  #cutoff_rf_smote <- s[which(abs(OUT_rf_smote[,1]-OUT_rf_smote[,2]) < 0.35)]
  cutoff_rf_smote <- s[which(abs(OUT_rf_smote[,1]-OUT_rf_smote[,2])==min(abs(OUT_rf_smote[,1]-OUT_rf_smote[,2])))]
  cutoff_rf_smote 
  
  # head(testPred_rf[,1],5)
  # optimal cutoff value is 0.059
  testpred_optimal_rf_smote<- factor(ifelse(testPred_rf_smote[,2] >= cutoff_rf_smote, "yes", "no"))
  conf_rf <- confusionMatrix(testpred_optimal_rf_smote, testactual_rf_smote, positive = "yes")
  conf_rf
  
  
  #   Prediction    no   yes
  #         no    12290   312
  #         yes    7804   551
  
  # Accuracy : 63.11%
  # Sensitivity : 60.71%       
  # Specificity : 63.20%
  
  ####################### KS - statistic -Random Forest Smote - Test Data######################## #######################
  
  testactual_rf_smote<-ifelse(testactual_rf_smote == "yes", 1,0)
  testpred_optimal_rf_smote<-ifelse(testpred_optimal_rf_smote == "yes", 1,0)
  
  pred_object_test<- prediction(testpred_optimal_rf_smote, testactual_rf_smote)
  
  performance_measures_test<- performance(pred_object_test, "tpr", "fpr")
  
  ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
    (attr(performance_measures_test, "x.values")[[1]])
  
  max(ks_table_test)  
  # 0.2392635
  #KS-statistic is 23.92%
  
  
  # find area under the roc curve
  roc <- performance(pred_object_test, measure = "auc")
  area <- roc@y.values[[1]]
  area 
  
  # 0.6196317
  
  
  tpr_fpr_table <- data.frame(fpr=unlist(performance_measures_test@x.values), tpr=unlist(performance_measures_test@y.values))
  
  ggplot(tpr_fpr_table ,aes(x=fpr, y=tpr)) +
    geom_line(colour="red") +
    geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1))) +
    labs(x="False Positive Rate",
         y="True Positive Rate",
         title="ROC Curve for Random Forest") +
    theme(axis.text.x=element_text(hjust=1)) 
  
  lift <- function(labels , predicted_prob,groups=10) {
    
    if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
    if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
    helper = data.frame(cbind(labels , predicted_prob))
    helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
    gaintable = helper %>% group_by(bucket)  %>%
      summarise_at(vars(labels ), funs(total = n(),
                                       totalresp=sum(., na.rm = TRUE))) %>%
      mutate(Cumresp = cumsum(totalresp),
             Gain=Cumresp/sum(totalresp)*100,
             Cumlift=Gain/(bucket*(100/groups))) 
    return(gaintable)
  }
  
  Default_decile = lift(testactual_rf_smote, testpred_optimal_rf_smote, groups = 10)
  Default_decile
  
  # Plotting Gain Chart
  ggplot(Default_decile, aes(x = bucket)) +
    labs(x = "Decile", y="Gain (%)")+
    geom_line(data=Default_decile,aes(x=bucket,y=Gain),color='red',size=1, group = 1)+
    scale_x_continuous(breaks = seq(1, 10, 1))+
    scale_y_continuous(breaks = seq(20, 100, 10),labels=function(x) paste0(x,"%"))+
    ggtitle("Gain Chart")
  
  
  # Plotting Lift Chart
  ggplot(Default_decile, aes(x = bucket)) +
    labs(x = "Decile", y="Lift")+
    geom_line(data=Default_decile,aes(x=bucket,y=Cumlift),color='red',size=1, group = 1)+
    scale_x_continuous(breaks = seq(1, 10, 1))+
    scale_y_continuous(breaks = seq(0.4, 4, 0.4))+
    ggtitle("Lift Chart")
  
  
  ###############################################################################################
  ################ Scorecard Using Random Forest Model ##########################
  # Build an application scorecard with the good to bad odds of 10 to 1 
  # at a score of 400 doubling every 20 points.
  master_data_wth_woe_sc <- avg_cc_woe
  colnames(master_data_wth_woe_sc) = gsub(":WOE", ".WOE", colnames(master_data_wth_woe_sc))
  master_data_wth_woe_sc$Performance.Tag.y <- as.factor(master_data_wth_woe_sc$Performance.Tag.y)
  
  final_dataset <- master_data_wth_woe_sc
  
  final_dataset$perdict_default  <- predict(model_rf, type = "response", newdata = final_dataset)
  # final_dataset$perdict_default  <- predict(final_smote_model_logistic, type = "response", newdata = final_dataset)
  # final_dataset$perdict_default  <- predict(final_model_logistic, type = "response", newdata = final_dataset)
  
  
  final_dataset$predict_NonDefault <- 1 - final_dataset$perdict_default
  final_dataset$odds <-  log(final_dataset$predict_NonDefault/final_dataset$perdict_default)
  
  
  final_dataset$Score <- floor(400 + ((20/(log(2))) * (final_dataset$odds-(log(10)))))
  
  str(final_dataset$Score)
  summary(final_dataset$Score)
  master_data$Score <- final_dataset$Score
  #final_dataset <- na.omit(final_dataset)
  # min - 319 to max - 592
  
  ggplot(final_dataset,aes(x = Score))+ geom_bar(aes(fill = factor(Performance.Tag.y)))
  
  # graphically we can see that there is a dip in the scores between 380-420
  # this can be choosen as the cutoff value.
  
  ##Taken 380,285,390,395,400,405,410,415,420 and calculated Accracy. Taken 410 cut off for 86% accuracy.
  ## We can take 380 but it's risky as we compare to profit  
  
  cutoff_score = 409
  defaulters_below_cutoff<-length(which(final_dataset$Performance.Tag==1 & final_dataset$Score<cutoff_score))
  defaulters_below_cutoff
  #2514
  
  total_defaulters<-length(which(final_dataset$Performance.Tag==1))
  total_defaulters
  #2947
  
  predicted_defaulters_below_cutoff_score<-ceiling((defaulters_below_cutoff/total_defaulters)*100)
  predicted_defaulters_below_cutoff_score 
  #86
  
  
  non_defaulters_below_cutoff<-length(which(final_dataset$Performance.Tag==0 & final_dataset$Score<cutoff_score))
  non_defaulters_below_cutoff
  #8317
  
  non_defaulters<-length(which(final_dataset$Performance.Tag==0))
  non_defaulters
  #66920
  
  predicted_non_defaulters_below_cutoff_score<-ceiling((non_defaulters_below_cutoff/non_defaulters)*100)
  predicted_non_defaulters_below_cutoff_score
  #13
  
  # cutoff score of 409 covers 86% of the defaulters
  
  ####################################################
  
  ###Calculating Financial Loss and Profit
  
  ####################################################
  
  # Acccepted_Data Set
  
  
  valid_records <- na.omit(master_data)
  
  
  default_users_outstanding <- valid_records$Outstanding.Balance[which(valid_records$Performance.Tag==1)]
  
  current_credit_loss<- sum(default_users_outstanding)
  
  current_credit_loss
  #Credit loss without any model : 3712927443
  
  default_users_outstanding_model <- valid_records$Outstanding.Balance[which(valid_records$Score >  409 & valid_records$Performance.Tag==1 )]
  
  model_credit_loss<- sum(default_users_outstanding_model)
  model_credit_loss
  # Credit loss without  model  : 510214087
  
  non_default_users_model <- length(which(valid_records$Score <  409 & valid_records$Performance.Tag==0 ))
  
  ## Profit per each customer assuming that 4000
  
  Profit1<- current_credit_loss- model_credit_loss - non_default_users_model*4000
  Profit1
  #3169445356
  
  
  ################ Scorecard Using Random Forest Model on Rejected Data Set ##########################
  
  summary(validation_dataset2)
  ## Checking missing values count in each column .
  
  sapply(validation_dataset2, function(x) sum(is.na(x)))
  
  Rejected_data<-validation_dataset2
  
  Rejected_data <- Rejected_data[,-c(1)]
  
  Rejected_data$Performance.Tag.y <- 1
  
  Rejected_data$Performance.Tag.x <- 1 
  
  sapply(Rejected_data, function(x) sum(is.na(x)))
  
  ## ABLE TO SEET that 2 PERFORMANCE TAGS AND WILL KEEP ONLY ONE BUT BEFORE DOING THAT LETS CHECK DOES THE PERFORMANE TAGS SHOWS DIFFERENT IN THE TWO DATASETS
  Rejected_data$PERFOMANCE_DIF <- Rejected_data$Performance.Tag.x - Rejected_data$Performance.Tag.y
  levels(factor(Rejected_data$PERFOMANCE_DIF)) # "0"
  
  
  ## FROM THE ABOVE THERE IS NO DIFFERENCE IN THE PERFORMANCE TAGS AND HENCE WE CAN KEEP ANY ONE PERFORMANCE TAG
  Rejected_data <- Rejected_data[, -which(names(Rejected_data) %in% c("Performance.Tag.x","PERFOMANCE_DIF"))]
  
  Rejected_data$Education[which(is.na(Rejected_data$Education))]<- 'Professional'
  Rejected_data$Profession[which(is.na(Rejected_data$Profession))]<- 'SAL'
  Rejected_data$Avgas.CC.Utilization.in.last.12.months<- 51
  
  Rejected_data$Gender <- as.factor(Rejected_data$Gender)
  Rejected_data$Marital.Status..at.the.time.of.application. <- as.factor(Rejected_data$Marital.Status..at.the.time.of.application. )
  Rejected_data$Education <- as.factor(Rejected_data$Education)
  Rejected_data$Profession <- as.factor(Rejected_data$Profession)
  Rejected_data$Type.of.residence <- as.factor(Rejected_data$Type.of.residence)
  
  avg_cc_woe_rejected<-DF.Replace.WOE(Rejected_data,Inf_Values,"Performance.Tag.y")
  master_data_wth_woe <- avg_cc_woe_rejected
  
  colnames(master_data_wth_woe) = gsub(":WOE", ".WOE", colnames(master_data_wth_woe))
  
  master_data_wth_woe$Performance.Tag.y <- as.factor(master_data_wth_woe$Performance.Tag.y)
  
  final_dataset <- master_data_wth_woe
  
  final_dataset$perdict_default  <- predict(model_rf, type = "response", newdata = final_dataset)
  final_dataset$predict_NonDefault <- 1 - final_dataset$perdict_default
  final_dataset$odds <-  log(final_dataset$predict_NonDefault/final_dataset$perdict_default)
  
  
  final_dataset$Score <- floor(400 + ((20/(log(2))) * (final_dataset$odds-(log(10)))))
  
  str(final_dataset$Score)
  summary(final_dataset$Score)
  Rejected_data$Score <- final_dataset$Score
  
  ##Min score: 364 Max Score:433
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  # 364.0   388.0   396.0   396.5   404.0   433.0       4
  
  ggplot(final_dataset,aes(x = Score))+ geom_bar(aes(fill = factor(Performance.Tag.y)))
  
  
  cutoff_score =409
  defaulters_below_cutoff<-length(which(final_dataset$Performance.Tag==1 & final_dataset$Score<cutoff_score))
  defaulters_below_cutoff
  #1196
  
  total_defaulters<-length(which(final_dataset$Performance.Tag==1))
  total_defaulters
  #1425
  
  predicted_defaulters_below_cutoff_score <-ceiling((defaulters_below_cutoff/total_defaulters)*100)
  predicted_defaulters_below_cutoff_score #84
  
  
  # cutoff score of 410 covers 88% of the defaulters
  
  ####################################################
  
  ###Calculating Financial Loss and Profit
  
  ####################################################
  
  # Rejected  Data Set
  
  
  # Profit on the rejected Data if we aquire customer. 
  
  non_default_users_model <- length(which(Rejected_data$Score >  410 & Rejected_data$Performance.Tag==1 ))
  
  Profit2 <- non_default_users_model*4000
  
  Total_Profit <- Profit1+Profit2
  Total_Profit
  
  ##3170133356
  
  ################################
  ## Accuracy Matrix
  ###############################
  
  ## Total records     : 69867+1425
  ## Actual Defaulters : 2947 (Accepted Data set)+1425 (Rejected Data Set)
  ## Using scorecard   : 2514 (Accepted Data set)+1196 (Rejected Data Set)
  
  
  
  
  
