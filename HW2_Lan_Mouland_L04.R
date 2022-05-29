library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(data.table)

## if i was giving my user defined functions  to a person to use i would include##
##install.packages("dplyr")


#at the bottom of the workspace I go through how my functions work

########################################
################Question 1#############
########################################
####functions created for question 1

## used for 1-10

#set your pre-test score to S, post-test score to R, P to P, and B to B

test.score <- function(S,P,B,R)
  if(S < P & R>= P+B) {
    print(paste("Updated grade is",P+B))
  } else if(S >= P & R>= S+B) {
    print(paste("Updated grade is",S+B))
  } else {
    print(paste("Updated grade is",R))
  }


#set your pre-test score to S, P to P, and B to B

r.score <- function(S,P,B) {
  if(S < P) {
    print(paste("Updated grade is",P+B))
  } else if (S >= P) {
    print(paste("Updated grade is",S+B))
  }
}

## used for 11-20
#https://cran.r-project.org/web/packages/crunch/vignettes/filters.html
#https://www.edureka.co/community/4963/creating-an-empty-data-frame-with-only-column-names-r
#https://www.datamentor.io/r-programming/if-else-statement/
#https://csawesome.runestone.academy/runestone/books/published/csawesome/Unit4-Iteration/topic-4-4-nested-loops.html
#https://www.datamentor.io/r-programming/while-loop/
## if i was giving my user defined functions  to a person to use i would include##
##install.packages("dplyr")


#set exam to whatever the imported test scores are as exam, e.g. thresh.boost(exam = "exam score name")


thresh.boost <- function(exam) {
  library(dplyr)
  scores <- data.frame(s = as.numeric(0), r = as.numeric(0))
  differences <- data.frame(p = as.numeric(0), b = as.numeric(0), min.sd = as.numeric(0), rmean = as.numeric(0))
  n <- length(exam)
  i <- 1
  j <- 1
  k <- 1
  m <- 1
  for(i in 1:200) {
    for(j in 1:n) {
      test.scores <- exam[j]
      scores[j,1] <- test.scores 
    }
    for(k in 1:200) {
      P.temp <- sample(seq.int(14,46,2), 1)
      B.temp <- sample(seq.int(14,46,2), 1)
      while ((P.temp + B.temp) != 60) {
        P.temp <- sample(seq.int(14,46,2), 1)
        B.temp <- sample(seq.int(14,46,2), 1)
        next
      }
      for(m in 1:n) {
        if(scores[m,1] < P.temp) {
          scores[m,2] <- P.temp + B.temp
          next
        }
        if(scores[m,1] >= P.temp) {
          scores[m,2] <- scores[m,1] + B.temp
          next
        }
      }
    }
    differences[i,1] <- P.temp
    differences[i,2] <- B.temp
    differences[i,3] <- sd(exam) - sd(scores[,2])
    differences[i,4] <- mean(scores[,2])
  }
  differences <-  differences[(differences$rmean > 70 & differences$rmean < 75),]
  low <- differences[which.min(differences[,3]), 1:2]
  return(low)
}


#------------------------------------------------------------------------------#

#Q1 50

test.score(21,30,30,50)

#Q2 60

test.score(21,30,30,60)

#Q3 60

test.score(21,30,30,70)

#Q4 60

r.score(21,30,30)

#Q5 67

test.score(54,22,30,67)

#Q6 78

test.score(54,22,30,78)

#Q7 84

test.score(54,22,30,89)

#Q8 84

r.score(54,22,30)

#Q9 60

r.score(22,30,30)

#Q10 116

r.score(86,30,30)

#Q11/12 ------ P = 26 / B = 34
## the code takes a while to run. After an initial analysis of the outputs of my function, 
# if i did not run enough combinations of P & B it might give me the right answer or give me the lowest P and B combo it simulated
#therefore it runs 200 times to hopefully get the ideal P & B values as per the answers to the D2L questions.

thresh.boost(exam_scores1)

#Q13/14 ------ P = 30 / B = 30
## the code takes a while to run. After an initial analysis of the outputs of my function, 
# if i did not run enough combinations of P & B it might give me the right answer or give me the lowest P and B combo it simulated
#therefore it runs 200 times to hopefully get the ideal P & B values as per the answers to the D2L questions.

thresh.boost(exam_scores2)

#Q15/16 ------ P = 38 / B = 22
## the code takes a while to run. After an initial analysis of the outputs of my function, 
# if i did not run enough combinations of P & B it might give me the right answer or give me the lowest P and B combo it simulated
#therefore it runs 200 times to hopefully get the ideal P & B values as per the answers to the D2L questions.

thresh.boost(exam_scores3)

#Q17/18 ------ P = 42 / B = 18
## the code takes a while to run. After an initial analysis of the outputs of my function, 
# if i did not run enough combinations of P & B it might give me the right answer or give me the lowest P and B combo it simulated
#therefore it runs 200 times to hopefully get the ideal P & B values as per the answers to the D2L questions.

thresh.boost(exam_scores4)

#Q19/20 ------ P = 22 / B = 38
## the code takes a while to run. After an initial analysis of the outputs of my function, 
# if i did not run enough combinations of P & B it might give me the right answer or give me the lowest P and B combo it simulated
#therefore it runs 200 times to hopefully get the ideal P & B values as per the answers to the D2L questions.

thresh.boost(exam_scores5)


########################################
################Question 2#############
########################################

#https://www.educba.com/random-number-generator-in-r/
##https://stackoverflow.com/questions/8273313/sample-random-rows-in-dataframe

#q 21 2
#q 22 yes
#q 23 yes
#q 24 yes
#q 25 no


##load CSV file as class.ros "NAME" <- read.CSV("name of CSV file")
##then set N as how many students you want to call on, and set class.ros = 
#to whatever you named your imported CSV file 
#see below for example

BTMA431L04 <- read.csv("BTMA 431 L04 - (Fall 2021).csv", stringsAsFactors = FALSE) 

random.stu.func <- function(N, class.ros)
  return(class.ros[sample(nrow(class.ros), N),])

random.stu.func(3, BTMA431L04)

########################################
################Question 3#############
########################################

#Q26 YES
#Q27 1
#Q28 no
#Q29 no
#Q30 no

#https://gt.rstudio.com/reference/cols_merge.html
#https://www.guru99.com/r-apply-sapply-tapply.html
#https://stackoverflow.com/questions/27954795/using-r-randomly-assigning-students-into-groups-of-4
#https://stackoverflow.com/questions/29836246/odd-behavior-of-shift-function-in-data-table-v1-9-5-r

#IGNORE THOSEEEEE SILLY WARNINGS THEY DONT MATTER WE GET THE TABLE AHAHAAAAHHAHA

## if i was giving my user defined functions  to a person to use i would include##
##install.packages("data.table")

#way to use peer.rev func
#------- set class.ros = to whatever you named your imported CSV file  ------ peer.rev(class.ros = test)
#see question 2 on how I imported a CSV of a class roster. 

peer.rev <- function(class.ros) {
  library(data.table)
  df <- data.frame(paste(class.ros$Last.Name,".",class.ros$First.Name))
  colnames(df) <- "reviewer"
  df["r1"] <- data.frame(shift(df, type = "lead", fill = df[3,1])) 
  
  df["r2"] <- data.frame(shift(df, type = "lead", n = 2L, fill = df[2,1]))
  
  df["r3"] <- data.frame(shift(df, type = "lead", n = 3L, fill = df[1,1]))
  return(df)
}

peer.rev(BTMA431L04)

########################################
################Question 4#############
########################################

#https://rfaqs.com/for-loop-in-r-simulating-data-using-for-loop/
#http://www.rexamples.com/5/Guess%20a%20random%20number%20game
#https://cran.r-project.org/web/packages/magicfor/vignettes/magicfor.html
#https://datacarpentry.org/semester-biology/materials/for-loops-R/

#set s as the number you would guess & choices as a string of number opponent can 
#choose from


expected.losses <- function(s, choices) {
  losses <- vector(mode = "numeric", length= length(choices))
  for (i in 1:length(choices)) {
    loss <- ( s - choices[i])^2
    losses[i] <- loss
  } 
  return(mean(losses)) 
}

#in this case its any number from 0 - 1000

choices <- 0:1000


#------------------------------4a 
#305000 q31

expected.losses(30, choices)

#------------------------------4b
#285000 q32

expected.losses(950, choices)

#------------------------------4c
#question 33, 85000

expected.losses(450, choices)

#------------------------------4d

# for question 34 500: 334000 was added since 
#it would be the largest expected loss if I chose 0 or 1000.  
s <- 0:1000
a <- s * (s - 1000) + 334000
qplot(s,a)
which.min(a)


#------------------------------4e
#q35 85000

expected.losses(500, choices)

#------------------------------4f
##q36 12390

#https://www.youtube.com/watch?v=QBbC3Cjsnjg

#the below is a test to see if my function would print the middle most value in the data set. 
c <- seq.int(0,1500, by = 2)

#put vector of numbers as num in numgame function ------ best.num(num = test)

best.num <- function(num) {
  df <- sort(num)
  return(mean(df))
}

best.num(HW2Q4)

################################################################################
##############Function Explanation##############################################
################################################################################


#follows guidelines and outputs what the updated score will be 
test.score <- function(S,P,B,R)
  if(S < P & R>= P+B) {
    print(paste("Updated grade is",P+B))
  } else if(S >= P & R>= S+B) {
    print(paste("Updated grade is",S+B))
  } else {
    print(paste("Updated grade is",R))
  }


#compares S values to S and P values, then applies P+B or S+B
r.score <- function(S,P,B) {
  if(S < P) {
    print(paste("Updated grade is",P+B))
  } else if (S >= P) {
    print(paste("Updated grade is",S+B))
  }
}



#------------------------------------------------------------------------------#
#using a combination of loops, conditional formatting, and empty data frames 
#I was able to achieve the requirements of the function


thresh.boost <- function(exam) {
  library(dplyr)
  #table for pre-test scores and post-test scores
  scores <- data.frame(s = as.numeric(0), r = as.numeric(0))
  #creates an empty df to store p, b, sd difference (between s and r), and r mean for combinations of P & B
  differences <- data.frame(p = as.numeric(0), b = as.numeric(0), min.sd = as.numeric(0), rmean = as.numeric(0))
  n <- length(exam)
  i <- 1
  j <- 1
  k <- 1
  m <- 1
  #outter loop that stores loop results in "differences" df
  for(i in 1:200) {
    #loop to store s values in "scores" df 
    for(j in 1:n) {
      test.scores <- exam_scores5[j]
      scores[j,1] <- test.scores 
    }
    #loop to choose even p and b values, i limited the numbers starting and ending numbers to narrow what values P & B could be
    # in order to have less simulations
    # starting at variable k for loop is where multiple P & B values are selected and applied to s scores
    for(k in 1:200) {
      P.temp <- sample(seq.int(14,46,2), 1, replace = TRUE)
      B.temp <- sample(seq.int(14,46,2), 1, replace = TRUE)
      #while loop checks if P and B is not equal to 60, and if not assigns them new values until equal to 60 then moves on in the loop
      #I chose "not equal to 60" instead of < because I want my combos of P and B to be as close to 60 as possible 
      #letting P + B go beyond 60 would require more simulations to get the best possible P & B score
      while ((P.temp + B.temp) != 60) {
        P.temp <- sample(seq.int(14,46,2), 1, replace = TRUE)
        B.temp <- sample(seq.int(14,46,2), 1, replace = TRUE)
        next
      }
      #starts checking s values to p value then applies proper equation, finally storing that value in "r" column of scores
      for(m in 1:n) {
        if(scores[m,1] < P.temp) {
          scores[m,2] <- P.temp + B.temp
          next
        }
        if(scores[m,1] >= P.temp) {
          scores[m,2] <- scores[m,1] + B.temp
          next
        }
      }
    }
    #stores the P & B values that was applied to post-test scores in table then takes the difference of sd and r mean 
    differences[i,1] <- P.temp
    differences[i,2] <- B.temp
    differences[i,3] <- sd(exam) - sd(scores[,2])
    differences[i,4] <- mean(scores[,2])
    #loop then runs through 199 more times for more P & B combos
  }
  #filters out any r mean that is 70<mean(r)<75
  differences <-  differences[(differences$rmean > 70 & differences$rmean < 75),]
  #finds row with lowest sd difference then shows the p and b value associated 
  #subsets differences df to print entire row, but instead I filtered it to only show the p and b column
  low <- differences[which.min(differences[,3]), 1:2]
  #finally returns p and b value that will product a mean between 70 and 75 with the lowest difference in sd 
  #between initial scores and final scores 
  return(low)
}
#------------------------------------------------------------------------------#
random.stu.func <- function(N, class.ros)
  #sample randomly selects N students from class.ros. 
  #simple but effective 
  #subsets the sample function to print entire row 
  return(class.ros[sample(nrow(class.ros), N),])
#------------------------------------------------------------------------------#
#okay im pretty proud of my simplistic approach to this function
#My original approach to this question was to create nested loops that added peoples names randomly
#however, the lecture stated that the output need only look random to the students 
#I started visualizing the columns as circles where if they were shifted up by x amount 
#that would push some names from the top of the list around the circle to the bottom 
#after some experimenting and researching I came across the package "data.table" that had 
# the exact method I wanted to complete the requirements of the function 
#though it does produces a warning the output is still a unique list that satisfies all requirements 
#the error is because the every subsequent use of the shift function tries to create 
#another column from the previously shifted column. fixing that outcome would create too many columns. so I believe 
#the warning works in my favor hence why I did not attempt to fix it. 


peer.rev <- function(class.ros) {
  library(data.table)
  #creates table of combined last and first names to make them more unique 
  df <- data.frame(paste(class.ros$Last.Name,"-",class.ros$First.Name))
  colnames(df) <- "reviewer"
  #shift second column up by 1 and fill table with third person's name at the bottom 1 time
  df["r1"] <- data.frame(shift(df, type = "lead", fill = df[3,1])) 
  #shift third column up by 2 and fill table with second person's name at the bottom 2 times
  df["r2"] <- data.frame(shift(df, type = "lead", n = 2L, fill = df[2,1]))
  #shift fourth column up by 3 and fill table with first person's name at the bottom 3 times
  df["r3"] <- data.frame(shift(df, type = "lead", n = 3L, fill = df[1,1]))
  return(df)
}

#------------------------------------------------------------------------------#

#don't really know what to put here... 
#literally allows you to set string of numbers as num = "x"
#then the function just takes the mean. 
# I originally coded a function that ran simulations but the output ended up not being 
#the correct D2L answer. And given the above questions and how the mean produced the 
#lowest expected loss I figured it would be the same for any string of numbers 
best.num <- function(num) {
  df <- sort(num)
  return(mean(df))
}

