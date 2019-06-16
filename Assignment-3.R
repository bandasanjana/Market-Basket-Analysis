#Author: Sanjana Banda
#Title: Assignment-3
#Purpose: Association Rules

setwd("/Users/sanjana/Documents/Spring2019/TA/R/R-TA")
library(arules)



#******************************************************************************************************************************#
#(a) Import the data to R. Copy the R code used below.
transactions.df <- read.csv(file="transactions.csv", header=TRUE, sep=",")

# Dropping Store and Quantity Columns
transactions.df<- select(transactions.df,-c(1,3))

#Converting the dataframe to transaction data
write.table(transactions.df, file = tmp <- file(), row.names = FALSE)
trans <- read.transactions(tmp, format = "single",
                            header = TRUE, cols = c("Customer", "Product"))
close(tmp)
inspect(head(trans, 10))
#******************************************************************************************************************************#



#******************************************************************************************************************************#
#(b) Set Support to 0.01, Confidence to 0.10, and Min Length to 2. Run apriori to obtain the rules. Sort
rules <- apriori (trans, parameter = list(supp = 0.001, conf = 0.10, minlen = 2))


#the rules according to “Lift” with descending order. Copy the R code used below.
rules_lift <- sort (rules, by="lift", decreasing=TRUE) 
#******************************************************************************************************************************#


#******************************************************************************************************************************#
#(c) Show the top ten Association Rules. Copy the code used and the result below
inspect(head(rules_lift, 10))


#******************************************************************************************************************************#
#(d) What is the highest lift value for the resulting rules? Which rule has this value? Show how this lift value was calculated.

#The highest lift value is for the resulting rules is 15.81754, The rule that has the highest value is {Magazine,Prescription Med}=> {Wrapping Paper}
# The lift is calculated by Confidence of {Magazine,Prescription Med}/ Support of {Wrapping Paper} 
itemFrequency(trans[,])
# Support of Wrapping Paper 
# 0.0517262413 
# Confidenceof {Magazine,Prescription Med}
#0.8181818
0.8181818/ 0.0517262413
# [1] 15.81754
#******************************************************************************************************************************#


#******************************************************************************************************************************#
# (e) Interpret the first five rules in the output in words.

#1.
# {Magazine,Prescription Med} => {Wrapping Paper}, This rule says that if a customer buys {Magazine,Prescription Med}, she/he is 15.817539 times
#likely to buy {Wrapping Paper} (than her/his chance of buying {Wrapping Paper}  if we know nothing about her/him). 

#2.
# {Magazine,Wrapping Paper} => {Prescription Med}, This rule says that if a customer buys {Magazine,Wrapping Paper}, she/he is 15.242355 times
#likely to buy {Prescription Med} (than her/his chance of buying {Prescription Med}  if we know nothing about her/him).

#3.
# {Bow,Perfume}=> {Toothbrush} , This rule says that if a customer buys {Bow,Perfume}, she/he is 10.430080 times
#likely to buy {Toothbrush} (than her/his chance of buying {Toothbrush} if we know nothing about her/him).

#4.
# {Toothbrush,Wrapping Paper}  => {Bow}   , This rule says that if a customer buys {Toothbrush,Wrapping Paper}, she/he is  8.986296times
#likely to buy {Bow} (than her/his chance of buying {Bow} if we know nothing about her/him).

#5.
# {Perfume,Wrapping Paper} => {Toothbrush}   , This rule says that if a customer buys {Perfume,Wrapping Paper}, she/he is  8.755778  times
#likely to buy {Toothbrush} (than her/his chance of buying {Toothbrush} if we know nothing about her/him).
#******************************************************************************************************************************#


#******************************************************************************************************************************#
# (f) Reviewing the top 10 rules, based on their lift ratios, comment on their redundancy and how you
# would assess their utility as a decision maker.
rules_top10 <- head(rules_lift, 10)
subsetRules <- which(colSums(is.subset(rules_top10, rules_lift)) > 1) # get subset rules in vector
length(subsetRules)  #> 6
rules_top10 <- rules_top10[-subsetRules] # remove subset rules.
inspect(rules_top10)



