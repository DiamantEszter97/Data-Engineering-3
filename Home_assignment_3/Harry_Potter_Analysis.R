rm(list=ls())
# install.packages("aws.comprehend", repos = c(cloudyr = "http://cloudyr.github.io/drat", getOption("repos")))

keyTable <- read.csv(paste0("C:/Users/diama/Documents/CEU-Business-Analytics-2020/Data_Engineering_3/", "accessKeys.csv"), header = T) # accessKeys.csv == the CSV downloaded from AWS containing your Acces & Secret keys
AWS_ACCESS_KEY_ID <- as.character(keyTable$Access.key.ID)
AWS_SECRET_ACCESS_KEY <- as.character(keyTable$Secret.access.key)

#activate
Sys.setenv("AWS_ACCESS_KEY_ID" = AWS_ACCESS_KEY_ID,
           "AWS_SECRET_ACCESS_KEY" = AWS_SECRET_ACCESS_KEY,
           "AWS_DEFAULT_REGION" = "eu-west-1") 

library(aws.comprehend)
library(readtext)
library(tidyverse)
library(data.table)
library(xtable)
# install.packages("kableExtra")
library(kableExtra)
library(dplyr)
library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())


# load the texts from internet. It create two value list
book1 <- readtext("https://raw.githubusercontent.com/amephraim/nlp/master/texts/J.%20K.%20Rowling%20-%20Harry%20Potter%201%20-%20Sorcerer's%20Stone.txt")
book2 <- readtext("http://www.glozman.com/TextPages/Harry%20Potter%202%20-%20Chamber%20of%20Secrets.txt")
book3 <- readtext("http://www.glozman.com/TextPages/Harry%20Potter%203%20-%20The%20Prisoner%20Of%20Azkaban.txt")
book4 <- readtext("http://www.glozman.com/TextPages/Harry%20Potter%204%20-%20The%20Goblet%20Of%20Fire.txt")
book5 <- readtext("http://www.glozman.com/TextPages/Harry%20Potter%205%20-%20Order%20of%20the%20Phoenix.txt")
book6 <- readtext("http://www.glozman.com/TextPages/Harry%20Potter%206%20-%20The%20Half%20Blood%20Prince.txt")
book7 <- readtext("http://www.glozman.com/TextPages/Harry%20Potter%207%20-%20Deathly%20Hollows.txt")

# load the texts only in a variable
text1 <- book1$text
text2 <- book2$text
text3 <- book3$text
text4 <- book4$text
text5 <- book5$text
text6 <- book6$text
text7 <- book7$text


# clean all of the text from junk characters
text1 <- str_replace_all(text1, "\n", " ")
text1 <- str_replace_all(text1, "\n\n\"", " ")
text1 <- str_replace_all(text1, "\"", " ")
text1 <- str_remove_all(text1, "--")

text2 <- str_replace_all(text2, "\n", " ")
text2 <- str_replace_all(text2, "\n\n\"", " ")
text2 <- str_replace_all(text2, "\"", " ")
text2 <- str_remove_all(text2, "--")


text3 <- str_replace_all(text3, "\n", " ")
text3 <- str_replace_all(text3, "\n\n\"", " ")
text3 <- str_replace_all(text3, "\"", " ")
text3 <- str_remove_all(text3, "--")

text4 <- str_replace_all(text4, "\n", " ")
text4 <- str_replace_all(text4, "\n\n\"", " ")
text4 <- str_replace_all(text4, "\"", " ")
text4 <- str_remove_all(text4, "--")

text5 <- str_replace_all(text5, "\n", " ")
text5 <- str_replace_all(text5, "\n\n\"", " ")
text5 <- str_replace_all(text5, "\"", " ")
text5 <- str_remove_all(text5, "--")

text6 <- str_replace_all(text6, "\n", " ")
text6 <- str_replace_all(text6, "\n\n\"", " ")
text6 <- str_replace_all(text6, "\"", " ")
text6 <- str_remove_all(text6, "--")

text7 <- str_replace_all(text7, "\n", " ")
text7 <- str_replace_all(text7, "\n\n\"", " ")
text7 <- str_replace_all(text7, "\"", " ")
text7 <- str_remove_all(text7, "--")


# cut the texts by 4000 characters in order to analyze them later on
len <- 4000 

chunks1 <- substring(text1, seq(1, nchar(text1)-1, len), seq(len, nchar(text1), len))
chunks2 <- substring(text2, seq(1, nchar(text2)-1, len), seq(len, nchar(text2), len))
chunks3 <- substring(text3, seq(1, nchar(text3)-1, len), seq(len, nchar(text3), len))
chunks4 <- substring(text4, seq(1, nchar(text4)-1, len), seq(len, nchar(text4), len))
chunks5 <- substring(text5, seq(1, nchar(text5)-1, len), seq(len, nchar(text5), len))
chunks6 <- substring(text6, seq(1, nchar(text6)-1, len), seq(len, nchar(text6), len))
chunks7 <- substring(text7, seq(1, nchar(text7)-1, len), seq(len, nchar(text7), len))

# remove the texts, the books and len
rm(text1, text2, text3, text4, text5, text6, text7,
   book1, book2, book3, book4, book5, book6, book7, len)
# print(chunks1[[1]])
# print(chunks2[[1]])
# print(chunks3[[1]])
# print(chunks4[[1]])
# print(chunks5[[1]])
# print(chunks6[[1]])
# print(chunks7[[1]])


#####
# create data tables for the sentiment analysis:

# create data frame for book1:
book1_df <- data.table()

# assigned cut text to the first column
book1_df$text <- t(rbind(chunks1))

# add columns for adding the sentiment analysis values
book1_df$Index <- cbind("")
book1_df$Sentiment <- cbind("")
book1_df$Mixed <- cbind("")
book1_df$Negative <- cbind("")
book1_df$Neutral <- cbind("")
book1_df$Positive <- cbind("")

# remove last row because it is empty:
book1_df <- book1_df[-nrow(book1_df),]

  # assign the outputs of sentiment analysis to the data table of book1
for (i in 1:nrow(book1_df)) {
  t <- detect_sentiment(book1_df$text[i])
  book1_df$Index[i] <- i
  book1_df$Sentiment[i] <- t[[2]]
  book1_df$Mixed[i] <- t[[3]]
  book1_df$Negative[i] <- t[[4]]
  book1_df$Neutral[i] <- t[[5]]
  book1_df$Positive[i] <- t[[6]]
}



# create data frame for book2:
book2_df <- data.table()

# assigned cut text to the first column
book2_df$text <- t(rbind(chunks2))

# add columns for adding the sentiment analysis values
book2_df$Index <- cbind("")
book2_df$Sentiment <- cbind("")
book2_df$Mixed <- cbind("")
book2_df$Negative <- cbind("")
book2_df$Neutral <- cbind("")
book2_df$Positive <- cbind("")

# remove last row because it is empty:
book2_df <- book2_df[-nrow(book2_df),]

# assign the outputs of sentiment analysis to the data table of book2
for (i in 1:nrow(book2_df)) {
  t <- detect_sentiment(book2_df$text[i])
  book2_df$Index[i] <- i
  book2_df$Sentiment[i] <- t[[2]]
  book2_df$Mixed[i] <- t[[3]]
  book2_df$Negative[i] <- t[[4]]
  book2_df$Neutral[i] <- t[[5]]
  book2_df$Positive[i] <- t[[6]]
}


# create data frame for book3:
book3_df <- data.table()

# assigned cut text to the first column
book3_df$text <- t(rbind(chunks3))

# add columns for adding the sentiment analysis values
book3_df$Index <- cbind("")
book3_df$Sentiment <- cbind("")
book3_df$Mixed <- cbind("")
book3_df$Negative <- cbind("")
book3_df$Neutral <- cbind("")
book3_df$Positive <- cbind("")

# remove last row because it is empty:
book3_df <- book3_df[-nrow(book3_df),]

# assign the outputs of sentiment analysis to the data table of book3
for (i in 1:nrow(book3_df)) {
  t <- detect_sentiment(book3_df$text[i])
  book3_df$Index[i] <- i
  book3_df$Sentiment[i] <- t[[2]]
  book3_df$Mixed[i] <- t[[3]]
  book3_df$Negative[i] <- t[[4]]
  book3_df$Neutral[i] <- t[[5]]
  book3_df$Positive[i] <- t[[6]]
}


# create data frame for book4:
book4_df <- data.table()

# assigned cut text to the first column
book4_df$text <- t(rbind(chunks4))

# add columns for adding the sentiment analysis values
book4_df$Index <- cbind("")
book4_df$Sentiment <- cbind("")
book4_df$Mixed <- cbind("")
book4_df$Negative <- cbind("")
book4_df$Neutral <- cbind("")
book4_df$Positive <- cbind("")

# remove last row because it is empty:
book4_df <- book4_df[-nrow(book4_df),]

# assign the outputs of sentiment analysis to the data table of book4
for (i in 1:nrow(book4_df)) {
  t <- detect_sentiment(book4_df$text[i])
  book4_df$Index[i] <- i
  book4_df$Sentiment[i] <- t[[2]]
  book4_df$Mixed[i] <- t[[3]]
  book4_df$Negative[i] <- t[[4]]
  book4_df$Neutral[i] <- t[[5]]
  book4_df$Positive[i] <- t[[6]]
}


# create data frame for book5:
book5_df <- data.table()

# assigned cut text to the first column
book5_df$text <- t(rbind(chunks5))

# add columns for adding the sentiment analysis values
book5_df$Index <- cbind("")
book5_df$Sentiment <- cbind("")
book5_df$Mixed <- cbind("")
book5_df$Negative <- cbind("")
book5_df$Neutral <- cbind("")
book5_df$Positive <- cbind("")

# remove last row because it is empty:
book5_df <- book5_df[-nrow(book5_df),]

# assign the outputs of sentiment analysis to the data table of book5
for (i in 1:nrow(book5_df)) {
  t <- detect_sentiment(book5_df$text[i])
  book5_df$Index[i] <- i
  book5_df$Sentiment[i] <- t[[2]]
  book5_df$Mixed[i] <- t[[3]]
  book5_df$Negative[i] <- t[[4]]
  book5_df$Neutral[i] <- t[[5]]
  book5_df$Positive[i] <- t[[6]]
}


# create data frame for book6:
book6_df <- data.table()

# assigned cut text to the first column
book6_df$text <- t(rbind(chunks6))

# add columns for adding the sentiment analysis values
book6_df$Index <- cbind("")
book6_df$Sentiment <- cbind("")
book6_df$Mixed <- cbind("")
book6_df$Negative <- cbind("")
book6_df$Neutral <- cbind("")
book6_df$Positive <- cbind("")

# remove last row because it is empty:
book6_df <- book6_df[-nrow(book6_df),]

# assign the outputs of sentiment analysis to the data table of book6
for (i in 1:nrow(book6_df)) {
  t <- detect_sentiment(book6_df$text[i])
  book6_df$Index[i] <- i
  book6_df$Sentiment[i] <- t[[2]]
  book6_df$Mixed[i] <- t[[3]]
  book6_df$Negative[i] <- t[[4]]
  book6_df$Neutral[i] <- t[[5]]
  book6_df$Positive[i] <- t[[6]]
}



# create data frame for book7:
book7_df <- data.table()

# assigned cut text to the first column
book7_df$text <- t(rbind(chunks7))

# add columns for adding the sentiment analysis values
book7_df$Index <- cbind("")
book7_df$Sentiment <- cbind("")
book7_df$Mixed <- cbind("")
book7_df$Negative <- cbind("")
book7_df$Neutral <- cbind("")
book7_df$Positive <- cbind("")

# remove last row because it is empty:
book7_df <- book7_df[-nrow(book7_df),]

# assign the outputs of sentiment analysis to the data table of book6
for (i in 1:nrow(book7_df)) {
  t <- detect_sentiment(book7_df$text[i])
  book7_df$Index[i] <- i
  book7_df$Sentiment[i] <- t[[2]]
  book7_df$Mixed[i] <- t[[3]]
  book7_df$Negative[i] <- t[[4]]
  book7_df$Neutral[i] <- t[[5]]
  book7_df$Positive[i] <- t[[6]]
}

# remove chunks, i and t:
rm(chunks1, chunks2, chunks3, chunks4, chunks5, chunks6, chunks7, t, i)



######################
# analysis of books

# making the numeric columns numeric
book1_df$Index <- as.numeric(book1_df$Index)
book1_df$Mixed <- as.numeric(book1_df$Mixed)
book1_df$Negative <- as.numeric(book1_df$Negative)
book1_df$Neutral <- as.numeric(book1_df$Neutral)
book1_df$Positive <- as.numeric(book1_df$Positive)

# create basic statistics for Positive values
p1 <- summarise(book1_df,
                sum = sum(Positive),
                mean = mean(Positive),
                median = median(Positive),
                std =sd(Positive),
                min = min(Positive),
                max = max(Positive))

p1$n <- book1_df %>% nrow()

# create basic statistics for Negative values
n1 <- summarise(book1_df,
                sum = sum(Negative),
                mean = mean(Negative),
                median = median(Negative),
                std =sd(Negative),
                min = min(Negative),
                max = max(Negative))

n1$n <- book1_df %>% nrow()

# put the basic statistic into one table:
summary_1 <- rbind(p1,n1)
rownames(summary_1) <- c("Positive", "Negative")



# making the numeric columns numeric
book2_df$Index <- as.numeric(book2_df$Index)
book2_df$Mixed <- as.numeric(book2_df$Mixed)
book2_df$Negative <- as.numeric(book2_df$Negative)
book2_df$Neutral <- as.numeric(book2_df$Neutral)
book2_df$Positive <- as.numeric(book2_df$Positive)

# create basic statistics for Positive values
p2 <- summarise(book2_df,
                sum = sum(Positive),
                mean = mean(Positive),
                median = median(Positive),
                std =sd(Positive),
                min = min(Positive),
                max = max(Positive))

p2$n <- book2_df %>% nrow()

# create basic statistics for Negative values
n2 <- summarise(book2_df,
                sum = sum(Negative),
                mean = mean(Negative),
                median = median(Negative),
                std =sd(Negative),
                min = min(Negative),
                max = max(Negative))

n2$n <- book2_df %>% nrow()

# put the basic statistic into one table:
summary_2 <- rbind(p2,n2)
rownames(summary_2) <- c("Positive", "Negative")





# making the numeric columns numeric
book3_df$Index <- as.numeric(book3_df$Index)
book3_df$Mixed <- as.numeric(book3_df$Mixed)
book3_df$Negative <- as.numeric(book3_df$Negative)
book3_df$Neutral <- as.numeric(book3_df$Neutral)
book3_df$Positive <- as.numeric(book3_df$Positive)

# create basic statistics for Positive values
p3 <- summarise(book3_df,
                sum = sum(Positive),
                mean = mean(Positive),
                median = median(Positive),
                std =sd(Positive),
                min = min(Positive),
                max = max(Positive))

p3$n <- book3_df %>% nrow()

# create basic statistics for Negative values
n3 <- summarise(book3_df,
                sum = sum(Negative),
                mean = mean(Negative),
                median = median(Negative),
                std =sd(Negative),
                min = min(Negative),
                max = max(Negative))

n3$n <- book3_df %>% nrow()

# put the basic statistic into one table:
summary_3 <- rbind(p3,n3)
rownames(summary_3) <- c("Positive", "Negative")





# making the numeric columns numeric
book4_df$Index <- as.numeric(book4_df$Index)
book4_df$Mixed <- as.numeric(book4_df$Mixed)
book4_df$Negative <- as.numeric(book4_df$Negative)
book4_df$Neutral <- as.numeric(book4_df$Neutral)
book4_df$Positive <- as.numeric(book4_df$Positive)

# create basic statistics for Positive values
p4 <- summarise(book4_df,
                sum = sum(Positive),
                mean = mean(Positive),
                median = median(Positive),
                std =sd(Positive),
                min = min(Positive),
                max = max(Positive))

p4$n <- book4_df %>% nrow()

# create basic statistics for Negative values
n4 <- summarise(book4_df,
                sum = sum(Negative),
                mean = mean(Negative),
                median = median(Negative),
                std =sd(Negative),
                min = min(Negative),
                max = max(Negative))

n4$n <- book4_df %>% nrow()

# put the basic statistic into one table:
summary_4 <- rbind(p4,n4)
rownames(summary_4) <- c("Positive", "Negative")





# making the numeric columns numeric
book5_df$Index <- as.numeric(book5_df$Index)
book5_df$Mixed <- as.numeric(book5_df$Mixed)
book5_df$Negative <- as.numeric(book5_df$Negative)
book5_df$Neutral <- as.numeric(book5_df$Neutral)
book5_df$Positive <- as.numeric(book5_df$Positive)

# create basic statistics for Positive values
p5 <- summarise(book5_df,
                sum = sum(Positive),
                mean = mean(Positive),
                median = median(Positive),
                std =sd(Positive),
                min = min(Positive),
                max = max(Positive))


p5$n <- book5_df %>% nrow()

# create basic statistics for Negative values
n5 <- summarise(book5_df,
                sum = sum(Negative),
                mean = mean(Negative),
                median = median(Negative),
                std =sd(Negative),
                min = min(Negative),
                max = max(Negative))

n5$n <- book5_df %>% nrow()

# put the basic statistic into one table:
summary_5 <- rbind(p5, n5)
rownames(summary_5) <- c("Positive", "Negative")





# making the numeric columns numeric
book6_df$Index <- as.numeric(book6_df$Index)
book6_df$Mixed <- as.numeric(book6_df$Mixed)
book6_df$Negative <- as.numeric(book6_df$Negative)
book6_df$Neutral <- as.numeric(book6_df$Neutral)
book6_df$Positive <- as.numeric(book6_df$Positive)

# create basic statistics for Positive values
p6 <- summarise(book6_df,
                sum = sum(Positive),
                mean = mean(Positive),
                median = median(Positive),
                std =sd(Positive),
                min = min(Positive),
                max = max(Positive))

p6$n <- book6_df %>% nrow()

# create basic statistics for Negative values
n6 <- summarise(book6_df,
                sum = sum(Negative),
                mean = mean(Negative),
                median = median(Negative),
                std =sd(Negative),
                min = min(Negative),
                max = max(Negative))

n6$n <- book6_df %>% nrow()

# put the basic statistic into one table:
summary_6 <- rbind(p6,n6)
rownames(summary_6) <- c("Positive", "Negative")



# making the numeric columns numeric
book7_df$Index <- as.numeric(book7_df$Index)
book7_df$Mixed <- as.numeric(book7_df$Mixed)
book7_df$Negative <- as.numeric(book7_df$Negative)
book7_df$Neutral <- as.numeric(book7_df$Neutral)
book7_df$Positive <- as.numeric(book7_df$Positive)

# create basic statistics for Positive values
p7 <- summarise(book7_df,
                sum = sum(Positive),
                mean = mean(Positive),
                median = median(Positive),
                std =sd(Positive),
                min = min(Positive),
                max = max(Positive))

p7$n <- book7_df %>% nrow()

# create basic statistics for Negative values
n7 <- summarise(book6_df,
                sum = sum(Negative),
                mean = mean(Negative),
                median = median(Negative),
                std =sd(Negative),
                min = min(Negative),
                max = max(Negative))

n7$n <- book7_df %>% nrow()


# put the basic statistic into one table:
summary_7 <- rbind(p7,n7)
rownames(summary_7) <- c("Positive", "Negative")


# total summary:
total_summary <- rbind(summary_1, summary_2, summary_3, summary_4, summary_5, summary_6, summary_7)
total_summary$Sentiment <- c("Positive", "Negative", "Positive", "Negative", "Positive", "Negative", "Positive", "Negative", "Positive", "Negative", "Positive", "Negative", "Positive", "Negative")
total_summary$Title <- c("Sorcerer's Stone", "Sorcerer's Stone", 
                          "Chamber of Secrets", "Chamber of Secrets", 
                          "Prisoner of Azkaban", "Prisoner of Azkaban", 
                          "Globet of Fire", "Globet of Fire",
                          "Order of the Phoenix", "Order of the Phoenix",
                          "Half Blood Prince", "Half Blood Prince", 
                          "Deathly Hallows", "Deathly Hallows")
# set the order of the columns:
col_order <- c("Title", "Sentiment", "sum", "mean", "median", "std", "min", "max", "n")
total_summary <- total_summary[, col_order]


# summary for positive:
positive_summary <- rbind(p1, p2, p3, p4, p5, p6, p7)
positive_summary$Title <- c("Sorcerer's Stone", 
                          "Chamber of Secrets", 
                          "Prisoner of Azkaban", 
                          "Globet of Fire",
                          "Order of the Phoenix",
                          "Half Blood Prince", 
                          "Deathly Hallows")

# set the order of columns:
col_order <- c("Title", "sum", "mean", "median", "std", "min", "max", "n")
positive_summary <- positive_summary[, col_order]


# summary for negative:
negative_summary <- rbind(n1, n2, n3, n4, n5, n6, n7)
negative_summary$Title <- c("Sorcerer's Stone", 
                            "Chamber of Secrets", 
                            "Prisoner of Azkaban", 
                            "Globet of Fire",
                            "Order of the Phoenix",
                            "Half Blood Prince", 
                            "Deathly Hallows")

# set the order of columns:
col_order <- c("Title", "sum", "mean", "median", "std", "min", "max", "n")
negative_summary <- negative_summary[, col_order]

# delete unnecessary data frames:
rm(p1, n1, p2, n2, p3, n3, p4, n4, p5, n5, p6, n6, p7, n7, col_order,
   summary_1, summary_2, summary_3, summary_4, summary_5, summary_6, summary_7)


# the most positive book:

# create column frequency:
positive_summary$frequency <- ""

# the sum of positives divided with the number of observations
for (i in 1:nrow(positive_summary)) {
  positive_summary$frequency[i] <- (positive_summary$sum[i]/positive_summary$n[i])
}

# set is as numeric:
positive_summary$frequency <- as.numeric(positive_summary$frequency)

# arrange the order of books by frequency that gives the order of positiveness of the books:
positive_summary <- positive_summary %>% arrange(-frequency)



# the most negative book:

# create column frequency:
negative_summary$frequency <- ""

# the sum of positives divided with the number of observations
for (i in 1:nrow(negative_summary)) {
  negative_summary$frequency[i] <- (negative_summary$sum[i]/negative_summary$n[i])
}

# set is as numeric:
negative_summary$frequency <- as.numeric(negative_summary$frequency)

# arrange the order of books by frequency that gives the order of positiveness of the books:
negative_summary <- negative_summary %>% arrange(-frequency)


#################
# HP1: 1 - 109
# HP2: 110 - 231
# HP3: 232 - 384
# HP4: 385 - 660
# HP5: 661 - 1030
# HP6: 1031 - 1275
# HP7: 1276 - 1557

#piut every table together: 
hp_total_df <- rbind(book1_df, book2_df, book3_df, book4_df, book5_df, book6_df, book7_df)

hp_total_df$book <- ""
# set index 1 to 1557:
for (i in (1:nrow(hp_total_df))) {
  hp_total_df$Index[i] <- i
  
  if (i < 110) {
    hp_total_df$book[i] <- "Harry Potter and the Sorcerer's Stone"
  }
  else if (i >= 110 && i < 232) {
    hp_total_df$book[i] <-  "Harry Potter and the Chamber of Secrets"
  }
  else if (i >= 232 && i < 385) {
    hp_total_df$book[i] <-  "Harry Potter and the Prisoner of Azkaban"
  }
  else if (i >= 385 && i < 661) {
    hp_total_df$book[i] <-  "Harry Potter and the Globet of Fire"
  }
  else if (i >= 661 && i < 1031) {
    hp_total_df$book[i] <-  "Harry Potter and the Order of the Phoenix"
  }
  else if (i >= 1031 && i < 1276) {
    hp_total_df$book[i] <-  "Harry Potter and the Half Blood Prince"
  } 
  else if (hp_total_df$Index[i] >= 1276) {
    hp_total_df$book[i] <-  "Harry Potter and the Deathly Hallows"
  }
}


ggplot(hp_total_df, aes(x = Index)) +
  geom_smooth(aes(y = Positive), method = "loess", color = "red") +
  geom_smooth(aes(y= Negative), method = "loess", color = "blue") +
  labs(y = "Sentiments") +
  scale_x_continuous(breaks = c(1, 110, 231, 384, 660, 1030, 1275, 1557)) +
  theme(legend.position = "bottom")


ggplot(hp_total_df, aes(x = Index)) +
  geom_smooth(aes(y = Positive, color = book), method = "loess") +
  geom_smooth(aes(y= Negative, color = book), method = "loess") +
  geom_smooth(aes(y = Positive), method = "loess", color = "red4") +
  geom_smooth(aes(y= Negative), method = "loess", color = "blue4") +
  labs(y = "Sentiments") +
  scale_x_continuous(breaks = c(1, 110, 231, 384, 660, 1030, 1275, 1557)) +
  theme(legend.position = "bottom")


ggplot() +
  geom_bar(data=negative_summary, aes(x= reorder(Title, -frequency), y = frequency), stat = 'identity',  fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot() +
  geom_bar(data=positive_summary, aes(x= reorder(Title, -frequency), y = frequency), stat = 'identity',  fill = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





