
# The Beige Book (http://www.federalreserve.gov/monetarypolicy/beigebook)
# The https_function, located on the authors' GitHub account 

## Step 1: Load set up Working Directory ----------------------------------------------

# Run sentiment.R function

setwd("//filer01/rwesslen/My Documents/BeigeBookSentimentAnalysis-master")

## Step 2: Import in Dictionaries -----------------------------------------------------

# import positive lexicons from your local directory defined in earlier step
pos<- scan(file.path('positive-words.txt'), what = 'character', 
           comment.char = ';')

# import financial positive lexicon from your local directory defined in earlier step
pos_finance<- scan(file.path('LoughranMcDonald_pos.csv'), 
                   what = 'character', comment.char = ';')

# combine both files into one
pos_all<- c(pos, pos_finance)

# import negative lexicons from your local directory defined in earlier step
neg<- scan(file.path('negative-words.txt'), what = 'character', comment.char = ';')

# import financial negative lexicon from your local directory defined in earlier step
neg_finance<- scan(file.path('LoughranMcDonald_neg.csv'), 
                   what = 'character', comment.char = ';')

# combine both files into one
neg_all<- c(neg, neg_finance)

## Step 3: Import Corpus/Text --------------------------------------------------------

# Import Beige Book data from Github and create a new data frame. 
# *Important* You have three options when ingesting Beige Book data. 
# beigebook_summary.csv is three years of data (2011 - 2013) 
# bb_full.csv is sixteen years of data (1996 - 2011) 
# BB_96_2013.csv is eighteen years of data (1996 - 2013)

#BB <- read.csv("beigebook_summary.csv", sep=",")
BB <- read.csv("BB_96_2013.csv", sep=",")

library(reshape)
colnames(BB)
# [1] "year""month""text"
cast(BB, year ~ month, length)


bad <- is.na(BB)   # create a new object "bad" that will hold missing data, in this case from BB.
BB[bad] 		# return all missing elements(
# character(0) 		
# returns zero missing elements. Alternately, adding !before "bad"				
# will return all good elements.

## Step 4: Pre-Processing  --------------------------------------------------------

# regular expressions help us clean our data
# gsub is a function of the R package grep and replaces content that matches are search
# gsub substitutes punctuation (must be surrounded by another set of square brackets    
# when used in a regular expression with # a space ' ' 

BB$text<- gsub('[[:punct:]]', ' ', as.character(BB$text))	
# gsub substitutes character classes that do not give an output such as feed, backspace and tabspaces with a space ' '. 
BB$text<- gsub('[[:cntrl:]]', ' ', BB$text)
# gsub substitutes numerical values with digits of one or greater with a space ' '. 
BB$text<- gsub('\\d+', ' ', BB$text)
# we are going to simplify our data frame and keep the clean text as well as keep both      
# year and a concatenated version of year/month/day and will format the latter. 
BB.text <- as.data.frame(BB$text)
BB.text$year<- BB$year
BB.text$Date <- as.Date( paste(BB$year, BB$month, BB$day, sep = "-" )  , format = 	"%Y-%m-%d" )
BB.text$Date <- strptime(as.character(BB.text$Date), "%Y-%m-%d")
colnames(BB.text) <- c("text", "year", "date")
colnames(BB.text)
# [1] "text" "year" "date"

## Step 5: Create Corpus & Tokenize  -----------------------------------------------------

# We can perform much of the same cleaning of the data using the tm package, 
# but our data then needs to be in a corpus, whereas regular expressions work on character vectors. 
#install.packages("tm")
library(tm)
v <- as.vector(BB.text$text) 
bb_corpus<- Corpus(VectorSource(v))
# tm_map allows transformation to a corpora. 
# getTransformations() shows us what transformations are available # via the tm_map function
getTransformations()
# "as.PlainTextDocument" "removeNumbers"        "removePunctuation"    	"removeWords"          "stemDocument" "stripWhitespace"  
bb_corpus<- tm_map(bb_corpus, content_transformer(tolower))
inspect(bb_corpus)



# # before cleaning:
# "The manufacturing sector continued to recover across all Districts." (2011,1)
# # after cleaning: 
# "the manufacturing sector continued to recover across all districts" (2011,1)(


# Stemming is rather useful for reducing words down to their core element or stem, 
# as we show in the Naive Bayes and IRT examples. An example of stemming for the words 
# stemming and stems would be stem - effectively dropping the -ing and -s suffixes, respectively. 

## Step 6: Stemming & Stop Words -----------------------------------------------------

# stemming can be done easily
# we just need the SnowBall C package, in addition to tm
# install.packages("SnowballC")
library(SnowballC)
bb.text_stm<- tm_map(bb_corpus, stemDocument)

# Standard stopwords such as the "SMART" list can be found in the tm package. 
stnd.stopwords<- stopwords("SMART")
head(stnd.stopwords)
length(stnd.stopwords)
# [1] 571

# the standard stopwords are useful starting points but we may want to add corpus specific words 
# the words below have been added as a consequence of exploring BB from subsequent steps
bb.stopwords<- c(stnd.stopwords, "district", "districts", "reported", "noted", "city", "cited", 	
                 "activity", "contacts", "chicago", "dallas", "kansas", "san", "richmond", "francisco", 	
                 "cleveland", "atlanta", "sales", "boston", "york", "philadelphia", "minneapolis", 
                 "louis", "services","year", "levels", " louis")

# bb.stopwords is a combination of stnd.stopwords and our custom list above. You can certainly imagine another scenario where these city names are kept and words associated with city names are examined. For this analysis, however, they were dropped. 

length(bb.stopwords)
# [1] 597

## Step 7: Term-Document Matrix -----------------------------------------------------

# additional cleaning to eliminate words that lack discriminatory power. 
# bb.tf will be used as a control for the creation of our term-document matrix.
bb.tf <- list(weighting = weightTf, stopwords  = bb.stopwords,
              removePunctuation = TRUE,
              tolower = TRUE,
              minWordLength = 4,
              removeNumbers = TRUE)

# create a term-document matrix
bb_tdm<- TermDocumentMatrix(bb_corpus, control = bb.tf)



dim(bb_tdm)
# [1] 1514   21
# bb_tdm
# A term-document matrix (1515 terms, 21 documents)

bb_tdm
# Non-/sparse entries: 5440/26354
# Sparsity           : 83%
# Maximal term length: 18 
# Weighting: term frequency (tf)

class(bb_tdm)
# [1] "TermDocumentMatrix""simple_triplet_matrix"

## Step 8: Explore Common Words -----------------------------------------------------

# We can get all terms n = 1515
Terms(bb_tdm)

bb.frequent<- sort(rowSums(as.matrix(bb_tdm)), decreasing = TRUE)

# sum of frequent words
sum(bb.frequent)
# [1] 8943

# further exploratory data analysis
bb.frequent[1:30]


# look at terms with a minimum frequency
findFreqTerms(bb_tdm, lowfreq = 60)
# [1] "conditions""construction""continued""demand""firms"
# [6] "growth""home""increased""loan""manufacturing"
# [11] "mixed""prices""remained""report""reports"
# [16] "steady""strong"

## Step 9: Add more pos/neg words ----------------------------------------------------

# Let us add some of these positive words:
pos.words<- c(pos_all, "spend", "buy", "earn", "hike", "increase", "increases", 	
              "development", "expansion", "raise", "surged", "add", "added", "advanced", "advances", 	
              "boom", "boosted", "boosting", "waxed",  "upbeat", "surge")

# And add the negative ones:
neg.words = c(neg_all, "earn", "shortfall", "weak", "fell", "decreases", "decreases", 	
              "decreased", "contraction", "cutback", "cuts", "drop", "shrinkage", "reduction", 	
              "abated", "cautious", "caution", "damped", "waned", "undermine", "unfavorable", 	
              "soft", "softening", "soften", "softer", "sluggish", "slowed", "slowdown", "slower", 	
              "recession")	

any(pos.words == "strong")
# [1] TRUE	
# TRUE is returned. Meaning, "strong" is already in our lexicon.
any(pos.words == "made")
# [1] FALSE
# FALSE is returned. Meaning, "made" is not already in our lexicon.

## Step 10: Word Associations -------------------------------------------------

# interestingly, demand is associated with "texas" and "oil"
findAssocs(bb_tdm, "demand", 0.40)

# "katrina" is associated with "evacuees", "new orleans", "hurricane"
findAssocs(bb_tdm, "katrina", 0.30)

# "growth" is associated with "revenue" but also "slower" and "contracted"
findAssocs(bb_tdm, "growth", 0.2)

# Remove sparse terms from term document matrix with
# a numeric value of .95; representing the maximal allowed sparsity.
BB.95 <- removeSparseTerms(bb_tdm, .95)

# Here we are sorting and counting the row sums of BB.95
BB.rsums <- sort(rowSums(as.matrix(BB.95)), decreasing=TRUE)

# We will need to create a data frame with the words and their frequencies. 
BBdf.rsums <- data.frame(word=names(BB.rsums), freq=BB.rsums)
colnames(BBdf.rsums)
# [1] "word" "freq"

## Step 11: Word Cloud ----------------------------------------------------------

# Install RColorBrewer for coloring our wordcloud
# install.packages("RColorBrewer")
library(RColorBrewer)

# RColorBrewer creates nice looking color palettes 
# Create a palette, blue to green, and name it palette using brewer.pal
palette <- brewer.pal(9, "BuGn")
palette <- palette[-(1:2)]

# install.packages("wordcloud")
library(wordcloud)
# Create a png and define where it will be saved and named

# Create a wordcloud and define the words and their frequencies as well as how those word sizes will scale.
bb_wordcloud <- wordcloud(BBdf.rsums$word, BBdf.rsums$freq, scale=c(7,.2), min.freq=4, max.words=200, 
                          random.order=FALSE, colors=palette)

## Step 12: Sentiment Scoring ------------------------------------------------------

# using our score.sentiment function on BB.text$text against pos.words and neg.words
# progress = 'text' is useful for monitoring scoring of large documents
# keep date and year since they are dropped in the score.sentiment output

BB.keeps <- BB.text[,c("date", "year")]
# run score.sentiment on our text field using pos.words and neg.words
BB.score<- score.sentiment(BB.text$text, pos.words, neg.words, .progress = 'text')
# add back BB.keeps to BB.score
BB.sentiment <- cbind(BB.keeps, BB.score)
# colnames(BB.sentiment shows that we kept "text", "date", and "year" field as well as the # new column "score"
colnames(BB.sentiment)
# [1] "date"   "year"  "score"    "text" 

# Examining BB.sentiment$score (three-year dataset) we discover a mean of 33. 
# In other words, most scores are already above zero suggesting that the sentiment is positive, 
# but thereby making interpretability difficult. To improve interpretability we mean-center our data 
# and shift our midpoint value from 33 to zero. The new empirically adjusted center may be interpreted 
# as an empirically neutral midpoint. The histograms below show both raw scores and centered scores.

## Step 13: Normalizing Scores --------------------------------------------------------

# calculate mean from raw score
BB.sentiment$mean <- mean(BB.sentiment$score)
# calculate sum and store it in BB.sum
BB.sum <- BB.sentiment$score
# center the data by subtracting BB.sum from BB.sentiment$mean
BB.sentiment$centered <- BB.sum - BB.sentiment$mean
# we can label observations above and below the centered values with 1 and code N/A values with 0.
BB.sentiment$pos[BB.sentiment$centered>0] <- 1
BB.sentiment$neg[BB.sentiment$centered<0] <- 1
BB.sentiment$neg[is.na(BB.sentiment$neg)] <- 0
BB.sentiment$pos[is.na(BB.sentiment$pos)] <- 0

# we can then sum the values to get a sense how balanced our data is. values show	# good balance on each size or our empirically adjusted neutral midpoint (sixteen years of # data)
sum(BB.sentiment$pos)
# [1] 8
sum(BB.sentiment$neg)
# [1] 13

## Step 14: Score Histograms -----------------------------------------------------------

# we can create a histogram of raw score and centered score to see the impact of mean  # centering
BB.hist <- hist(BB.sentiment$score, main="Sentiment Histogram", xlab="Score", ylab="Frequency")
BB.hist <- hist(BB.sentiment$centered, main="Sentiment Histogram", xlab="Score",ylab="Frequency")

# Some of the upcoming plots will use a package named ggplot2, created by Hadley Wickham. 
# Though difficult to master, ggplot2 offers some rather elegant and powerful graphing. 

## Step 15: Plot Historical Sentiment--------------------------------------------------

# install and load ggplot2	 
# install.packages("ggplot2")
library(ggplot2)
# using the results from the function to score our documents we create a boxplot to 	
# examine the distribution of opinion relating to economic conditions 
# the labeling assumes here that you imported the summary file of three years
BB.boxplot<- ggplot(BB.sentiment, aes(x = BB.sentiment$year, 
                                      y = BB.sentiment$centered, group = BB.sentiment$year)) + 
  
  geom_boxplot(aes(fill = "grey80"), outlier.colour = "black", outlier.shape = 16, outlier.size = 2) +
  guides(fill=FALSE)
# add labels to our boxplot using xlab ("Year"), ylab("Sentiment(Centered)"), and ggtitle             
# ("Economic Sentiment - Beige Book (1996-2013)")
BB.boxplot<- BB.boxplot + xlab("Year") + ylab("Sentiment (Centered)") +
  ggtitle("Economic Sentiment - Beige Book (1996-2013)")
# draw boxplot
BB.boxplot

## Step 16: Run beigebookplots.R file----------------------------------------------------
