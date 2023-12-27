#Author: Albert Ventayol-Boada
#Date: December 27, 2023

rm(list=ls(all=TRUE))
setwd("C:/Users/alveb/Documents/UCSB/Dissertation/Ch_PH/")

source("https://www.stgries.info/exact.matches.2.r") # get exact.matches.2

install.packages(c("sjmisc", 
                   "stringi", 
                   "rpart", 
                   "randomForest", 
                   "pROC",   
                   "caret", 
                   "pdp"))
library(sjmisc)
library(stringi)
library(rpart)
library(randomForest)
library(pROC)
library(caret)
library(pdp)


#### Step 1: Finding matches ####

#Specify the directory path
corpus.files <- dir(           # make corpus.files the content of the directory
  "all_files/",                # <all_files>
  recursive=TRUE,              # browse all sub-folders
  full.names=TRUE)             # keep full path information


#Create collector vector
all.matches <- character()

#For-loop to collect matches and their discourse features
system.time({
  for (i in seq(corpus.files)) {  # for each file
    cat(corpus.files[i], "\n")    
    current.corpus.file <-        # make current.corpus.file the result of
      scan(corpus.files[i],       # loading the i-th corpus file
           what=character(),      # which contains text, not numbers
           sep="\n",              # elements are separated by line breaks
           quote="",              # there are no quotes in there
           comment.char="",       # and no comment characters
           quiet=TRUE,            # suppress length feedback
           encoding="UTF-8")      # the encoding for the input is UTF-8
    
    #retrieve the speaker's name information
    name.speaker <- 
      exact.matches.2("(?x)        # set free-spacing
         (?<=name\\=\")            # look to the left and see the tag for name
         .*?                       # then 'stuff', till you
         (?=\")",                  # look to the right and see, but do NOT consume the closing "
         current.corpus.file)[[1]] # in current.corpus.file, only the exact matches
    

    #Identify matches and return them w 5 lines of context
    matches <- 
      exact.matches.2("(?x)            #set free-spacing
        (?<=[>\\[ ])                   #look to the left and see >, ] or space
        льэ(-[-\\p{L}]*)?              #then льэ (followed maybe by a hyphen and cyrillic characters and hyphens)
        (?=[\\.,\\]— ])",              #look to the right and see .,] em dash or space 
        current.corpus.file,           #in current.corpus.file
        lines.around = 5,              #and retrieve 5 lines around the match
        clean.up.spaces = FALSE)[1:4]
    
    #make curr.matches the content of matches[[4]]
    curr.matches <- matches[[4]]   
    
    #Get the IU number info
    ##Make curr.IU.numbs the numeric output of match output of exact.matches.2 in curr.matches
    ##Look to the left for the IU tag, then one or more digit character, and
    ##then look to the right for nondigit characters until the match льэ
    #Note: free spacing doesn't work here; I suppose it's bc of as.numeric()
    curr.IU.numbs <- as.numeric(          
      exact.matches.2("(?<=IU n=\")\\d+(?=\\D+\tльэ)",
                      curr.matches)[[1]])
    
    #Clean the matches & return match with following context
    #Essentially, we're getting rid of the preceding 5 lines of context
    ##Make curr.match.and.foll.ctx the result of 
    ##applying to each element of matches the substr function, where
    ##the start is the IU with the match in curr.matches
    ##the end is the last character in curr.matches
    curr.match.and.foll.ctx <- sapply(
      matches,
      substr,
      start=regexpr(
        pattern = "<IU n=\"[0-9]+\">([\\p{L}\\s\\[\\=\\.-]*)\tльэ.*",
        text = curr.matches,
        perl = TRUE),
      stop=nchar(curr.matches))
    
    #Fix the format output as a character vector
    curr.match.and.foll.ctx <- curr.match.and.foll.ctx[[4]]
    
    #This step converts tabs into 5 spaces (they get messed up in the previous steps)
    #To Replace spaces back for tabs:
    ##Make curr.match.and.foll.ctx the result of substituting
    ##5 spaces by a tab in the same vector
    curr.match.and.foll.ctx <- gsub(
      pattern = "\\s{5}",
      replacement = "\t",
      curr.match.and.foll.ctx)
    
    #clean the hits
    ##get rid of sentence initial tag
    curr.match.and.foll.ctx <- gsub(   #Make the vector the result of substituting
        "<IU n=\"[0-9]+\">",           #the IU sentence tag
        "",                            #by nothing
        curr.match.and.foll.ctx,       #in curr.match.and.foll.ctx
        perl=TRUE)                          
    
    ##get rid of IU closing tag
    curr.match.and.foll.ctx <- gsub("</IU>", "", curr.match.and.foll.ctx, perl=TRUE)
    
    ##get rid of the space preceding em dashes
    curr.match.and.foll.ctx <- gsub(" —", "—", curr.match.and.foll.ctx, perl=TRUE)
    
    ##get rid of any final file metadata
    curr.match.and.foll.ctx <- gsub(
        "[\\s]+</stext> </corpusDoc>", 
        "", 
        curr.match.and.foll.ctx, 
        perl=TRUE)
    
    #Count how many tabs there are in the last match
    tabs.in.last.match <- 
      stri_count(tail(curr.match.and.foll.ctx, 1), regex="\t")
    
    #If the last match does not have 7 tabs,
    #Add however many are missing so the output doesn't get messed up
    if(tabs.in.last.match != 7) {
      tabs.to.add <- strrep("\t", times=7-tabs.in.last.match)
      curr.match.and.foll.ctx[length(curr.match.and.foll.ctx)] <- 
        stri_join(tail(curr.match.and.foll.ctx, 1), tabs.to.add, sep="")
    }
      
    #Put only the match and its IU in a vector
    IU.matches <- exact.matches.2(
      ".*\\tльэ[\\p{L}\\s-]*\\t[\\p{L}\\s-]*[,.—]",
      curr.match.and.foll.ctx)[[1]]

    #Calculate the number of words x IU & the position of the match
    ##Split the hits up into words by splitting on one or more spaces
    IU.matches.list <- strsplit(IU.matches, " +", perl=TRUE)
    
    #Get the number of words x IU
    ##by applying to all elements of IU.matches.list the function length
    IU.lengths <- sapply(IU.matches.list, length)
    
    #Get the match position in IU
    curr.poss.of.hits <- sapply(  #Apply to all elements of
      IU.matches.list,            #this list
      grep,                       #the function grep (which return the positions where
      pattern="льэ.*\t",          #this pattern is found)
      perl=TRUE, 
      ignore.case=TRUE)
    
    #Normalize match position
    normalized.hit.position <- (curr.poss.of.hits-1)/(IU.lengths-1)
    
    #For matches that are the only word in an IU, the normalization above returns NaN
    #Turn NaN's into 1's
    normalized.hit.position[is.nan(normalized.hit.position)] <- 1
    
    #Store the match separately and retrieve the morphology
    morpho.segments <- unname(unlist(matches[1]))
    
    #Separate the morphemes from the hit
    morpho.segments <- gsub(  # make morpho.segments the result of replacing
      "(льэ)([\\w-]*)",       # the whole match
      "\\2",                  # by the memorized character strings of the morph
      morpho.segments,        # in morpho.segments
      perl=TRUE)
    
    #Add NA for any cases where there is no morphology
    morpho.segments[which(is_empty(morpho.segments, first.only=FALSE))] <- "NA"
    
    #Retrieve the intonation information
    ##by getting the last character in the matches (which tells the intonation pattern)
    IU.intonation <- stri_sub(IU.matches, -1)
    
    intonation <- as.character(factor(IU.intonation,
           levels = c(".", ",", "—"),
           labels = c("Falling", "Continuing", "Truncated")))
    
    #Automatically tagging for lexical retrieval (will need manual tweaking afterwards)
    ##Chunk out the following context
    foll.ctx <- substr(
      curr.match.and.foll.ctx, 
      start=nchar(IU.matches)+2,
      stop=nchar(curr.match.and.foll.ctx))
    
    ##Make a regex from the morpho.segments
    morpho.regex <- paste(morpho.segments, "[.,—\\s]", sep="")
    
    ##Find if the morpho.segments appear again in subsequent IUs
    ##Assumption: if they do, there's prolly lexical retrieval (again, will need manual check-up)
    
    #First make collector vector, and then loop over the following contexts
    all.lexical.retrievals <- character()
    
    for (k in seq(foll.ctx)) {
      item.retrieved <- grepl(morpho.regex[k], 
                       foll.ctx[k],
                       perl=TRUE)
      
      all.lexical.retrievals <- c(all.lexical.retrievals, item.retrieved)
    }
    
   
   #Identify what examples have a pause immediately following the match
   foll.pause.in.it <- grepl("^\\(.{3}\\)", foll.ctx, perl=TRUE)
   
   #Extract the pause info
   foll.pauses <- exact.matches.2("^\\(.{3}\\)", foll.ctx)[[1]]
   
   #Clean the pauses by removing the ()s
   foll.pauses <- gsub(
     "\\((.{3})\\)",
     "\\1",
     foll.pauses,
     perl=TRUE)
   
   #Add 0s where there isn't a following pause
   for (l in seq(foll.pause.in.it)) {
     if(!foll.pause.in.it[l]) {
       foll.pauses <- append(foll.pauses, 0, after=l-1)
     }
   }
  
   
   #Clean the matches to return preceding context
   preceding.ctx <- sapply(
     matches,
     substr,
     start=1,
     stop=(regexpr("\\s{5}<IU n=\"[0-9]+\">([\\p{L}\\s-]*)\tльэ",
                  curr.matches,
                  perl = TRUE))-1)
   
   #Put the preceding context into a character vector
   preceding.ctx <- preceding.ctx[[4]]

   #Identify what examples have a pause immediately preceding the match
   prec.pause.in.it <- grepl("\\(.{3}\\)</IU>$", 
                             preceding.ctx, 
                             perl=TRUE)
   
   #Extract the preceding pause info
   prec.pauses <- exact.matches.2("\\(.{3}\\)(?=</IU>$)", preceding.ctx)[[1]]
   
   #clean the pauses by removing the ()s
   prec.pauses <- gsub(
     "\\((.{3})\\)",
     "\\1",
     prec.pauses,
     perl=TRUE)
   
   #add 0s where there isn't a preceding pause
   for (m in seq(prec.pause.in.it)) {
     if(!prec.pause.in.it[m]) {
       prec.pauses <- append(prec.pauses, 0, after=m-1)
     }
   }
   

  #paste the concordance output
  current.matches <- paste(       # make current.matches the result of pasting together
    basename(corpus.files[i]),    # the base name of the current corpus file
    name.speaker,                 # the name of the speaker
    curr.match.and.foll.ctx,
    curr.IU.numbs,
    curr.poss.of.hits,
    normalized.hit.position,
    IU.lengths,
    intonation,
    morpho.segments,
    all.lexical.retrievals,
    prec.pauses,
    foll.pauses,
    sep="\t")                     # separated by tabs
    
  #put the matches in the collector structure
  all.matches <- c(all.matches, current.matches)
    
  } })
object.size(all.matches)



#Create the output for the annotation of functions
cat("CASE\tFILE\tSPEAKER\tPRECEDING\tMATCH\tFOLLOWING\tIU+1\tIU+2\tIU+3\tIU+4\tIU+5\tIU_NUM\tMATCH_POSITION\tNORMALIZED_MATCH_POSITION\tIU_LENGTH\tINTONATION\tMORPHOLOGY\tLEXICAL_RET\tPREC_PAUSE\tFOLL_PAUSE",               # print a header
    paste(seq(all.matches),          # then paste together a case number
          all.matches,               # with the matches
          sep="\t"),                 # all separated by tab stops (for columns)
    file="PH_output_December.csv", 
    sep="\n")



#### Step 2: Some descriptive statistics: corpus size, speaker ratios, root/affixes types and tokens ####

#Make collector structures
speaker.ratio.df <- data.frame(matrix(ncol=3, nrow=length(corpus.files)))
colnames(speaker.ratio.df) <- c("corpus.file", "speaker", "corpus.length")

all.words <- c()

for(i in seq(corpus.files)) {           # access each corpus file
  current.corpus.file <- tolower(scan(  # make current.corpus.file the result of lowering
    corpus.files[i],                    # the i-th/current corpus file
    what=character(),                   # as a character vector
    sep="\n",                           # with linebreaks as separators between vector elements
    quiet=TRUE))                        # no quote characters & no feedback about elements read
  
  name.speaker <- 
    exact.matches.2("(?x)          # set free-spacing
         (?<=name\\=\")            # look to the left and see the tag for name
         .*?                       # then 'stuff', till you
         (?=\")",                  # look to the right and see, but do NOT consume the closing "
         current.corpus.file)[[1]] # in current.corpus.file, only the exact matches
  
  current.sentences <- grep(     # find
    "<iu n=",                    # the sentence number tags
    current.corpus.file,         # in current.corpus.file
    perl=TRUE,                   # using Perl-compatible regular expressions
    value=TRUE)
  
  new.sentences <- gsub("<.*?>", "", current.sentences, perl=TRUE)
  
  clean.sentences <- gsub("[{}\\?\\[\\]\\.\\(\\),]|(\\s—)|[0-9]", 
                          "", new.sentences, perl=TRUE)
  
  words <- unlist(strsplit(clean.sentences, " "))
  corpus.words <- words[nzchar(words)]
  
  all.words <- c(all.words, corpus.words)
  
  speaker.ratio.df[i, 1] <- basename(corpus.files[i])
  speaker.ratio.df[i, 2] <- name.speaker
  speaker.ratio.df[i, 3] <- length(corpus.words)
  
}

#Make the speaker column a factor
speaker.ratio.df$speaker <- as.factor(speaker.ratio.df$speaker)

#Aggregate the number of words each speaker utters
##by adding the texts of each speaker
ratios <- aggregate(speaker.ratio.df$corpus.length, 
          by=list(speaker.ratio.df$speaker), 
          FUN=sum)

#Add column names to the ratios dataframe
colnames(ratios) <- c("speaker", "corpus.length")

#Add a new column that adds the total percentage of each speaker's contribution
ratios$speaker.contribution.ratio <- ratios$corpus.length/sum(ratios$corpus.length)

#speaker corpus.length      ratio
#1       anna shadrina           153 0.01491810
#2    dmitrij djachkov           584 0.05694228
#3 fevron'ja shalugina           319 0.03110374
#4   grigorij shalugin           405 0.03948908
#5       ivan dolganov           405 0.03948908
#6    nikolaj lixachev          1360 0.13260530
#7    vasilij shalugin          7030 0.68545242


#Calculate all the types and tokens in the corpus
#Split all.words by the hyphen
split <- strsplit(all.words, "-")

#Extract the first element in each list to get the total number of roots
roots <- sapply(split, "[", 1)
length(roots) #10256 root tokens
length(unique(roots)) #1099 root types


#Make a collector vector for the affixes
all.affixes <- c()

for (j in seq(split)) {
  if (lengths(split)[[j]] > 1) {
    current.affixes <- split[[j]][2:lengths(split)[[j]]]
    all.affixes <- c(all.affixes, current.affixes)
  }
  else { next }
}

length(all.affixes) #11315 affix tokens
length(unique(all.affixes)) #140 affix types



#### Step 3: Analysis and outputs ####

df <- read.delim("PH_df_final.csv", 
                 sep = "\t", 
                 stringsAsFactors = TRUE)


max(prop.table(table(df$FUNCTION))) #0.6732892

#Add interaction predictor with dot product of match position and IU length
df$POSITIONxWORDS <- df$NORMALIZED_MATCH_POSITION*df$WORDS_IN_IU

#Run random model forest
set.seed(1234); PH.RF <- randomForest(FUNCTION ~
        NORMALIZED_MATCH_POSITION + POSITIONxWORDS + 
        WORDS_IN_IU + INTONATION + SPEAKER +
        PREC_PAUSE + FOLL_PAUSE, 
        data = df,
        na.action = na.omit,
        ntree = 10000,
        importance = TRUE)

#OOB estimate of  error rate: 25.83%
#Confusion matrix:
#  COP  PH class.error
#COP  76  72   0.4797297
#PH   45 260   0.1442623

#Calculate if different btw baseline and model output is statistically significant
sum(dbinom(332:453, 453, 305/453)) #0.003524275: it is statistically significant


#Calculate variable importance
varImp.RF <- as.data.frame(importance(PH.RF, type=1))
varImp.RF_Sorted <- varImp.RF[order(-varImp.RF$MeanDecreaseAccuracy), , drop=FALSE]

#Plot variable importance
varImpPlot(PH.RF,
           main = NULL)

#Calculate pd scores for the interaction
pd.interaction <- partial(PH.RF, 
      train=df, 
      pred.var="POSITIONxWORDS",
      which.class = 2)

#Plot pd scores for the interaction
par(mfrow=c(1, 1))
plot(pd.interaction,
     xlab="Interaction between Match Position and Number of Words in IU",
     ylab="Partial dependence score",
     type = "l",
     lwd=1.5); grid() 
rug(jitter(df$POSITIONxWORDS, 15), 
    side = 1, col = "black", lwd = 1.5, ticksize = 0.02)
abline(h = log(tab.c[["PH"]]/tab.c[["COP"]]), 
       col = "black", lty = 2)


#Calculate pd scores for the intonation predictor
pd.intonation <- partial(PH.RF, 
      train=df, 
      pred.var="INTONATION",
      which.class = 2)

#Plot pd scores for the intonation predictor
par(mfrow=c(1, 1))          # make the plotting window have 1 rows & 1 column
tab.c <- table(df$FUNCTION) # determine the frequencies of the functions
barplot(                                                   # make a bar plot
  #main="Partial dependence of PLACEHOLDER on INTONATION", # w/ this heading
  col="darkgrey",                                          # grey bars
  height=pd.intonation$yhat,                               # whose heights are the PDP scores
  xlab="Intonation Contours",
  ylab="Partial dependence score",
  names.arg=pd.intonation$INTONATION,  # label the bars like this
  # make the widths of the bars represent the proportions of function
  width=prop.table(tab.c)); grid()
abline(h = log(tab.c[["PH"]]/tab.c[["COP"]]), col = "black", lty = 2)

