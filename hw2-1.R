---
title: "sta141bhw2"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
```



```{r}
library(stringr)
library(scales)



####  please type the track of "SpamAssassinTrain" to run this program
Track = "~/Desktop/UCD/Third Year/sta141b/SpamAssassinTrain"
####
setwd(Track)

##read the folders
Foldernames = list.files(path = Track)

##number of files in folder
Foldernums = length(Foldernames)

##read all files inside the folder
numLinesInBody = list()  ##number of header lines
isRe = list()  ## Re or not
AtchL = list()  ##put attachment information
bodyCharacterCount = list() #num of body char
replyUnderline = list() # Reply-To with _, a-zA-z, 0-9
subjectExclamationCount = list()  # count "!" in subjects
subjectQuestCount = list()  # count "?" in subjects
numAttachments = list()  #num of attachments
priority = list()  # high priority or not
numRecipients = list() #num of recpient
percentCapitals = list()  #percentage of upper case
isInReplyTo = list()  #whether in-reply-to
subjectPunctuationCheck = list() # wether punctuations surrounded
hourSent = list() #hour sent
multipartText = list() #multipart or text
numDollarSigns = list() #all capital
isSpam = list()
i = 1
Head = list()

for (j in 1:Foldernums){
  filenames = list.files(Foldernames[j])
  filelen = length(list.files(Foldernames[j]))
################################################    HEADER    ###############################################
for (k in 1:filelen) {
  filetrack = paste(Track,Foldernames[j],filenames[k], sep = "/")
  #split the header
  L = readLines(filetrack, encoding = "latin1")
  BL = grep('^$',L)[1]    #find position of the first blank line, BL-1 is the lines of header, below the BL is body + attachments
  if (any(grepl('^$',L)) == TRUE){
  ######split the head by each key value
  H = L[1:BL-1]
  keyline = grep("^[A-Za-z]+.*:", H)
  
  
  for (p in 2:length(keyline)){
    Head[p] = toString(H[keyline[p-1]:(keyline[p]-1)])
    if (p == length(keyline)){
      Head[p+1] = toString(H[keyline[p]:length(H)])
    }
  }

  ######finished splitting the header
                                            ##look for isRe##
  Re = any(grepl("^Subject: Re:+", Head, ignore.case = TRUE))
  isRe[i] = Re    ##variable: isRe
                                            ##replyUnderline
  Underline = any(grepl("^Reply-To:.*[a-zA-Z0-9]+.*_+|^Reply-To:.*_+.*[a-zA-Z0-9]+", Head))
  replyUnderline[i] = Underline
  
  Sub = grep("^Subject:+", Head, value = TRUE) # line of subject
  Sub[length(Sub) == 0] = ""   # avoid length zero's
  subjectExclamationCount[i] = str_count(Sub, pattern = "!")
  subjectQuestCount[i] = str_count(Sub, pattern = "\\?")
  
  X_prio = any(grepl("\\X-Priority.*High|\\X-Smell-Priority.*High", Head, ignore.case = TRUE))
  priority[i] = X_prio  ##variable: priority
  
  To_num = grep("^To:", Head, value = TRUE)
  Cv_num = grep("^Cc:", Head, value = TRUE)
  To_num[length(To_num) == 0] = "No Recipient"
  numRecipients[i] = toString(c(To_num,Cv_num))  #combine Cc lines and To lines
  numRecipients[i] = str_count(numRecipients[i], "@")
  
  isInReplyTo[i] = any(grepl("^In-Reply-To:", Head))
  
  subjectPunctuationCheck[i] = any(grepl("^Subject:.[[:punct:]]+.|^Subject:.[0-9]+." ,Head))
  
  Date = grep("^Date:", Head, value = TRUE)
  hourSet = strsplit(strsplit(Date, ":")[[1]][2]," ")
  hourindex = length(hourSet[[1]])
  hourSent[i] = as.numeric(hourSet[[1]][hourindex])
  
  multext = any(grepl("multipart", Head, ignore.case = TRUE))
  multipartText[i] = multext
  Head = NULL
  
  
  #########################################    Body and Attachment  ###############################################
  
  #split the Rest
  Rest = L[BL:length(L)]  ##rest of the email
  SL = grep('(^--.*=+)',Rest)[1] ##find the position of attachment
  # make sure that the content type is not plain, otherwise the attachment may include the body of email.
  if(grepl("plain",Rest[SL+1],ignore.case = TRUE) == FALSE){  ##no plain text, there may have attachment
  if (is.na(SL) == FALSE){  #when there is attachment
    #get body
    Body = Rest[1:SL-1]
    bodyCharacterCount[i] = sum(nchar(Body, type = "width"))
    numLinesInBody[i] = length(Body)  #variable numLinesInBody
    Capitalsnum = str_count(toString(Body),"[[:upper:]]")
    percentCapitals[i] = Capitalsnum/sum(nchar(Body, type = "width"))
    # Body = toString(Body)
    # BodyL[i] = Body
    #get attachment
    Attach = Rest[SL:length(Rest)]
    Attach = toString(Attach)
    AtchL[i] = Attach
    
  }
    if (is.na(SL) == TRUE){ # when no attachment, then the only rest part is body
    #get body
    Body = Rest[1:length(Rest)]
    numLinesInBody[i] = length(Body)
    bodyCharacterCount[i] = sum(nchar(Body, type = "width"))
    Capitalsnum = str_count(toString(Body),"[[:upper:]]")
    percentCapitals[i] = Capitalsnum/sum(nchar(Body, type = "width"))
    #get attachment
    AtchL[i] = ""
    }
    
    
  }
  ## if plain text
  else {
    SL = grep('(^--.*=+)',Rest)[2]  #the second content-type not plain: have attachment
    if (is.na(SL) == FALSE){
    #get body
    Body = Rest[1:SL-1]
    numLinesInBody[i] = length(Body)
    bodyCharacterCount[i] = sum(nchar(Body, type = "width"))
    Capitalsnum = str_count(toString(Body),"[[:upper:]]")
    percentCapitals[i] = Capitalsnum/sum(nchar(Body, type = "width"))
    # Body = toString(Body)
    # BodyL[i] = Body
    #get attachment
    Attach = Rest[SL:length(Rest)]
    Attach = toString(Attach)
    AtchL[i] = Attach
  }
    if (is.na(SL) == TRUE){  ##no attachment, only plain text
    #get body lines
    Body = Rest[1:length(Rest)]
    numLinesInBody[i] = length(body)
    bodyCharacterCount[i] = sum(nchar(Body, type = "width"))
    Capitalsnum = str_count(toString(Body),"[[:upper:]]")
    percentCapitals[i] = Capitalsnum/sum(nchar(Body, type = "width"))
    #get attachment
    AtchL[i] = ""
    }
  }
  numAttachments[i] = str_count(as.vector(AtchL[i]), pattern = "Content-Type:")  #store all attachment lines as string, each attachment should have a content type key
  numDollarSigns[i] = str_count(toString(Body), pattern = "\\$")
  isSpam[i] = any(grepl("spam+",Foldernames[j]))
}
  else   #skip the wrong file
    next
  
  
  
  i = i + 1

}

}
numLinesInBody = unlist(numLinesInBody)  ##number of header lines
isRe = unlist(isRe)  ## Re or not
bodyCharacterCount = unlist(bodyCharacterCount) #num of body char
replyUnderline = unlist(replyUnderline) # Reply-To with _, a-zA-z, 0-9
subjectExclamationCount = unlist(subjectExclamationCount)  # count "!" in subjects
subjectQuestCount = unlist(subjectQuestCount)  # count "?" in subjects
numAttachments = unlist(numAttachments)  #num of attachments
priority = unlist(priority)  # high priority or not
numRecipients = unlist(numRecipients) #num of recpient
percentCapitals = unlist(percentCapitals)  #percentage of upper case
isInReplyTo = unlist(isInReplyTo)  #whether in-reply-to
subjectPunctuationCheck = unlist(subjectPunctuationCheck) # wether punctuations surrounded
hourSent = unlist(hourSent) #hour sent
multipartText = unlist(multipartText) #multipart or text
numDollarSigns = unlist(numDollarSigns) #all capital
isSpam = unlist(isSpam)

############################################### set the data frame  #############################################
the.data = data.frame(isSpam, numLinesInBody, isRe, bodyCharacterCount, replyUnderline, subjectExclamationCount, subjectQuestCount, numAttachments, priority, numRecipients, percentCapitals, isInReplyTo, subjectPunctuationCheck, hourSent, multipartText, numDollarSigns)

Ham = the.data[the.data$isSpam == FALSE,]
Spam = the.data[the.data$isSpam == TRUE,]
summary(Ham)
summary(Spam)

library(ggplot2)
sum(Spam$subjectExclamationCount)
ggplot(data=the.data, aes(x=isSpam, y=subjectExclamationCount, fill = isSpam)) + geom_bar(stat="identity")
ggplot(data=the.data, aes(x=factor(isRe),y = frequency(isRe) , fill = isSpam)) + geom_bar(stat="identity")
ggplot(data=the.data, aes(x=factor(replyUnderline),y = frequency(replyUnderline) , fill = isSpam)) + geom_bar(stat="identity")

ggplot(data=the.data, aes(x=factor(isInReplyTo),y = frequency(isInReplyTo) , fill = isSpam)) + geom_bar(stat="identity")