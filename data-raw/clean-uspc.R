getwd()
setwd("/Users/sojunp/concordance/data-raw")

install.packages("textreadr")
library(textreadr)
library(Hmisc)
library(stringr)
library(tidyverse)

class <- read_html("https://www.uspto.gov/web/patents/classification/selectnumwithtitle.htm")
class.1 <- class[which(class == "002"):which(class=="Plants")]
class.2 <- class.1[class.1 != "Class Number and Title"]
class.num <- class.2[seq(1, length(class.2), by = 2)]
class.title <- class.2[seq(2, length(class.2), by = 2)]

#1. read each class and subclass descriptions from the USPTO website
uspc.temp <- data.frame(code = "002", desc = "Apparel")
uspc <- uspc.temp[0,]
for (i in 1:(length(class.num) - 1)){
  temp.num <- class.num[i]
  temp.class <- class.title[i]
  temp.url.1 <- paste("https://www.uspto.gov/web/patents/classification/uspc", "/sched", sep = temp.num)
  temp.url.2 <- paste(temp.url.1, ".htm", sep = temp.num)
  #a. create 6-digits USPC subclasses and their descriptions within each USPC class
  temp.1 <- read_html(temp.url.2)
  temp.start.index <- which(grepl("Indent Level", temp.1, fixed = TRUE))
  start.index <- temp.start.index[length(temp.start.index)] + 1
  end.index <- which(temp.1 == "Skip footer and go to main content") - 1
  temp.2 <- temp.1[start.index:end.index]
  #b. make each description tidy : remove "(numbers)"
  #https://stackoverflow.com/questions/38231081/extract-info-inside-parenthesis-in-r
  inside.paranthesis <- str_match(temp.2, "\\(([^()]*)\\)")[,2]
  num.index <- grepl("\\d", temp.2) & is.na(as.numeric(inside.paranthesis)) 
  temp.new.num <- temp.2[num.index]
  #consider subclasses with more than 6 characters, including "." as a regular expression (e.g. 005/104.001)
  #https://stackoverflow.com/questions/10294284/remove-all-special-characters-from-a-string-in-r
  temp.new.num.2 <- temp.new.num[nchar(gsub("[[:punct:]]", "", temp.new.num)) <= 6]  
  temp.new.desc <- temp.2[which(temp.2 %in% temp.new.num.2) + 1]
  #c. combine 6-digits USPC subclasses and their descriptions with their 3-digits USPC classes
  temp.code <- c(temp.num, paste(temp.num, temp.new.num.2, sep = "/"))
  temp.desc <- c(temp.class, temp.new.desc)
  temp.uspc <- cbind(temp.code, temp.desc)
  #d. pile
  uspc <- rbind(uspc, temp.uspc)
}

#2. Work on the last class "PLT" separately
temp.num <- "PLT"
temp.class <- "Plants"
temp.1 <- read_html("https://www.uspto.gov/web/patents/classification/uspcplt/schedplt.htm")
temp.start.index <- which(grepl("Indent", temp.1, fixed = TRUE))
start.index <- temp.start.index[length(temp.start.index)] + 1
end.index <- which(temp.1 == "Skip footer and go to main content") - 1
temp.2 <- temp.1[start.index:end.index]
#tidy descriptions : remove "(numbers)"
#https://stackoverflow.com/questions/38231081/extract-info-inside-parenthesis-in-r
inside.paranthesis <- str_match(temp.2, "\\(([^()]*)\\)")[,2]
num.index <- grepl("\\d", temp.2) & is.na(as.numeric(inside.paranthesis)) 
temp.new.num <- temp.2[num.index]
temp.new.num.2 <- temp.new.num[nchar(temp.new.num) <= 6]
temp.new.desc <- temp.2[which(temp.2 %in% temp.new.num.2) + 1]
#combine 6-digits USPC subclasses and their descriptions with their 3-digits USPC classes
temp.code.final <- c(temp.num, paste(temp.num, temp.new.num.2, sep = "/"))[1:413]
temp.desc.final <- c(temp.class, temp.new.desc)[1:413]
temp.uspc <- cbind(temp.code.final, temp.desc.final)
colnames(temp.uspc) <- c("temp.code", "temp.desc")
#combine with the previous results
uspc.final <- rbind(uspc, temp.uspc)

#3. make descriptions more tidy
str(uspc.final)
uspc.desc.1 <- tolower(uspc.final$temp.desc)
uspc.desc.2 <- capitalize(uspc.desc.1)
uspc.final$temp.desc <- uspc.desc.2

#a. remove irregular expressions that are specific to each class (e.g. introductory parts of class 204 and 205)

uspc.final[which(uspc.final$temp.code %in% class.num)+1, 2] 
uspc.final[which(uspc.final$temp.code %in% class.num)+2, 2] 
odd.index <- c(34, 87, 105, 143, 144, 285, 286, 357, 362:364, 366, 367, 369:396, 398:403)
odd.row.index <- (which(uspc.final$temp.code %in% class.num)+1)[odd.index]
uspc.final[odd.row.index-1, 1]

odd.index.true <- c(13308, 36303:36305, 36538:36540, 42020, 53634, 54261:54263, 74168:74263, 107455:107457, 
                    107556:107558, 135549:135551, 136516:136518, 136675:136677, 137172:137174, 137785:137787, 
                    139125:139127, 139469:139479, 139483:139485, 139686:139688, 139895:139897, 140120:140122, 
                    140941:140943, 141646:141648, 142072:142074, 142115:142117, 142589:142591, 142860:142878, 
                    142882:142884, 143243:143245, 143510:143512, 143911:143913, 144317:144319, 144709:144711, 
                    145270:145272, 145729:145731, 146027:146029, 146263:146265, 146529:146531, 146987:146989, 
                    147310:147312, 147759:147761, 148276:148278, 148965:148967, 149134:149136, 149701:149703, 
                    150193:150195, 150390:150392, 150479:150481, 151068:151070, 151430:151432)

odd.index.final <- odd.index.true-1

#check
uspc.final[odd.index.final, 1]
uspc.final[odd.index.final, 2]
length(uspc.final[odd.row.index-1, 1]) == length(unique(substr(uspc.final[odd.index.final, 1], 1, 3)))
uspc.final.2 <- uspc.final[-odd.index.final,]

#b. remove parentheses and texts within parentheses + remove other trivial expressions (e.g. *, **, ***, e.g., i.e., etc.)
uspc.final.3 <- uspc.final.2
temp.desc.1 <- uspc.final.3$temp.desc
temp.desc.2 <- gsub("\\[", "\\(", temp.desc.1)
temp.desc.3 <- gsub("\\]", "\\)", temp.desc.2)
temp.desc.4 <- gsub("\\s*\\([^\\)]+\\)", "", temp.desc.3)
temp.desc.5 <- gsub("\\*", "", temp.desc.4)
temp.desc.6 <- gsub("\\(", "", temp.desc.5)
temp.desc.7 <- gsub("\\)", "", temp.desc.6)
temp.desc.8 <- trimws(temp.desc.7)
temp.desc.9 <- gsub("; e.g..*", "", temp.desc.8)
temp.desc.10 <- gsub(", e.g.,.*", "", temp.desc.9)
temp.desc.11  <- gsub(", i.e.,.*", "", temp.desc.10)
uspc.final.3$temp.desc <- gsub(", etc..*", ", etc.", temp.desc.11)

#save
uspc.final.3$code <- as.character(uspc.final.3$code)
names(uspc.final.2) <- c("code", "desc")
names(uspc.final.3) <- c("code", "desc")

#rename
uspc2012_desc <- uspc.final.3

save(uspc2012_desc, file = "uspc2012_desc.RData")
#write.csv(uspc.final, "uspc2012.csv", row.names = F)
write.csv(uspc.final.2, "uspc2012 description.csv", row.names = F)
write.csv(uspc2012_desc, "uspc2012 description (clean).csv", row.names = F)
