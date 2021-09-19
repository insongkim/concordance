getwd()
setwd("/Users/sojunp/concordance/data-raw")

install.packages("textreadr")
library(textreadr)
library(Hmisc)
library(stringr)
library(stringi)
library(tidyverse)

#1. year 2011

uspc.naics.2011 <- read.csv("naics_co11.csv", header = T)
str(uspc.naics.2011)
#view(uspc.naics.2011)

#1.1. tame USPC codes : subclass
#for both first and last three digits : never remove trailing zeros

#1.1.1. For first three digits : always remove leading zeros

#(1) if there are characters followed by numbers after remoivng leading zeros (there is no number followed by characters in the first three digits)
#(a) if there are zeros between characters and numbers : remove and paste
#e.g. E05008 -> E5.008
#(b) if there is no zero between characters and numbers : leave
#e.g. E29015 -> E29.015

temp.subclass <- uspc.naics.2011$SUBCLASS

temp.subclass.1 <- substr(temp.subclass, 1, 3)
temp.subclass.1.1 <- str_remove(temp.subclass.1, "^0+") #removing.leading zeros
unique(temp.subclass.1.1[is.na(as.numeric(temp.subclass.1.1)) & temp.subclass.1.1 != ""])
temp.subclass.1.1[temp.subclass.1.1 == "E05"] <- "E5"
temp.subclass.1.1[temp.subclass.1.1 == "E01"] <- "E1"

#1.1.2. For last three digits : always remove trailing zeros (there is not code whose first three digits are "FOR")
#e.g. 236010 -> 236.01, 229100 -> 229.1

#(1) if there are only characters left after removing leading zeros, then remove leading zeros, and paste the first three digits without "." 
#e.g. 06200A -> 62A, 08600R -> 86R, 0830WM -> 83WM, 1970FM -> 197FM, 1030CM -> 103CM, 

#(2) if there are either only numbers or numbers followed by characters left after removing leading zeros, 
#(a) if the first three digits are only characters, other than "FOR": then remove leading zeros in the last three digits, and paste the first three digits without "." 
#e.g. DIG001 -> DIG1, DIG010 -> DIG10, DIG014 -> DIG14
#(b) if the first three digits are only characters with "FOR": then do not remove leading zeros in the last three digits, and paste the first three digits without "." 
#e.g. FOR000 -> FOR000, FOR100 -> FOR100 (FOR is always followed by three full digits that are all numbers)
#(c) otherwise, do not remove leading zeros in the last three digits, and paste the first three digits with "." ()
#e.g. E29015 -> E29.015, 06145M -> 61.45M, E05008 -> E5.008, 08110C -> 81.1C, 00101P -> 1.01P

temp.subclass.2 <- substr(temp.subclass, 4, 6)
temp.subclass.2.1 <- temp.subclass.2
temp.subclass.2.1[temp.subclass.1.1 != "DIG"] <- str_remove(temp.subclass.2.1[temp.subclass.1.1 != "DIG"], "0+$") #removing trailing zeros, except for those who are matched with "DIG" as the first three digits (DIG010 -> DIG10)
#preserving leading zeros if the last three digits have only numerics (e.g. 001 -> .001) or start with numerics, followed by characters (01P -> .01P), except for the last three digits who are matched with DIG (DIG001 -> DIG1)
#i.e., removing leading zeros if the last three digits contain only characters after removing leading zeros
non.num.index <- is.na(as.numeric(temp.subclass.2.1)) 
only.character.index <- grepl("^[A-Za-z]+$", str_remove(temp.subclass.2.1, "^0+")) | temp.subclass.1.1 == "DIG" 
temp.subclass.2.1[only.character.index] <- str_remove(temp.subclass.2.1[only.character.index], "^0+") 

unique(temp.subclass.2.1[non.num.index & temp.subclass.2.1 != ""])
temp.subclass.2.1[temp.subclass.2.1 == "*"] <- ""

temp.index <- unique(temp.subclass.2.1[non.num.index & temp.subclass.2.1 != "" & substr(temp.subclass.2.1, 2, 2) == "0"])
temp.str <- temp.subclass.2.1[temp.subclass.2.1 %in% temp.index]
temp.subclass.2.1[temp.subclass.2.1 %in% temp.index] <- paste(substr(temp.str, 1, 1), substr(temp.str, 3, 3), sep = "")

length(temp.subclass.1.1) == length(temp.subclass.2.1)
num.index <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
subclass.final <- temp.subclass.1.1
with.dot.index <- (substr(temp.subclass.2.1, 1, 1) %in% num.index) & (temp.subclass.1.1 != "DIG")
subclass.final[with.dot.index] <- paste(temp.subclass.1.1[with.dot.index], temp.subclass.2.1[with.dot.index], sep = ".")
subclass.final[!with.dot.index] <- paste(temp.subclass.1.1[!with.dot.index], temp.subclass.2.1[!with.dot.index], sep = "")

#check
subclass.final[subclass.final == "E5.008"]
subclass.final[subclass.final == "E29.015"]
subclass.final[subclass.final == "236.01"]
subclass.final[subclass.final == "229.1"]
subclass.final[subclass.final == "62A"]
subclass.final[subclass.final == "86R"]
subclass.final[subclass.final == "83WM"]
subclass.final[subclass.final == "197FM"]
subclass.final[subclass.final == "103CM"]
subclass.final[subclass.final == "DIG1"]
subclass.final[subclass.final == "DIG10"]
subclass.final[subclass.final == "DIG14"]
subclass.final[subclass.final == "E29.015"]
subclass.final[subclass.final == "61.45M"]
subclass.final[subclass.final == "E5.008"]
subclass.final[subclass.final == "81.1C"]
subclass.final[subclass.final == "1.01P"]

#1.2. tame USPC codes : class
#add leading zeros

temp.class <- uspc.naics.2011$CLASS
class.final <- str_pad(temp.class, 3, pad = "0")

#1.3. tame NAICS codes : transform OTAF into 4 digits
 
temp.naics <- as.character(uspc.naics.2011$OTAF)

uspc.naics.2011.temp <- uspc.naics.2011 %>% mutate(uspc.code = paste(class.final, subclass.final, sep = "/")) %>% select(-CLASS, -SUBCLASS)
tail.location <- nchar(uspc.naics.2011.temp$uspc.code)
uspc.naics.2011.temp$uspc.code[substr(uspc.naics.2011.temp$uspc.code, tail.location, tail.location) == "/"] <- "001"

#check : these USPC codes do not have descriptions provided by USPC website
uspc.naics.2011.temp$uspc.code[!(uspc.naics.2011.temp$uspc.code %in% uspc.final.3$code)]

#compile
uspc.naics.2011.temp.2 <- uspc.naics.2011.temp %>% select(uspc.code, seq = SEQ)
unique(uspc.naics.2011.temp.2$seq)[order(unique(uspc.naics.2011.temp.2$seq))]

uspc.naics.2011.final <- data.frame(uspc.code = "", naics.code = "")
uspc.naics.2011.final$uspc.code <- as.character(uspc.naics.2011.final$uspc.code)
uspc.naics.2011.final$naics.code <- as.character(uspc.naics.2011.final$naics.code)

for (i in 1:nrow(uspc.naics.2011.temp.2)){
  temp.seq <- uspc.naics.2011.temp.2$seq[i]
  if (temp.seq == 1){
    temp <- data.frame(uspc.code = uspc.naics.2011.temp.2$uspc.code[i], naics.code = c("3111", "3112", "3113", "3114", "3115", "3116", "3117", "3118", "3119"))
  }
  if (temp.seq == 2){
    temp <- data.frame(uspc.code = uspc.naics.2011.temp.2$uspc.code[i], naics.code = c("3121", "3122"))
  }
  if (temp.seq == 3){
    temp <- data.frame(uspc.code = uspc.naics.2011.temp.2$uspc.code[i], naics.code = c("3131", "3132", "3133", "3141", "3149", "3151", "3152", "3159", "3161", "3162", "3169"))
  }
  if (temp.seq == 4){
    temp <- data.frame(uspc.code = uspc.naics.2011.temp.2$uspc.code[i], naics.code = c("3211", "3212", "3219"))
  }
  if (temp.seq == 5){
    temp <- data.frame(uspc.code = uspc.naics.2011.temp.2$uspc.code[i], naics.code = c("3221", "3222", "3231"))
  }
  if (temp.seq == 6){
    temp <- data.frame(uspc.code = uspc.naics.2011.temp.2$uspc.code[i], naics.code = c("3251", "3252", "3253", "3254", "3255", "3256", "3259"))
  }
  if (temp.seq == 7){
    temp <- data.frame(uspc.code = uspc.naics.2011.temp.2$uspc.code[i], naics.code = c("3251"))
  }
  if (temp.seq == 8){
    temp <- data.frame(uspc.code = uspc.naics.2011.temp.2$uspc.code[i], naics.code = c("3252"))
  }
  if (temp.seq == 9){
    temp <- data.frame(uspc.code = uspc.naics.2011.temp.2$uspc.code[i], naics.code = c("3254"))
  }
  if (temp.seq == 10){
    temp <- data.frame(uspc.code = uspc.naics.2011.temp.2$uspc.code[i], naics.code = c("3253", "3255", "3256", "3259"))
  }
  if (temp.seq == 11){
    temp <- data.frame(uspc.code = uspc.naics.2011.temp.2$uspc.code[i], naics.code = c("3261", "3262"))
  }
  if (temp.seq == 12){
    temp <- data.frame(uspc.code = uspc.naics.2011.temp.2$uspc.code[i], naics.code = c("3271", "3272", "3273", "3274", "3279"))
  }
  if (temp.seq == 13){
    temp <- data.frame(uspc.code = uspc.naics.2011.temp.2$uspc.code[i], naics.code = c("3311", "3312", "3313", "3314", "3315"))
  }
  if (temp.seq == 14){
    temp <- data.frame(uspc.code = uspc.naics.2011.temp.2$uspc.code[i], naics.code = c("3321", "3322", "3323", "3324", "3325", "3326", "3327", "3328", "3329"))
  }
  if (temp.seq == 15){
    temp <- data.frame(uspc.code = uspc.naics.2011.temp.2$uspc.code[i], naics.code = c("3331", "3332", "3333", "3334", "3335", "3336", "3339"))
  }
  if (temp.seq == 16){
    temp <- data.frame(uspc.code = uspc.naics.2011.temp.2$uspc.code[i], naics.code = c("3341", "3342", "3343", "3344", "3345", "3346"))
  }
  if (temp.seq == 17){
    temp <- data.frame(uspc.code = uspc.naics.2011.temp.2$uspc.code[i], naics.code = c("3341"))
  }
  if (temp.seq == 18){
    temp <- data.frame(uspc.code = uspc.naics.2011.temp.2$uspc.code[i], naics.code = c("3342"))
  }
  if (temp.seq == 19){
    temp <- data.frame(uspc.code = uspc.naics.2011.temp.2$uspc.code[i], naics.code = c("3344"))
  }
  if (temp.seq == 20){
    temp <- data.frame(uspc.code = uspc.naics.2011.temp.2$uspc.code[i], naics.code = c("3345"))
  }
  if (temp.seq == 21){
    temp <- data.frame(uspc.code = uspc.naics.2011.temp.2$uspc.code[i], naics.code = c("3343", "3346"))
  }
  if (temp.seq == 22){
    temp <- data.frame(uspc.code = uspc.naics.2011.temp.2$uspc.code[i], naics.code = c("3351", "3352", "3353", "3359"))
  }
  if (temp.seq == 23){
    temp <- data.frame(uspc.code = uspc.naics.2011.temp.2$uspc.code[i], naics.code = c("3361", "3362", "3363", "3364", "3365", "3366", "3369"))
  }
  if (temp.seq == 24){
    temp <- data.frame(uspc.code = uspc.naics.2011.temp.2$uspc.code[i], naics.code = c("3361", "3362", "3363"))
  }
  if (temp.seq == 25){
    temp <- data.frame(uspc.code = uspc.naics.2011.temp.2$uspc.code[i], naics.code = c("3364"))
  }
   if (temp.seq == 26){
    temp <- data.frame(uspc.code = uspc.naics.2011.temp.2$uspc.code[i], naics.code = c("3365", "3366", "3369"))
  }
  if (temp.seq == 27){
    temp <- data.frame(uspc.code = uspc.naics.2011.temp.2$uspc.code[i], naics.code = c("3371", "3372", "3379"))
  }
  if (temp.seq == 28){
    temp <- data.frame(uspc.code = uspc.naics.2011.temp.2$uspc.code[i], naics.code = c("3391", "3399"))
  }
  if (temp.seq == 29){
    temp <- data.frame(uspc.code = uspc.naics.2011.temp.2$uspc.code[i], naics.code = c("3391"))
  }
  if (temp.seq == 30){
    temp <- data.frame(uspc.code = uspc.naics.2011.temp.2$uspc.code[i], naics.code = c("3399"))
  }
  if (temp.seq == 31){
    temp <- data.frame(uspc.code = uspc.naics.2011.temp.2$uspc.code[i], naics.code = c("3111", "3112", "3113", "3114", "3115", "3116", "3117", "3118", "3119",
                                                                                 "3121", "3122", "3131", "3132", "3133", "3141", "3149", "3151", "3152",
                                                                                 "3159", "3161", "3162", "3169", "3211", "3212", "3219", "3221", "3222",
                                                                                 "3231", "3241", "3251", "3252", "3254", "3255", "3256", "3259", "3261",
                                                                                 "3262", "3271", "3272", "3273", "3274", "3279", "3311", "3312", "3313",
                                                                                 "3314", "3315", "3321", "3322", "3323", "3324", "3325", "3326", "3327",
                                                                                 "3328", "3329", "3331", "3332", "3333", "3334", "3335", "3336", "3339",
                                                                                 "3341", "3342", "3343", "3344", "3345", "3346", "3351", "3352", "3353",
                                                                                 "3359", "3361", "3362", "3363", "3364", "3365", "3366", "3369", "3371",
                                                                                 "3372", "3379", "3391", "3399"))
  }
  uspc.naics.2011.final <- rbind(uspc.naics.2011.final, temp)
}
uspc.naics.2011.last <- uspc.naics.2011.final[-1,]

#rename

uspc2011_naics2002 <- uspc.naics.2011.last

write.csv(uspc2011_naics2002, file = "naics_co2011 (clean).csv", row.names = F)
save(uspc2011_naics2002, file = "uspc2011_naics2002.RData")

#2. year 2009

uspc.naics.2009 <- read.csv("naics_co09.csv", header = T)
str(uspc.naics.2009)
#view(uspc.naics.2009)

#2.1. tame USPC codes : subclass
#for both first and last three digits : never remove trailing zeros

#2.1.1. For first three digits : always remove leading zeros

#(1) if there are characters followed by numbers after remoivng leading zeros (there is no number followed by characters in the first three digits)
#(a) if there are zeros between characters and numbers : remove and paste
#e.g. E05008 -> E5.008
#(b) if there is no zero between characters and numbers : leave
#e.g. E29015 -> E29.015

temp.subclass <- uspc.naics.2009$SUBCLASS

temp.subclass.1 <- substr(temp.subclass, 1, 3)
temp.subclass.1.1 <- str_remove(temp.subclass.1, "^0+") #removing.leading zeros
unique(temp.subclass.1.1[is.na(as.numeric(temp.subclass.1.1)) & temp.subclass.1.1 != ""])
temp.subclass.1.1[temp.subclass.1.1 == "E05"] <- "E5"
temp.subclass.1.1[temp.subclass.1.1 == "E01"] <- "E1"

#2.1.2. For last three digits : always remove trailing zeros (there is not code whose first three digits are "FOR")
#e.g. 236010 -> 236.01, 229100 -> 229.1

#(1) if there are only characters left after removing leading zeros, then remove leading zeros, and paste the first three digits without "." 
#e.g. 06200A -> 62A, 08600R -> 86R, 0830WM -> 83WM, 1970FM -> 197FM, 1030CM -> 103CM, 

#(2) if there are either only numbers or numbers followed by characters left after removing leading zeros, 
#(a) if the first three digits are only characters, other than "FOR": then remove leading zeros in the last three digits, and paste the first three digits without "." 
#e.g. DIG001 -> DIG1, DIG010 -> DIG10, DIG014 -> DIG14
#(b) if the first three digits are only characters with "FOR": then do not remove leading zeros in the last three digits, and paste the first three digits without "." 
#e.g. FOR000 -> FOR000, FOR100 -> FOR100 (FOR is always followed by three full digits that are all numbers)
#(c) otherwise, do not remove leading zeros in the last three digits, and paste the first three digits with "." ()
#e.g. E29015 -> E29.015, 06145M -> 61.45M, E05008 -> E5.008, 08110C -> 81.1C, 00101P -> 1.01P

temp.subclass.2 <- substr(temp.subclass, 4, 6)
temp.subclass.2.1 <- temp.subclass.2
temp.subclass.2.1[temp.subclass.1.1 != "DIG"] <- str_remove(temp.subclass.2.1[temp.subclass.1.1 != "DIG"], "0+$") #removing trailing zeros, except for those who are matched with "DIG" as the first three digits (DIG010 -> DIG10)
#preserving leading zeros if the last three digits have only numerics (e.g. 001 -> .001) or start with numerics, followed by characters (01P -> .01P), except for the last three digits who are matched with DIG (DIG001 -> DIG1)
#i.e., removing leading zeros if the last three digits contain only characters after removing leading zeros
non.num.index <- is.na(as.numeric(temp.subclass.2.1)) 
only.character.index <- grepl("^[A-Za-z]+$", str_remove(temp.subclass.2.1, "^0+")) | temp.subclass.1.1 == "DIG" 
temp.subclass.2.1[only.character.index] <- str_remove(temp.subclass.2.1[only.character.index], "^0+") 

unique(temp.subclass.2.1[non.num.index & temp.subclass.2.1 != ""])
temp.subclass.2.1[temp.subclass.2.1 == "*"] <- ""

temp.index <- unique(temp.subclass.2.1[non.num.index & temp.subclass.2.1 != "" & substr(temp.subclass.2.1, 2, 2) == "0"])
temp.str <- temp.subclass.2.1[temp.subclass.2.1 %in% temp.index]
temp.subclass.2.1[temp.subclass.2.1 %in% temp.index] <- paste(substr(temp.str, 1, 1), substr(temp.str, 3, 3), sep = "")

length(temp.subclass.1.1) == length(temp.subclass.2.1)
num.index <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
subclass.final <- temp.subclass.1.1
with.dot.index <- (substr(temp.subclass.2.1, 1, 1) %in% num.index) & (temp.subclass.1.1 != "DIG")
subclass.final[with.dot.index] <- paste(temp.subclass.1.1[with.dot.index], temp.subclass.2.1[with.dot.index], sep = ".")
subclass.final[!with.dot.index] <- paste(temp.subclass.1.1[!with.dot.index], temp.subclass.2.1[!with.dot.index], sep = "")

#check
subclass.final[subclass.final == "E5.008"]
subclass.final[subclass.final == "E29.015"]
subclass.final[subclass.final == "236.01"]
subclass.final[subclass.final == "229.1"]
subclass.final[subclass.final == "62A"]
subclass.final[subclass.final == "86R"]
subclass.final[subclass.final == "83WM"]
subclass.final[subclass.final == "197FM"]
subclass.final[subclass.final == "103CM"]
subclass.final[subclass.final == "DIG1"]
subclass.final[subclass.final == "DIG10"]
subclass.final[subclass.final == "DIG14"]
subclass.final[subclass.final == "E29.015"]
subclass.final[subclass.final == "61.45M"]
subclass.final[subclass.final == "E5.008"]
subclass.final[subclass.final == "81.1C"]
subclass.final[subclass.final == "1.01P"]

#2.2. tame USPC codes : class
#add leading zeros

temp.class <- uspc.naics.2009$CLASS
class.final <- str_pad(temp.class, 3, pad = "0")

#2.3. tame NAICS codes : transform OTAF into 4 digits

temp.naics <- as.character(uspc.naics.2009$OTAF)

uspc.naics.2009.temp <- uspc.naics.2009 %>% mutate(uspc.code = paste(class.final, subclass.final, sep = "/")) %>% select(-CLASS, -SUBCLASS)
tail.location <- nchar(uspc.naics.2009.temp$uspc.code)
uspc.naics.2009.temp$uspc.code[substr(uspc.naics.2009.temp$uspc.code, tail.location, tail.location) == "/"] <- "001"

#check : these USPC codes do not have descriptions provided by USPC website
uspc.naics.2009.temp$uspc.code[!(uspc.naics.2009.temp$uspc.code %in% uspc.final.3$code)]

#compile
uspc.naics.2009.temp.2 <- uspc.naics.2009.temp %>% select(uspc.code, seq = SEQ)
unique(uspc.naics.2009.temp.2$seq)[order(unique(uspc.naics.2009.temp.2$seq))]

uspc.naics.2009.final <- data.frame(uspc.code = "", naics.code = "")
uspc.naics.2009.final$uspc.code <- as.character(uspc.naics.2009.final$uspc.code)
uspc.naics.2009.final$naics.code <- as.character(uspc.naics.2009.final$naics.code)

for (i in 1:nrow(uspc.naics.2009.temp.2)){
  temp.seq <- uspc.naics.2009.temp.2$seq[i]
  if (temp.seq == 1){
    temp <- data.frame(uspc.code = uspc.naics.2009.temp.2$uspc.code[i], naics.code = c("3111", "3112", "3113", "3114", "3115", "3116", "3117", "3118", "3119"))
  }
  if (temp.seq == 2){
    temp <- data.frame(uspc.code = uspc.naics.2009.temp.2$uspc.code[i], naics.code = c("3121", "3122"))
  }
  if (temp.seq == 3){
    temp <- data.frame(uspc.code = uspc.naics.2009.temp.2$uspc.code[i], naics.code = c("3131", "3132", "3133", "3141", "3149", "3151", "3152", "3159", "3161", "3162", "3169"))
  }
  if (temp.seq == 4){
    temp <- data.frame(uspc.code = uspc.naics.2009.temp.2$uspc.code[i], naics.code = c("3211", "3212", "3219"))
  }
  if (temp.seq == 5){
    temp <- data.frame(uspc.code = uspc.naics.2009.temp.2$uspc.code[i], naics.code = c("3221", "3222", "3231"))
  }
  if (temp.seq == 6){
    temp <- data.frame(uspc.code = uspc.naics.2009.temp.2$uspc.code[i], naics.code = c("3251", "3252", "3253", "3254", "3255", "3256", "3259"))
  }
  if (temp.seq == 7){
    temp <- data.frame(uspc.code = uspc.naics.2009.temp.2$uspc.code[i], naics.code = c("3251"))
  }
  if (temp.seq == 8){
    temp <- data.frame(uspc.code = uspc.naics.2009.temp.2$uspc.code[i], naics.code = c("3252"))
  }
  if (temp.seq == 9){
    temp <- data.frame(uspc.code = uspc.naics.2009.temp.2$uspc.code[i], naics.code = c("3254"))
  }
  if (temp.seq == 10){
    temp <- data.frame(uspc.code = uspc.naics.2009.temp.2$uspc.code[i], naics.code = c("3253", "3255", "3256", "3259"))
  }
  if (temp.seq == 11){
    temp <- data.frame(uspc.code = uspc.naics.2009.temp.2$uspc.code[i], naics.code = c("3261", "3262"))
  }
  if (temp.seq == 12){
    temp <- data.frame(uspc.code = uspc.naics.2009.temp.2$uspc.code[i], naics.code = c("3271", "3272", "3273", "3274", "3279"))
  }
  if (temp.seq == 13){
    temp <- data.frame(uspc.code = uspc.naics.2009.temp.2$uspc.code[i], naics.code = c("3311", "3312", "3313", "3314", "3315"))
  }
  if (temp.seq == 14){
    temp <- data.frame(uspc.code = uspc.naics.2009.temp.2$uspc.code[i], naics.code = c("3321", "3322", "3323", "3324", "3325", "3326", "3327", "3328", "3329"))
  }
  if (temp.seq == 15){
    temp <- data.frame(uspc.code = uspc.naics.2009.temp.2$uspc.code[i], naics.code = c("3331", "3332", "3333", "3334", "3335", "3336", "3339"))
  }
  if (temp.seq == 16){
    temp <- data.frame(uspc.code = uspc.naics.2009.temp.2$uspc.code[i], naics.code = c("3341", "3342", "3343", "3344", "3345", "3346"))
  }
  if (temp.seq == 17){
    temp <- data.frame(uspc.code = uspc.naics.2009.temp.2$uspc.code[i], naics.code = c("3341"))
  }
  if (temp.seq == 18){
    temp <- data.frame(uspc.code = uspc.naics.2009.temp.2$uspc.code[i], naics.code = c("3342"))
  }
  if (temp.seq == 19){
    temp <- data.frame(uspc.code = uspc.naics.2009.temp.2$uspc.code[i], naics.code = c("3344"))
  }
  if (temp.seq == 20){
    temp <- data.frame(uspc.code = uspc.naics.2009.temp.2$uspc.code[i], naics.code = c("3345"))
  }
  if (temp.seq == 21){
    temp <- data.frame(uspc.code = uspc.naics.2009.temp.2$uspc.code[i], naics.code = c("3343", "3346"))
  }
  if (temp.seq == 22){
    temp <- data.frame(uspc.code = uspc.naics.2009.temp.2$uspc.code[i], naics.code = c("3351", "3352", "3353", "3359"))
  }
  if (temp.seq == 23){
    temp <- data.frame(uspc.code = uspc.naics.2009.temp.2$uspc.code[i], naics.code = c("3361", "3362", "3363", "3364", "3365", "3366", "3369"))
  }
  if (temp.seq == 24){
    temp <- data.frame(uspc.code = uspc.naics.2009.temp.2$uspc.code[i], naics.code = c("3361", "3362", "3363"))
  }
  if (temp.seq == 25){
    temp <- data.frame(uspc.code = uspc.naics.2009.temp.2$uspc.code[i], naics.code = c("3364"))
  }
  if (temp.seq == 26){
    temp <- data.frame(uspc.code = uspc.naics.2009.temp.2$uspc.code[i], naics.code = c("3365", "3366", "3369"))
  }
  if (temp.seq == 27){
    temp <- data.frame(uspc.code = uspc.naics.2009.temp.2$uspc.code[i], naics.code = c("3371", "3372", "3379"))
  }
  if (temp.seq == 28){
    temp <- data.frame(uspc.code = uspc.naics.2009.temp.2$uspc.code[i], naics.code = c("3391", "3399"))
  }
  if (temp.seq == 29){
    temp <- data.frame(uspc.code = uspc.naics.2009.temp.2$uspc.code[i], naics.code = c("3391"))
  }
  if (temp.seq == 30){
    temp <- data.frame(uspc.code = uspc.naics.2009.temp.2$uspc.code[i], naics.code = c("3399"))
  }
  if (temp.seq == 31){
    temp <- data.frame(uspc.code = uspc.naics.2009.temp.2$uspc.code[i], naics.code = c("3111", "3112", "3113", "3114", "3115", "3116", "3117", "3118", "3119",
                                                                                 "3121", "3122", "3131", "3132", "3133", "3141", "3149", "3151", "3152",
                                                                                 "3159", "3161", "3162", "3169", "3211", "3212", "3219", "3221", "3222",
                                                                                 "3231", "3241", "3251", "3252", "3254", "3255", "3256", "3259", "3261",
                                                                                 "3262", "3271", "3272", "3273", "3274", "3279", "3311", "3312", "3313",
                                                                                 "3314", "3315", "3321", "3322", "3323", "3324", "3325", "3326", "3327",
                                                                                 "3328", "3329", "3331", "3332", "3333", "3334", "3335", "3336", "3339",
                                                                                 "3341", "3342", "3343", "3344", "3345", "3346", "3351", "3352", "3353",
                                                                                 "3359", "3361", "3362", "3363", "3364", "3365", "3366", "3369", "3371",
                                                                                 "3372", "3379", "3391", "3399"))
  }
  uspc.naics.2009.final <- rbind(uspc.naics.2009.final, temp)
}
uspc.naics.2009.last <- uspc.naics.2009.final[-1,]

#rename

uspc2009_naics2002 <- uspc.naics.2009.last

write.csv(uspc2009_naics2002, file = "naics_co2009 (clean).csv", row.names = F)
save(uspc2009_naics2002, file = "uspc2009_naics2002.RData")

#3. year 2012

uspc.naics.2012 <- read.csv("naics_co12.csv", header = T)
str(uspc.naics.2012)
#view(uspc.naics.2012)

#3.1. tame USPC codes : subclass
#for both first and last three digits : never remove trailing zeros

#3.1.1. For first three digits : always remove leading zeros

#(1) if there are characters followed by numbers after remoivng leading zeros (there is no number followed by characters in the first three digits)
#(a) if there are zeros between characters and numbers : remove and paste
#e.g. E05008 -> E5.008
#(b) if there is no zero between characters and numbers : leave
#e.g. E29015 -> E29.015

temp.subclass <- uspc.naics.2012$SUBCLASS

temp.subclass.1 <- substr(temp.subclass, 1, 3)
temp.subclass.1.1 <- str_remove(temp.subclass.1, "^0+") #removing.leading zeros
unique(temp.subclass.1.1[is.na(as.numeric(temp.subclass.1.1)) & temp.subclass.1.1 != ""])
temp.subclass.1.1[temp.subclass.1.1 == "E05"] <- "E5"
temp.subclass.1.1[temp.subclass.1.1 == "E01"] <- "E1"

#3.1.2. For last three digits : always remove trailing zeros (there is not code whose first three digits are "FOR")
#e.g. 236010 -> 236.01, 229100 -> 229.1

#(1) if there are only characters left after removing leading zeros, then remove leading zeros, and paste the first three digits without "." 
#e.g. 06200A -> 62A, 08600R -> 86R, 0830WM -> 83WM, 1970FM -> 197FM, 1030CM -> 103CM, 

#(2) if there are either only numbers or numbers followed by characters left after removing leading zeros, 
#(a) if the first three digits are only characters, other than "FOR": then remove leading zeros in the last three digits, and paste the first three digits without "." 
#e.g. DIG001 -> DIG1, DIG010 -> DIG10, DIG014 -> DIG14
#(b) if the first three digits are only characters with "FOR": then do not remove leading zeros in the last three digits, and paste the first three digits without "." 
#e.g. FOR000 -> FOR000, FOR100 -> FOR100 (FOR is always followed by three full digits that are all numbers)
#(c) otherwise, do not remove leading zeros in the last three digits, and paste the first three digits with "." ()
#e.g. E29015 -> E29.015, 06145M -> 61.45M, E05008 -> E5.008, 08110C -> 81.1C, 00101P -> 1.01P

temp.subclass.2 <- substr(temp.subclass, 4, 6)
temp.subclass.2.1 <- temp.subclass.2
temp.subclass.2.1[temp.subclass.1.1 != "DIG"] <- str_remove(temp.subclass.2.1[temp.subclass.1.1 != "DIG"], "0+$") #removing trailing zeros, except for those who are matched with "DIG" as the first three digits (DIG010 -> DIG10)
#preserving leading zeros if the last three digits have only numerics (e.g. 001 -> .001) or start with numerics, followed by characters (01P -> .01P), except for the last three digits who are matched with DIG (DIG001 -> DIG1)
#i.e., removing leading zeros if the last three digits contain only characters after removing leading zeros
non.num.index <- is.na(as.numeric(temp.subclass.2.1)) 
only.character.index <- grepl("^[A-Za-z]+$", str_remove(temp.subclass.2.1, "^0+")) | temp.subclass.1.1 == "DIG" 
temp.subclass.2.1[only.character.index] <- str_remove(temp.subclass.2.1[only.character.index], "^0+") 

unique(temp.subclass.2.1[non.num.index & temp.subclass.2.1 != ""])
temp.subclass.2.1[temp.subclass.2.1 == "*"] <- ""

temp.index <- unique(temp.subclass.2.1[non.num.index & temp.subclass.2.1 != "" & substr(temp.subclass.2.1, 2, 2) == "0"])
temp.str <- temp.subclass.2.1[temp.subclass.2.1 %in% temp.index]
temp.subclass.2.1[temp.subclass.2.1 %in% temp.index] <- paste(substr(temp.str, 1, 1), substr(temp.str, 3, 3), sep = "")

length(temp.subclass.1.1) == length(temp.subclass.2.1)
num.index <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
subclass.final <- temp.subclass.1.1
with.dot.index <- (substr(temp.subclass.2.1, 1, 1) %in% num.index) & (temp.subclass.1.1 != "DIG")
subclass.final[with.dot.index] <- paste(temp.subclass.1.1[with.dot.index], temp.subclass.2.1[with.dot.index], sep = ".")
subclass.final[!with.dot.index] <- paste(temp.subclass.1.1[!with.dot.index], temp.subclass.2.1[!with.dot.index], sep = "")

#check
subclass.final[subclass.final == "E5.008"]
subclass.final[subclass.final == "E29.015"]
subclass.final[subclass.final == "236.01"]
subclass.final[subclass.final == "229.1"]
subclass.final[subclass.final == "62A"]
subclass.final[subclass.final == "86R"]
subclass.final[subclass.final == "83WM"]
subclass.final[subclass.final == "197FM"]
subclass.final[subclass.final == "103CM"]
subclass.final[subclass.final == "DIG1"]
subclass.final[subclass.final == "DIG10"]
subclass.final[subclass.final == "DIG14"]
subclass.final[subclass.final == "E29.015"]
subclass.final[subclass.final == "61.45M"]
subclass.final[subclass.final == "E5.008"]
subclass.final[subclass.final == "81.1C"]
subclass.final[subclass.final == "1.01P"]

#3.2. tame USPC codes : class
#add leading zeros

temp.class <- uspc.naics.2012$CLASS
class.final <- str_pad(temp.class, 3, pad = "0")

#3.3. tame NAICS codes : transform OTAF into 4 digits

temp.naics <- as.character(uspc.naics.2012$OTAF)

uspc.naics.2012.temp <- uspc.naics.2012 %>% mutate(uspc.code = paste(class.final, subclass.final, sep = "/")) %>% select(-CLASS, -SUBCLASS)
tail.location <- nchar(uspc.naics.2012.temp$uspc.code)
uspc.naics.2012.temp$uspc.code[substr(uspc.naics.2012.temp$uspc.code, tail.location, tail.location) == "/"] <- "001"

#check : these USPC codes do not have descriptions provided by USPC website
uspc.naics.2012.temp$uspc.code[!(uspc.naics.2012.temp$uspc.code %in% uspc.final.3$code)]

#compile
uspc.naics.2012.temp.2 <- uspc.naics.2012.temp %>% select(uspc.code, seq = SEQ)
unique(uspc.naics.2012.temp.2$seq)[order(unique(uspc.naics.2012.temp.2$seq))]

uspc.naics.2012.final <- data.frame(uspc.code = "", naics.code = "")
uspc.naics.2012.final$uspc.code <- as.character(uspc.naics.2012.final$uspc.code)
uspc.naics.2012.final$naics.code <- as.character(uspc.naics.2012.final$naics.code)

for (i in 1:nrow(uspc.naics.2012.temp.2)){
  temp.seq <- uspc.naics.2012.temp.2$seq[i]
  if (temp.seq == 1){
    temp <- data.frame(uspc.code = uspc.naics.2012.temp.2$uspc.code[i], naics.code = c("3111", "3112", "3113", "3114", "3115", "3116", "3117", "3118", "3119"))
  }
  if (temp.seq == 2){
    temp <- data.frame(uspc.code = uspc.naics.2012.temp.2$uspc.code[i], naics.code = c("3121", "3122"))
  }
  if (temp.seq == 3){
    temp <- data.frame(uspc.code = uspc.naics.2012.temp.2$uspc.code[i], naics.code = c("3131", "3132", "3133", "3141", "3149", "3151", "3152", "3159", "3161", "3162", "3169"))
  }
  if (temp.seq == 4){
    temp <- data.frame(uspc.code = uspc.naics.2012.temp.2$uspc.code[i], naics.code = c("3211", "3212", "3219"))
  }
  if (temp.seq == 5){
    temp <- data.frame(uspc.code = uspc.naics.2012.temp.2$uspc.code[i], naics.code = c("3221", "3222", "3231"))
  }
  if (temp.seq == 6){
    temp <- data.frame(uspc.code = uspc.naics.2012.temp.2$uspc.code[i], naics.code = c("3251", "3252", "3253", "3254", "3255", "3256", "3259"))
  }
  if (temp.seq == 7){
    temp <- data.frame(uspc.code = uspc.naics.2012.temp.2$uspc.code[i], naics.code = c("3251"))
  }
  if (temp.seq == 8){
    temp <- data.frame(uspc.code = uspc.naics.2012.temp.2$uspc.code[i], naics.code = c("3252"))
  }
  if (temp.seq == 9){
    temp <- data.frame(uspc.code = uspc.naics.2012.temp.2$uspc.code[i], naics.code = c("3254"))
  }
  if (temp.seq == 10){
    temp <- data.frame(uspc.code = uspc.naics.2012.temp.2$uspc.code[i], naics.code = c("3253", "3255", "3256", "3259"))
  }
  if (temp.seq == 11){
    temp <- data.frame(uspc.code = uspc.naics.2012.temp.2$uspc.code[i], naics.code = c("3261", "3262"))
  }
  if (temp.seq == 12){
    temp <- data.frame(uspc.code = uspc.naics.2012.temp.2$uspc.code[i], naics.code = c("3271", "3272", "3273", "3274", "3279"))
  }
  if (temp.seq == 13){
    temp <- data.frame(uspc.code = uspc.naics.2012.temp.2$uspc.code[i], naics.code = c("3311", "3312", "3313", "3314", "3315"))
  }
  if (temp.seq == 14){
    temp <- data.frame(uspc.code = uspc.naics.2012.temp.2$uspc.code[i], naics.code = c("3321", "3322", "3323", "3324", "3325", "3326", "3327", "3328", "3329"))
  }
  if (temp.seq == 15){
    temp <- data.frame(uspc.code = uspc.naics.2012.temp.2$uspc.code[i], naics.code = c("3331", "3332", "3333", "3334", "3335", "3336", "3339"))
  }
  if (temp.seq == 16){
    temp <- data.frame(uspc.code = uspc.naics.2012.temp.2$uspc.code[i], naics.code = c("3341", "3342", "3343", "3344", "3345", "3346"))
  }
  if (temp.seq == 17){
    temp <- data.frame(uspc.code = uspc.naics.2012.temp.2$uspc.code[i], naics.code = c("3341"))
  }
  if (temp.seq == 18){
    temp <- data.frame(uspc.code = uspc.naics.2012.temp.2$uspc.code[i], naics.code = c("3342"))
  }
  if (temp.seq == 19){
    temp <- data.frame(uspc.code = uspc.naics.2012.temp.2$uspc.code[i], naics.code = c("3344"))
  }
  if (temp.seq == 20){
    temp <- data.frame(uspc.code = uspc.naics.2012.temp.2$uspc.code[i], naics.code = c("3345"))
  }
  if (temp.seq == 21){
    temp <- data.frame(uspc.code = uspc.naics.2012.temp.2$uspc.code[i], naics.code = c("3343", "3346"))
  }
  if (temp.seq == 22){
    temp <- data.frame(uspc.code = uspc.naics.2012.temp.2$uspc.code[i], naics.code = c("3351", "3352", "3353", "3359"))
  }
  if (temp.seq == 23){
    temp <- data.frame(uspc.code = uspc.naics.2012.temp.2$uspc.code[i], naics.code = c("3361", "3362", "3363", "3364", "3365", "3366", "3369"))
  }
  if (temp.seq == 24){
    temp <- data.frame(uspc.code = uspc.naics.2012.temp.2$uspc.code[i], naics.code = c("3361", "3362", "3363"))
  }
  if (temp.seq == 25){
    temp <- data.frame(uspc.code = uspc.naics.2012.temp.2$uspc.code[i], naics.code = c("3364"))
  }
  if (temp.seq == 26){
    temp <- data.frame(uspc.code = uspc.naics.2012.temp.2$uspc.code[i], naics.code = c("3365", "3366", "3369"))
  }
  if (temp.seq == 27){
    temp <- data.frame(uspc.code = uspc.naics.2012.temp.2$uspc.code[i], naics.code = c("3371", "3372", "3379"))
  }
  if (temp.seq == 28){
    temp <- data.frame(uspc.code = uspc.naics.2012.temp.2$uspc.code[i], naics.code = c("3391", "3399"))
  }
  if (temp.seq == 29){
    temp <- data.frame(uspc.code = uspc.naics.2012.temp.2$uspc.code[i], naics.code = c("3391"))
  }
  if (temp.seq == 30){
    temp <- data.frame(uspc.code = uspc.naics.2012.temp.2$uspc.code[i], naics.code = c("3399"))
  }
  if (temp.seq == 31){
    temp <- data.frame(uspc.code = uspc.naics.2012.temp.2$uspc.code[i], naics.code = c("3111", "3112", "3113", "3114", "3115", "3116", "3117", "3118", "3119",
                                                                                 "3121", "3122", "3131", "3132", "3133", "3141", "3149", "3151", "3152",
                                                                                 "3159", "3161", "3162", "3169", "3211", "3212", "3219", "3221", "3222",
                                                                                 "3231", "3241", "3251", "3252", "3254", "3255", "3256", "3259", "3261",
                                                                                 "3262", "3271", "3272", "3273", "3274", "3279", "3311", "3312", "3313",
                                                                                 "3314", "3315", "3321", "3322", "3323", "3324", "3325", "3326", "3327",
                                                                                 "3328", "3329", "3331", "3332", "3333", "3334", "3335", "3336", "3339",
                                                                                 "3341", "3342", "3343", "3344", "3345", "3346", "3351", "3352", "3353",
                                                                                 "3359", "3361", "3362", "3363", "3364", "3365", "3366", "3369", "3371",
                                                                                 "3372", "3379", "3391", "3399"))
  }
  uspc.naics.2012.final <- rbind(uspc.naics.2012.final, temp)
}
uspc.naics.2012.last <- uspc.naics.2012.final[-1,]

#create new variables
uspc.naics.2012.last$uspc.code.2 <- substr(uspc.naics.2012.last$uspc.code, 1, 3)
uspc.naics.2012.last$naics.code.2 <- substr(uspc.naics.2012.last$naics.code, 1, 3)
uspc.naics.2012.last$naics.code.3 <- substr(uspc.naics.2012.last$naics.code, 1, 2)

#rename

uspc2012_naics2002 <- uspc.naics.2012.last %>% select(uspc_subclass = uspc.code, uspc_class = uspc.code.2,
                                                      NAICS2002_4d = naics.code, NAICS2002_3d = naics.code.2, NAICS2002_2d = naics.code.3)

write.csv(uspc2012_naics2002, file = "naics_co2012 (clean).csv", row.names = F)
save(uspc2012_naics2002, file = "uspc2012_naics2002.RData")

#4. year 2013

uspc.naics.2013 <- read.csv("naics_co13.csv", header = T)
str(uspc.naics.2013)
#view(uspc.naics.2013)

#4.1. tame USPC codes : subclass
#for both first and last three digits : never remove trailing zeros

#4.1.1. For first three digits : always remove leading zeros

#(1) if there are characters followed by numbers after remoivng leading zeros (there is no number followed by characters in the first three digits)
#(a) if there are zeros between characters and numbers : remove and paste
#e.g. E05008 -> E5.008
#(b) if there is no zero between characters and numbers : leave
#e.g. E29015 -> E29.015

temp.subclass <- uspc.naics.2013$SUBCLASS

temp.subclass.1 <- substr(temp.subclass, 1, 3)
temp.subclass.1.1 <- str_remove(temp.subclass.1, "^0+") #removing.leading zeros
unique(temp.subclass.1.1[is.na(as.numeric(temp.subclass.1.1)) & temp.subclass.1.1 != ""])
temp.subclass.1.1[temp.subclass.1.1 == "E05"] <- "E5"
temp.subclass.1.1[temp.subclass.1.1 == "E01"] <- "E1"

#4.1.2. For last three digits : always remove trailing zeros (there is not code whose first three digits are "FOR")
#e.g. 236010 -> 236.01, 229100 -> 229.1

#(1) if there are only characters left after removing leading zeros, then remove leading zeros, and paste the first three digits without "." 
#e.g. 06200A -> 62A, 08600R -> 86R, 0830WM -> 83WM, 1970FM -> 197FM, 1030CM -> 103CM, 

#(2) if there are either only numbers or numbers followed by characters left after removing leading zeros, 
#(a) if the first three digits are only characters, other than "FOR": then remove leading zeros in the last three digits, and paste the first three digits without "." 
#e.g. DIG001 -> DIG1, DIG010 -> DIG10, DIG014 -> DIG14
#(b) if the first three digits are only characters with "FOR": then do not remove leading zeros in the last three digits, and paste the first three digits without "." 
#e.g. FOR000 -> FOR000, FOR100 -> FOR100 (FOR is always followed by three full digits that are all numbers)
#(c) otherwise, do not remove leading zeros in the last three digits, and paste the first three digits with "." ()
#e.g. E29015 -> E29.015, 06145M -> 61.45M, E05008 -> E5.008, 08110C -> 81.1C, 00101P -> 1.01P

temp.subclass.2 <- substr(temp.subclass, 4, 6)
temp.subclass.2.1 <- temp.subclass.2
temp.subclass.2.1[temp.subclass.1.1 != "DIG"] <- str_remove(temp.subclass.2.1[temp.subclass.1.1 != "DIG"], "0+$") #removing trailing zeros, except for those who are matched with "DIG" as the first three digits (DIG010 -> DIG10)
#preserving leading zeros if the last three digits have only numerics (e.g. 001 -> .001) or start with numerics, followed by characters (01P -> .01P), except for the last three digits who are matched with DIG (DIG001 -> DIG1)
#i.e., removing leading zeros if the last three digits contain only characters after removing leading zeros
non.num.index <- is.na(as.numeric(temp.subclass.2.1)) 
only.character.index <- grepl("^[A-Za-z]+$", str_remove(temp.subclass.2.1, "^0+")) | temp.subclass.1.1 == "DIG" 
temp.subclass.2.1[only.character.index] <- str_remove(temp.subclass.2.1[only.character.index], "^0+") 

unique(temp.subclass.2.1[non.num.index & temp.subclass.2.1 != ""])
temp.subclass.2.1[temp.subclass.2.1 == "*"] <- ""

temp.index <- unique(temp.subclass.2.1[non.num.index & temp.subclass.2.1 != "" & substr(temp.subclass.2.1, 2, 2) == "0"])
temp.str <- temp.subclass.2.1[temp.subclass.2.1 %in% temp.index]
temp.subclass.2.1[temp.subclass.2.1 %in% temp.index] <- paste(substr(temp.str, 1, 1), substr(temp.str, 3, 3), sep = "")

length(temp.subclass.1.1) == length(temp.subclass.2.1)
num.index <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
subclass.final <- temp.subclass.1.1
with.dot.index <- (substr(temp.subclass.2.1, 1, 1) %in% num.index) & (temp.subclass.1.1 != "DIG")
subclass.final[with.dot.index] <- paste(temp.subclass.1.1[with.dot.index], temp.subclass.2.1[with.dot.index], sep = ".")
subclass.final[!with.dot.index] <- paste(temp.subclass.1.1[!with.dot.index], temp.subclass.2.1[!with.dot.index], sep = "")

#check
subclass.final[subclass.final == "E5.008"]
subclass.final[subclass.final == "E29.015"]
subclass.final[subclass.final == "236.01"]
subclass.final[subclass.final == "229.1"]
subclass.final[subclass.final == "62A"]
subclass.final[subclass.final == "86R"]
subclass.final[subclass.final == "83WM"]
subclass.final[subclass.final == "197FM"]
subclass.final[subclass.final == "103CM"]
subclass.final[subclass.final == "DIG1"]
subclass.final[subclass.final == "DIG10"]
subclass.final[subclass.final == "DIG14"]
subclass.final[subclass.final == "E29.015"]
subclass.final[subclass.final == "61.45M"]
subclass.final[subclass.final == "E5.008"]
subclass.final[subclass.final == "81.1C"]
subclass.final[subclass.final == "1.01P"]

#4.2. tame USPC codes : class
#add leading zeros

temp.class <- uspc.naics.2013$CLASS
class.final <- str_pad(temp.class, 3, pad = "0")

#4.3. tame NAICS codes : transform OTAF into 4 digits

temp.naics <- as.character(uspc.naics.2013$OTAF)

uspc.naics.2013.temp <- uspc.naics.2013 %>% mutate(uspc.code = paste(class.final, subclass.final, sep = "/")) %>% select(-CLASS, -SUBCLASS)
tail.location <- nchar(uspc.naics.2013.temp$uspc.code)
uspc.naics.2013.temp$uspc.code[substr(uspc.naics.2013.temp$uspc.code, tail.location, tail.location) == "/"] <- "001"

#check : these USPC codes do not have descriptions provided by USPC website
uspc.naics.2013.temp$uspc.code[!(uspc.naics.2013.temp$uspc.code %in% uspc.final.3$code)]

#compile
uspc.naics.2013.temp.2 <- uspc.naics.2013.temp %>% select(uspc.code, seq = SEQ)
unique(uspc.naics.2013.temp.2$seq)[order(unique(uspc.naics.2013.temp.2$seq))]

uspc.naics.2013.final <- data.frame(uspc.code = "", naics.code = "")
uspc.naics.2013.final$uspc.code <- as.character(uspc.naics.2013.final$uspc.code)
uspc.naics.2013.final$naics.code <- as.character(uspc.naics.2013.final$naics.code)

for (i in 1:nrow(uspc.naics.2013.temp.2)){
  temp.seq <- uspc.naics.2013.temp.2$seq[i]
  if (temp.seq == 1){
    temp <- data.frame(uspc.code = uspc.naics.2013.temp.2$uspc.code[i], naics.code = c("3111", "3112", "3113", "3114", "3115", "3116", "3117", "3118", "3119"))
  }
  if (temp.seq == 2){
    temp <- data.frame(uspc.code = uspc.naics.2013.temp.2$uspc.code[i], naics.code = c("3121", "3122"))
  }
  if (temp.seq == 3){
    temp <- data.frame(uspc.code = uspc.naics.2013.temp.2$uspc.code[i], naics.code = c("3131", "3132", "3133", "3141", "3149", "3151", "3152", "3159", "3161", "3162", "3169"))
  }
  if (temp.seq == 4){
    temp <- data.frame(uspc.code = uspc.naics.2013.temp.2$uspc.code[i], naics.code = c("3211", "3212", "3219"))
  }
  if (temp.seq == 5){
    temp <- data.frame(uspc.code = uspc.naics.2013.temp.2$uspc.code[i], naics.code = c("3221", "3222", "3231"))
  }
  if (temp.seq == 6){
    temp <- data.frame(uspc.code = uspc.naics.2013.temp.2$uspc.code[i], naics.code = c("3251", "3252", "3253", "3254", "3255", "3256", "3259"))
  }
  if (temp.seq == 7){
    temp <- data.frame(uspc.code = uspc.naics.2013.temp.2$uspc.code[i], naics.code = c("3251"))
  }
  if (temp.seq == 8){
    temp <- data.frame(uspc.code = uspc.naics.2013.temp.2$uspc.code[i], naics.code = c("3252"))
  }
  if (temp.seq == 9){
    temp <- data.frame(uspc.code = uspc.naics.2013.temp.2$uspc.code[i], naics.code = c("3254"))
  }
  if (temp.seq == 10){
    temp <- data.frame(uspc.code = uspc.naics.2013.temp.2$uspc.code[i], naics.code = c("3253", "3255", "3256", "3259"))
  }
  if (temp.seq == 11){
    temp <- data.frame(uspc.code = uspc.naics.2013.temp.2$uspc.code[i], naics.code = c("3261", "3262"))
  }
  if (temp.seq == 12){
    temp <- data.frame(uspc.code = uspc.naics.2013.temp.2$uspc.code[i], naics.code = c("3271", "3272", "3273", "3274", "3279"))
  }
  if (temp.seq == 13){
    temp <- data.frame(uspc.code = uspc.naics.2013.temp.2$uspc.code[i], naics.code = c("3311", "3312", "3313", "3314", "3315"))
  }
  if (temp.seq == 14){
    temp <- data.frame(uspc.code = uspc.naics.2013.temp.2$uspc.code[i], naics.code = c("3321", "3322", "3323", "3324", "3325", "3326", "3327", "3328", "3329"))
  }
  if (temp.seq == 15){
    temp <- data.frame(uspc.code = uspc.naics.2013.temp.2$uspc.code[i], naics.code = c("3331", "3332", "3333", "3334", "3335", "3336", "3339"))
  }
  if (temp.seq == 16){
    temp <- data.frame(uspc.code = uspc.naics.2013.temp.2$uspc.code[i], naics.code = c("3341", "3342", "3343", "3344", "3345", "3346"))
  }
  if (temp.seq == 17){
    temp <- data.frame(uspc.code = uspc.naics.2013.temp.2$uspc.code[i], naics.code = c("3341"))
  }
  if (temp.seq == 18){
    temp <- data.frame(uspc.code = uspc.naics.2013.temp.2$uspc.code[i], naics.code = c("3342"))
  }
  if (temp.seq == 19){
    temp <- data.frame(uspc.code = uspc.naics.2013.temp.2$uspc.code[i], naics.code = c("3344"))
  }
  if (temp.seq == 20){
    temp <- data.frame(uspc.code = uspc.naics.2013.temp.2$uspc.code[i], naics.code = c("3345"))
  }
  if (temp.seq == 21){
    temp <- data.frame(uspc.code = uspc.naics.2013.temp.2$uspc.code[i], naics.code = c("3343", "3346"))
  }
  if (temp.seq == 22){
    temp <- data.frame(uspc.code = uspc.naics.2013.temp.2$uspc.code[i], naics.code = c("3351", "3352", "3353", "3359"))
  }
  if (temp.seq == 23){
    temp <- data.frame(uspc.code = uspc.naics.2013.temp.2$uspc.code[i], naics.code = c("3361", "3362", "3363", "3364", "3365", "3366", "3369"))
  }
  if (temp.seq == 24){
    temp <- data.frame(uspc.code = uspc.naics.2013.temp.2$uspc.code[i], naics.code = c("3361", "3362", "3363"))
  }
  if (temp.seq == 25){
    temp <- data.frame(uspc.code = uspc.naics.2013.temp.2$uspc.code[i], naics.code = c("3364"))
  }
  if (temp.seq == 26){
    temp <- data.frame(uspc.code = uspc.naics.2013.temp.2$uspc.code[i], naics.code = c("3365", "3366", "3369"))
  }
  if (temp.seq == 27){
    temp <- data.frame(uspc.code = uspc.naics.2013.temp.2$uspc.code[i], naics.code = c("3371", "3372", "3379"))
  }
  if (temp.seq == 28){
    temp <- data.frame(uspc.code = uspc.naics.2013.temp.2$uspc.code[i], naics.code = c("3391", "3399"))
  }
  if (temp.seq == 29){
    temp <- data.frame(uspc.code = uspc.naics.2013.temp.2$uspc.code[i], naics.code = c("3391"))
  }
  if (temp.seq == 30){
    temp <- data.frame(uspc.code = uspc.naics.2013.temp.2$uspc.code[i], naics.code = c("3399"))
  }
  if (temp.seq == 31){
    temp <- data.frame(uspc.code = uspc.naics.2013.temp.2$uspc.code[i], naics.code = c("3111", "3112", "3113", "3114", "3115", "3116", "3117", "3118", "3119",
                                                                                 "3121", "3122", "3131", "3132", "3133", "3141", "3149", "3151", "3152",
                                                                                 "3159", "3161", "3162", "3169", "3211", "3212", "3219", "3221", "3222",
                                                                                 "3231", "3241", "3251", "3252", "3254", "3255", "3256", "3259", "3261",
                                                                                 "3262", "3271", "3272", "3273", "3274", "3279", "3311", "3312", "3313",
                                                                                 "3314", "3315", "3321", "3322", "3323", "3324", "3325", "3326", "3327",
                                                                                 "3328", "3329", "3331", "3332", "3333", "3334", "3335", "3336", "3339",
                                                                                 "3341", "3342", "3343", "3344", "3345", "3346", "3351", "3352", "3353",
                                                                                 "3359", "3361", "3362", "3363", "3364", "3365", "3366", "3369", "3371",
                                                                                 "3372", "3379", "3391", "3399"))
  }
  uspc.naics.2013.final <- rbind(uspc.naics.2013.final, temp)
}
uspc.naics.2013.last <- uspc.naics.2013.final[-1,]

#rename

uspc2013_naics2002 <- uspc.naics.2013.last

write.csv(uspc2013_naics2002, file = "naics_co2013 (clean).csv", row.names = F)
save(uspc2013_naics2002, file = "uspc2013_naics2002.RData")

#5. year 2014

uspc.naics.2014 <- read.csv("naics_co14.csv", header = T)
str(uspc.naics.2014)
#view(uspc.naics.2014)

#5.1. tame USPC codes : subclass
#for both first and last three digits : never remove trailing zeros

#5.1.1. For first three digits : always remove leading zeros

#(1) if there are characters followed by numbers after remoivng leading zeros (there is no number followed by characters in the first three digits)
#(a) if there are zeros between characters and numbers : remove and paste
#e.g. E05008 -> E5.008
#(b) if there is no zero between characters and numbers : leave
#e.g. E29015 -> E29.015

temp.subclass <- uspc.naics.2014$SUBCLASS

temp.subclass.1 <- substr(temp.subclass, 1, 3)
temp.subclass.1.1 <- str_remove(temp.subclass.1, "^0+") #removing.leading zeros
unique(temp.subclass.1.1[is.na(as.numeric(temp.subclass.1.1)) & temp.subclass.1.1 != ""])
temp.subclass.1.1[temp.subclass.1.1 == "E05"] <- "E5"
temp.subclass.1.1[temp.subclass.1.1 == "E01"] <- "E1"

#5.1.2. For last three digits : always remove trailing zeros (there is not code whose first three digits are "FOR")
#e.g. 236010 -> 236.01, 229100 -> 229.1

#(1) if there are only characters left after removing leading zeros, then remove leading zeros, and paste the first three digits without "." 
#e.g. 06200A -> 62A, 08600R -> 86R, 0830WM -> 83WM, 1970FM -> 197FM, 1030CM -> 103CM, 

#(2) if there are either only numbers or numbers followed by characters left after removing leading zeros, 
#(a) if the first three digits are only characters, other than "FOR": then remove leading zeros in the last three digits, and paste the first three digits without "." 
#e.g. DIG001 -> DIG1, DIG010 -> DIG10, DIG014 -> DIG14
#(b) if the first three digits are only characters with "FOR": then do not remove leading zeros in the last three digits, and paste the first three digits without "." 
#e.g. FOR000 -> FOR000, FOR100 -> FOR100 (FOR is always followed by three full digits that are all numbers)
#(c) otherwise, do not remove leading zeros in the last three digits, and paste the first three digits with "." ()
#e.g. E29015 -> E29.015, 06145M -> 61.45M, E05008 -> E5.008, 08110C -> 81.1C, 00101P -> 1.01P

temp.subclass.2 <- substr(temp.subclass, 4, 6)
temp.subclass.2.1 <- temp.subclass.2
temp.subclass.2.1[temp.subclass.1.1 != "DIG"] <- str_remove(temp.subclass.2.1[temp.subclass.1.1 != "DIG"], "0+$") #removing trailing zeros, except for those who are matched with "DIG" as the first three digits (DIG010 -> DIG10)
#preserving leading zeros if the last three digits have only numerics (e.g. 001 -> .001) or start with numerics, followed by characters (01P -> .01P), except for the last three digits who are matched with DIG (DIG001 -> DIG1)
#i.e., removing leading zeros if the last three digits contain only characters after removing leading zeros
non.num.index <- is.na(as.numeric(temp.subclass.2.1)) 
only.character.index <- grepl("^[A-Za-z]+$", str_remove(temp.subclass.2.1, "^0+")) | temp.subclass.1.1 == "DIG" 
temp.subclass.2.1[only.character.index] <- str_remove(temp.subclass.2.1[only.character.index], "^0+") 

unique(temp.subclass.2.1[non.num.index & temp.subclass.2.1 != ""])
temp.subclass.2.1[temp.subclass.2.1 == "*"] <- ""

temp.index <- unique(temp.subclass.2.1[non.num.index & temp.subclass.2.1 != "" & substr(temp.subclass.2.1, 2, 2) == "0"])
temp.str <- temp.subclass.2.1[temp.subclass.2.1 %in% temp.index]
temp.subclass.2.1[temp.subclass.2.1 %in% temp.index] <- paste(substr(temp.str, 1, 1), substr(temp.str, 3, 3), sep = "")

length(temp.subclass.1.1) == length(temp.subclass.2.1)
num.index <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
subclass.final <- temp.subclass.1.1
with.dot.index <- (substr(temp.subclass.2.1, 1, 1) %in% num.index) & (temp.subclass.1.1 != "DIG")
subclass.final[with.dot.index] <- paste(temp.subclass.1.1[with.dot.index], temp.subclass.2.1[with.dot.index], sep = ".")
subclass.final[!with.dot.index] <- paste(temp.subclass.1.1[!with.dot.index], temp.subclass.2.1[!with.dot.index], sep = "")

#check
subclass.final[subclass.final == "E5.008"]
subclass.final[subclass.final == "E29.015"]
subclass.final[subclass.final == "236.01"]
subclass.final[subclass.final == "229.1"]
subclass.final[subclass.final == "62A"]
subclass.final[subclass.final == "86R"]
subclass.final[subclass.final == "83WM"]
subclass.final[subclass.final == "197FM"]
subclass.final[subclass.final == "103CM"]
subclass.final[subclass.final == "DIG1"]
subclass.final[subclass.final == "DIG10"]
subclass.final[subclass.final == "DIG14"]
subclass.final[subclass.final == "E29.015"]
subclass.final[subclass.final == "61.45M"]
subclass.final[subclass.final == "E5.008"]
subclass.final[subclass.final == "81.1C"]
subclass.final[subclass.final == "1.01P"]

#5.2. tame USPC codes : class
#add leading zeros

temp.class <- uspc.naics.2014$CLASS
class.final <- str_pad(temp.class, 3, pad = "0")

#5.3. tame NAICS codes : transform OTAF into 4 digits

temp.naics <- as.character(uspc.naics.2014$OTAF)

uspc.naics.2014.temp <- uspc.naics.2014 %>% mutate(uspc.code = paste(class.final, subclass.final, sep = "/")) %>% select(-CLASS, -SUBCLASS)
tail.location <- nchar(uspc.naics.2014.temp$uspc.code)
uspc.naics.2014.temp$uspc.code[substr(uspc.naics.2014.temp$uspc.code, tail.location, tail.location) == "/"] <- "001"

#check : these USPC codes do not have descriptions provided by USPC website
uspc.naics.2014.temp$uspc.code[!(uspc.naics.2014.temp$uspc.code %in% uspc.final.3$code)]

#compile
uspc.naics.2014.temp.2 <- uspc.naics.2014.temp %>% select(uspc.code, seq = SEQ)
unique(uspc.naics.2014.temp.2$seq)[order(unique(uspc.naics.2014.temp.2$seq))]

uspc.naics.2014.final <- data.frame(uspc.code = "", naics.code = "")
uspc.naics.2014.final$uspc.code <- as.character(uspc.naics.2014.final$uspc.code)
uspc.naics.2014.final$naics.code <- as.character(uspc.naics.2014.final$naics.code)

for (i in 1:nrow(uspc.naics.2014.temp.2)){
  temp.seq <- uspc.naics.2014.temp.2$seq[i]
  if (temp.seq == 1){
    temp <- data.frame(uspc.code = uspc.naics.2014.temp.2$uspc.code[i], naics.code = c("3111", "3112", "3113", "3114", "3115", "3116", "3117", "3118", "3119"))
  }
  if (temp.seq == 2){
    temp <- data.frame(uspc.code = uspc.naics.2014.temp.2$uspc.code[i], naics.code = c("3121", "3122"))
  }
  if (temp.seq == 3){
    temp <- data.frame(uspc.code = uspc.naics.2014.temp.2$uspc.code[i], naics.code = c("3131", "3132", "3133", "3141", "3149", "3151", "3152", "3159", "3161", "3162", "3169"))
  }
  if (temp.seq == 4){
    temp <- data.frame(uspc.code = uspc.naics.2014.temp.2$uspc.code[i], naics.code = c("3211", "3212", "3219"))
  }
  if (temp.seq == 5){
    temp <- data.frame(uspc.code = uspc.naics.2014.temp.2$uspc.code[i], naics.code = c("3221", "3222", "3231"))
  }
  if (temp.seq == 6){
    temp <- data.frame(uspc.code = uspc.naics.2014.temp.2$uspc.code[i], naics.code = c("3251", "3252", "3253", "3254", "3255", "3256", "3259"))
  }
  if (temp.seq == 7){
    temp <- data.frame(uspc.code = uspc.naics.2014.temp.2$uspc.code[i], naics.code = c("3251"))
  }
  if (temp.seq == 8){
    temp <- data.frame(uspc.code = uspc.naics.2014.temp.2$uspc.code[i], naics.code = c("3252"))
  }
  if (temp.seq == 9){
    temp <- data.frame(uspc.code = uspc.naics.2014.temp.2$uspc.code[i], naics.code = c("3254"))
  }
  if (temp.seq == 10){
    temp <- data.frame(uspc.code = uspc.naics.2014.temp.2$uspc.code[i], naics.code = c("3253", "3255", "3256", "3259"))
  }
  if (temp.seq == 11){
    temp <- data.frame(uspc.code = uspc.naics.2014.temp.2$uspc.code[i], naics.code = c("3261", "3262"))
  }
  if (temp.seq == 12){
    temp <- data.frame(uspc.code = uspc.naics.2014.temp.2$uspc.code[i], naics.code = c("3271", "3272", "3273", "3274", "3279"))
  }
  if (temp.seq == 13){
    temp <- data.frame(uspc.code = uspc.naics.2014.temp.2$uspc.code[i], naics.code = c("3311", "3312", "3313", "3314", "3315"))
  }
  if (temp.seq == 14){
    temp <- data.frame(uspc.code = uspc.naics.2014.temp.2$uspc.code[i], naics.code = c("3321", "3322", "3323", "3324", "3325", "3326", "3327", "3328", "3329"))
  }
  if (temp.seq == 15){
    temp <- data.frame(uspc.code = uspc.naics.2014.temp.2$uspc.code[i], naics.code = c("3331", "3332", "3333", "3334", "3335", "3336", "3339"))
  }
  if (temp.seq == 16){
    temp <- data.frame(uspc.code = uspc.naics.2014.temp.2$uspc.code[i], naics.code = c("3341", "3342", "3343", "3344", "3345", "3346"))
  }
  if (temp.seq == 17){
    temp <- data.frame(uspc.code = uspc.naics.2014.temp.2$uspc.code[i], naics.code = c("3341"))
  }
  if (temp.seq == 18){
    temp <- data.frame(uspc.code = uspc.naics.2014.temp.2$uspc.code[i], naics.code = c("3342"))
  }
  if (temp.seq == 19){
    temp <- data.frame(uspc.code = uspc.naics.2014.temp.2$uspc.code[i], naics.code = c("3344"))
  }
  if (temp.seq == 20){
    temp <- data.frame(uspc.code = uspc.naics.2014.temp.2$uspc.code[i], naics.code = c("3345"))
  }
  if (temp.seq == 21){
    temp <- data.frame(uspc.code = uspc.naics.2014.temp.2$uspc.code[i], naics.code = c("3343", "3346"))
  }
  if (temp.seq == 22){
    temp <- data.frame(uspc.code = uspc.naics.2014.temp.2$uspc.code[i], naics.code = c("3351", "3352", "3353", "3359"))
  }
  if (temp.seq == 23){
    temp <- data.frame(uspc.code = uspc.naics.2014.temp.2$uspc.code[i], naics.code = c("3361", "3362", "3363", "3364", "3365", "3366", "3369"))
  }
  if (temp.seq == 24){
    temp <- data.frame(uspc.code = uspc.naics.2014.temp.2$uspc.code[i], naics.code = c("3361", "3362", "3363"))
  }
  if (temp.seq == 25){
    temp <- data.frame(uspc.code = uspc.naics.2014.temp.2$uspc.code[i], naics.code = c("3364"))
  }
  if (temp.seq == 26){
    temp <- data.frame(uspc.code = uspc.naics.2014.temp.2$uspc.code[i], naics.code = c("3365", "3366", "3369"))
  }
  if (temp.seq == 27){
    temp <- data.frame(uspc.code = uspc.naics.2014.temp.2$uspc.code[i], naics.code = c("3371", "3372", "3379"))
  }
  if (temp.seq == 28){
    temp <- data.frame(uspc.code = uspc.naics.2014.temp.2$uspc.code[i], naics.code = c("3391", "3399"))
  }
  if (temp.seq == 29){
    temp <- data.frame(uspc.code = uspc.naics.2014.temp.2$uspc.code[i], naics.code = c("3391"))
  }
  if (temp.seq == 30){
    temp <- data.frame(uspc.code = uspc.naics.2014.temp.2$uspc.code[i], naics.code = c("3399"))
  }
  if (temp.seq == 31){
    temp <- data.frame(uspc.code = uspc.naics.2014.temp.2$uspc.code[i], naics.code = c("3111", "3112", "3113", "3114", "3115", "3116", "3117", "3118", "3119",
                                                                                 "3121", "3122", "3131", "3132", "3133", "3141", "3149", "3151", "3152",
                                                                                 "3159", "3161", "3162", "3169", "3211", "3212", "3219", "3221", "3222",
                                                                                 "3231", "3241", "3251", "3252", "3254", "3255", "3256", "3259", "3261",
                                                                                 "3262", "3271", "3272", "3273", "3274", "3279", "3311", "3312", "3313",
                                                                                 "3314", "3315", "3321", "3322", "3323", "3324", "3325", "3326", "3327",
                                                                                 "3328", "3329", "3331", "3332", "3333", "3334", "3335", "3336", "3339",
                                                                                 "3341", "3342", "3343", "3344", "3345", "3346", "3351", "3352", "3353",
                                                                                 "3359", "3361", "3362", "3363", "3364", "3365", "3366", "3369", "3371",
                                                                                 "3372", "3379", "3391", "3399"))
  }
  uspc.naics.2014.final <- rbind(uspc.naics.2014.final, temp)
}
uspc.naics.2014.last <- uspc.naics.2014.final[-1,]

#rename

uspc2014_naics2002 <- uspc.naics.2014.last 

write.csv(uspc2014_naics2002, file = "naics_co2014 (clean).csv", row.names = F)
save(uspc2014_naics2002, file = "uspc2014_naics2002.RData")