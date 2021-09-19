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
class.num <- class.num[c(1:429, length(class.num))]

uspc.reference <- read.csv("uspc2012 description (clean).csv", header = TRUE)
dat.final <- data.frame(uspc.subclass = "", ipc.subclass = "", ipc.group = "")
dat.final <- dat.final[0,]

for (i in 1:length(class.num)){
  
  #0. only utility classes have IPC concordance
  temp.class.num <- class.num[i]
  if(!is.na(as.numeric(temp.class.num))){
    temp.class.num.url <- temp.class.num
  } else {
    temp.class.num.url <- tolower(temp.class.num)
  }
  
  #1. for each USPC class, order USPC subclasses for reference
  uspc.reference.2 <- uspc.reference %>% filter(substr(code, 1, 3) == temp.class.num)
  uspc.reference.3 <- unlist(str_split(as.character(uspc.reference.2$code), "/"))
  uspc.reference.4 <- c(uspc.reference.3[1], "0", uspc.reference.3[2:length(uspc.reference.3)]) #"0" for the leading USPC class without any subclass
  uspc.reference.5 <- uspc.reference.4[uspc.reference.4 != temp.class.num]
  uspc.reference.6 <- uspc.reference.5[!is.na(as.numeric(substr(uspc.reference.5, 1, 1))) | substr(uspc.reference.5, 1, 1) == "."] #"." for subclasses that are less than 1 (e.g. .01)
  uspc.reference.7 <- gsub("[a-zA-Z]", "", uspc.reference.6)
  uspc.reference.8 <- uspc.reference.6[order(as.numeric(uspc.reference.7))]
  
  #2. construct initial data
  
  #(1) construct a column of IPC group
  temp.html.1 <- paste("https://www.uspto.gov/web/patents/classification/uspc", "/us", sep = temp.class.num.url)
  temp.html.2 <- paste(temp.html.1,"toipc8.htm", sep = temp.class.num.url)
  temp.1 <- read_html(temp.html.2)
  temp.2 <- gsub("\\s", "", temp.1)
  temp.ipc.group <- temp.2[grepl("/", temp.2, fixed = TRUE) & !is.na(as.numeric(substr(temp.2, 1, 1)))]
  
  #(2) construct a column of IPC subclass
  temp.3 <- temp.2[!grepl("/", temp.2, fixed = TRUE)]
  temp.start.index <- which(grepl("IPC8Group", temp.3, fixed = TRUE))
  start.index <- temp.start.index[length(temp.start.index)] + 1
  end.index <- which(temp.3 == "Skipfooterandgotomaincontent") - 1
  temp.4 <- temp.3[start.index:end.index]
  temp.ipc.subclass <- temp.4[temp.4 != "-" & is.na(as.numeric(substr(temp.4, 1, 1))) & substr(temp.4, 1, 1) != "."]
  
  #(3) combine (1) and (2)
  temp.uspc.to.ipc <- as.data.frame(cbind(temp.ipc.subclass, temp.ipc.group))
  names(temp.uspc.to.ipc) <- c("ipc.subclass", "ipc.group")

  #3. construct final data
  dat <- data.frame(uspc.subclass = "", ipc.subclass = "", ipc.group = "")
  dat <- dat[0,]
  temp.5 <- temp.4[-which(temp.4 %in% temp.ipc.subclass)]
  n <- nrow(temp.uspc.to.ipc)
  m <- length(temp.5)
  j <- 1 #index for row in the data frame "temp.uspc.to.ipc"
  k <- 1 #index for temp.5
  
  while (j <= n & k <= m){
      if (temp.5[k + 1] != "-" | k == m){
        temp.dat <- data.frame(uspc.subclass = paste(temp.class.num, temp.5[k], sep = "/"), ipc.subclass = temp.uspc.to.ipc$ipc.subclass[j], ipc.group = temp.uspc.to.ipc$ipc.group[j])
        j <- j + 1
        k <- k + 1
      } else {
        begin.temp <- paste(temp.class.num, temp.5[k], sep = "/")
        end.temp <- paste(temp.class.num, temp.5[k+2], sep = "/")
        begin.reference <- which(begin.temp == paste(temp.class.num, uspc.reference.8, sep = "/"))
        end.reference <- which(end.temp == paste(temp.class.num, uspc.reference.8, sep = "/"))
        begin.reference <- min(begin.reference, end.reference)
        end.reference <- max(begin.reference, end.reference)
        extension.temp.new <- paste(temp.class.num, uspc.reference.8[seq(begin.reference, end.reference, by = 1)], sep = "/")
        temp.dat <- data.frame(uspc.subclass = extension.temp.new, ipc.subclass = temp.uspc.to.ipc$ipc.subclass[j], ipc.group = temp.uspc.to.ipc$ipc.group[j])
        j <- j + 1
        k <- k + 3
      }
    dat <- rbind(dat, temp.dat)
  }
  
  #4. pile
  dat.final <- rbind(dat.final, dat)
}

#check
dat.final %>% filter(uspc.subclass == "PLT/395")
dat.final %>% filter(uspc.subclass == "030/118")
dat.final %>% filter(uspc.subclass == "030/118")
dat.final %>% filter(uspc.subclass == "082/106")

#save
dat.final$uspc.subclass <- as.character(dat.final$uspc.subclass)
dat.final$ipc.subclass <- as.character(dat.final$ipc.subclass)
dat.final$ipc.group <- as.character(dat.final$ipc.group)
str(dat.final)

uspc.to.ipc <- dat.final
uspc.to.ipc.1 <- uspc.to.ipc %>% mutate(uspc.code = uspc.subclass, ipc.code = paste(ipc.subclass, ipc.group, sep = " ")) %>% select(uspc.code, ipc.code)
uspc.to.ipc.1$uspc.code <- as.character(uspc.to.ipc.1$uspc.code)
str(uspc.to.ipc.1)
 
#create new variables : with different layers of classes and subclasses, matched between IPC and USPC
str(uspc.to.ipc.1)
uspc.to.ipc.1$uspc.code.2 <- substr(uspc.to.ipc.1$uspc.code, 1, 3)
uspc.to.ipc.1$ipc.code.2 <- sub("/.*", "", uspc.to.ipc.1$ipc.code)
uspc.to.ipc.1$ipc.code.3 <- substr(uspc.to.ipc.1$ipc.code, 1, 4) 
uspc.to.ipc.1$ipc.code.4 <- substr(uspc.to.ipc.1$ipc.code, 1, 3) 
uspc.to.ipc.1$ipc.code.5 <- substr(uspc.to.ipc.1$ipc.code, 1, 1) 
#view(uspc.to.ipc.1)

#rename
uspc2012_ipc2012 <- uspc.to.ipc.1 %>% select(uspc_subclass = uspc.code, uspc_class = uspc.code.2, ipc_subroup = ipc.code, 
                                             ipc_group = ipc.code.2, ipc_subclass = ipc.code.3, ipc_class = ipc.code.4, ipc_section = ipc.code.5)
uspc.to.ipc.raw <- uspc.to.ipc.1 %>% select(uspc.code, ipc.code)

#save
write.csv(uspc.to.ipc.raw, file = "2012_USPC_to_IPC.csv", row.names=FALSE)
write.csv(uspc2012_ipc2012, file = "2012_USPC_to_IPC (clean).csv", row.names=FALSE)
save(uspc2012_ipc2012, file = "uspc2012_ipc2012.RData")