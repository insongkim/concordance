getwd()
setwd("/Users/sojunp/concordance/data-raw")

install.packages("Hmisc")
library(Hmisc)
library(devtools)
library(roxygen2)
library(openxlsx)
library(tidyverse)
library(stringr)

#1. make tidy : description

tidy.ipc <- function(dat){
  desc.1 <- dat$desc
  desc.2 <- tolower(desc.1)
  #https://intellipaat.com/community/7463/capitalize-the-first-letter-of-both-words-in-a-two-word-string
  desc.3 <- capitalize(desc.2)
  #https://stringr.tidyverse.org/reference/str_replace.html
  desc.4 <- str_replace_all(desc.3, ";", ",")
  #http://rfunction.com/archives/2354
  #https://stackoverflow.com/questions/26682107/remove-the-letters-between-two-patterns-of-strings-in-r
  desc.5 <- gsub("\\s\\(.*\\)", "", desc.4)
  #https://garthtarr.github.io/meatR/strings_regex.html
  #https://stackoverflow.com/questions/51466957/r-regex-for-matching-comma-separated-sections-in-a-column-vector
  desc.6 <- gsub(",\\se\\.g.*", "", desc.5) #removing ", e.g."
  desc.7 <- gsub("\\.\\se\\.g.*", "", desc.6) #removing ". e.g."
  desc.8 <- gsub("\\se\\.g.*", "", desc.7) #removing " e.g."
  desc.9 <- gsub(",\\si\\.e.*", "", desc.8) #removing ", i.e."
  desc.10 <- gsub("\\.\\si\\.e.*", "", desc.9) #removing ". i.e."
  desc.11 <- gsub("\\si\\.e.*", "", desc.10) #removing " i.e."
  desc.12 <- gsub(",,", "", desc.11)
  dat$desc <- desc.12
  return(dat)
}

#(1) ipc 2008

ipc2008.a <- read.xlsx("ipc2008_desc.xlsx", sheet = 1)
str(ipc2008.a)
names(ipc2008.a) <- c("code", "desc")
ipc2008.a.new <- tidy.ipc(ipc2008.a)
str(ipc2008.a.new)
ipc2008.a.final <- rbind(c("A", "Human necessities"), ipc2008.a.new)
str(ipc2008.a.final)
#View(ipc2008.a.final)

ipc2008.b <- read.xlsx("ipc2008_desc.xlsx", sheet = 2)
str(ipc2008.b)
names(ipc2008.b) <- c("code", "desc")
ipc2008.b.new <- tidy.ipc(ipc2008.b)
str(ipc2008.b.new)
ipc2008.b.final <- rbind(c("B", "Performing operations, transporting"), ipc2008.b.new)
str(ipc2008.b.final)
#View(ipc2008.b.final)

ipc2008.c <- read.xlsx("ipc2008_desc.xlsx", sheet = 3)
str(ipc2008.c)
names(ipc2008.c) <- c("code", "desc")
ipc2008.c.new <- tidy.ipc(ipc2008.c)
str(ipc2008.c.new)
ipc2008.c.final <- rbind(c("C", "Chemistry, metallurgy"), ipc2008.c.new)
str(ipc2008.c.final)
#View(ipc2008.c.final)

ipc2008.d <- read.xlsx("ipc2008_desc.xlsx", sheet = 4)
str(ipc2008.d)
names(ipc2008.d) <- c("code", "desc")
ipc2008.d.new <- tidy.ipc(ipc2008.d)
str(ipc2008.d.new)
ipc2008.d.final <- rbind(c("D", "Textiles, paper"), ipc2008.d.new)
str(ipc2008.d.final)
#View(ipc2008.d.final)

ipc2008.e <- read.xlsx("ipc2008_desc.xlsx", sheet = 5)
str(ipc2008.e)
names(ipc2008.e) <- c("code", "desc")
ipc2008.e.new <- tidy.ipc(ipc2008.e)
str(ipc2008.e.new)
ipc2008.e.final <- rbind(c("E", "Fixed constructions"), ipc2008.e.new)
str(ipc2008.e.final)
#View(ipc2008.e.final)

ipc2008.f <- read.xlsx("ipc2008_desc.xlsx", sheet = 6)
str(ipc2008.f)
names(ipc2008.f) <- c("code", "desc")
ipc2008.f.new <- tidy.ipc(ipc2008.f)
str(ipc2008.f.new)
ipc2008.f.final <- rbind(c("F", "Mechanical engineering, lighting, heating, weapons, blasting"), ipc2008.f.new)
str(ipc2008.f.final)
#View(ipc2008.f.final)

ipc2008.g <- read.xlsx("ipc2008_desc.xlsx", sheet = 7)
str(ipc2008.g)
names(ipc2008.g) <- c("code", "desc")
ipc2008.g.new <- tidy.ipc(ipc2008.g)
str(ipc2008.g.new)
ipc2008.g.final <- rbind(c("G", "Physics"), ipc2008.g.new)
str(ipc2008.g.final)
#View(ipc2008.g.final)

ipc2008.h <- read.xlsx("ipc2008_desc.xlsx", sheet = 8)
str(ipc2008.h)
names(ipc2008.h) <- c("code", "desc")
ipc2008.h.new <- tidy.ipc(ipc2008.h)
str(ipc2008.h.new)
ipc2008.h.final <- rbind(c("H", "Electricity"), ipc2008.h.new)
str(ipc2008.h.final)
#View(ipc2008.h.final)

ipc2008 <- rbind(ipc2008.a.final, ipc2008.b.final, ipc2008.c.final, ipc2008.d.final,
                 ipc2008.e.final, ipc2008.f.final, ipc2008.g.final, ipc2008.h.final)

#write.csv(ipc2008, "ipc2008.csv", row.names = F)

#(2) ipc 2009

ipc2009.a <- read.xlsx("ipc2009_desc.xlsx", sheet = 1)
str(ipc2009.a)
names(ipc2009.a) <- c("code", "desc")
ipc2009.a.new <- tidy.ipc(ipc2009.a)
str(ipc2009.a.new)
ipc2009.a.final <- rbind(c("A", "Human necessities"), ipc2009.a.new)
str(ipc2009.a.final)
#View(ipc2009.a.final)

ipc2009.b <- read.xlsx("ipc2009_desc.xlsx", sheet = 2)
str(ipc2009.b)
names(ipc2009.b) <- c("code", "desc")
ipc2009.b.new <- tidy.ipc(ipc2009.b)
str(ipc2009.b.new)
ipc2009.b.final <- rbind(c("B", "Performing operations, transporting"), ipc2009.b.new)
str(ipc2009.b.final)
#View(ipc2009.b.final)

ipc2009.c <- read.xlsx("ipc2009_desc.xlsx", sheet = 3)
str(ipc2009.c)
names(ipc2009.c) <- c("code", "desc")
ipc2009.c.new <- tidy.ipc(ipc2009.c)
str(ipc2009.c.new)
ipc2009.c.final <- rbind(c("C", "Chemistry, metallurgy"), ipc2009.c.new)
str(ipc2009.c.final)
#View(ipc2009.c.final)

ipc2009.d <- read.xlsx("ipc2009_desc.xlsx", sheet = 4)
str(ipc2009.d)
names(ipc2009.d) <- c("code", "desc")
ipc2009.d.new <- tidy.ipc(ipc2009.d)
str(ipc2009.d.new)
ipc2009.d.final <- rbind(c("D", "Textiles, paper"), ipc2009.d.new)
str(ipc2009.d.final)
#View(ipc2009.d.final)

ipc2009.e <- read.xlsx("ipc2009_desc.xlsx", sheet = 5)
str(ipc2009.e)
names(ipc2009.e) <- c("code", "desc")
ipc2009.e.new <- tidy.ipc(ipc2009.e)
str(ipc2009.e.new)
ipc2009.e.final <- rbind(c("E", "Fixed constructions"), ipc2009.e.new)
str(ipc2009.e.final)
#View(ipc2009.e.final)

ipc2009.f <- read.xlsx("ipc2009_desc.xlsx", sheet = 6)
str(ipc2009.f)
names(ipc2009.f) <- c("code", "desc")
ipc2009.f.new <- tidy.ipc(ipc2009.f)
str(ipc2009.f.new)
ipc2009.f.final <- rbind(c("F", "Mechanical engineering, lighting, heating, weapons, blasting"), ipc2009.f.new)
str(ipc2009.f.final)
#View(ipc2009.f.final)

ipc2009.g <- read.xlsx("ipc2009_desc.xlsx", sheet = 7)
str(ipc2009.g)
names(ipc2009.g) <- c("code", "desc")
ipc2009.g.new <- tidy.ipc(ipc2009.g)
str(ipc2009.g.new)
ipc2009.g.final <- rbind(c("G", "Physics"), ipc2009.g.new)
str(ipc2009.g.final)
#View(ipc2009.g.final)

ipc2009.h <- read.xlsx("ipc2009_desc.xlsx", sheet = 8)
str(ipc2009.h)
names(ipc2009.h) <- c("code", "desc")
ipc2009.h.new <- tidy.ipc(ipc2009.h)
str(ipc2009.h.new)
ipc2009.h.final <- rbind(c("H", "Electricity"), ipc2009.h.new)
str(ipc2009.h.final)
#View(ipc2009.h.final)

ipc2009 <- rbind(ipc2009.a.final, ipc2009.b.final, ipc2009.c.final, ipc2009.d.final,
                 ipc2009.e.final, ipc2009.f.final, ipc2009.g.final, ipc2009.h.final)

#write.csv(ipc2009, "ipc2009.csv", row.names = F)

#(3) ipc 2010

ipc2010.a <- read.xlsx("ipc2010_desc.xlsx", sheet = 1)
str(ipc2010.a)
names(ipc2010.a) <- c("code", "desc")
ipc2010.a.new <- tidy.ipc(ipc2010.a)
str(ipc2010.a.new)
ipc2010.a.final <- rbind(c("A", "Human necessities"), ipc2010.a.new)
str(ipc2010.a.final)
#View(ipc2010.a.final)

ipc2010.b <- read.xlsx("ipc2010_desc.xlsx", sheet = 2)
str(ipc2010.b)
names(ipc2010.b) <- c("code", "desc")
ipc2010.b.new <- tidy.ipc(ipc2010.b)
str(ipc2010.b.new)
ipc2010.b.final <- rbind(c("B", "Performing operations, transporting"), ipc2010.b.new)
str(ipc2010.b.final)
#View(ipc2010.b.final)

ipc2010.c <- read.xlsx("ipc2010_desc.xlsx", sheet = 3)
str(ipc2010.c)
names(ipc2010.c) <- c("code", "desc")
ipc2010.c.new <- tidy.ipc(ipc2010.c)
str(ipc2010.c.new)
ipc2010.c.final <- rbind(c("C", "Chemistry, metallurgy"), ipc2010.c.new)
str(ipc2010.c.final)
#View(ipc2010.c.final)

ipc2010.d <- read.xlsx("ipc2010_desc.xlsx", sheet = 4)
str(ipc2010.d)
names(ipc2010.d) <- c("code", "desc")
ipc2010.d.new <- tidy.ipc(ipc2010.d)
str(ipc2010.d.new)
ipc2010.d.final <- rbind(c("D", "Textiles, paper"), ipc2010.d.new)
str(ipc2010.d.final)
#View(ipc2010.d.final)

ipc2010.e <- read.xlsx("ipc2010_desc.xlsx", sheet = 5)
str(ipc2010.e)
names(ipc2010.e) <- c("code", "desc")
ipc2010.e.new <- tidy.ipc(ipc2010.e)
str(ipc2010.e.new)
ipc2010.e.final <- rbind(c("E", "Fixed constructions"), ipc2010.e.new)
str(ipc2010.e.final)
#View(ipc2010.e.final)

ipc2010.f <- read.xlsx("ipc2010_desc.xlsx", sheet = 6)
str(ipc2010.f)
names(ipc2010.f) <- c("code", "desc")
ipc2010.f.new <- tidy.ipc(ipc2010.f)
str(ipc2010.f.new)
ipc2010.f.final <- rbind(c("F", "Mechanical engineering, lighting, heating, weapons, blasting"), ipc2010.f.new)
str(ipc2010.f.final)
#View(ipc2010.f.final)

ipc2010.g <- read.xlsx("ipc2010_desc.xlsx", sheet = 7)
str(ipc2010.g)
names(ipc2010.g) <- c("code", "desc")
ipc2010.g.new <- tidy.ipc(ipc2010.g)
str(ipc2010.g.new)
ipc2010.g.final <- rbind(c("G", "Physics"), ipc2010.g.new)
str(ipc2010.g.final)
#View(ipc2010.g.final)

ipc2010.h <- read.xlsx("ipc2010_desc.xlsx", sheet = 8)
str(ipc2010.h)
names(ipc2010.h) <- c("code", "desc")
ipc2010.h.new <- tidy.ipc(ipc2010.h)
str(ipc2010.h.new)
ipc2010.h.final <- rbind(c("H", "Electricity"), ipc2010.h.new)
str(ipc2010.h.final)
#View(ipc2010.h.final)

ipc2010 <- rbind(ipc2010.a.final, ipc2010.b.final, ipc2010.c.final, ipc2010.d.final,
                 ipc2010.e.final, ipc2010.f.final, ipc2010.g.final, ipc2010.h.final)

#write.csv(ipc2010, "ipc2010.csv", row.names = F)

#(4) ipc 2011

ipc2011.a <- read.xlsx("ipc2011_desc.xlsx", sheet = 1)
str(ipc2011.a)
names(ipc2011.a) <- c("code", "desc")
ipc2011.a.new <- tidy.ipc(ipc2011.a)
str(ipc2011.a.new)
ipc2011.a.final <- rbind(c("A", "Human necessities"), ipc2011.a.new)
str(ipc2011.a.final)
#View(ipc2011.a.final)

ipc2011.b <- read.xlsx("ipc2011_desc.xlsx", sheet = 2)
str(ipc2011.b)
names(ipc2011.b) <- c("code", "desc")
ipc2011.b.new <- tidy.ipc(ipc2011.b)
str(ipc2011.b.new)
ipc2011.b.final <- rbind(c("B", "Performing operations, transporting"), ipc2011.b.new)
str(ipc2011.b.final)
#View(ipc2011.b.final)

ipc2011.c <- read.xlsx("ipc2011_desc.xlsx", sheet = 3)
str(ipc2011.c)
names(ipc2011.c) <- c("code", "desc")
ipc2011.c.new <- tidy.ipc(ipc2011.c)
str(ipc2011.c.new)
ipc2011.c.final <- rbind(c("C", "Chemistry, metallurgy"), ipc2011.c.new)
str(ipc2011.c.final)
#View(ipc2011.c.final)

ipc2011.d <- read.xlsx("ipc2011_desc.xlsx", sheet = 4)
str(ipc2011.d)
names(ipc2011.d) <- c("code", "desc")
ipc2011.d.new <- tidy.ipc(ipc2011.d)
str(ipc2011.d.new)
ipc2011.d.final <- rbind(c("D", "Textiles, paper"), ipc2011.d.new)
str(ipc2011.d.final)
#View(ipc2011.d.final)

ipc2011.e <- read.xlsx("ipc2011_desc.xlsx", sheet = 5)
str(ipc2011.e)
names(ipc2011.e) <- c("code", "desc")
ipc2011.e.new <- tidy.ipc(ipc2011.e)
str(ipc2011.e.new)
ipc2011.e.final <- rbind(c("E", "Fixed constructions"), ipc2011.e.new)
str(ipc2011.e.final)
#View(ipc2011.e.final)

ipc2011.f <- read.xlsx("ipc2011_desc.xlsx", sheet = 6)
str(ipc2011.f)
names(ipc2011.f) <- c("code", "desc")
ipc2011.f.new <- tidy.ipc(ipc2011.f)
str(ipc2011.f.new)
ipc2011.f.final <- rbind(c("F", "Mechanical engineering, lighting, heating, weapons, blasting"), ipc2011.f.new)
str(ipc2011.f.final)
#View(ipc2011.f.final)

ipc2011.g <- read.xlsx("ipc2011_desc.xlsx", sheet = 7)
str(ipc2011.g)
names(ipc2011.g) <- c("code", "desc")
ipc2011.g.new <- tidy.ipc(ipc2011.g)
str(ipc2011.g.new)
ipc2011.g.final <- rbind(c("G", "Physics"), ipc2011.g.new)
str(ipc2011.g.final)
#View(ipc2011.g.final)

ipc2011.h <- read.xlsx("ipc2011_desc.xlsx", sheet = 8)
str(ipc2011.h)
names(ipc2011.h) <- c("code", "desc")
ipc2011.h.new <- tidy.ipc(ipc2011.h)
str(ipc2011.h.new)
ipc2011.h.final <- rbind(c("H", "Electricity"), ipc2011.h.new)
str(ipc2011.h.final)
#View(ipc2011.h.final)

ipc2011 <- rbind(ipc2011.a.final, ipc2011.b.final, ipc2011.c.final, ipc2011.d.final,
                 ipc2011.e.final, ipc2011.f.final, ipc2011.g.final, ipc2011.h.final)

#write.csv(ipc2011, "ipc2011.csv", row.names = F)

#(5) ipc 2012

ipc2012.a <- read.xlsx("ipc2012_desc.xlsx", sheet = 1)
str(ipc2012.a)
names(ipc2012.a) <- c("code", "desc")
ipc2012.a.new <- tidy.ipc(ipc2012.a)
str(ipc2012.a.new)
ipc2012.a.final <- rbind(c("A", "Human necessities"), ipc2012.a.new)
str(ipc2012.a.final)
#View(ipc2012.a.final)

ipc2012.b <- read.xlsx("ipc2012_desc.xlsx", sheet = 2)
str(ipc2012.b)
names(ipc2012.b) <- c("code", "desc")
ipc2012.b.new <- tidy.ipc(ipc2012.b)
str(ipc2012.b.new)
ipc2012.b.final <- rbind(c("B", "Performing operations, transporting"), ipc2012.b.new)
str(ipc2012.b.final)
#View(ipc2012.b.final)

ipc2012.c <- read.xlsx("ipc2012_desc.xlsx", sheet = 3)
str(ipc2012.c)
names(ipc2012.c) <- c("code", "desc")
ipc2012.c.new <- tidy.ipc(ipc2012.c)
str(ipc2012.c.new)
ipc2012.c.final <- rbind(c("C", "Chemistry, metallurgy"), ipc2012.c.new)
str(ipc2012.c.final)
#View(ipc2012.c.final)

ipc2012.d <- read.xlsx("ipc2012_desc.xlsx", sheet = 4)
str(ipc2012.d)
names(ipc2012.d) <- c("code", "desc")
ipc2012.d.new <- tidy.ipc(ipc2012.d)
str(ipc2012.d.new)
ipc2012.d.final <- rbind(c("D", "Textiles, paper"), ipc2012.d.new)
str(ipc2012.d.final)
#View(ipc2012.d.final)

ipc2012.e <- read.xlsx("ipc2012_desc.xlsx", sheet = 5)
str(ipc2012.e)
names(ipc2012.e) <- c("code", "desc")
ipc2012.e.new <- tidy.ipc(ipc2012.e)
str(ipc2012.e.new)
ipc2012.e.final <- rbind(c("E", "Fixed constructions"), ipc2012.e.new)
str(ipc2012.e.final)
#View(ipc2012.e.final)

ipc2012.f <- read.xlsx("ipc2012_desc.xlsx", sheet = 6)
str(ipc2012.f)
names(ipc2012.f) <- c("code", "desc")
ipc2012.f.new <- tidy.ipc(ipc2012.f)
str(ipc2012.f.new)
ipc2012.f.final <- rbind(c("F", "Mechanical engineering, lighting, heating, weapons, blasting"), ipc2012.f.new)
str(ipc2012.f.final)
#View(ipc2012.f.final)

ipc2012.g <- read.xlsx("ipc2012_desc.xlsx", sheet = 7)
str(ipc2012.g)
names(ipc2012.g) <- c("code", "desc")
ipc2012.g.new <- tidy.ipc(ipc2012.g)
str(ipc2012.g.new)
ipc2012.g.final <- rbind(c("G", "Physics"), ipc2012.g.new)
str(ipc2012.g.final)
#View(ipc2012.g.final)

ipc2012.h <- read.xlsx("ipc2012_desc.xlsx", sheet = 8)
str(ipc2012.h)
names(ipc2012.h) <- c("code", "desc")
ipc2012.h.new <- tidy.ipc(ipc2012.h)
str(ipc2012.h.new)
ipc2012.h.final <- rbind(c("H", "Electricity"), ipc2012.h.new)
str(ipc2012.h.final)
#View(ipc2012.h.final)

ipc2012 <- rbind(ipc2012.a.final, ipc2012.b.final, ipc2012.c.final, ipc2012.d.final,
                 ipc2012.e.final, ipc2012.f.final, ipc2012.g.final, ipc2012.h.final)

#write.csv(ipc2012, "ipc2012.csv", row.names = F)

#(6) ipc 2013

ipc2013.a <- read.xlsx("ipc2013_desc.xlsx", sheet = 1)
str(ipc2013.a)
names(ipc2013.a) <- c("code", "desc")
ipc2013.a.new <- tidy.ipc(ipc2013.a)
str(ipc2013.a.new)
ipc2013.a.final <- rbind(c("A", "Human necessities"), ipc2013.a.new)
str(ipc2013.a.final)
#View(ipc2013.a.final)

ipc2013.b <- read.xlsx("ipc2013_desc.xlsx", sheet = 2)
str(ipc2013.b)
names(ipc2013.b) <- c("code", "desc")
ipc2013.b.new <- tidy.ipc(ipc2013.b)
str(ipc2013.b.new)
ipc2013.b.final <- rbind(c("B", "Performing operations, transporting"), ipc2013.b.new)
str(ipc2013.b.final)
#View(ipc2013.b.final)

ipc2013.c <- read.xlsx("ipc2013_desc.xlsx", sheet = 3)
str(ipc2013.c)
names(ipc2013.c) <- c("code", "desc")
ipc2013.c.new <- tidy.ipc(ipc2013.c)
str(ipc2013.c.new)
ipc2013.c.final <- rbind(c("C", "Chemistry, metallurgy"), ipc2013.c.new)
str(ipc2013.c.final)
#View(ipc2013.c.final)

ipc2013.d <- read.xlsx("ipc2013_desc.xlsx", sheet = 4)
str(ipc2013.d)
names(ipc2013.d) <- c("code", "desc")
ipc2013.d.new <- tidy.ipc(ipc2013.d)
str(ipc2013.d.new)
ipc2013.d.final <- rbind(c("D", "Textiles, paper"), ipc2013.d.new)
str(ipc2013.d.final)
#View(ipc2013.d.final)

ipc2013.e <- read.xlsx("ipc2013_desc.xlsx", sheet = 5)
str(ipc2013.e)
names(ipc2013.e) <- c("code", "desc")
ipc2013.e.new <- tidy.ipc(ipc2013.e)
str(ipc2013.e.new)
ipc2013.e.final <- rbind(c("E", "Fixed constructions"), ipc2013.e.new)
str(ipc2013.e.final)
#View(ipc2013.e.final)

ipc2013.f <- read.xlsx("ipc2013_desc.xlsx", sheet = 6)
str(ipc2013.f)
names(ipc2013.f) <- c("code", "desc")
ipc2013.f.new <- tidy.ipc(ipc2013.f)
str(ipc2013.f.new)
ipc2013.f.final <- rbind(c("F", "Mechanical engineering, lighting, heating, weapons, blasting"), ipc2013.f.new)
str(ipc2013.f.final)
#View(ipc2013.f.final)

ipc2013.g <- read.xlsx("ipc2013_desc.xlsx", sheet = 7)
str(ipc2013.g)
names(ipc2013.g) <- c("code", "desc")
ipc2013.g.new <- tidy.ipc(ipc2013.g)
str(ipc2013.g.new)
ipc2013.g.final <- rbind(c("G", "Physics"), ipc2013.g.new)
str(ipc2013.g.final)
#View(ipc2013.g.final)

ipc2013.h <- read.xlsx("ipc2013_desc.xlsx", sheet = 8)
str(ipc2013.h)
names(ipc2013.h) <- c("code", "desc")
ipc2013.h.new <- tidy.ipc(ipc2013.h)
str(ipc2013.h.new)
ipc2013.h.final <- rbind(c("H", "Electricity"), ipc2013.h.new)
str(ipc2013.h.final)
#View(ipc2013.h.final)

ipc2013 <- rbind(ipc2013.a.final, ipc2013.b.final, ipc2013.c.final, ipc2013.d.final,
                 ipc2013.e.final, ipc2013.f.final, ipc2013.g.final, ipc2013.h.final)

#write.csv(ipc2013, "ipc2013.csv", row.names = F)

#(7) ipc 2014

ipc2014.a <- read.xlsx("ipc2014_desc.xlsx", sheet = 1)
str(ipc2014.a)
names(ipc2014.a) <- c("code", "desc")
ipc2014.a.new <- tidy.ipc(ipc2014.a)
str(ipc2014.a.new)
ipc2014.a.final <- rbind(c("A", "Human necessities"), ipc2014.a.new)
str(ipc2014.a.final)
#View(ipc2014.a.final)

ipc2014.b <- read.xlsx("ipc2014_desc.xlsx", sheet = 2)
str(ipc2014.b)
names(ipc2014.b) <- c("code", "desc")
ipc2014.b.new <- tidy.ipc(ipc2014.b)
str(ipc2014.b.new)
ipc2014.b.final <- rbind(c("B", "Performing operations, transporting"), ipc2014.b.new)
str(ipc2014.b.final)
#View(ipc2014.b.final)

ipc2014.c <- read.xlsx("ipc2014_desc.xlsx", sheet = 3)
str(ipc2014.c)
names(ipc2014.c) <- c("code", "desc")
ipc2014.c.new <- tidy.ipc(ipc2014.c)
str(ipc2014.c.new)
ipc2014.c.final <- rbind(c("C", "Chemistry, metallurgy"), ipc2014.c.new)
str(ipc2014.c.final)
#View(ipc2014.c.final)

ipc2014.d <- read.xlsx("ipc2014_desc.xlsx", sheet = 4)
str(ipc2014.d)
names(ipc2014.d) <- c("code", "desc")
ipc2014.d.new <- tidy.ipc(ipc2014.d)
str(ipc2014.d.new)
ipc2014.d.final <- rbind(c("D", "Textiles, paper"), ipc2014.d.new)
str(ipc2014.d.final)
#View(ipc2014.d.final)

ipc2014.e <- read.xlsx("ipc2014_desc.xlsx", sheet = 5)
str(ipc2014.e)
names(ipc2014.e) <- c("code", "desc")
ipc2014.e.new <- tidy.ipc(ipc2014.e)
str(ipc2014.e.new)
ipc2014.e.final <- rbind(c("E", "Fixed constructions"), ipc2014.e.new)
str(ipc2014.e.final)
#View(ipc2014.e.final)

ipc2014.f <- read.xlsx("ipc2014_desc.xlsx", sheet = 6)
str(ipc2014.f)
names(ipc2014.f) <- c("code", "desc")
ipc2014.f.new <- tidy.ipc(ipc2014.f)
str(ipc2014.f.new)
ipc2014.f.final <- rbind(c("F", "Mechanical engineering, lighting, heating, weapons, blasting"), ipc2014.f.new)
str(ipc2014.f.final)
#View(ipc2014.f.final)

ipc2014.g <- read.xlsx("ipc2014_desc.xlsx", sheet = 7)
str(ipc2014.g)
names(ipc2014.g) <- c("code", "desc")
ipc2014.g.new <- tidy.ipc(ipc2014.g)
str(ipc2014.g.new)
ipc2014.g.final <- rbind(c("G", "Physics"), ipc2014.g.new)
str(ipc2014.g.final)
#View(ipc2014.g.final)

ipc2014.h <- read.xlsx("ipc2014_desc.xlsx", sheet = 8)
str(ipc2014.h)
names(ipc2014.h) <- c("code", "desc")
ipc2014.h.new <- tidy.ipc(ipc2014.h)
str(ipc2014.h.new)
ipc2014.h.final <- rbind(c("H", "Electricity"), ipc2014.h.new)
str(ipc2014.h.final)
#View(ipc2014.h.final)

ipc2014 <- rbind(ipc2014.a.final, ipc2014.b.final, ipc2014.c.final, ipc2014.d.final,
                 ipc2014.e.final, ipc2014.f.final, ipc2014.g.final, ipc2014.h.final)

#write.csv(ipc2014, "ipc2014.csv", row.names = F)

#2. make tidy : code

#(1) ipc 2008

ipc2008$subclass <- substr(ipc2008$code, 1, 4)

temp.group <- substr(ipc2008$code, 5, 8)
#remove leading zeros
#https://stackoverflow.com/questions/49186893/remove-leading-0s-with-stringr-in-r
temp.group.new <- sub("^0+", "", temp.group)     

temp.subgroup.1 <- substr(ipc2008$code, 9, 10) 
temp.subgroup.2 <- substr(ipc2008$code, 11, 14) 
#remove trailing zeros
#https://statisticsglobe.com/remove-leading-and-trailing-zeros-in-r
temp.subgroup.2.new <- sub("0+$", "", temp.subgroup.2)  
temp.subgroup.new <- paste(temp.subgroup.1, temp.subgroup.2.new, sep = "")

ipc2008$group <- paste(temp.group.new, temp.subgroup.new, sep = "/")

#https://www.delftstack.com/howto/r/remove-last-character-in-r/
ipc2008 <- ipc2008 %>% select(-code) %>% mutate(code = paste(subclass, group, sep = " "))
ipc2008$code[ipc2008$group == "/"] <- gsub('.{2}$', '', ipc2008$code[ipc2008$group == "/"])

ipc2008.new <- ipc2008 %>% select(-subclass, -group) %>% select(code, desc)

#rename
ipc2008_desc <- ipc2008.new

save(ipc2008_desc, file = "ipc2008_desc.RData")
#write.csv(ipc2008_desc, "ipc2008 (clean).csv", row.names = F)

#(2) ipc 2009

ipc2009$subclass <- substr(ipc2009$code, 1, 4)

temp.group <- substr(ipc2009$code, 5, 8)
#remove leading zeros
#https://stackoverflow.com/questions/49186893/remove-leading-0s-with-stringr-in-r
temp.group.new <- sub("^0+", "", temp.group)     

temp.subgroup.1 <- substr(ipc2009$code, 9, 10) 
temp.subgroup.2 <- substr(ipc2009$code, 11, 14) 
#remove trailing zeros
#https://statisticsglobe.com/remove-leading-and-trailing-zeros-in-r
temp.subgroup.2.new <- sub("0+$", "", temp.subgroup.2)  
temp.subgroup.new <- paste(temp.subgroup.1, temp.subgroup.2.new, sep = "")

ipc2009$group <- paste(temp.group.new, temp.subgroup.new, sep = "/")

#https://www.delftstack.com/howto/r/remove-last-character-in-r/
ipc2009 <- ipc2009 %>% select(-code) %>% mutate(code = paste(subclass, group, sep = " "))
ipc2009$code[ipc2009$group == "/"] <- gsub('.{2}$', '', ipc2009$code[ipc2009$group == "/"])

ipc2009.new <- ipc2009 %>% select(-subclass, -group) %>% select(code, desc)

#rename
ipc2009_desc <- ipc2009.new

save(ipc2009_desc, file = "ipc2009_desc.RData")
#write.csv(ipc2009_desc, "ipc2009 (clean).csv", row.names = F)

#(3) ipc 2010

ipc2010$subclass <- substr(ipc2010$code, 1, 4)

temp.group <- substr(ipc2010$code, 5, 8)
#remove leading zeros
#https://stackoverflow.com/questions/49186893/remove-leading-0s-with-stringr-in-r
temp.group.new <- sub("^0+", "", temp.group)     

temp.subgroup.1 <- substr(ipc2010$code, 9, 10) 
temp.subgroup.2 <- substr(ipc2010$code, 11, 14) 
#remove trailing zeros
#https://statisticsglobe.com/remove-leading-and-trailing-zeros-in-r
temp.subgroup.2.new <- sub("0+$", "", temp.subgroup.2)  
temp.subgroup.new <- paste(temp.subgroup.1, temp.subgroup.2.new, sep = "")

ipc2010$group <- paste(temp.group.new, temp.subgroup.new, sep = "/")

#https://www.delftstack.com/howto/r/remove-last-character-in-r/
ipc2010 <- ipc2010 %>% select(-code) %>% mutate(code = paste(subclass, group, sep = " "))
ipc2010$code[ipc2010$group == "/"] <- gsub('.{2}$', '', ipc2010$code[ipc2010$group == "/"])

ipc2010.new <- ipc2010 %>% select(-subclass, -group) %>% select(code, desc)

#rename
ipc2010_desc <- ipc2010.new

save(ipc2010_desc, file = "ipc2010_desc.RData")
#write.csv(ipc2010_desc, "ipc2010 (clean).csv", row.names = F)

#(4) ipc 2011

ipc2011$subclass <- substr(ipc2011$code, 1, 4)

temp.group <- substr(ipc2011$code, 5, 8)
#remove leading zeros
#https://stackoverflow.com/questions/49186893/remove-leading-0s-with-stringr-in-r
temp.group.new <- sub("^0+", "", temp.group)     

temp.subgroup.1 <- substr(ipc2011$code, 9, 10) 
temp.subgroup.2 <- substr(ipc2011$code, 11, 14) 
#remove trailing zeros
#https://statisticsglobe.com/remove-leading-and-trailing-zeros-in-r
temp.subgroup.2.new <- sub("0+$", "", temp.subgroup.2)  
temp.subgroup.new <- paste(temp.subgroup.1, temp.subgroup.2.new, sep = "")

ipc2011$group <- paste(temp.group.new, temp.subgroup.new, sep = "/")

#https://www.delftstack.com/howto/r/remove-last-character-in-r/
ipc2011 <- ipc2011 %>% select(-code) %>% mutate(code = paste(subclass, group, sep = " "))
ipc2011$code[ipc2011$group == "/"] <- gsub('.{2}$', '', ipc2011$code[ipc2011$group == "/"])

ipc2011.new <- ipc2011 %>% select(-subclass, -group) %>% select(code, desc)

#rename
ipc2011_desc <- ipc2011.new

save(ipc2011_desc, file = "ipc2011_desc.RData")
#write.csv(ipc2011_desc, "ipc2011 (clean).csv", row.names = F)

#(5) ipc 2012

ipc2012$subclass <- substr(ipc2012$code, 1, 4)

temp.group <- substr(ipc2012$code, 5, 8)
#remove leading zeros
#https://stackoverflow.com/questions/49186893/remove-leading-0s-with-stringr-in-r
temp.group.new <- sub("^0+", "", temp.group)     

temp.subgroup.1 <- substr(ipc2012$code, 9, 10) 
temp.subgroup.2 <- substr(ipc2012$code, 11, 14) 
#remove trailing zeros
#https://statisticsglobe.com/remove-leading-and-trailing-zeros-in-r
temp.subgroup.2.new <- sub("0+$", "", temp.subgroup.2)  
temp.subgroup.new <- paste(temp.subgroup.1, temp.subgroup.2.new, sep = "")

ipc2012$group <- paste(temp.group.new, temp.subgroup.new, sep = "/")

#https://www.delftstack.com/howto/r/remove-last-character-in-r/
ipc2012 <- ipc2012 %>% select(-code) %>% mutate(code = paste(subclass, group, sep = " "))
ipc2012$code[ipc2012$group == "/"] <- gsub('.{2}$', '', ipc2012$code[ipc2012$group == "/"])

ipc2012.new <- ipc2012 %>% select(-subclass, -group) %>% select(code, desc)

#rename
ipc2012_desc <- ipc2012.new

save(ipc2012_desc, file = "ipc2012_desc.RData")
#write.csv(ipc2012_desc, "ipc2012 (clean).csv", row.names = F)

#(6) ipc 2013

ipc2013$subclass <- substr(ipc2013$code, 1, 4)

temp.group <- substr(ipc2013$code, 5, 8)
#remove leading zeros
#https://stackoverflow.com/questions/49186893/remove-leading-0s-with-stringr-in-r
temp.group.new <- sub("^0+", "", temp.group)     

temp.subgroup.1 <- substr(ipc2013$code, 9, 10) 
temp.subgroup.2 <- substr(ipc2013$code, 11, 14) 
#remove trailing zeros
#https://statisticsglobe.com/remove-leading-and-trailing-zeros-in-r
temp.subgroup.2.new <- sub("0+$", "", temp.subgroup.2)  
temp.subgroup.new <- paste(temp.subgroup.1, temp.subgroup.2.new, sep = "")

ipc2013$group <- paste(temp.group.new, temp.subgroup.new, sep = "/")

#https://www.delftstack.com/howto/r/remove-last-character-in-r/
ipc2013 <- ipc2013 %>% select(-code) %>% mutate(code = paste(subclass, group, sep = " "))
ipc2013$code[ipc2013$group == "/"] <- gsub('.{2}$', '', ipc2013$code[ipc2013$group == "/"])

ipc2013.new <- ipc2013 %>% select(-subclass, -group) %>% select(code, desc)

#rename
ipc2013_desc <- ipc2013.new

save(ipc2013_desc, file = "ipc2013_desc.RData")
#write.csv(ipc2013_desc, "ipc2013 (clean).csv", row.names = F)

#(7) ipc 2014

ipc2014$subclass <- substr(ipc2014$code, 1, 4)

temp.group <- substr(ipc2014$code, 5, 8)
#remove leading zeros
#https://stackoverflow.com/questions/49186893/remove-leading-0s-with-stringr-in-r
temp.group.new <- sub("^0+", "", temp.group)     

temp.subgroup.1 <- substr(ipc2014$code, 9, 10) 
temp.subgroup.2 <- substr(ipc2014$code, 11, 14) 
#remove trailing zeros
#https://statisticsglobe.com/remove-leading-and-trailing-zeros-in-r
temp.subgroup.2.new <- sub("0+$", "", temp.subgroup.2)  
temp.subgroup.new <- paste(temp.subgroup.1, temp.subgroup.2.new, sep = "")

ipc2014$group <- paste(temp.group.new, temp.subgroup.new, sep = "/")

#https://www.delftstack.com/howto/r/remove-last-character-in-r/
ipc2014 <- ipc2014 %>% select(-code) %>% mutate(code = paste(subclass, group, sep = " "))
ipc2014$code[ipc2014$group == "/"] <- gsub('.{2}$', '', ipc2014$code[ipc2014$group == "/"])

ipc2014.new <- ipc2014 %>% select(-subclass, -group) %>% select(code, desc)

#rename
ipc2014_desc <- ipc2014.new

save(ipc2014_desc, file = "ipc2014_desc.RData")
#write.csv(ipc2014_desc, "ipc2014 (clean).csv", row.names = F)