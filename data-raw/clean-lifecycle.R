library(tidyverse) 
library(foreign)

load("uspc2012_naics2002.RData")
grant <- read.dta("patsic06_mar09_ipc.dta") #unit : patent applicants/owners
cite <- read.dta("cite76_06.dta") #unit : patents

grant.citing <- grant %>% select(citing = patent, appyearciting = appyear, gyearciting = gyear, nclassciting = nclass)
grant.citing <- grant.citing[order(grant.citing$citing), ]

grant.cited <- grant %>% select(cited = patent, appyearcited = appyear, gyearcited = gyear, nclasscited = nclass)
grant.cited <- grant.cited[order(grant.cited$cited), ]

cite <- cite[order(cite$cited), ]
cite.1 <- left_join(cite, grant.cited, by = "cited")
#nrow(cite.1) > nrow(cite) : because one patent might be cited by more than one patent applicant
cite.2 <- left_join(cite.1, grant.citing, by = "citing")

cite.final <- cite.2 %>% select(-ncites7606, -appyearcited, -gyearciting, -nclassciting) %>% filter(gyearcited < 1991) %>% mutate(citelag = appyearciting - gyearcited) 
cite.last <- cite.final %>% group_by(nclasscited) %>% summarise(AverageCiteLag = mean(citelag, na.rm = T), Pct75CiteLag = quantile(citelag, probs = c(0.75), na.rm = T), Pct85CiteLag = quantile(citelag, probs = c(0.85), na.rm = T))
cite.last$nclasscited <- str_pad(cite.last$nclasscited, 3, pad = "0")
names(cite.last)[1] <- "uspc_class"

concord.uspc.naics <- uspc2012_naics2002 %>% filter(!is.na(NAICS2002_2d) & !is.na(NAICS2002_3d) & !is.na(NAICS2002_4d))
lifecycle <- left_join(cite.last, concord.uspc.naics, by = "uspc_class") %>% filter(!is.na(AverageCiteLag) & !is.na(Pct75CiteLag) & !is.na(Pct85CiteLag)) %>% filter(!is.na(NAICS2002_2d)) %>% select(uspc_class, uspc_subclass, NAICS2002_4d, NAICS2002_3d, NAICS2002_2d, AverageCiteLag, Pct75CiteLag, Pct85CiteLag)
lifecycle_NAICS2002_2d <- lifecycle %>% group_by(NAICS2002_2d) %>% summarise(MeanACL = mean(AverageCiteLag, na.rm = T), MeanP75CL = mean(Pct75CiteLag, na.rm = T), MeanP85CL = mean(Pct85CiteLag, na.rm = T))
lifecycle_NAICS2002_3d <- lifecycle %>% group_by(NAICS2002_3d) %>% summarise(MeanACL = mean(AverageCiteLag, na.rm = T), MeanP75CL = mean(Pct75CiteLag, na.rm = T), MeanP85CL = mean(Pct85CiteLag, na.rm = T))
lifecycle_NAICS2002_4d <- lifecycle %>% group_by(NAICS2002_4d) %>% summarise(MeanACL = mean(AverageCiteLag, na.rm = T), MeanP75CL = mean(Pct75CiteLag, na.rm = T), MeanP85CL = mean(Pct85CiteLag, na.rm = T))
lifecycle_USPC2012_class <- lifecycle %>% group_by(uspc_class) %>% summarise(MeanACL = mean(AverageCiteLag, na.rm = T), MeanP75CL = mean(Pct75CiteLag, na.rm = T), MeanP85CL = mean(Pct85CiteLag, na.rm = T))
lifecycle_USPC2012_subclass <- lifecycle %>% group_by(uspc_subclass) %>% summarise(MeanACL = mean(AverageCiteLag, na.rm = T), MeanP75CL = mean(Pct75CiteLag, na.rm = T), MeanP85CL = mean(Pct85CiteLag, na.rm = T))

names(lifecycle_NAICS2002_2d)[1] <- "source"
names(lifecycle_NAICS2002_3d)[1] <- "source"
names(lifecycle_NAICS2002_4d)[1] <- "source"
names(lifecycle_USPC2012_class)[1] <- "source"
names(lifecycle_USPC2012_subclass)[1] <- "source"

save(lifecycle_NAICS2002_2d, file = "lifecycle_NAICS2002_2d.RData")
save(lifecycle_NAICS2002_3d, file = "lifecycle_NAICS2002_3d.RData")
save(lifecycle_NAICS2002_4d, file = "lifecycle_NAICS2002_4d.RData")
save(lifecycle_USPC2012_class, file = "lifecycle_USPC2012_class.RData")
save(lifecycle_USPC2012_subclass, file = "lifecycle_USPC2012_subclass.RData")