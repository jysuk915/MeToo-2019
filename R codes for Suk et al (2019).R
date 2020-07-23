library(data.table)
library(dplyr)
library(stringr)
library(spacyr)
library(ggplot2)
df <- fread("MeToo SSCR ver.csv")
df$doc_id <- c(1:nrow(df))
df$tweet_all <- with(df, ifelse(df$retweet.id == "\\N", tweet, retweet))

tweet <- df %>% select(doc_id, tweet.id, created.at, tweet_all)
names(tweet)[4] <- "text"

tweet$clean <- gsub("(f|ht)(tp)(s?)(://)(\\S*)", "", tweet$text) #remove urls
tweet$clean <- gsub("(f|ht)(tp)(s?)(\\S*)", "", tweet$clean) #remove urls2
tweet$clean <- gsub("rt @\\w+: ", "", tweet$clean) #remove rt @
#tweet$clean <- gsub("@\\w+", "", tweet$clean) #remove @mentions
tweet$clean <- gsub("<U\\+\\w+>\\s*","", tweet$clean) #remove emojis
tweet$clean <- gsub("[^0-9A-Za-z///' ]", "", tweet$clean) #remove signs/hashtags/etc
tweet$clean <- tolower(tweet$clean)
#tweet$clean <- subset(tweet$clean, !tweet2 == "")

spacy_initialize()
start <- Sys.time()
tw_all_result_v2 <- spacy_parse(tweet$clean, dependency = TRUE, lemma = TRUE, tag = TRUE)
end <- Sys.time()
timetaken <- end - start
write.csv(tw_all_result_v2, "tw_result_all_v2.csv", row.names=FALSE)


########Settings
tw_all_result <- fread("tw_result_all_v2.csv")
tw_all_result$doc_id <- gsub("text", "", tw_all_result$doc_id)
tw_all_result$doc_id <- as.numeric(tw_all_result$doc_id)

df$tweet_full <- with(df, ifelse(df$retweet.id == "\\N", tweet_all, paste0("rt @", df$retweet.user.handle, ": ", tweet_all)))
df_trim <- df %>% select(doc_id, tweet_full)
df_trim$tweet_full <- tolower(df_trim$tweet_full)

tw_token_tag <- tw_all_result %>% select(doc_id, sentence_id, token_id, token, pos, tag, head_token_id, dep_rel) %>% 
        mutate(token_tag = paste0(token, "//", tag))

tw_tag_all <- tw_token_tag %>% group_by(doc_id) %>% summarise(tag_all = paste0(token_tag, collapse = " ")) %>% inner_join(df_trim)


backlash <- c("aziz ansari", "#azizansari", "fake", "false", "#fake", "#false", "#feminismiscancer", "#fakecases", "#fakefeminism", "#fakefeminist")

firstPRP2 <- tw_token_tag %>% filter(token=="i"|token=="me"|token=="my"|token=="mine"|token=="myself") %>%
        group_by(doc_id) %>% summarise(tag_all = paste0(token_tag, collapse = " ")) %>% inner_join(df_trim)
write.csv(firstPRP2, "metoo output/firstPRP.csv", row.names = FALSE)
firstPRP2 <- fread("firstPRP.csv")

#::::::::::::::::::: NA :::::::::::::::::::::::::#
#######1) A/H/A/R (past/past participle/-er) + FPP Singular 
ahar_vb <- tw_token_tag %>% filter(token=="assaulted"|token=="assaulter"|token=="assaulters"|
                                           token=="harassed"|token=="harasser"|token=="harassers"|
                                           token=="abused"|token=="abuser"|token=="abusers"|
                                           token=="raped"|token=="rapist"|token=="rapists") %>% 
        left_join(df_trim) %>% select(doc_id, token_tag, tweet_full)
ahar_vbFPP <- inner_join(ahar_vb, firstPRP2, by="doc_id") %>% select(doc_id, tag_all, token_tag, tweet_full.x)

ahar_vbFPP <- ahar_vbFPP %>% select(doc_id, tweet_full.x)
na_aharvbFPP_unique <- ahar_vbFPP[!duplicated(ahar_vbFPP$doc_id),]
names(na_aharvbFPP_unique)[2] <- "tweet_full" 

#######2) a bag of words/hashtags
na_bow <- c("#heforshe", "#ibelieveyou", "#believewomen", "#believesurvivors", "#wethepeople", "#standup", 
            "#ihearyou", "#hertoo", "#yesallwomen", "#solidarity", "#survivor", "#supportsurvivors", 
            "#notokay","#ustoo", "#ptsd", 
            "speak up", "speak out", "stand with", "stand for",
            "ptsd")

na_wordhash = df_trim %>% filter(str_detect(tweet_full, paste(na_bow, collapse="|")))


#######3) word combinations using dependencies information
stats <- merge(tw_all_result, tw_all_result, 
               by.x = c("doc_id", "sentence_id", "head_token_id"),
               by.y = c("doc_id", "sentence_id", "token_id"),
               all.x = TRUE, all.y = FALSE, 
               suffixes = c("", "_parent"), sort = FALSE)

na_stat <- subset(stats, dep_rel %in% "dobj" & token_parent %in% c("believe"))
na_stat$term <- paste(na_stat$token_parent, na_stat$token, sep = " ")
na1 <- as.data.frame(table(na_stat$term)) %>% arrange(desc(Freq))
na_stat1 <- subset(stats, dep_rel %in% "dobj" & token_parent %in% c("believe") & token %in% c("woman", "you", "women", "victims", "survivors"))
na_believe <- df_trim[(na_stat1$doc_id),]

na_stat2 <- subset(stats, dep_rel %in% "dobj" & token_parent %in% c("support"))
na_stat2$term <- paste(na_stat2$token_parent, na_stat2$token, sep = " ")
na2 <- as.data.frame(table(na_stat2$term)) %>% arrange(desc(Freq))
na_stat2 <- subset(stats, dep_rel %in% "dobj" & token_parent %in% c("support") & token %in% c("women", "survivors", "you", "victims", "woman"))
na_support <- df_trim[(na_stat2$doc_id),]

na_stat3 <- subset(stats, dep_rel %in% c("dobj") & token %in% c("story", "stories"))
na_stat3$term <- paste(na_stat3$token_parent, na_stat3$token, sep = " ")
na3 <- as.data.frame(table(na_stat3$term)) %>% arrange(desc(Freq))
na_stat3 <- subset(stats, dep_rel %in% "dobj" & token_parent %in% c("share", "sharing") & token %in% c("story", "stories"))
na_share <- df_trim[(na_stat3$doc_id),]

na_stat4 <- subset(stats, dep_rel %in% c("poss") & token_parent %in% c("story", "stories"))
na_stat4$term <- paste(na_stat4$token, na_stat4$token_parent, sep = " ")
na4 <- as.data.frame(table(na_stat4$term)) %>% arrange(desc(Freq))
na_stat4 <- subset(stats, dep_rel %in% "poss" & token_parent %in% c("stories") & token %in% c("my","our"))
na_mystory <- df_trim[(na_stat4$doc_id),]

na_stat4_2 <- subset(stats, dep_rel %in% "poss" & token_parent %in% c("story") & token %in% c("my"))
na_mystory2 <- df_trim[(na_stat4_2$doc_id),]

na_stat5 <- subset(stats, dep_rel %in% c("dobj") & token_parent %in% c("hear"))
na_stat5$term <- paste(na_stat5$token_parent, na_stat5$token, sep = " ")
na5 <- as.data.frame(table(na_stat5$term)) %>% arrange(desc(Freq))
na_stat5 <- subset(stats, dep_rel %in% "dobj" & token_parent %in% c("hear") & token %in% c("you"))
na_hear <- df_trim[(na_stat5$doc_id),]


na_bigram <- rbind(na_believe, na_support)
na_bigram <- rbind(na_bigram, na_mystory)
na_bigram <- rbind(na_bigram, na_share)
na_bigram <- rbind(na_bigram, na_mystory2)
na_bigram <- rbind(na_bigram, na_hear)

####### Compilation
na_final0 <- rbind(na_aharvbFPP_unique, na_wordhash)
na_final <- rbind(na_final0, na_bigram)
na_final <- na_final %>% filter(!str_detect(tweet_full, "#timesup|@timesupnow|@timesupnw")) %>% 
        filter(!str_detect(tweet_full, paste(backlash, collapse="|")))
na_final <- na_final[!duplicated(na_final$doc_id),]

#::::::::::::::::::: Activism :::::::::::::::::::::::::#
act <- c("#himthough", "#howiwillchange", "#mosquemetoo", "#metook12", "#churchtoo", "#metoomilitary", 
         "#metoowhatnext", "#metoocongress", "#timeisnow", "#itstime", "#enoughisenough", "#stoprape", 
         "#metoophd", "#metoomarch", "#metoonatsec", "#metoofgm", "#metoomedicine", "#neveragain", "#nomore", "#rapeculture")

activism <- df_trim %>% filter(str_detect(tweet_full, paste(act, collapse="|")))


stats_aux_resign <- subset(stats, dep_rel %in% "aux" & token %in% c("must", "should") & token_parent %in% c("resign")) 
aux_resign <- df_trim[(stats_aux_resign$doc_id),]

stats_aux_end <- subset(stats, dep_rel %in% "aux" & token %in% c("must", "can") & token_parent %in% c("end")) 
aux_end <- df_trim[(stats_aux_end$doc_id),]

stats_aux_stop <- subset(stats, dep_rel %in% "aux" & token %in% c("must", "should", "can") & token_parent %in% c("stop"))
aux_stop <- df_trim[(stats_aux_stop$doc_id),]

stats_aux_inv <- subset(stats, dep_rel %in% "aux" & token %in% c("must", "should") & token_parent %in% c("investigate"))
aux_inv <- df_trim[(stats_aux_inv$doc_id),]

stats_aux_ch <- subset(stats, dep_rel %in% "aux" & token %in% c("must", "should", "can") & token_parent %in% c("change"))
aux_ch <- df_trim[(stats_aux_ch$doc_id),]

hasto <- df_trim %>% filter(str_detect(tweet_full, "has to stop|have to stop|has to end|have to end|has to resign|have to resign|has to investigate|
                                       have to investigate|has to change|have to change|have to join|has to join"))

aux_comb <- rbind(aux_resign, aux_end)
aux_comb <- rbind(aux_comb, aux_stop)
aux_comb <- rbind(aux_comb, aux_inv)
aux_comb <- rbind(aux_comb, aux_ch)
aux_comb <- rbind(aux_comb, hasto)


act_stat1 <- subset(stats, dep_rel %in% "dobj" & token_parent %in% c("stop", "end", "change", "join", "investigate", "resign"))
act_stat1$term <- paste(act_stat1$token_parent,act_stat1$token, sep = " ")
act <- as.data.frame(table(act_stat1$term)) %>% arrange(desc(Freq))

act_stat1 <- subset(stats, dep_rel %in% "dobj" & token_parent %in% c("stop") & token %in% c("assault", "assaults", "harassment", "harassments", "violence", "abuse", "abuses","rape","rapes", "exploitation"))
act_vb1 <- df_trim[(act_stat1$doc_id),]

act_stat2 <- subset(stats, dep_rel %in% "dobj" & token_parent %in% c("end") & token %in% c("assault", "assaults", "harassment", "harassments", "violence", "abuse", "rape", "abuses","rapes", "culture", "discrimination", "oppression", 
                                                                                           "misconduct"))
act_vb2 <- df_trim[(act_stat2$doc_id),]

act_stat3 <- subset(stats, dep_rel %in% "dobj" & token_parent %in% c("change") & token %in% c("culture", "epidemic", "behavior", "policies", "laws"))
act_vb3 <- df_trim[(act_stat3$doc_id),]

act_stat4 <- subset(stats, dep_rel %in% "dobj" & token_parent %in% c("join") & token %in% c("us", "me", "movement", "march","campaign", "protest", "fight"))
act_vb4 <- df_trim[(act_stat4$doc_id),]

act_stat5 <- subset(stats, dep_rel %in% "dobj" & token_parent %in% c("investigate") & token %in% c("allegations", "claims", "charges", "assault", "harassment", "crimes", "accusations", "assaults", "abuse"))
act_vb5 <- df_trim[(act_stat5$doc_id),]


act_vb <- rbind(act_vb1, act_vb2)
act_vb <- rbind(act_vb, act_vb3)
act_vb <- rbind(act_vb, act_vb4)
act_vb <- rbind(act_vb, act_vb5)

###### Compilation
act_final0 <- rbind(activism, aux_comb)
act_final <- rbind(act_final0, act_vb)
act_final <- act_final %>% filter(!str_detect(tweet_full, paste(backlash, collapse="|")))
act_final <- act_final[!duplicated(act_final$doc_id),]


####### Final tuning
act_docid <- act_final$doc_id
na_final <- na_final %>% filter(!doc_id %in% act_docid) 

write.csv(na_final, "na_final0417.csv", row.names = FALSE)
write.csv(act_final, "act_final0417.csv", row.names = FALSE)
