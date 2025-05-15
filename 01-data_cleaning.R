## Clean data
library(tidyverse)
library(tidytext)
library(tm) 
library(topicmodels)
library(lda)
library(tictoc)
library(fs)


### upload csv of papers for screening 2025
dat <- read_csv("data/p25.csv") 
str(dat)

### Filter for 'includes'
dat <- dat %>%
  mutate(
    decision = as_factor(decision),
    topicdecision = as_factor(topicdecision),
    decision = str_to_lower(decision)
  ) %>% 
  filter(decision == "include")

### upload latest 'all data' file
alldat <- read.csv("data/data_all_Mar16_2023.csv") %>% 
  as_tibble()
alldat

### remove duplicate titles in papers for screening 2025 
# uniquedat <- (dat[!duplicated(dat$id), ] ) #title old 
# rm(uniquedat)
## I believe uniquedat is incorrect because it removes completely the duplicates/Juan

dat %>% 
  group_by(id) %>% 
  summarize(n=n())

dat %>% 
  as_tibble() %>% 
  select(-starts_with("old")) %>% 
  unique() %>% 
  group_by(id) %>% 
  summarize(n=n()) %>% 
  arrange(desc(n))

# teh unique id's of included 
ids <- dat %>% pull(id) %>% unique()


df_tm <- alldat %>% 
  select(id, title, abstract, index_keywords, author_keywords) %>% 
  filter(id %in% ids) %>% # filder for the included files 
  as_tibble()

df_tm %>% group_by(id) %>% 
  summarize(n=n()) %>% 
  arrange(desc(n)) # test pass, no duplicates

write_csv(df_tm, file = "data_topic_model_round_2.csv")


names(alldat)[names(alldat) %in% names(df_tm)]

df_cleaned <- df_tm %>% 
  left_join(
    alldat, by = c("title", "id", "abstract", "index_keywords", "author_keywords")) %>% 
  as_tibble()


write_csv(df_cleaned, file = "data_all_Apr2_2025_included_papers.csv")


# print (paste0("nrow(uniquedat) = ", nrow(uniquedat))) #5417
# 
# ### Join dat and alldat
# joined <- left_join(uniquedat, alldat, by = "title")
# print (paste0("nrow(joined) = ", nrow(joined))) #5513
# write.csv(joined, "joined.csv")
# 
# ### identify duplicate IDs in joined file
# joined$id.x <- factor(joined$id.x)
# dup <- joined[duplicated(joined$id.x), ] 
# print (paste0("nrow(dup) = ",  (nrow(dup))))
# write.csv(dup, "duplicateIDs.csv") # export for Tom to check
# 
# ## FIX DUPLICATES USING TOM'S LIST
# print (paste0("nrow(joined) = ",  nrow(joined)))
# del <- read_csv("DuplicatesDelete.csv")
# print (paste0("nrow(del) = ",  nrow(del)))
# 
# joined2 <- joined %>% 
#   anti_join(del, by = "id.y")
# print (paste0("nrow(joined2) = ",  nrow(joined2)))
# 
# ### Filter subset of columns to keep & export
# dataall0425 <- joined2 %>% select(id.x, title, year.x, doi.x, topic, gamma, 
#                                   id.y, atk, tk, abstract, authors, author_s_id, 
#                                   source_title, affiliations, 
#                                   authors_with_affiliations, cited_by, 
#                                   correspondence_address, index_keywords, 
#                                   author_keywords, document_type, volume, issue, 
#                                   page_start, page_end, editors, publisher, issn, 
#                                   isbn)
# write.csv(dataall0425, "dataall0425.csv")

