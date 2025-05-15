library(tidyverse)
library(tidytext)
library(tm)
library (topicmodels)
library(lda)
library(tictoc)
library(network)
library(patchwork)
library(fs)

# Here we use the object df_tm created in script Juan Emilie
df_tm <- read_csv("data/data_topic_model_round_2.csv")

df_tm

## clean data: extra words
too_words <- tibble(
  word = c("john", "wiley", "sons", "springer", "verlag", "b.v", "abstract", "press", # 2021 terms 
           "reserved", "rights", "author", "taylor", "francis", "elsevier", "i.e.", 
           "e.g.", "publisher", "publishers", "published", "publishing", "ii", "iv", 
           "mdpi", "copyright", "journal", "authors", "blackwell", "oxford", "cambridge", 
           "publisher", "university", "book", "volume", "gmbh", # added new below here March 1st 2023 
           "e.g", "i.e", "e .g", "i .e", "e. g", "i. e", # we found these versions in our topics so removing
           "paper", "study", "aim", "aims", "objective", "objectives", "purpose", # from panarchy paper
           "na", "NA", "type", "result", "results",   # Added by behaviors team 2023 
           "finding","findings", "research", "report", "reports", "synthesis" ) # Added by behaviors team 2023
)

stop_words_sp <- tibble(word = tm::stopwords("spanish"), lexicon = "custom")

# create document text matrix:
tic()
dtm <- df_tm %>% 
  select(abstract, title, id) %>% 
  unnest_tokens(word, abstract) %>% 
  filter(!is.na(word)) %>% 
  anti_join(stop_words) %>% 
  anti_join(too_words) %>%
  anti_join(stop_words_sp) %>% # remove spanish stop words
  filter(!str_detect(word, "[:digit:]")) %>%
  mutate(word = textstem::lemmatize_words(word)) %>% 
  group_by(id) %>%  
  dplyr::count(word , sort = TRUE ) %>%
  cast_dtm(document = id, term = word, value = n)  
toc() #5s

## Choosing best algorithm
SEED <- 2021
k <- 10 # using higher k values is possible, but using eg 100 does not work for some algorithms

tic()
tset.TM <- list (
  VEM0 = LDA(dtm, k=k, control = list ( seed = SEED)),
  VEM_fixed= LDA(dtm, k=k, control= list (estimate.alpha = F, seed = SEED)),
  Gibbs = LDA (dtm, k=k, method ="Gibbs", control = list (
    seed = SEED, burnin= 1000, thin = 100, iter= 1000)),
  CTM = CTM (dtm, k=k, control = list(seed = SEED, var= list (tol= 10^-4), em= list (tol = 10^-3))))
toc() #  153s

# What algorithm is better?
sapply (tset.TM[1:3], slot, "alpha")

#### Finding number of topics
k <- c(25, 50, 100, 250, 500)

tic()
topicNumber.TM <- map(
  .x = k,
  .f = function(x) {
    LDA(dtm, k = x, control= list (seed = SEED), method = "Gibbs")
  })
toc() # 

save(tset.TM, topicNumber.TM, dtm, file = "data/250412_topic_models_fit.Rda")

load("data/250412_topic_models_fit.Rda")
#### visualizations ####
df_topic_number <- tibble(
  topic_number = k,
  entropy = map_dbl (topicNumber.TM, function (x)
    mean(apply(posterior(x)$topics, 1, function (z) - sum(z * log(z)))) # maximize Entropy
  ),
  alpha = map_dbl(topicNumber.TM, slot, "alpha"),
  log_lik = map_dbl(topicNumber.TM, logLik) #,  #maximize loglik
  #perplexity = map_dbl(topicNumber.TM, perplexity) #minimize perplexity
)


df_stats <- tibble(
  model = names(lapply(tset.TM, logLik)),
  loglik = as.numeric(lapply(tset.TM, logLik)), #maximize loglik
  entropy = lapply (tset.TM, function (x) 
    mean(apply(posterior(x)$topics,
               1, function (z) - sum(z * log(z))))) %>% as.numeric()#maximize ENTROPY
  
)

perp <-  lapply(tset.TM[c(1,2,4)], perplexity) # Gibbs (TM 3) does not have perplexity
perp$Gibbs <- NA 

# pretty names:
df_stats$model <- c("VEM alpha", "VEM fixed", "Gibbs", "CTM")

g1 <- df_stats %>%
  add_column(perplexity = as.numeric(perp[c(1,2,4,3)])) %>% #minimize perplexity
  pivot_longer(cols = 2:4, names_to = "measure", values_to = "value") %>%
  ggplot(aes(model, value)) + 
  geom_col() + 
  # scale_y_continuous(labels = scales::label_scientific) +
  facet_wrap(.~measure, scales = "free_y") +
  labs(x = "Algorithm", y = "Value", tag = "A") +
  theme_light(base_size = 8) + 
  theme(axis.text.x = element_text(size = 5))
g1

g2 <- df_topic_number %>%
  # mutate(alpha_log = log10(alpha)) %>%
  pivot_longer(cols = 2:last_col(), names_to = "measure", values_to = "value") %>%
  # filter(measure != "alpha") %>%
  ggplot(aes(as.factor(topic_number), value)) +
  geom_col() + 
  # scale_y_continuous(labels = scales::label_scientific) +
  labs(x = "Number of topics", y = "Value", tag = "B") +
  facet_wrap(.~measure, scales = "free", ncol = 4, nrow = 1) +
  theme_light(base_size = 8)

g1/g2


df_topics100 <- tidy(topicNumber.TM[[3]], matrix = "beta")  

g1 <- df_topics100 %>%
  filter(topic <= 50) %>% 
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, - beta) %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_col(aes(fill = as.factor(topic)),show.legend = FALSE) +
  coord_flip() +
  scale_x_reordered() +
  labs(y = "Probability of word explaining the topic", x = "Word ranking") +
  facet_wrap(.~ topic, scales = "free_y", ncol = 5) +
  theme_light(base_size = 10)


g2 <- df_topics100 %>%
  filter(topic > 50) %>% 
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, - beta) %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_col(aes(fill = as.factor(topic)),show.legend = FALSE) +
  coord_flip() +
  scale_x_reordered() +
  labs(y = "Probability of word explaining the topic", x = "Word ranking") +
  facet_wrap(.~ topic, scales = "free_y", ncol = 5) +
  theme_light(base_size = 10)

ggsave(
  g2, file = "second50topics.png", device = "png", width = 7, height = 7,
  dpi = 300, bg = "white"
)


## Papers
df_docs <- tidy(topicNumber.TM[[3]], matrix = "gamma") 



df_doc <- df_docs %>% 
  group_by(document) %>% 
  mutate(max = max(gamma),
         is_max = gamma == max) %>% 
  filter(is_max) %>% 
  select(-max, -is_max)

df_doc %>% filter(topic == 10)
df_doc %>% filter(topic == 1)df_doc %>% filter(topic == 5)

df_doc %>% 
  group_by(topic) %>% 
  summarize(papers = n()) %>% 
  print(n = 100)

#### Ordination ####

df_docs |> 
    pivot_wider(names_from = topic, names_prefix = "topic_", values_from = gamma) |> 
    arrange(desc(document))

pca <- df_topics100 |> 
    mutate(beta = log(beta)) |> 
    pivot_wider(names_from = topic, names_prefix = "topic_", values_from = beta) |> 
    select(-term) |> 
    princomp()

pca |> plot()
names(pca)
ggbiplot::ggbiplot(pca)


#### interactive #####
# script 03


#### others ####
# csv for Tom
df_tm |> 
    left_join(df_doc |> mutate(document = as.numeric(document )), 
              by = c("id" = "document")) |> 
    select(-index_keywords, -author_keywords) |> 
    write_csv(file = "data/documents_and_topics.csv")
