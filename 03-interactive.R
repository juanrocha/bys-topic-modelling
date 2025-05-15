library(tidyverse)
library(tidytext)
library(tm)
library (topicmodels)
library(lda)
library(tictoc)
library(LDAvis)

load("data/250412_topic_models_fit.Rda")

slotNames(topicNumber.TM[[3]])

z <- createJSON(
    phi = exp(slot(topicNumber.TM[[3]], "beta")) ,
    theta = slot(topicNumber.TM[[3]], "gamma") ,
    doc.length = rowSums(as.matrix(dtm)>0) ,
    vocab = slot(topicNumber.TM[[3]], "terms"),
    term.frequency = colSums(as.matrix(dtm))/dim(slot(topicNumber.TM[[3]], "beta"))[1]
)

serVis(z, out.dir = "vis", open.browser = FALSE)
serVis(z)
