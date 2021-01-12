# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load libraries 
library(tm)
#library(pdftools)
#install.packages("pdftools")

# Load corpus
filename = "./Corpus/The_Latitudines_breves_and_Late_Medieval.pdf"
pdf <- readPDF(control = list(text = "-layout"))(elem = list(uri = filename),
                                                 language = "lat",
                                                 id = "id1")
summary(pdf$content)

# Remove pages without source text and concatenate remaining pages.
pdf$content = pdf$content[36:51]
pdf$content = paste(pdf$content, collapse = ' ')


corpus = Corpus(VectorSource(pdf))
remove(pdf)

# Remove text which is not part of source text and split into three documents
strsplits <- function(x, splits, ...)
{
  for (split in splits)
  {
    x <- unlist(strsplit(x, split, ...))
  }
  return(x[!x == ""]) # Remove empty values
}

corpus <- Corpus(VectorSource(
  strsplits(as.character(corpus[[1]]),  ## coerce to character
           c("V.2.1         Latitudines breves I\n","APPENDIX TO LATITUDINES BREVES I\n"),   
           fixed=TRUE)))
corpus <- Corpus(VectorSource(
  strsplits(as.character(corpus[[2]]),  ## coerce to character
           c("V.2.2    Latitudines breves II\n","V.2.3     Latitudines breves III\n "),   
           fixed=TRUE))) 

# Add ids to docs
ids <- c('Latitudines breves I','Latitudines breves II','Latitudines breves III')
df <- data.frame(doc_id = ids,
                   text = content(corpus))
corpus = Corpus(DataframeSource(df))
remove(df)

inspect(corpus[1])

# Start cleaning of dataset

removeLine <- content_transformer(function (x , pattern ) gsub(pattern, "\n", x))
toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
replaceLigature <- content_transformer(function(x, pattern) { return (gsub(pattern, "ff", x))})
removeCompletely <- content_transformer(function(x, pattern) { return (gsub(pattern, "", x))})

# Remove headlines
corpus <- tm_map(corpus, removeLine, "\n\\s*SCIAMVS.*?\n")
corpus <- tm_map(corpus, removeLine, "\n\\s*[0-9]*\\s*Di\\sLiscia.*?\n")

# Replace ligature
corpus <- tm_map(corpus, replaceLigature, "ﬀ")

# Remove references
corpus <- tm_map(corpus, toSpace, "\\[(.*?)\\]")

# Remove critical notes
corpus <- tm_map(corpus, removeLine, "\n\\s*\\d+[^\n]*?].*?\n")
corpus <- tm_map(corpus, removeLine, "\n[^\n]*?].*?\n")

# Remove footnotes
corpus <- tm_map(corpus, removeLine, "\n\\s*\\d+\\n.*?).*?\n") # Matches footnotes with reference.

# Remove figure captions
corpus <- tm_map(corpus, removeLine, "\n[^\n]*?Fig\\..*?\n")
corpus <- tm_map(corpus, removeLine, "\n[^\n]*?Fig\\..*?\n") # Repeat once to capture repeat captions.

# Remove lines with figure labels
corpus <- tm_map(corpus, removeLine, "\n(\\s+\\w\\s*)+\n")

# Remove comments from copyist
corpus <- tm_map(corpus, toSpace, "⌈.*?⌋")

# Remove Siglae
corpus <- tm_map(corpus, removeCompletely, "Mü\\d+")
corpus <- tm_map(corpus, removeCompletely, "Wi\\d+.")
corpus <- tm_map(corpus, removeCompletely, "Mn\\d+.")

# Remove word breaks at EOL
corpus <- tm_map(corpus, removeCompletely, "-\\s*\\d*\n\\s*")
corpus <- tm_map(corpus, removeCompletely, "-\\s*.\n") # Necessary since sigla regex with Umlaut doesn't correctly remove recto denominator 

# Remove folio delimiter
corpus <- tm_map(corpus, toSpace, "\\|+")

# Remove additions of editor
corpus <- tm_map(corpus, toSpace, "⟨.*?⟩")

# Remove text in brackets
corpus <- tm_map(corpus, toSpace, "\\(.*?\\)")

# Remove ordinal numbers
corpus <- tm_map(corpus, toSpace, "\\d{1,2}[a-z]{,2}\\s*:") # removes colons after ordinal numbers as well
corpus <- tm_map(corpus, toSpace, "\\d{1,2}[a-z]{1,2}\\s")

# Remove quotation marks
corpus <- tm_map(corpus, removeCompletely, "(“|”)")

# Final transformations
corpus = tm_map(corpus,removeNumbers)
corpus = tm_map(corpus,stripWhitespace)
corpus = tm_map(corpus,content_transformer(tolower))

corpus.base = tm_map(corpus,removePunctuation)
corpus.base = tm_map(corpus.base,stripWhitespace)


# Inspection time

inspect(corpus[1])
inspect(corpus[2])
inspect(corpus[3])

summary(corpus[1:3])

inspect(corpus.base[1])


# udpipe

library(udpipe)

udmodel.ittb <- udpipe_download_model(language = "latin-ittb")
udmodel.ittb <- udpipe_load_model(file = udmodel.ittb$file_model)

x <- udpipe_annotate(udmodel.ittb, x = content(corpus))
x <- as.data.frame(x, detailed = TRUE)


# Experiment 1: 
# Compare base tdm with tdm after lemmatization via udmodel
library(tidyverse)
x = tibble(x)
doc1 <- x %>% filter(doc_id=="doc1") %>% select(lemma) %>% pull %>% paste(collapse = ' ')
doc2 <- x %>% filter(doc_id=="doc2") %>% select(lemma) %>% pull %>% paste(collapse = ' ')
doc3 <- x %>% filter(doc_id=="doc3") %>% select(lemma) %>% pull %>% paste(collapse = ' ')

df <- data.frame(doc_id = ids,
                 text = c(doc1, doc2, doc3))
corpus.lemma = Corpus(DataframeSource(df))
remove(df)

corpus.lemma = tm_map(corpus.lemma, removePunctuation)
corpus.lemma = tm_map(corpus.lemma, stripWhitespace)


# Define stopwords
myStopwords = c(stopwords(),"est","non","per","quedam", "ibi", "quo", "quandam",
                "cuius", "que", "vel", "quod", "quia", "sum", "quidam", "qui", "habeo")


tdm.base = TermDocumentMatrix(corpus.base, control = list(stopwords = myStopwords))
tdm.base

tdm.lemma = TermDocumentMatrix(corpus.lemma, control = list(stopwords = myStopwords))
tdm.lemma

freq.base = rowSums(as.matrix(tdm.base))
freq.lemma = rowSums(as.matrix(tdm.lemma))

plot(sort(freq.lemma, decreasing = T),col="blue",main="Word frequencies",
     xlab="Frequency-based rank", ylab="Frequency")
# Thirty most frequent terms
tail(sort(freq.base),n=30)
tail(sort(freq.lemma),n=30)
sum(freq == 1)
df <- as.data.frame(as.matrix(tdm))

enframe(freq.base)
enframe(freq.lemma)

tdm2 <- full_join(enframe(freq.base),enframe(freq.lemma),by="name") %>%
  rename(base = value.x, lemma = value.y) %>% 
  mutate(base = replace_na(base,0),lemma = replace_na(lemma,0)) %>%
  filter_at(vars("lemma"),any_vars(. > 10))

library(ggplot2)

tdm2$name <- factor(tdm2$name, levels = tdm2$name[order(desc(tdm2$lemma))]) 


ggplot(tdm2, aes(x=name)) + geom_point(aes(y=base), color='blue') + 
  geom_point(aes(y=lemma), color='red') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



# Experiment 2:
# Evaluate UPOS annotation via confusion matrix on manually created labels.

upos <- x %>% filter(sentence_id<3) %>% select(sentence_id, token, upos)
true_upos <- pull(upos, upos)

# Manually correcting the annotations of the first 3 sentences for each of the 3 docs
print(upos,n=107)

true_upos[7] <- 'NOUN'
true_upos[8] <- 'ADJ'
true_upos[15] <- 'ADJ'
true_upos[21] <- 'NOUN'
true_upos[23] <- 'NOUN'
true_upos[29] <- 'NOUN'
true_upos[30] <- 'AUX'
true_upos[35] <- 'NOUN'
true_upos[41] <- 'ADJ'
true_upos[42] <- 'ADJ'
true_upos[44] <- 'ADJ'
true_upos[74] <- 'NOUN'
true_upos[75] <- 'ADJ'
true_upos[77] <- 'NOUN'
true_upos[102] <- 'NOUN'
true_upos[106] <- 'NOUN'

# Creating & Plotting confusion matrix.
upos_tags = c("ADJ","ADP","ADV","AUX","CCONJ","DET","INTJ","NOUN","NUM","PART","PRON","PROPN","PUNCT","SCONJ","SYM","VERB","X")

true_upos = factor(true_upos, levels = upos_tags)
upos$upos = factor(upos$upos, levels = upos_tags)
upos <- upos %>% mutate(true_upos = true_upos)

library(yardstick)
cm <- upos %>% conf_mat(true_upos, upos)
autoplot(cm, type = "heatmap") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

