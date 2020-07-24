
library(haven);library(tidyverse);library(readtext);library(quanteda); library(tm);library(topicmodels);
library(broom); library(tidytext);library(ngram);library(Rtsne);library(textmineR)

#Lista de archivos a trabajar
word_files <- list.files(path = "data", pattern = "^Consumo.*doc$", full.names = T)

#Cargar todos los datos y apilar en una sola tabla
datos_brutos <-  map(word_files, readtext, text_field = "texts") %>% 
  reduce(bind_rows) %>% 
  as.data.frame() %>% 
  mutate(entrevista = paste0("entrevista", 1:length(word_files)),
         doc_id = paste0("doc", 1:length(word_files)))


##################
#Preprocesamiento#
##################

#Es necesario sacar el encabezado y la información de contexto
datos_brutos <- datos_brutos %>% 
  mutate(text = gsub(pattern = "CENTRO DE INVESTIGACIÓN.*Entrevistador.{20}", replacement = "", text))

#Separar las entrevistas por párrafos y construir un dataframe
#parrafos <- strsplit(datos_brutos$text, "\r\n\r\n[[:digit:]]{1,3}", fixed = FALSE)

parrafos <- strsplit(datos_brutos$text, "\\(\\d{1,3}\\)|\r\n\r\n{1,3}", fixed = FALSE)

parrafos_df <-  map(parrafos, as.data.frame) %>% 
  reduce(bind_rows) %>% 
  rename(text = ".x[[i]]")

#Eliminar celdas vacías y párrafos con pocos caracteres
stop_words <- c(stopwords("spanish"), "ehh", "si", "creo", "sé", "pa", "cómo", "ser", "eh", "ir", "va", "entonces", "ah", "po", "ahí",
                "así", "pucha", "cachai")

parrafos_df <- parrafos_df %>% 
  mutate(text = tolower(text),
         text = gsub("[[:digit:]]|[[:punct:]]", "", text),
         text = tm::removeWords(x = text, stop_words),
         text = gsub("\r\n", "", text),
         text = gsub("   ", " ", text),
         text = gsub("  ", " ", text),
         text = gsub("  ", " ", text),
         text = trimws(text, "both")) %>% 
  mutate(n_words = map_dbl(text, wordcount)) %>% 
  filter(text != "" & nchar(text) >= 4 & n_words > 4) 


##################################
#Exploración inicial con quanteda
################################

frecuencias <- parrafos_df %>% 
  corpus() %>%  
  tokens(remove_punct = T,
         remove_symbols = T,
         remove_numbers = T) %>% 
  dfm(remove = stopwords("spanish")) %>%
  textstat_frequency()

most_freq <- frecuencias %>% 
  dplyr::slice(1:50)

#Graficar palabras más comunes en cada una de las entrevistas
df <- most_freq %>% 
  ungroup() %>%
  arrange(group, frequency) %>% 
  mutate(order = row_number()) 

df %>%  
  ggplot(aes(order,  frequency, fill = feature)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = df$order,
                     labels = df$feature)
  

####
#LDA
####

dfm_matrix <- parrafos_df %>% 
  corpus() %>%  
  tokens(remove_punct = T,
         remove_symbols = T,
         remove_numbers = T) %>% 
  dfm(remove = stopwords("spanish")) 


burnin <- 4000
iter <- 500
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE
topics = 10

ldaOut <-LDA(dfm_matrix, k = topics, method ="Gibbs",control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))



#Matriz con betas
beta <- tidy(ldaOut, matrix = "beta") %>% 
  as.data.frame()

gamma <- tidy(ldaOut, matrix = "gamma") %>% 
  as.data.frame()

#Graficar palabras asociadas a cada tópico
ap_top_terms <- beta %>%
  group_by(topic) %>%
  top_n(8, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

#Armar matriz con gammas
topic_text <- gamma %>% 
  spread(key = topic, value = gamma) %>% 
  select(-document)

#Seleccionar la categoría asignada a cada texto. Esto se hace tomando la clase con la probabilidad más alta
categoria <- gamma %>% 
  group_by(document) %>% 
  slice(which.max(gamma)) 


names(topic_text)[1:topics] <- paste0("topic", 1:topics)


#Aplanar con t-SNE
sne <-  Rtsne(as.matrix(topic_text), check_duplicates=FALSE, pca=TRUE, perplexity=30, theta=0.5, dims=2)


#Generar una base para ggplot. Es necesario agregar la categoría del tópico generada más arrriba
dimensiones <-  as.data.frame(sne$Y)  
dimensiones$topic <- categoria$topic


#Graficar con ggplot
ggplot(dimensiones, aes(x=V1, y=V2, color = as.factor(topic))) +  
  geom_point(size=3) +
  guides(colour=guide_legend(override.aes=list(size=4))) +
  xlab("") + ylab("") +
  ggtitle("t-SNE") +
  theme_light(base_size=20) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())












#Proyectar en dos dimensiones


#Generación de embeddings para las frases

#Algoritmo de clustering para encontrar frases similares

