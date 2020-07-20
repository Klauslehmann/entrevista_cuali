
library(haven);library(tidyverse);library(readtext);library(quanteda); library(tm);library(topicmodels);
library(broom); library(tidytext)

#Lista de archivos a trabajar
word_files <- list.files(path = "data", pattern = "^Consumo.*doc$", full.names = T)

#Cargar todos los datos y apilar en una sola tabla
datos_brutos <-  map(word_files, readtext, text_field = "texts") %>% 
  reduce(bind_rows) %>% 
  as.data.frame() %>% 
  mutate(entrevista = paste0("entrevista", 1:length(word_files)),
         doc_id = paste0("doc", 1:length(word_files)))

text <- strsplit(datos_brutos$text[1], ".", fixed = TRUE)

##################
#Preprocesamiento#
##################

#Es necesario sacar el encabezado y la información de contexto
datos_brutos <- datos_brutos %>% 
  mutate(text = gsub(pattern = "CENTRO DE INVESTIGACIÓN.*Entrevistador.{20}", replacement = "", text))


##################################
#Exploración inicial con quanteda
################################

frecuencias <- datos_brutos %>% 
  corpus() %>%  
  tokens(remove_punct = T,
         remove_symbols = T,
         remove_numbers = T) %>% 
  dfm(remove = stopwords("spanish")) %>%
  textstat_frequency(groups =  "entrevista")

most_freq <- frecuencias %>% 
  group_by(group) %>% 
  arrange(desc(frequency)) %>% 
  dplyr::slice(1:10)

#Graficar palabras más comunes en cada una de las entrevistas
df <- most_freq %>% 
  ungroup() %>%
  arrange(group, frequency) %>% 
  mutate(order = row_number()) 

df %>%  
  ggplot(aes(order,  frequency, fill = feature)) +
  geom_bar(stat = "identity") +
  facet_wrap(~group, scales = "free") +
  scale_x_continuous(breaks = df$order,
                     labels = df$feature)
  


#Aplicar LSA
dfm_matrix <- datos_brutos %>% 
  corpus() %>%  
  tokens(remove_punct = T,
         remove_symbols = T,
         remove_numbers = T) %>% 
  dfm(remove = stopwords("spanish")) 


dfm_matrix2 <- convert(dfm_matrix, to = "topicmodels")
lda <- LDA(dfm_matrix, k = 4, control = list(seed = 1234))
beta <- tidy(lda, matrix = "beta") %>% 
  as.data.frame()


ap_top_terms <- beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()



#Generación de embeddings para las frases

#Algoritmo de clustering para encontrar frases similares

