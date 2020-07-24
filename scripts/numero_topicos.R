
#########################
#BUSCAR NÚMERO DE TÓPICOS
#########################

#Evaluar perplexity
perplexity_df <- data.frame()

burnin = 100
iter = 500
keep = 50
topics <- c(2:25)

for (i in topics){
  fitted <- LDA(dfm_matrix2, k = i, method = "Gibbs",
                control = list(burnin = 180, iter = iter, keep = keep) )
  perplexity_df[i,1] <- perplexity(fitted, newdata = dfm_matrix2)
  perplexity_df[i,2] <- i
  print(i)
}

ggplot(perplexity_df, aes(x = V2, y = V1)) +
  geom_line() +
  scale_x_continuous(breaks = seq(2, 25, by = 1) )

result <- ldatuning::FindTopicsNumber(
  dfm_matrix,
  topics = seq(from = 2, to = 15, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)

FindTopicsNumber_plot(result)
