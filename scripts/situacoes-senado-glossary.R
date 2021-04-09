library(jsonlite)
require(magrittr)

url_situacoes_glossary <- "https://legis.senado.leg.br/dadosabertos/materia/situacoes.json"

json_situacoes <- fromJSON(url_situacoes_glossary, flatten = T)

#extract situacoes objects
situacoes_data <-
  json_situacoes %>%
  extract2("ListaSituacoes") %>%
  extract2("Situacoes")

#transform into a data frame and rename headers
situacoes <- as.data.frame(situacoes_data)
situacoes$Situacao.DataCriacao <- NULL
names(situacoes) <- c("codigo", "sigla", "descricao")

situacoes$codigo <- as.numeric(situacoes$codigo)

#create file
write_csv(situacoes, "../inst/extdata/Senado/situacoes-glossary-senado.csv")