library(jsonlite)
require(magrittr)

url_glossary <- "http://legis.senado.leg.br/dadosabertos/glossario/lista?v=4"

json_glossary <- fromJSON(url_glossary, flatten = T)

#extract termos objects
glossary_data <-
  json_glossary %>%
  extract2("Glossario") %>%
  extract2("Termos")

#transform into a data frame and rename headers
glossary <- as.data.frame(glossary_data)
glossary$Termo.IndicadorTermoAgrupador <- NULL
names(glossary) <- c("termo", "significado", "codigo")

#reorder
glossary <- glossary[c("codigo", "termo", "significado")]

#create file
write_csv(glossary, "docs/glossario-termos-senado.csv")
