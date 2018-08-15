source(here::here("R/camara-lib.R"))
source(here::here("R/analyzers/senado_analyzer.R"))
source(here::here("R/fetcher.R"))
source(here::here("R/congresso-lib.R"))
source(here::here("R/analyzers/camara_analyzer.R"))

process_proposicao <- function(id, casa) {
  if ("CAMARA" == toupper(casa)) {
    process_proposicao_camara(id)
  } else if ("SENADO" == toupper(casa)) {
    senado_env <-
      jsonlite::fromJSON(here::here("R/config/environment_senado.json"))
    senado_constants <- senado_env$constants
    process_proposicao_senado(id)
  }
}
