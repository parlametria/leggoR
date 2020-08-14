#' @title Regex left match
#' @description Identifica eventos por regex e adiciona uma nova coluna
#' @param df dataframe a ser adicionado nova coluna
#' @param regex_df dataframe dos regex
#' @param new_column nova coluna
#' @return Dataframe com os eventos detectados pelo regex
#' @importFrom magrittr %<>%
#' @export
regex_left_match <- function(df, regex_df, new_column) {
  if('despacho' %in% names(regex_df)) {
    df %<>%
      dplyr::mutate(UQ(rlang::sym('despacho')) := stringr::str_replace_all(UQ(rlang::sym('despacho')), '\n|\r', ''))
  }
  if('texto_tramitacao' %in% names(regex_df)) {
    df %<>%
      dplyr::mutate(UQ(rlang::sym('texto_tramitacao')) := stringr::str_replace_all(UQ(rlang::sym('texto_tramitacao')), '\n|\r', ''))
  }

  columns <-
    names(regex_df)[names(regex_df) != new_column] %>% sapply(function(x) {
      paste0(x, "X")
    }
    , USE.NAMES = TRUE)
  names(regex_df) %<>% sapply(function(column) {
    if (column == new_column)
      column
    else
      paste0(column, "X")
  }
  , USE.NAMES = TRUE)

  regex_df[is.na(regex_df)] <- ".*"


  df %>%
    dplyr::mutate(sort = dplyr::row_number()) %>%
    fuzzyjoin::regex_left_join(regex_df, by = columns, ignore_case = TRUE) %>%
    dplyr::group_by(sort) %>%
    dplyr::ungroup() %>%
    dplyr::select(-tidyselect::ends_with("X"), -sort)

}

#' @title Renomeia as colunas do dataframe
#' @description Renomeia as colunas do dataframe usando o padrão de letras minúsculas e underscore
#' @param df Dataframe
#' @return Dataframe com as colunas renomeadas.
#' @importFrom magrittr %<>%
#' @export
rename_df_columns <- function(df) {
  names(df) %<>% to_underscore
  df
}

#' @title Renomeia um vetor com o padrão de underscores e minúsculas
#' @description Renomeia cada item do vetor com o padrão: separado por underscore e letras minúsculas
#' @param x Vetor de strings
#' @return Vetor contendo as strings renomeadas.
#' @examples
#' to_underscore(c("testName", "TESTNAME"))
#' @export
to_underscore <- function(x) {
  gsub('([A-Za-z])([A-Z])([a-z])', '\\1_\\2\\3', x) %>%
    gsub('.', '_', ., fixed = TRUE) %>%
    gsub('([a-z])([A-Z])', '\\1_\\2', .) %>%
    tolower()
}

#' @title Filtra datas não-úteis da tramitação de um congresso.
#' @description Remove do dataframe completo da tramitação as linhas cuja data representam dias não-úteis do Congresso,
#' incluindo fins de semana e dias de recesso parlamentar.
#' @param tramitacao_df Dataframe com a tramitação completa
#' @return Dataframe com a tramitação completa apenas com os dias úteis
#' @examples
#' eventos_extendidos <- eventos_df %>%
#' dplyr::mutate(data = lubridate::floor_date(data_hora, unit="day"))
#' full_dates <- tibble::tibble(data = seq(min(eventos_extendidos$data), max(eventos_extendidos$data), by = "1 day"))
#' eventos_extendidos <- eventos_extendidos %>%
#'   merge(full_dates,by="data", all.x = TRUE) %>%
#'   filtra_dias_nao_uteis_congresso(.)
filtra_dias_nao_uteis_congresso <- function(tramitacao_df) {
  tramitacao_filtrada_df <- tramitacao_df %>%
    dplyr::filter(!(lubridate::wday(periodo) %in% c(1,7))) %>%
    dplyr::filter(lubridate::month(periodo) != 1) %>%
    dplyr::filter(!((lubridate::month(periodo) == 2) & (lubridate::day(periodo) < 2))) %>%
    dplyr::filter(!((lubridate::month(periodo) == 7) & (lubridate::day(periodo) > 17))) %>%
    dplyr::filter(!((lubridate::month(periodo) == 12) & (lubridate::day(periodo) > 22)))
}

fetch_json_try <- function(url) {
  count <- 0
  repeat {
    json_data <- NULL
    tryCatch({
      json_data <- jsonlite::fromJSON(url, flatten = T)
    },
    error = function(msg) {
    })
    if (!is.null(json_data) & is.null(json_data$ERROR)) {
      break
    } else {
      print("Erro ao baixar dados, tentando outra vez...")
      count <- count + 1
      print(paste("Tentativas: ", count))
      Sys.sleep(2)
    }
  }
  return(json_data)
}

#' @title Renomeia as colunas do dataframe passado para o formato underscore
#' @description Renomeia as colunas do dataframe usando o padrão
#' de underscore e letras minúsculas
#' @param df Dataframe do Senado
#' @return Dataframe com as colunas renomeadas
#' @export
rename_table_to_underscore <- function(df) {
  new_names = names(df) %>%
    to_underscore()

  names(df) <- new_names

  df
}

#' @title Retorna um dataframe a partir de uma coluna com listas encadeadas
#' @description Retorna um dataframe a partir de uma coluna com listas encadeadas.
#' @param column Coluna
#' @return Dataframe com as informações provenientes de uma coluna com listas encadeadas.
#' @examples
#' generate_dataframe(column)
#' @export
generate_dataframe <- function (column) {
  as.data.frame(column) %>%
    tidyr::unnest() %>%
    rename_df_columns()
}

build_data_filepath <- function(folder_path,data_prefix,house,bill_id) {
  filename <- paste0(paste(bill_id,data_prefix,house, sep='-'),'.csv')
  filepath <- paste(folder_path, house, filename, sep='/')
}

#' @title Verifica dataframe
#' @description Verifica se um determinado daframe dado é nulo ou vazio.
#' @param df Dataframe a ser verificado
#' @return Dataframe vazio.
#' @export
check_dataframe <- function(df) {
  if ((is.null(df) || (nrow(df) == 0))) {
    warning("Dataframe de entrada deve ser não-nulo e não-vazio.")
    return(FALSE)
  }
  return(TRUE)
}

#' @title Concateca dois elementos com um separador no meio
#' @description Recebe duas variáveis x e y e retorna a união "x:y".
#' @param x Primeira variável a ser concatenada
#' @param y Segunda variável a ser concatenada
#' @param sep Separador a ser concatenado
#' @return String concatenada com a primeira variável + separador + segunda variável
paste_cols_sorted <- function(x, y, sep = ":") {
  stopifnot(length(x) == length(y))
  return(lapply(1:length(x), function(i) {
    paste0(sort(c(x[i], y[i])), collapse = sep)
  }) %>%
    unlist())
}

#' @title Formata o nome eleitoral dos parlamentares
#' @description Recebe o nome, o partido e o uf e concatena-os.
#' @param nome Nome do parlamentar
#' @param partido Partido do parlamentar
#' @param uf Estado do parlamentar
#' @return String
#' @export
formata_nome_eleitoral <- function(nome, partido, uf) {
  if(is.na(uf)) {
    uf = '-' 
  }
    
  if(is.na(partido)){
    partido = '-' 
  }
    
  return(paste0(nome, " (", partido, "/", uf, ")"))
}

#' @title Formata o nome dos senadores
#' @description Recebe a string do senador retornada pela api do Seando
#' e retorna Sen. nome.
#' @param nome_autor Nome do parlamentar
#' @param tipo_autor Tipo do Autor (deputado ou senador)
#' @return String
#' @export
formata_nome_senadores <- function(nome_autor, tipo_autor) {
  if (tolower(tipo_autor) == "senador") {
    return(stringr::str_replace(nome_autor,
                                "(\\()(.*?)(\\))|(^Senador(a)* )|(^Líder do ((.*?)(\\s)))|(^Presidente do Senado Federal: Senador )", "Sen. "))
  } else if (tolower(tipo_autor) == "deputado") {
    return(stringr::str_replace(nome_autor,
                                "(\\()(.*?)(\\))|(^Deputad(o|a) Federal )|(^Deputad(o|a) )", "Dep. "))
  } else {
    return(nome_autor)
  }

}

#' @title Formata o nome dos deputados
#' @description Recebe o nome do deputado
#' e retorna Dep. nome.
#' @param nome_autor Nome do parlamentar
#' @param tipo_autor Tipo do Autor (deputado ou senador)
#' @return String
#' @export
formata_nome_deputados <- function(nome_autor, tipo_autor) {
  if (tolower(tipo_autor) == "deputado") {
    return(paste0('Dep. ', nome_autor))
  } else {
    return(nome_autor)
  }
}

#' @title Padroniza um texto
#' @description Recebe uma string, remove pontuações e 
#' transforma as letras em minúsculas
#' @param string String a ser processada
#' @return String sem acentos e pontuações
padroniza_string <- function(string) {
  return(
    iconv(string, 
        from="UTF-8", 
        to="ASCII//TRANSLIT") %>% 
    tolower())
  
}

#' @title Checa se um valor é logical e se sim, substitui por NA
#' @description Recebe um valor e checa se é logical e se sim, substitui por NA. Se não, retorna o valor.
#' @param valor Valor a ser checado
#' @return NA ou o valor passado
#' @export
check_is_logical <- function(valor) {
  if (is.logical(valor)) {
    return(NA)
  }
  return(valor)
}
