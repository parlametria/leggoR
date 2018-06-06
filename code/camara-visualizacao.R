df_fase <- 
  tramitacao_pl_6726 %>% 
  arrange(data_hora) %>% 
  mutate(end_data = lead(data_hora)) %>% 
  group_by(fase, 
    sequence = data.table::rleid(fase)) %>%
    summarise(start = min(data_hora),
              end = max(end_data)) %>% 
    ungroup() %>% 
  arrange(sequence) %>%
  select(-sequence) %>%
  mutate(groups = "Fase",
         color = case_when(fase == "iniciativa" ~ "#7fc97f",
                           fase == "relatoria" ~ "#fdc086",
                           fase == "discussao_deliberacao" ~ "#beaed4",
                           fase == "virada_de_casa" ~ "#ffff99",
                           fase == "final" ~ "#f4fa58"))
  

vistime::vistime(data, start = "start", end = "end", groups = "groups", events = "fase", showLabels = FALSE)
