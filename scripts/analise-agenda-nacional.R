library(rcongresso)
library(dplyr)
library(ggplot2)

#agenda_nacional <-

plp_441_2017_id <- fetch_id_proposicao("PLP", 441, 2017)

plp_441_2017 <- fetch_proposicao(plp_441_2017_id)
plp_441_2017_votacoes <- fetch_votacoes(plp_441_2017_id)

plp_441_2017_votos <- plp_441_2017_votacoes %>%
  rowwise() %>%
  do(fetch_votos(.$id))

plp_441_2017_orientacoes <- plp_441_2017_votacoes %>%
  rowwise() %>%
  do(fetch_orientacoes(.$id)) %>%
  rename(orientacao = voto)

plp_441_2017_orientacao_gov <- plp_441_2017_orientacoes %>%
  filter(nomeBancada == "GOV.") %>%
  select(id_votacao,orientacao_gov = orientacao)

plp_441_2017_orientacao_partido <- plp_441_2017_votos_orientacoes %>%
  filter(nomeBancada == parlamentar.siglaPartido) %>%
  select(id_votacao,parlamentar.id,orientacao_partido = orientacao)

votos_orientacoes_deputados <- plp_441_2017_votos %>%
                select(id_votacao,parlamentar.id,parlamentar.nome,parlamentar.siglaPartido,voto) %>%
                inner_join(plp_441_2017_orientacao_gov, by='id_votacao') %>%
                inner_join(plp_441_2017_orientacao_partido, by=c("id_votacao", "parlamentar.id")) %>%
                filter(voto != 'null')

obediencia <- votos_orientacoes_deputados %>%
                    ungroup() %>%
                    group_by(parlamentar.nome,parlamentar.siglaPartido) %>%
                    summarise(obediencia_gov = sum(voto == orientacao_gov)/n(),
                              obediencia_partido = sum(voto == orientacao_partido)/n(),
                              babao_gov = sum((voto == orientacao_gov) & (voto != orientacao_partido))/n(),
                              diferentao = sum((voto != orientacao_gov) & (voto != orientacao_partido))/n())

obediencia_partidos <- votos_orientacoes_deputados %>%
                          ungroup() %>%
                          group_by(parlamentar.siglaPartido) %>%
                          summarise(obediencia_gov = sum(voto == orientacao_gov)/n(),
                                    obediencia_partido = sum(voto == orientacao_partido)/n(),
                                    babao_gov = sum((voto == orientacao_gov) & (voto != orientacao_partido))/n(),
                                    diferentao = sum((voto != orientacao_gov) & (voto != orientacao_partido))/n())


obediencia_partidos %>%
  ggplot(aes(x=reorder(parlamentar.siglaPartido,obediencia_gov),y=obediencia_gov)) +
  geom_bar(stat='identity') +
  coord_flip() +
  ggtitle('Temerômetro')

obediencia_partidos %>%
  ggplot(aes(x=reorder(parlamentar.siglaPartido,obediencia_partido),y=obediencia_partido)) +
  geom_bar(stat='identity') +
  coord_flip() +
  ggtitle('Bons Meninos')

obediencia_partidos %>%
  ggplot(aes(x=reorder(parlamentar.siglaPartido,babao_gov),y=babao_gov)) +
  geom_bar(stat='identity') +
  coord_flip() +
  ggtitle('Babômetro')

obediencia_partidos %>%
  ggplot(aes(x=reorder(parlamentar.siglaPartido,diferentao),y=diferentao)) +
  geom_bar(stat='identity') +
  coord_flip() +
  ggtitle('Diferentômetro')
