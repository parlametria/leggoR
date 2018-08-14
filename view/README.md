# Visualização 

## Informações Gerais

A Ágora Digital possui um conjunto de scripts que facilitam a visualização de tramitação
de projetos de lei nas duas casas legislativas. Para isso, é utilizados os dados gerados
pelo script [analyzer](../R/analyzers/analyzer.R) para exibir uma timeline com os principais
eventos e por onde o projeto passou nesse tempo de tramitação.

## Scripts
### [data-formatter-vistime-camara.R](./formatter/data-formatter-vistime-camara.R)

Contém a função *build_vis_csv* que importa os CSVs contendo a tramitação de uma proposição
e o converte para uma versão adaptada que poderá ser lido por *build-chart.R* na construção 
do gráfico. Ao fim, a função salva o CSV adaptado em [data/vis/tramitacao](/data/vis/tramitacao).

#### Exemplo
```R
 build_vis_csv(2121442)
 ```
 
### [build-chart.R](./visualizer/build-chart.R)

Contém a função *create_chart* que recebe um id e casa. Ao executar a função, o script
busca pelo CSV daquela proposição com o respectivo id e casa e retorna um gráfico em timeline.

#### Exemplo

```R
create_chart(91341, "senado")
create_chart(257161, "camara")
```
