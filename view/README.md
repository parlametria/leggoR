# Tramitação

* [build-data.R](./build-data.R)

Dado um id e casa, o script gera todos os CSVs necessários para a criação do gráfico
timeline. O script chama funções para importar e processar, dado o id da proposição 
e a casa a qual está está relacionada a proposição.

* [build-chart.R](./build-chart.R)

Contém a função *create_chart* que recebe um id e casa. Ao executar a função, o script
busca pelo CSV daquela proposição com o respectivo id e casa e retorna um gráfico em timeline.

* data-chart-<camara/senado>.R

Contém a função *build_vis_csv* que importa os CSVs contendo a tramitação de uma proposição
e o converte para uma versão adaptada que poderá ser lido por *build-chart.R* na construção 
do gráfico. Ao fim, a função salva o CSV adaptado em [data/vis/tramitacao](/data/vis/tramitacao).
