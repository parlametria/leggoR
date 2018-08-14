# Tramitação

* [build-chart.R](./visualizer/build-chart.R)

Contém a função *create_chart* que recebe um id e casa. Ao executar a função, o script
busca pelo CSV daquela proposição com o respectivo id e casa e retorna um gráfico em timeline.


* data-formatter-vistime-<camara/senado>.R

Contém a função *build_vis_csv* que importa os CSVs contendo a tramitação de uma proposição
e o converte para uma versão adaptada que poderá ser lido por *build-chart.R* na construção 
do gráfico. Ao fim, a função salva o CSV adaptado em [data/vis/tramitacao](/data/vis/tramitacao).
