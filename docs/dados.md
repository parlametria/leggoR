Dados
=====
Dados Abertos da Câmara
-----------------------
[http://www.w3c.br/pub/Materiais/PublicacoesW3C/Manual\_Dados\_Abertos\_WEB.pdf](http://www.w3c.br/pub/Materiais/PublicacoesW3C/Manual_Dados_Abertos_WEB.pdf) - Manual sobre dados abertos no geral.
### Publicação dos Dados Abertos
[http://dados.gov.br/pagina/cartilha-publicacao-dados-abertos](http://dados.gov.br/pagina/cartilha-publicacao-dados-abertos) - Cartilha Técnica para Publicação de Dados Abertos no Brasil v1.0
### V1
[http://www2.camara.leg.br/transparencia/dados-abertos/dados-abertos-legislativo](http://www2.camara.leg.br/transparencia/dados-abertos/dados-abertos-legislativo)
Não mais mantida, mas talvez tenha dados que não estão na V2.
#### API
Há dados desse ano, logo parecem atuais.
#### Arquivos
Dados desatualizados, são de 2002 à 2016.
### V2
Melhor estruturada e mantida, mas talvez não tenha todos os dados que estão na V1. Apresenta bugs.
#### API
[https://dadosabertos.camara.leg.br/swagger/api.html\#api](https://dadosabertos.camara.leg.br/swagger/api.html#api)
Há dados desde 1934.
##### /proposicoes
Permite listagem das proposições, mas é paginado, máximo 100 itens por página.
##### /proposicoes/{id}
Retorna dados gerais sobre uma proposição específica, mas não tudo que há na API. É preciso chamar os outros endpoints para ter dados de autores, tramitação, votações etc.
#### Arquivos

[https://dadosabertos.camara.leg.br/swagger/api.html\#staticfile](https://dadosabertos.camara.leg.br/swagger/api.html#staticfile)

Tem a listagem de todas as proposições de um ano, mas apenas com dados gerais sobre elas (menos ainda do que '/proposicoes'). É preciso chamar a API para coletar mais dados sobre cada uma.
Os arquivos sobre proposições parecem atuais (há dados de hoje, 16/05/18).
Há dados desde 1934.

Dados Abertos do Senado
-----------------------

[https://www12.senado.leg.br/dados-abertos](https://www12.senado.leg.br/dados-abertos)

O código da matéria usado pela API é o mesmo da site. Ex.:

[http://legis.senado.leg.br/dadosabertos/materia/PLS/212/2017](http://legis.senado.leg.br/dadosabertos/materia/PLS/212/2017)

retorna código: 129808, que é o mesmo usado no final do link abaixo:

[https://www25.senado.leg.br/web/atividade/materias/-/materia/129808](https://www25.senado.leg.br/web/atividade/materias/-/materia/129808)

CamaraCrawler
-------------

[https://github.com/BurgosNY/CamaraCrawler](https://github.com/BurgosNY/CamaraCrawler)

Coleta dados da 'V2/proposicoes' e do 'urlInteiroTeor' extraindo informações dos PDFs usando a biblioteca 'textract'.

serenata-toolbox
----------------

[https://github.com/okfn-brasil/serenata-toolbox](https://github.com/okfn-brasil/serenata-toolbox)

O código parece que só baixa das APIs dados relativos aos gastos do Congresso.
Parece o próprio projeto disponibiliza alguns arquivos compactados com dados do Congresso, mas o das proposições parece estar desatualizado:

[https://github.com/okfn-brasil/serenata-toolbox/blob/master/serenata\_toolbox/datasets/downloader.py\#L42](https://github.com/okfn-brasil/serenata-toolbox/blob/master/serenata_toolbox/datasets/downloader.py#L42)

serenata-de-amor
----------------

[https://github.com/okfn-brasil/serenata-de-amor](https://github.com/okfn-brasil/serenata-de-amor)

Não achei no repositório um crawler relacionado aos dados que precisamos...

thiagofelix/camara-deputados-api
--------------------------------

[https://github.com/thiagofelix/camara-deputados-api](https://github.com/thiagofelix/camara-deputados-api)

Parece extrair dados tanto do site da Câmara como da API V1, mas apenas sobre votos, presença e parlamentares.

shgo/baixa\_camara
------------------

[https://github.com/shgo/baixa\_camara](https://github.com/shgo/baixa_camara)

Parece baixar os dados da API V1. Usa a biblioteca 'pdfminer' para extrair dados dos PDFs.

gilsondev/discursoaberto-crawler
--------------------------------

[https://github.com/gilsondev/discursoaberto-crawler](https://github.com/gilsondev/discursoaberto-crawler)

Parece que extrai só os discursos, e do próprio site Câmara, pois parece que não estão disponíveis pelas APIs.
