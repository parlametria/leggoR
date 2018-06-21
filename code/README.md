
# Scripts

* [Importar dados do Senado](./importa-dados-Senado.R)

Importa votações, tramitação, relatorias de um proposição dado o seu id no senado.
O script recebe os dados e os salva na pasta [data/Senado](/data/Senado).

* [Processar dados do Senado](./processa-dados-Senado.R)

Com as informações importadas, o script adiciona as fases adequadas para cada evento.
O script processa as fases e os salva em um csv em [data/Senado](/data/Senado).

* [Importar e processar dados da Câmara](./camara-process-data.R)

Importa tramitação, relatorias de um proposição dado o seu id na câmara.
Com as informações importadas, o script adiciona as fases adequadas para cada evento.
O script recebe os dados e os salva na pasta [data/camara](/data/camara).

* [Gerar Timeline](./vis/tramitacao/)

Contém scripts para geração de gráfico timeline. Nele são executados os scripts de 
importe e processamento de proposição listados acima.
