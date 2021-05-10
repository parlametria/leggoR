# Módulo apensadas

Este módulo é responsável por processar os dados de proposições apensadas monitoradas pelo Parlametria. Este processamento envolve criar um csv com a lista de proposições apensadas e o mapeamento para o id da proposição principal (na casa de origem), o id da proposição principal raiz da árvore de apensados (na casa de origem) e o id da proposição principal raiz no Parlametria.

## Arquivos

### **export_apensadas.R**

Arquivo que fornece uma interface via linha de comando para executar o processamento das apensadas e salvar em CSV. São inputs para o processamento: o caminho para o csv de proposições (metadados - proposicoes.csv) processado pelo pacote do leggoR, caminho para o csv de interesses.csv com o mapeamento de proposições monitoradas e seus interesse no Parlametria, caminho para exportação dos dados de proposições apensadas.

Uso:

```
Rscript export_apensadas.R -p <caminho_para_proposicoes.csv> -i <caminho_para_interesses.csv> -o <caminho_para_export_folder>
```

### **process_apensadas.R**
Arquivo principal para execução do processamento de apensadas. Neste arquivo existem funções para processar o dataframe de apensadas que será exportado como csv e o dataframe de proposições apensadas que ainda não tem sua proposição principal raiz monitorada (função *process_apensadas*).

Uma **proposição principal raiz** é a proposição principal na qual todas as outras proposições da árvore de apensamentos estão apensadas. Por exemplo, a proposição [2250675](https://www.camara.leg.br/proposicoesWeb/fichadetramitacao?idProposicao=2250675) é uma proposição apensada. A sua proposição principal é a [2123222](https://www.camara.leg.br/proposicoesWeb/fichadetramitacao?idProposicao=2123222), mas sua proposição principal raiz é a [2190084](https://www.camara.leg.br/proposicoesWeb/fichadetramitacao?idProposicao=2190084).

Neste script também existem funções internas (*.find_proposicao_raiz* *.get_proposicao_raiz*, *.get_proposicao_raiz_from_lista*) que a partir de uma lista já processada com os links (proposição apensada -> proposição raiz) recuperam as proposição principal raiz de uma proposição apensada.

### **process_lista_apensadas.R**

Este arquivo é responsável por processar uma lista que tem como chave o id da proposição apensada e como valor o id da proposição principal. Neste arquivo existe a lógica para processar e salvar essa lista em csv, bem como reaproveitar a execução anterior dessa lista para percorrer nós já percorridos. Proposições principais que já são monitoradas pelo Parlametria e que já tiveram seus ids checados em busca de apensamentos a outras proposições terão o valor "raiz" considerado na lista da árvore de apensados.

A função *process_lista_apensadas* processa as duas listas da árvore de apensados para câmara e para o senado separadamente. A função *process_lista_apensadas_por_casa* exportada pelo pacote do leggoR executa o processamento por casa, reaproveitando ou não a execução anterior. A função *fetch_proposicao_raiz* exportada pelo pacote do leggoR checa se a proposição chave já teve sua proposição principal pesquisada ou seja se está presente na lista, se não então faz a verificação usando a função agoradigital::fetch_proposicao e recuperando o campo uri_prop_principal. Este procedimento é repetido recursivamente até se encontrar a proposição que tem uri_prop_principal como NULL ou como raiz, sendo assim a proposição principal raiz.

## Arquivos de saída

### **props_apensadas.csv**
Lista de proposições apensadas que serão importadas para o BD do Parlametria.
Neste csv temos a informação da proposição apensada no Parlametria (id_leggo), da proposição principal (id_ext_prop_principal), da casa da proposição principal raiz, do id_leggo da proposição principal raiz e do id da proposição principal raiz (casa_prop_principal, id_leggo_prop_principal, id_ext_prop_principal_raiz, respectivamente), e o interesse.

### **props_apensadas_nao_monitoradas.csv**
Lista de proposições apensadas que tem proposições principais (raiz) que não são monitoradas pelo Parlametria ou não são monitoradas dentro do mesmo painel. Este arquivo é usado como log de execução e para facilitar a identificação destes casos que precisam ser observados/notados.

### **lista_apensadas_camara.csv**
Dataframe para o contexto da câmara com o par chave->valor no qual proposição apensada -> proposição raiz. Este csv é transformado em lista para o processamento das proposições apensadas.

### **lista_apensadas_senado.csv**
Dataframe para o contexto do senado com o par chave->valor no qual proposição apensada -> proposição raiz. Este csv é transformado em lista para o processamento das proposições apensadas.

