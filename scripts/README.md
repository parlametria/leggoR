# Módulo de scripts com os pipeline de dados

Este é o módulo do leggoR que possui todos os scripts utilizados no pipeline de atualização dos dados do Painel Parlametria.

Monitoramos proposições de interesse de diversos painéis diferentes (que a nível de código são chamados de **interesses**), como reforma administrativa, licenciamento ambiental, direitos humanos, etc., cada um contendo um conjunto específico de proposições. Abaixo explicamos um passo-a-passo de como geramos os dados de entrada de vários scripts:

1. Obter as planilhas de entrada;
2. Processar os dados de entrada;
3. Executar scripts.

## 1. Obter as planilhas de entrada

### Proposições de entrada

Este conjunto de proposições é essencial e o novo input inicial para que outros dados sejam gerados e permitindo a chamada da maioria dos scripts.

Temos um csv de exemplo com alguns dados de proposições que usamos como input em `inst/extdata/proposicoes_input.csv`, e segue o seguinte formato:

proposicao | id_camara | id_senado | apelido | tema | advocacy_link | keywords | tipo_agenda | prioridade | explicacao_projeto
-- | -- | -- | -- | -- | -- | -- | -- | -- | --
PEC 205/2012 | 553099 |   |   |   |   |   |   |   |  


### Interesses de entrada

Como falamos anteriormente, temos um conjunto de proposições para cada interesse, então este mapeamento é feito em uma planilha, também de input para a geração e processamento de vários dados.

Também temos um exemplo desses dados em `inst/extdata/interesses_input.csv`, e segue o formato abaixo:

| interesse | url | nome | descricao |
| -- | -- | -- | -- |
| painel | ./inst/ext_data/proposicoes_input.csv | Proposições | Lista de proposições|


Perceba que a **url** passada na planilha é o endereço do caminho do csv com as proposições a serem processadas.

## 2. Processar os dados de entrada

Uma vez obtidos os dados, precisamos processar os dados das proposições em um formato que seja aceito pelos scripts, chamados (por padrão, mas fica a critério escolher outros nomes) de `pls_interesses.csv` e `interesses.csv`. Isso é feito em duas etapas, e uma terceira que atualiza dos dados de `parlamentares.csv`:

1. Execute o script que gera `pls_interesses.csv`:
```{r}
Rscript scripts/interesses/export_pls_leggo.R -u <interesses_input_filepath> -e <pls_interesses_filepath>
```

2. Execute o script que gera `interesses.csv`:
```{r}
Rscript scripts/interesses/export_mapeamento_interesses.R -u <interesses_input_filepath> -p <pls_interesses_filepath> -e <interesses_filepath>
```

3. Execute o script que gera os dados dos `parlamentares.csv`, que também é utilizado como entrada em alguns scripts:

```{r}
Rscript scripts/parlamentares/update_parlamentares.R -p <export_folderpath>
```

## 3. Executar scripts

Uma vez processados os csvs de entrada no formato aceito para a maioria dos scripts utilizados no nosso pipeline de atualização, exemplicaremos a sua utilização no script `fetch_updated_bills_data.R`, que baixa dados relacionados a várias coisas, como proposições, emendas, autores, relatores e comissões. Para mais informações, acesse o help do script.

No contexto desta explicação, utilizaremos este script para baixar os **dados principais de proposições**, como seus metadados, tramitações, progressos, histórico de temperatura e locais atuais. Para isso:

```{r}
Rscript scripts/fetch_updated_bills_data.R -p <pls_interesses_filepath> -f 2 -e <export_datapath>  
```
O `<export_datapath>` é o caminho da pasta onde todos os csvs com os dados de proposições serão salvos. Uma observação sobre este script é que ele internamente lê os dados de parlamentares, então o csv de parlamentares deve estar dentro desta pasta.