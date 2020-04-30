# Dados usados pelo Leggo

Este documento tem o objetivo de apresentar informações iniciais sobre os dados processados pelo Leggo através do pacote agora-digital e por outros repositórios do projeto leggo ([leggoTrends](https://github.com/parlametria/leggoTrends), [leggo-content](https://github.com/parlametria/leggo-content)) e que são usados na aplicação leggo.org.br.

Existem outros CSV's que podem ser gerados pelo pacote agora-digital, mas apenas aqueles usados para a construção do banco de dados da aplicação leggo.org.br serão descritos aqui.

## Descrição dos CSV's

- **proposicoes.csv**: este é um dos principais dados gerados pelo repositório. Contém a lista de proposições analisadas pelo Leggo com suas respectivas informações associadas. Cada linha deste csv é uma proposição em alguma casa (Câmara ou Senado). Caso uma mesma matéria tiver tramitado nas duas casas então haverá uma ligação entre as proposições através da coluna id_leggo. No leggo, a proposição possui um id próprio (id_leggo) e que nesse caso conecta a proposição na tramitação da câmara e na tramitação do senado. Outras colunas importantes desse csv são as de data de apresentação, ementa, autores da proposição, regime de tramitação, forma de apreciação, dentre outras. A coluna id_ext representa o id externo na proposição na casa correspondente (presente na coluna casa).

- **trams.csv**: CSV que apresenta detalhes de todos os eventos que aconteceram nas proposições analisadas. Os eventos são capturados no monitoramento da tramitação da proposição. O eventos possuem um nível de relevância (1, 2 ou 3), acontecem em um local (sigla_local), possuem título, status, fase, situação, podem ter origem e destino dentro da casa local, dentre outras colunas.

- **hists_temperatura.csv**: CSV com o histórico da temperatura das proposições analisadas pelo Leggo. Neste CSV cada linha é a temperatura calculada para uma proposição (id_leggo) em um período (periodo). A temperatura é uma informação gerada pelo pacote agora-digital usando a classificação dos eventos e seus respecitivos pesos. Quanto mais eventos relevantes a proposição tiver maior será sua temperatura no período. Portanto, a temperatura é uma medida de quanto a proposição está sendo discutida, mexida e efetivamente em tramitação no Congresso.

- **coautorias_nodes.csv**: CSV com o nós usados na visualização da rede de influência associada as proposições. Cada linha deste CSV é um parlamentar (id_autor) que atuou em uma proposição (id_leggo) com a apresentação de documentos. Existem informações específicas do parlamentar como partido, uf, bancada, nome eleitoral e node size (peso do parlamentar na atuação da proposição).

- **coautorias_edges.csv**: CSV com as ligações/arestas entre parlamentares/nós usadas na visualização da rede de influência associada as proposições. Cada linha deste CSV é uma ligação dois parlamentar (source e target) em uma proposição (id_leggo). Esta ligação tem um peso calculado baseado na quantidade de documentos apresentados juntos (para cada proposição o peso de uma ligação de autores (coautoria) é dada pela fórmula: 1/nº de autores na proposição). O peso da ligação entre dois autores se dá pela soma dos pesos das coautorias desses autores em uma proposição.

- **autorias.csv**: CSV com a lista de documentos e seus respectivos autores. Cada linha deste CSV é um documento (id_documento), associado a um autor (id_autor) e a uma proposição (id_leggo). Existe informação também do tipo de documento, link para o inteiro teor, dentre outras.

- **pautas.csv**: CSV com a lista de proposições que estão na pauta em alguma casa do Congresso. Apresenta informação de qual é a proposição (id_ext) e em qual local (local), a sigla da proposição, a semana e o ano.

- **progressos.csv**: CSV com o caminho da tramitação das proposições analisadas pelo Leggo. Cada linha do CSV contém informação da proposição (id_ext, casa), da fase de tramitação (fase_global), do local (local), se pulou a etapa ou não, dentro outras.

- **emendas.csv**: CSV com as emendas ligadas as proposições analisadas pelo Leggo. Cada linha do CSV é uma emenda (codigo_emenda) apresentada a uma proposição (id_ext, casa) e inclui informações da data de apresentação, autor, inteiro teor, tipo e distância (A distância é calculada com o axílio do repositório [leggo-content](https://github.com/parlametria/leggo-content) que analisa o conteúdo das emendas com o objetivo de identificar o nível das mudanças realizadas quando comparadas com o texto original).

- **atores.csv**: CSV com a lista de atores/parlamentares que atuaram nas proposições analisadas pelo Leggo. Cada linha deste CSV é um parlamentar (id_autor) que atuou em uma proposição (id_ext, casa) e contém informações como o tipo de autoria (o tipo de documento que foi feito pelo autor), o nome do autor, partido, uf, quantidade de documentos e o peso dos documentos (considerando o peso da coautoria de documentos).

- **comissoes.csv**: CSV com a composição das Comissões capturadas pelo Leggo. Cada linha deste CSV é um parlamentar (id_parlamentar) que possui um cargo numa comissão (sigla) de uma casa do Congresso (casa). Também há o link para a foto oficial do parlamentar na casa correspondente.

- **pressao.csv**: CSV com o histório da pressão capturada para as proposições do Leggo. A pressão é uma medida calculada com o auxílio do repositório [leggoTrends](https://github.com/parlametria/leggoTrends) e leva em consideração a pressão externa (fora do Congresso) que a proposição tem. Isso envolve no quanto a proposição está sendo pesquisada (Google Trends) e no quanto está sendo citada no Twitter (análise dos tweets dos parlamentares).
Cada linha deste CSV tem informações das pressões considerando uma proposição (id_leggo) associada a um interesse (interesse) em um determinado período (date). popularity é a coluna que combina os valores de pressão no Google Trends e no Twitter.

- **interesses.csv**: CSV com a associação entre as proposições analisadas pelo Leggo e qual o interesse (pode ser mais de um) que ela pertence. Um interesse é um assunto geral ao qual uma determinada proposição pertence. Existe o interesse geral ('leggo') que foi o primeiro abordado pelo Leggo, mas outros interesses já estão sendo analisados pelo Leggo. Cada interesse possui um conjunto de proposições associadas. O apelido, tema, palavras-chave e outras informações são associadas a uma proposição a depender do interesse.


Estes foram os dados principais gerados pelos repositórios do projeto Leggo. Existem muitos outros CSV's intermediários que são usados no fluxo de processamento dos dados preparados para o leggo.org.br.

[Contribuições](https://github.com/parlametria/leggoR) a esta documentação são bem-vindas! 
