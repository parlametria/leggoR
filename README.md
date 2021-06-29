[![pipeline status](https://gitlab.com/analytics-ufcg/agora-digital/badges/master/pipeline.svg)](https://gitlab.com/parlametria/leggoR/commits/master)
[![codecov](https://codecov.io/gh/parlametria/leggoR/branch/master/graph/badge.svg?token=wktXOtRlEI)](https://codecov.io/gh/parlametria/leggoR)

# leggoR
Acesso, tradução e modelos usando dados do congresso nacional.
 
- [Site com documentação](https://parlametria.github.io/leggoR/public/)
- [Descrição dos dados gerados](docs/dados/Sobre.md) e usados pelo [Parlametria](https://painel.parlametria.org.br/paineis).

## Objetivo e módulos principais

Este repositório tem por objetivo principal prover o conjunto de funções e serviços que permitem capturar e processar dados de proposições e parlamentares do congresso nacional. Para isto existem dois módulos principais:
 -  O **pacote agoradigital** que exporta funções que acessam e processam dados do Congresso;

 -  O **módulo scripts de atualização dos dados do painel** que executa e salva os dados capturados e processados.

### Estrutura do repositório

* O código principal do pacote você pode encontrar em [R/](https://github.com/parlametria/leggoR/tree/master/R)
* Em [scripts/](https://github.com/parlametria/leggoR/tree/master/scripts) se encontram alguns submódulos responsávels por orquestrar a execução do processamento de dados do repositório e salvar em CSV.
* Em [docs/reports/](https://github.com/parlametria/leggoR/tree/master/docs/reports) pode se visualizar reports de Pls já testadas (podem existir informações desatualizadas).
* [tests/](https://github.com/parlametria/leggoR/tree/master/tests) contém testes de uso.
* [view/](https://github.com/parlametria/leggoR/tree/master/view) contém alguns exemplos de visualizações que utilizam o agoradigital


## O pacote agoradigital

O pacote agoradigital exporta várias funções que extraem e processam os dados do Congresso Nacional.

### Instalação

Antes de instalar nosso pacote, é necessário que você tenha o [R](https://www.r-project.org/) instalado.

  > **Aviso:** Os comandos desta seção devem ser executados dentro do interpretador de R. Mas você também pode executar qualquer código R direto no terminal usando o comando `Rscript -e`. Exemplo: 
    ```Rscript -e 'agoradigital::fetch_proposicao(82051, "senado")'
    ```

Recomendamos que você instale o pacote [devtools](https://github.com/r-lib/devtools) que é uma ferramenta que irá te ajudar no processo de instalação e desenvolvimento.

```R
install.packages("devtools")
```

Após isso, você pode instalar o pacote no seu ambiente R de preferência:

```R 
devtools::install_github("parlametria/leggoR")
```

Para utilizá-lo:

```R
library(agoradigital)
```

Para verificar se toda a instalação foi feita de maneira correta, use o exemplo:

```R
agoradigital::fetch_proposicao(82051, "senado")
```

### Uso
Exemplos básicos de uso.

#### Como capturar ids de proposições:
A maioria de nossos métodos necessita do id da proposição que você deseja extrair informações,
há várias formas de capturar esse id, serão mostradas as mais básicas:
    
  - Pelo site da câmara e do senado:
      
      Tendo como exemplo o PL 3729/2004 da *Câmara*, você pode visualizar o id pela URL da página da Câmara. Nesse caso o id é **257161**:
        
        https://www.camara.gov.br/proposicoesWeb/fichadetramitacao?idProposicao=257161
      
    No *Senado* é da mesma forma, tendo como exemplo o PLS 441/2007, o id é **82051**:
      
        https://www25.senado.leg.br/web/atividade/materias/-/materia/82051

    Agora você pode utilizar esses ids nos métodos que precisar. Exemplo:
    
    ```R
    agoradigital::fetch_proposicao(82051, "senado")
    ```

### Para contribuir com o pacote

É necessário garantir que todas as suas funções estejam documentadas e disponíveis. Para isso, você precisa adicionar `@export` a documentação da função para garantir que essa função será exportada, como em qualquer pacote R. Após isso:

1. Gere a documentação do package.
  
  > ```devtools::document()```
  
Para rodar os testes: 

2. Teste o pacote
 
 > ```devtools::test()```
  
3. Instale o pacote a partir da branch que deseja:

 > ```devtools::install_github('parlametria/leggoR@nome_da_branch')```
 
Depois faça os testes necessários para garantir que o pacote está funcionando conforme esperado. 

Abaixo temos alguns comandos extras que podem ser úteis no desenvolvimento do pacote:

 - Mostrar cobertura do código:
> ```library(covr);package_coverage()```

 - Fazer verificações gerais um pouco mais bonitinhas:

> ```rcmdcheck::rcmdcheck()```

 - Rodar o linter:
  > ```devtools::lint()```

 - Para regerar o site da documentação:
  > ```pkgdown::build_site(override = list(destination = "public"))```
  
## Scripts de atualização dos dados do Painel

O módulo scripts é responsável por englobar diversos submódulos responsáveis por executar a atualização dos dados utilizados pelo Painel Parlametria e geralmente chamados pelo orquestrador [leggo-geral](https://github.com/parlametria/leggo-geral). Explicamos mais detalhes no [README deste diretório](https://github.com/parlametria/leggoR/tree/master/scripts).
    

# Docker

Este repositório possui uma configuração do docker para facilitar o desenvolvimento, teste e deploy do leggoR considerando seus módulos principais: pacote agoradigital, módulo de scripts.

Para usar o docker será necessário:

1. Instale o [docker](https://docs.docker.com/install/) e o [docker-compose](https://docs.docker.com/compose/install/). 

2. Configure as variáveis de ambiente: crie uma cópia do arquivo .`env.sample` e o renomeie para `.env`. Em seguida preencha as variáveis com os valores adequados para execução. Atualmente temos a seguinte variável de ambiente:

> **APP_SECRET** = hash da url do *bot Voz Ativa* para onde mandamos os resultados das atualizações e mensagens de novas proposições apensadas.

3. Realize o build da imagem (na primeira vez esse passo pode demorar bastante dependendo da sua conexão com a internet).
  > ```docker-compose build```

Agora é possível usar o rmod (nome do serviço levantado pelo docker-compose) para executar o código do leggoR:

Exemplo:

```
docker-compose run --rm rmod Rscript scripts/fetch_updated_bills_data.R -e <export_path> -f 4
```
O `<export_path>` é o caminho onde os csvs do resultado serão salvos.

Sempre que o código do repositório for alterado é necessário realizar um novo build da imagem docker para que as mudanças sejam refletidas quando o container estiver em execução.

É possível com algumas alterações criar um volume entre o código do repositório e o container para facilitar o teste e desenvolvimento do leggoR mas isto ainda não está implementado.

## Como contribuir

Se encontrou algum problema ou deseja fazer alguma melhoria. Por favor, abra uma [issue](https://github.com/parlametria/leggoR/issues) e descreva o problema com clareza, se possível com exemplos que possamos reproduzir.
  
Toda ajuda é bem vinda e de grande importância :) sinta-se à vontade.