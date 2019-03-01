[![pipeline status](https://gitlab.com/analytics-ufcg/agora-digital/badges/master/pipeline.svg)](https://gitlab.com/analytics-ufcg/agora-digital/commits/master)
[![codecov](https://codecov.io/gh/analytics-ufcg/leggoR/branch/master/graph/badge.svg)](https://codecov.io/gh/analytics-ufcg/leggoR)

# leggoR
Acesso, tradução e modelos usando dados do congresso nacional.
 
- [Site com documentação](https://analytics-ufcg.github.io/agora-digital/public)

## Instalação

Antes de instalar nosso pacote, é necessário que você tenha o [R](https://www.r-project.org/) instalado.

  - **Aviso:** Os comandos desta seção devem ser executados dentro do interpretador de R. Mas você também pode executar qualquer código R direto no terminal usando o comando `Rscript -e`. Exemplo: 
      ```
      Rscript -e 'agoradigital::fetch_proposicao(82051, "senado")'
      ```

Recomendamos que você instale o pacote [devtools](https://github.com/r-lib/devtools) que é uma ferramenta que irá te ajudar no processo de instalação e desenvolvimento.

```R
install.packages("devtools")
```

Após isso, você pode instalar o pacote no seu ambiente R de preferência:

```R 
devtools::install_github("analytics-ufcg/agora-digital")
```

Para utilizá-lo:

```R
library(agoradigital)
```

Para verificar se toda a instalação foi feita de maneira correta, use o exemplo:

```R
agoradigital::fetch_proposicao(82051, "senado")
```

## Uso
Exemplos básicos de uso.


### Como capturar ids de proposições:
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
    
### Exemplos
Você pode encontrar mais exemplos de uso do *agoradigital* [aqui](https://github.com/analytics-ufcg/agora-digital/tree/master/vignettes).

## Estrutura do projeto

* O Código principal do pacote você pode encontrar em [R/](https://github.com/analytics-ufcg/agora-digital/tree/master/R)
* Exemplos de csv gerados pode se ver em [data/](https://github.com/analytics-ufcg/agora-digital/tree/master/data)
* Em [docs/](https://github.com/analytics-ufcg/agora-digital/tree/master/docs) pode se visualizar reports de Pls já
 testadas.
* Em [scripts/](https://github.com/analytics-ufcg/agora-digital/tree/master/scripts) pode se ver alguns scripts que facilitam o processo de importar e geração de dados.
* [tests/](https://github.com/analytics-ufcg/agora-digital/tree/master/tests) contém testes de uso.
* [view/](https://github.com/analytics-ufcg/agora-digital/tree/master/view) contém alguns exemplos de visualizações que utilizam o agoradigital
* [vignettes/](https://github.com/analytics-ufcg/agora-digital/tree/master/vignettes) exemplos.
 
## Comandos

Baixar dados e gerar relatórios:

    $ ./scripts/build_all.R

Gerar documentação:

    > devtools::document()

Rodar testes:

    > devtools::test()

Mostrar cobertura do código:

    > library(covr);package_coverage()

Fazer verificações gerais um pouco mais bonitinhas:

    > rcmdcheck::rcmdcheck()

Rodar o linter:
    
    > devtools::lint()
    
## Como verificar o package localmente

1. É necessário garantir que todas as suas funções estão documentadas e disponíveis para isso, para isso, você precisa adicionar `@export` a documentação da função para garantir que essa função será exportada, apois isso, gere a documentação do package.
  
  > devtools::document()
  
Para rodar os testes: 

2. Teste o pacote
 
 > devtools::test()
  
3. Instale o pacote a partir da branch que deseja:

 > devtools::install_github('analytics-ufcg/agora-digital@nome_da_branch')
 
Depois faça os testes necessários para garantir que o pacote está funcionando conforme esperado.
  
## Como contribuir

  Se encontrou algum problema ou deseja fazer alguma melhoria. Por favor, abra uma [issue](https://github.com/analytics-ufcg/agora-digital/issues) e descreva o problema com clareza, se possível com exemplos que possamos reproduzir.
  
  Toda ajuda é bem vinda e de grande importância :) sinta-se à vontade.
