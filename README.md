[![pipeline status](https://gitlab.com/analytics-ufcg/agora-digital/badges/master/pipeline.svg)](https://gitlab.com/analytics-ufcg/agora-digital/commits/master)
[![coverage report](https://gitlab.com/analytics-ufcg/agora-digital/badges/master/coverage.svg)](https://gitlab.com/analytics-ufcg/agora-digital/commits/master)

# Ágora Digital
 Acesso, tradução e modelos usando dados do congresso nacional.
 
- [Link para página](https://analytics-ufcg.github.io/agora-digital/)

Raiz do projeto Ágora Digital

- [Documentação](/docs)
- [Scripts](/scripts)

## Instalação

Antes de instalar nosso pacote, é necessário que você tenha o [R](https://www.r-project.org/) instalado e também o [devtools](https://github.com/r-lib/devtools) que é uma ferramenta que irá ajuda-lo no processo de instalação e desenvolvimento:

```R
install.packages("devtools")
```

ou no terminal:

```
Rscript -e 'install.packages("devtools")'
```

Após isso, você pode instalar o pacote no seu ambiente R de preferência:

```R 
devtools::install_github("analytics-ufcg/agora-digital")
```
  
  ou:
```
Rscript -e 'devtools::install_github("analytics-ufcg/agora-digital")'
```


Para utilizá-lo:

```R
library(agoradigital)
```

Para verificar se toda a instalação foi feita de maneira correta, use o exemplo:

```R
agoradigital::fetch_prop("senado", 7492)
```

ou 
 
```
Rscript -e 'agoradigital::fetch_prop("senado", 7492)'
```

## Uso
Exemplos básicos de uso.


### Como capturar ids de proposições:
A maioria de nossos métodos necessita do id da proposição que você deseja extrair informações,
há várias formas de capturar esse id, serão mostradas as mais básicas:
    
  - Pelo site da câmara e do senado:
      
      Tendo como exemplo a PL 3729/2004 da *câmara*, você pode visualizar o id pela URL da página da   câmara
        
        
        https://www.camara.gov.br/proposicoesWeb/fichadetramitacao?idProposicao=257161
  
  
    O id é **257161**
      
    No *senado* é da mesma forma, tendo como exemplo a PL 441/2007, o id é **82051**
      
      
        https://www25.senado.leg.br/web/atividade/materias/-/materia/82051?o=d


    Agora você pode utilizar esse id nos métodos que precisar:
    
    
    ```R
      agoradigital::fetch_prop("senado", 7492)
    ```
   
    
### Exemplos
  Você pode encontrar mais exemplos de uso do *agoradigital* [aqui](https://github.com/analytics-ufcg/agora-digital/tree/master/docs).


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
    
## Mais informações

  Atualmente o pacote já foi testado para as seguintes proposições:
  
  - [PL 3729/2004](http://www.camara.gov.br/proposicoesWeb/fichadetramitacao?idProposicao=257161)
  - [PL 6726/16](http://www.camara.gov.br/proposicoesWeb/fichadetramitacao?idProposicao=2121442)
  - [PL 449/2016](https://www25.senado.leg.br/web/atividade/materias/-/materia/127753?o=d) 
  - [PL 229/2009](https://www25.senado.leg.br/web/atividade/materias/-/materia/91341)
  
  Podendo apresentar problemas para outras proposições.
  
## Como contribuir

  Se encontrou algum problema ou deseja fazer alguma melhoria. Por favor, abra uma [issue](https://github.com/analytics-ufcg/agora-digital/issues) e descreva o problema com clareza, se possível com exemplos que possamos reproduzir.
  
  Toda ajuda é bem vinda e de grande importância :) sinta-se à vontade.
