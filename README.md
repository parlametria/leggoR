[![pipeline status](https://gitlab.com/analytics-ufcg/agora-digital/badges/master/pipeline.svg)](https://gitlab.com/analytics-ufcg/agora-digital/commits/master)
[![coverage report](https://gitlab.com/analytics-ufcg/agora-digital/badges/master/coverage.svg)](https://gitlab.com/analytics-ufcg/agora-digital/commits/master)

# Ágora Digital

- [Link para página](https://analytics-ufcg.github.io/agora-digital/)

Raiz do projeto Ágora Digital

- [Documentação](/docs)
- [Scripts](/scripts)

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
