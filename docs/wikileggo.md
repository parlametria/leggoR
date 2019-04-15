# Wiki Leggo
### Sobre o projeto
##### O que é o Leggo?
Plataforma digital com o objetivo de informar o cidadão o andamento das proposições que estão sendo discutidas no congresso nacional através de parâmetros como temperatura das proposições, progresso, autores, relator entre outros.

##### O que é utilizado:
- Repositórios:
    * [rcongresso](https://github.com/analytics-ufcg/rcongresso)
    * [leggoR](https://github.com/analytics-ufcg/leggoR)
    * [leggo-backend](https://github.com/analytics-ufcg/leggo-backend)
    * [leggo-frontend](https://github.com/analytics-ufcg/leggo-frontend)
    * [leggo-content](https://github.com/analytics-ufcg/leggo-content)
    * [leggo-geral](https://github.com/analytics-ufcg/leggo-geral)
* [Arquitetura do sistema](https://github.com/analytics-ufcg/leggo-geral/blob/master/arquitetura.md)
* Ferramentas:
    * Pacote rcongresso: responsável por capturar dados da api do congresso nacional
    * Pacote agora-digital(leggoR): responsável por capturar os dados do rcongresso e adaptá-los ao contexto do projeto como garimpar informações das proposições com seus respectivos autores, temas, status de casa e etc.
    * Django → backend
    * Vuejs → frontend
    * Docker
    
### Entendendo um pouco a estrutura política no Brasil
#### 1. Estrutura política
A estrutura do nosso país é composta por 3 poderes, judiciário, legislativo e executivo, sendo que cada um desses poderes possuem responsabilidades distintas, mas que, de forma harmônica, funcionam sob os âmbitos das áreas do governo federal, estadual e municipal
Executivo:
Judiciário:
Legislativo: a seguir

#### 2. O que é o sistema legislativo?
Sistema responsável por elaborar as leis que regem o país e monitorar as decisões tomadas pelo poder executivo
Formado pelo Congresso Nacional, que consequentemente, é formado por senadores e deputados.

#### 3. O que são proposições?
Toda matéria submetida à apreciação do senado, câmara ou congresso. Existem alguns tipos de proposição, sendo elas:
* PEC
* Leis complementares
* Leis ordinárias
* Medidas provisórias
* Decreto legislativo
* Resolução
A sequência acima foi proposital, até porque há uma hierarquia de “importância” entre elas. 
### [Colocar figura da pirâmide de kelsen]

#### 4. Regime de tramitação(RT) vs Regime de apreciação(RA)
* RT: É o status das proposições no parlamento. Sendo assim, de acordo com a urgência da proposição, será refletido diretamente na velocidade de votação para a aprovação ou não da proposição, sendo classificados da seguinte forma:
    * Urgência
    * Prioridade
    * Ordinário
* RA: se a proposição será apreciada apenas pelas comissões (regime conclusivo), ou também pelo plenário da casa.

#### 5. Relator
É o parlamentar designado pelo presidente da comissão para apresentar parecer sobre matéria de competência do colegiado
O relator ao apresentar o parecer vai ser aprovando, reprovando ou substitutivo (uma espécie de relatório para reestruturação completa do projeto em questão).
* vídeo rápido: [Qual o papel de um relator](https://www.youtube.com/watch?v=yDIRCb_LWuQ&list=PLysoTmRxzFnVfy1J8qY2VzGi1k3dvzN75&t=0s&index=6)

#### 6. Comissões
* Permanentes: discutem e votam pareceres técnicos sobre as propostas de leis apresentadas antes de serem levadas ao plenário.
* Temporárias: há um prazo fixado para o final da criação ou se é alcançado o objetivo.
    * Comissões Especiais: emitir pareceres sobre proposições em situações especiais (PEC, Códigos...) ou oferecer estudos sobre temas específicos (4 ou mais comissões designadas para um determinado projeto, ex: CPI)
    * Comissões Externas: acompanham assuntos específicos fora da sede da casa
    * Comissões Parlamentares de Inquérito: investigar fato determinado
* vídeo rápido: [As comissões](https://www.youtube.com/watch?v=sX4uWFJnwMQ&list=PLysoTmRxzFnVfy1J8qY2VzGi1k3dvzN75&index=4&t=0s)

#### 7. Emenda vs Ementa:
* Emenda: 
* Ementa: 

 








