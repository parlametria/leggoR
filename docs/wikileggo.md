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
A estrutura do nosso país é composta por 3 poderes, judiciário, legislativo e executivo, sendo que cada um desses poderes possuem responsabilidades distintas, mas que, de forma harmônica, funcionam sob os âmbitos das áreas do governo federal, estadual e municipal.
* **Executivo:** é o poder do estado que, nos moldes da constituição de um país, possui a atribuição de governar o povo e administrar os interesses públicos, cumprindo fielmente as ordenações legais
* **Judiciário:** exercido pelos juízes e possui a capacidade e a prerrogativa de julgar, de acordo com as regras constitucionais e leis criadas pelo poder legislativo em determinado país
* **Legislativo:** é aquele que tem num país a tarefa de legislar, ou seja, fazer as leis

#### 2. O que é o sistema legislativo?
Sistema responsável por elaborar as leis que regem o país e monitorar as decisões tomadas pelo poder executivo
Formado pelo Congresso Nacional, que consequentemente, é formado por senadores e deputados.

#### 3. Quem pode apresentar um projeto de lei?
* Presidente da República
* Deputado ou Senador
* Comissão do Senado ou da Câmara 

Ou ainda, em matérias específicas:
* STF, Tribunais Superiores e o Procurador Geral da República

#### 4. O que são proposições?
Toda matéria submetida à apreciação do senado, câmara ou congresso. Existem alguns tipos de proposição, sendo elas:
* Propostas de Emenda à Constituição (PEC)
* Projetos
  * Leis complementares
  * Leis ordinárias
  * Medidas provisórias
  * Decreto legislativo
  * Resolução

A sequência acima foi proposital, até porque há uma hierarquia de “importância” entre elas. 
### ![pirâmide de kelsen](https://github.com/analytics-ufcg/leggoR/blob/wiki-leggo/docs/piramide-kelsen.png)

#### 5. Regime de tramitação (RT) vs Regime de apreciação (RA)
* **RT:** É o status das proposições no parlamento. Sendo assim, de acordo com a urgência da proposição, será refletido diretamente na velocidade de votação para a aprovação ou não da proposição, sendo classificados da seguinte forma:
    * Urgência: tramitação com **maior** urgência
    * Prioridade: tramitação com **alguma** urgência
    * Ordinário: tramitação padrão, de **menor** urgência
* **RA:** se a proposição será apreciada apenas pelas comissões (regime conclusivo), ou também pelo plenário da casa.

#### 6. Relator
É o parlamentar designado pelo presidente da comissão para apresentar parecer sobre matéria de competência do colegiado
O relator ao apresentar o parecer vai ser aprovando, reprovando ou substitutivo (uma espécie de relatório para reestruturação completa do projeto em questão).
* vídeo rápido: [Qual o papel de um relator?](https://www.youtube.com/watch?v=yDIRCb_LWuQ&list=PLysoTmRxzFnVfy1J8qY2VzGi1k3dvzN75&t=0s&index=6)

#### 7. Comissões
* **Permanentes:** discutem e votam pareceres técnicos sobre as propostas de leis apresentadas antes de serem levadas ao plenário
* **Temporárias:** há um prazo fixado para o final da criação ou se é alcançado o objetivo
    * Comissões Especiais: emitir pareceres sobre proposições em situações especiais (PEC, Códigos...) ou oferecer estudos sobre temas específicos (4 ou mais comissões designadas para um determinado projeto, ex: CPI)
    * Comissões Externas: acompanham assuntos específicos fora da sede da casa
    * Comissões Parlamentares de Inquérito: investigar fato determinado
* vídeo rápido: [As comissões](https://www.youtube.com/watch?v=sX4uWFJnwMQ&list=PLysoTmRxzFnVfy1J8qY2VzGi1k3dvzN75&index=4&t=0s)

#### 8. Ementa vs Emenda:
* **Ementa:** uma breve apresentação dos pontos relevantes de uma proposição
* **Emenda:** destina-se a alterar o texto da proposição principal, seja suprimindo ou acrescentando disposições, seja modificando ou substituindo as já existentes. Emendas são também espécies do gênero "proposição", mas têm um caráter acessório, ficando sempre vinculadas a uma proposição de tipo principal


 








