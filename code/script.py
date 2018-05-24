import http.client
import pandas as pd
import json

URL = "https://dadosabertos.camara.leg.br/api/v2/"

# GET /proposicoes/{id} - Informações detalhadas sobre uma proposição específica
# GET /proposicoes/{id}/tramitacoes - O histórico de passos na tramitação de uma proposta

connection = http.client.HTTPSConnection("dadosabertos.camara.leg.br")
id_proposicao = '15743'

connection.request('GET', '/api/v2/proposicoes/' + id_proposicao)
proposicao = connection.getresponse().read().decode('utf-8')
proposicao = json.loads(proposicao)

connection.request('GET', '/api/v2/proposicoes/' + id_proposicao + '/tramitacoes')
tramitacao = connection.getresponse().read().decode('utf-8')
tramitacao = json.loads(tramitacao)

for prop in proposicao['dados']["statusProposicao"]:
    proposicao['dados']["status_proposicao_" + prop] = proposicao['dados']["statusProposicao"][prop]

del(proposicao['dados']["statusProposicao"])

tramitacao = pd.DataFrame(tramitacao['dados'])
proposicao = pd.DataFrame(proposicao['dados'], range(len(proposicao['dados'])))


arq = open('tramitacao.csv', 'w')
arq.write(tramitacao.to_csv(index=False))
arq.close()

arq = open('proposicao.csv', 'w')
arq.write(proposicao.to_csv(index=False))
arq.close()
