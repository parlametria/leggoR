# -*- coding: utf-8 -*-
#http://legis.senado.leg.br/dadosabertos
#GET /materia/movimentacoes/{codigo} O link será executado em nova janela
#Obtém os a movimentação da matéria, incluindo tramitação, prazos, despachos, situação

#GET /materia/votacoes/{codigo} O link será executado em nova janela
#Obtém as votações de uma matéria

from urllib2 import URLError
from urllib2 import HTTPError
import urllib2
import urllib


def format_url(url_base, information, num_prop):
	return url_base + information + "/" + str(num_prop)

def get(url_base, information, num_prop, response_format):
	url = format_url(url_base, votacoes, num_prop)
	req = urllib2.Request(url, headers={"Accept" : response_format})

	f = urllib2.urlopen(req)

	contents = f.read()
	f.close()
	print contents

try:

	movimentacoes = "movimentacoes"
	votacoes = "votacoes"
	url_base = "http://legis.senado.leg.br/dadosabertos/materia/"
	num_prop = 129808
	response_format = "application/json"

	get(url_base, movimentacoes, num_prop, response_format)

except HTTPError, e:
	print("Ocorreu um erro ao requisitar o conteúdo do servidor!\n")
	print("Cod.: ", e.code)

except URLError, e:
	print("URL inválido!\n")
	print("Mensagem: ", e.reason)
