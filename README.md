# Avaliação 1 – Econometria I (2025)

Este repositório contém os scripts em R utilizados na Avaliação 1 da disciplina de Econometria I, ministrada pela Profª Ana Hermeto (Cedeplar/UFMG) no primeiro semestre de 2025. O exercício envolve a aplicação do método de Mínimos Quadrados Ordinários (MQO) a dados da PNAD Contínua, com foco em modelos de regressão simples e múltipla.

## Estrutura

- `samuel-topico-1.R`: script completo e sequencial, com referências explícitas às seções do relatório (ex: 2.4.1), facilitando a navegação entre texto e código.

## Dados

A base utilizada neste exercício é um recorte da PNAD Contínua (Pesquisa Nacional por Amostra de Domicílios Contínua), de acesso público, disponibilizada pelo IBGE. O arquivo original utilizado chama-se `pnadc_2023_5_capitais.dta` e contém dados da quinta entrevista referentes ao ano de 2023, com foco em capitais selecionadas.

Para reproduzir os resultados, recomenda-se:

1. Obter os microdados brutos diretamente do site do IBGE:  
   https://www.ibge.gov.br/estatisticas/sociais/trabalho/9173-pesquisa-nacional-por-amostra-de-domicilios-continua-trimestral.html

2. Garantir que o arquivo `pnadc_2023_5_capitais.dta` esteja disponível no diretório adequado. 

## Reprodutibilidade

Os scripts foram organizados com comentários e numeração coerente com o relatório final, permitindo a replicação fiel dos resultados apresentados. 

## Informações da sessão R

A versão do R, sistema operacional, pacotes utilizados e seus caminhos de instalação estão documentados no arquivo [`session-info.txt`](session-info.txt), gerado com `devtools::session_info()`. Esse registro complementa os scripts e reforça o compromisso com a reprodutibilidade computacional.
