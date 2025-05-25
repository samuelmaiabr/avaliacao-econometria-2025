# Avaliação 1 – Econometria I (2025)

Este repositório contém os scripts em R utilizados na Avaliação 1 da disciplina de Econometria I, ministrada pela Profª Ana Hermeto (Cedeplar/UFMG) no primeiro semestre de 2025. O exercício envolve a aplicação do método de Mínimos Quadrados Ordinários (MQO) a dados da PNAD Contínua, com foco em modelos de regressão simples e múltipla.

## Dados

A base utilizada neste exercício é um recorte da PNAD Contínua (Pesquisa Nacional por Amostra de Domicílios Contínua), de acesso público, disponibilizada pelo IBGE. O arquivo original utilizado se chama `pnadc_2023_5_capitais.dta` e contém dados da quinta entrevista referentes ao ano de 2023, com foco em capitais selecionadas.

Para reproduzir os resultados, recomendo:

1. Obter os microdados brutos diretamente do site do IBGE:  
   https://www.ibge.gov.br/estatisticas/sociais/trabalho/9173-pesquisa-nacional-por-amostra-de-domicilios-continua-trimestral.html

2. Garantir que o arquivo `pnadc_2023_5_capitais.dta` esteja disponível no diretório adequado. 


## Estrutura e Reprodutibilidade

- `samuel-topico-1-2.R`: script organizado com comentários e numeração coerente com o exercício final, permitindo a replicação fiel dos resultados apresentados. 

- A versão do R, sistema operacional, pacotes utilizados e seus caminhos de instalação estão documentados no arquivo [`session-info.txt`](session-info.txt), gerado com `devtools::session_info()`.
