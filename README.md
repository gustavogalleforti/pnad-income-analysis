# Impacto da crise de 2016 sobre renda e desemprego (PNAD Contínua)

Análise do efeito da crise econômica de 2016 sobre a renda do trabalho e o desemprego. O trabalho compara o 3º trimestre de 2013 (pré-crise) com o 3º trimestre de 2016. A amostra são chefes de domicílio de 21 a 50 anos em áreas urbanas metropolitanas.

## Métodos

- Microdados da PNAD Contínua baixados direto do IBGE (pacote `PNADcIBGE`)
- Desenho amostral complexo com `survey`: estrato, UPA e peso de calibração `V1028`
- Renda de 2016 deflacionada para R$ de setembro de 2013 pelo IPCA
- Testes t ponderados para a diferença de renda real e desemprego entre os períodos
- Gaps por sexo, raça/cor e escolaridade, com modelos de interação período x grupo (`svyglm`) para testar se os gaps mudaram entre 2013 e 2016

## Estrutura

```
├── R/
│   ├── 00_setup.R               # pacotes e funções auxiliares
│   ├── 01_dados.R               # download, filtros, variáveis e desenho amostral
│   ├── 02_renda_desemprego.R    # renda real e desemprego, 2013 vs 2016
│   ├── 03_gaps_grupos.R         # gaps por sexo, raça/cor e escolaridade
│   └── 04_resumo_marcadores.R   # tabela resumo por marcador
└── outputs/                     # tabelas geradas (.csv, .html)
```

## Como executar

```r
# a partir da raiz do repositório; roda 00 -> 04 em sequência
source("R/04_resumo_marcadores.R")
```

Cada script carrega o anterior com `source()`. Os pacotes ausentes são instalados pelo `00_setup.R`.

## Referências

- IBGE. *Pesquisa Nacional por Amostra de Domicílios Contínua*. Rio de Janeiro, 2013 e 2016.
- Lumley, T. (2010). *Complex Surveys: A Guide to Analysis Using R*. Wiley.
