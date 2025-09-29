# Impacto Econômico da Margem Equatorial

Este projeto estima os impactos econômicos da produção de petróleo na Margem Equatorial (Amapá, Pará, Maranhão, Ceará, Rio Grande do Norte) entre 2026 e 2030. A análise é feita com base em:

- Modelos de **Event Study** com efeitos fixos (`fixest`)
- Projeções estocásticas via **Monte Carlo**
- Dados reais de PIB, emprego, royalties, petróleo e câmbio

## 📦 Variáveis

| Variável        | Descrição                                         |
|------------------|--------------------------------------------------|
| `pib_pc`         | PIB per capita (preços constantes, R$)           |
| `emprego`        | Estoque de emprego formal (CAGED)                |
| `capex`          | Investimento previsto na produção (CAPEX)        |
| `royalties`      | Royalties estimados (ANP)                        |
| `brent`          | Preço médio anual do petróleo Brent              |
| `cambio`         | Taxa de câmbio R$/US$ (média anual)              |
| `selic`          | Taxa de juros básica (Selic, média anual)        |
| `fase_producao`  | Dummy para estados em fase de produção (g=2028)  |

## 📊 Resultados

- Projeção de **PIB per capita** e **emprego formal** com exploração
- Gráficos com intervalos de confiança (P5–P95)
- Mapa temático da variação percentual por estado

## 📁 Estrutura

- `scripts/`: códigos R para limpeza, modelagem e projeção
- `data/`: base de dados original
- `outputs/`: gráficos finais prontos para publicação

## 📦 Requisitos

- R >= 4.2
- Pacotes: `fixest`, `ggplot2`, `dplyr`, `mvtnorm`, `geobr`, `janitor`, `purrr`

## ✍️ Autor

© Carlos Sena — Economista | XP Investimentos
