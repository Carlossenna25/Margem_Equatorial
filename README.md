# Margem_Equatorial

# Impacto Econômico da Margem Equatorial

Este projeto estima os impactos econômicos da produção de petróleo na Margem Equatorial (Amapá, Pará, Maranhão, Ceará, Rio Grande do Norte) entre 2026 e 2030. A análise é feita com base em:

- Modelos de **Event Study** com efeitos fixos (`fixest`)
- Projeções estocásticas via **Monte Carlo**
- Dados reais de PIB, emprego, royalties, petróleo, câmbio, etc.

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

Carlos Sena • [LinkedIn](https://linkedin.com/in/carlos-sena-0776381a5)
