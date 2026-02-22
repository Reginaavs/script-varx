# script-varx
# Assessment of the Impacts of Diesel Prices on Agricultural Freight Rates and Inflation in Brazil

** Beatriz Del Polzo & Regina Ávila Santos (2026)**

This repository provides the replication script used to estimate VAR, VARX, and VARX models with structural breaks. Orthogonalized impulse response functions (Cholesky decomposition) are computed due to the rejection of overidentifying restrictions in the SVAR specification.


## Files
- `script_varx.R` — Main replication script
- `paper.pdf` — Article

## Data Sources

All variables are publicly available from official sources:

- **Exchange Rate (Câmbio)**  
  IPEA Data:  
  https://ipeadata.gov.br/exibeserie.aspx?serid=38389  

- **Brent Oil Price (FOB)**  
  IPEA Data:  
  https://www.ipeadata.gov.br/ExibeSerie.aspx?module=m&serid=1650971490&oper=view  

- **Diesel Price (ANP)**  
  Agência Nacional do Petróleo (ANP):  
  https://www.gov.br/anp/pt-br/centrais-de-conteudo/dados-estatisticos  

- **IPCA (Consumer Price Index)**  
  IBGE – SIDRA:  
  https://sidra.ibge.gov.br/pesquisa/snipc/ipca/tabelas  

- **Agricultural Freight Rates**  
  SIFRECA – ESALQ/USP:  
  https://sifreca.esalq.usp.br/  


## Replication Instructions

1. Download the datasets from the sources above.
2. Organize them into the file `Dados_artigo.xlsx`.
3. Place the Excel file in the same directory as `script_varx.R`.
4. Run the script in R (version 4.x recommended).


## Citation


