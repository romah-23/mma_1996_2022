# Manuscrito: Análise temporal, regional e sociodemográfica
# da Mortalidade Materna por aborto no Brasil (1996-2022)

# Carregando pacotes necessários 
library(dplyr)     # Pacote para manuseio de dados de df
library(openxlsx)  # Pacote para abrir arquivos de excel
library(prais)     # Pacote para análise de regressão de prais-winsten
library(readxl)    # Pacote para importar arquivos de excel 

# Download do banco de dados 1 de teste e carregamento do arquivo
download.file("https://raw.githubusercontent.com/kleydmann/Timeseriestutorial/main/Casos_anuais_AP_2003-2022.csv",
              "Casos_anuais_AP_2003-2022.csv", method = "auto")
df = read.csv2("Casos_anuais_AP_2003-2022.csv")

================================================================================
# 1. Análise regional (norte, nordeste, sudeste, sul, centro-oeste e Brasil)

# Download de arquivo excel
df <- read.xlsx("C:/Users/pedro.omar/OneDrive/Área de Trabalho/BACKUP/MMA/1 - DADOS/4 - R (Prais-Winsten)/1_regional.xlsx")

# Transformando unidade de casos em escala logaritmica
df <- df %>%
  mutate(norte_log = log(norte + 1),
         nordeste_log = log(nordeste + 1),
         sudeste_log = log(sudeste + 1),
         sul_log = log(sul + 1),
         centroeste_log = log(centroeste + 1),
         brasil_log = log(brasil + 1))

# Df apenas com dados logaritmos por região - 2 casas decimais
df_log <- df %>% 
  select(c(norte_log,
           nordeste_log,
           sudeste_log,
           sul_log,
           centroeste_log,
           brasil_log)) %>% 
  mutate(across(everything(), ~ round(.x, 2)))

# Aplicando a regressão de Prais-Winsten
pw_norte <- prais_winsten(norte_log ~ ano, data = df, index = "ano")
pw_nordeste <- prais_winsten(nordeste_log ~ ano, data = df, index = "ano")
pw_sudeste <- prais_winsten(sudeste_log ~ ano, data = df, index = "ano")
pw_sul <- prais_winsten(sul_log ~ ano, data = df, index = "ano")
pw_centroeste <- prais_winsten(centroeste_log ~ ano, data = df, index = "ano")
pw_brasil <- prais_winsten(brasil_log ~ ano, data = df, index = "ano")

# Extraindo valores preditos (variável dependente = y)
df <- df %>%
  mutate(Predito_norte = exp(pw_norte$fitted.values) - 1,
         Predito_nordeste = exp(pw_nordeste$fitted.values) - 1,
         Predito_sudeste = exp(pw_sudeste$fitted.values) - 1,
         Predito_sul = exp(pw_sul$fitted.values) - 1,
         Predito_centroeste = exp(pw_centroeste$fitted.values) - 1,
         Predito_brasil = exp(pw_brasil$fitted.values) - 1)

# Calculando Coeficiente estimado, erro padrão e p-valor
beta_norte <- coef(summary(pw_norte))["ano", "Estimate"]
beta_nordeste <- coef(summary(pw_nordeste))["ano", "Estimate"]
beta_sudeste <- coef(summary(pw_sudeste))["ano", "Estimate"]
beta_sul <- coef(summary(pw_sul))["ano", "Estimate"]
beta_centroeste <- coef(summary(pw_centroeste))["ano", "Estimate"]
beta_brasil <- coef(summary(pw_brasil))["ano", "Estimate"]

ErroPadrao_norte <- coef(summary(pw_norte))["ano", "Std. Error"]
ErroPadrao_nordeste <- coef(summary(pw_nordeste))["ano", "Std. Error"]
ErroPadrao_sudeste <- coef(summary(pw_sudeste))["ano", "Std. Error"]
ErroPadrao_sul <- coef(summary(pw_sul))["ano", "Std. Error"]
ErroPadrao_centroeste <- coef(summary(pw_centroeste))["ano", "Std. Error"]
ErroPadrao_brasil <- coef(summary(pw_brasil))["ano", "Std. Error"]

pval_norte <- summary(pw_norte)$coefficients["ano", "Pr(>|t|)"]
pval_nordeste <- summary(pw_nordeste)$coefficients["ano", "Pr(>|t|)"]
pval_sudeste <- summary(pw_sudeste)$coefficients["ano", "Pr(>|t|)"]
pval_sul <- summary(pw_sul)$coefficients["ano", "Pr(>|t|)"]
pval_centroeste <- summary(pw_centroeste)$coefficients["ano", "Pr(>|t|)"]
pval_brasil <- summary(pw_brasil)$coefficients["ano", "Pr(>|t|)"]

# Arbitrando Valor crítico t para IC95%
t_value <- 1.96

# Calculando Variação Percentual Anual (VPA)
VPA_norte <- round((-1 + 10^(beta_norte))*100, 2)
VPA_nordeste <- round((-1 + 10^(beta_nordeste))*100, 2)
VPA_sudeste <- round((-1 + 10^(beta_sudeste))*100, 2)
VPA_sul <- round((-1 + 10^(beta_sul))*100, 2)
VPA_centroeste <- round((-1 + 10^(beta_centroeste))*100, 2)
VPA_brasil <- round((-1 + 10^(beta_brasil))*100, 2)

# Calculando IC95% superior e inferior
IC95_sup_norte <- round((-1 + 10^(beta_norte + t_value * ErroPadrao_norte)) * 100, 2)
IC95_inf_norte <- round((-1 + 10^(beta_norte - t_value * ErroPadrao_norte)) * 100, 2)

IC95_sup_nordeste <- round((-1 + 10^(beta_nordeste + t_value * ErroPadrao_nordeste)) * 100, 2)
IC95_inf_nordeste <- round((-1 + 10^(beta_nordeste - t_value * ErroPadrao_nordeste)) * 100, 2)

IC95_sup_sudeste <- round((-1 + 10^(beta_sudeste + t_value * ErroPadrao_sudeste)) * 100, 2)
IC95_inf_sudeste <- round((-1 + 10^(beta_sudeste - t_value * ErroPadrao_sudeste)) * 100, 2)

IC95_sup_sul <- round((-1 + 10^(beta_sul + t_value * ErroPadrao_sul)) * 100, 2)
IC95_inf_sul <- round((-1 + 10^(beta_sul - t_value * ErroPadrao_sul)) * 100, 2)

IC95_sup_centroeste <- round((-1 + 10^(beta_centroeste + t_value * ErroPadrao_centroeste)) * 100, 2)
IC95_inf_centroeste <- round((-1 + 10^(beta_centroeste - t_value * ErroPadrao_centroeste)) * 100, 2)

IC95_sup_brasil <- round((-1 + 10^(beta_brasil + t_value * ErroPadrao_brasil)) * 100, 2)
IC95_inf_brasil <- round((-1 + 10^(beta_brasil - t_value * ErroPadrao_brasil)) * 100, 2)

# Imprimindo resultados
data.frame( VPA_norte = c("Variação Percentual Anual (VPA)", VPA_norte),
            IC95_inf_norte = c("IC95% Inferior", IC95_inf_norte),
            IC95_sup_norte = c("IC95% Superior", IC95_sup_norte))

data.frame( VPA_nordeste = c("Variação Percentual Anual (VPA)", VPA_nordeste),
            IC95_inf_nordeste = c("IC95% Inferior", IC95_inf_nordeste),
            IC95_sup_nordeste = c("IC95% Superior", IC95_sup_nordeste))

data.frame( VPA_sudeste = c("Variação Percentual Anual (VPA)", VPA_sudeste),
            IC95_inf_sudeste = c("IC95% Inferior", IC95_inf_sudeste),
            IC95_sup_sudeste = c("IC95% Superior", IC95_sup_sudeste))

data.frame( VPA_sul = c("Variação Percentual Anual (VPA)", VPA_sul),
            IC95_inf_sul = c("IC95% Inferior", IC95_inf_sul),
            IC95_sup_sul = c("IC95% Superior", IC95_sup_sul))

data.frame( VPA_centroeste = c("Variação Percentual Anual (VPA)", VPA_centroeste),
            IC95_inf_centroeste = c("IC95% Inferior", IC95_inf_centroeste),
            IC95_sup_centroeste = c("IC95% Superior", IC95_sup_centroeste))

data.frame( VPA_brasil = c("Variação Percentual Anual (VPA)", VPA_brasil),
            IC95_inf_brasil = c("IC95% Inferior", IC95_inf_brasil),
            IC95_sup_brasil = c("IC95% Superior", IC95_sup_brasil))

# Inlcluindo Função para calcular R²
calculate_r_squared <- function(observed, predicted) {
  valid_indices <- !is.na(observed) & !is.na(predicted)
  observed <- observed[valid_indices]
  predicted <- predicted[valid_indices]
  if(length(observed) < 2) {
    return(NA)
  }
  
# Calculando Soma dos Quadrados Totais (SST)
  mean_observed <- mean(observed)
  SST <- sum((observed - mean_observed)^2)
  
# Calculando Soma dos Quadrados dos Erros (SSE))
  SSE <- sum((observed - predicted)^2)
  
# Calculando R²
  R_squared <- 1 - (SSE / SST)
  
  return(R_squared)
}

# Aplicadando função R² em cada região
R2_reg <- data.frame(
  R2_norte = calculate_r_squared(df$norte, df$Predito_norte),
  R2_nordeste = calculate_r_squared(df$nordeste, df$Predito_nordeste),
  R2_sudeste = calculate_r_squared(df$sudeste, df$Predito_sudeste),
  R2_sul = calculate_r_squared(df$sul, df$Predito_sul),
  R2_centroeste = calculate_r_squared(df$centroeste, df$Predito_centroeste),
  R2_brasil = calculate_r_squared(df$brasil, df$Predito_brasil)
)
print(results_r2)

================================================================================
# 2. Análise faixa etária [f1(10-14), f2(15-19), f3(20-29), f4(30-39), f5(40-49)
# e Brasil]

# Carregando pacotes necessários
library(dplyr)     # Pacote para manuseio de dados de df
library(openxlsx)  # Pacote para abrir arquivos de excel
library(prais)     # Pacote para análise de regressão de prais-winsten
library(readxl)    # Pacote para importar arquivos de excel 

# Download de arquivo excel
df_idade <- read.xlsx("C:/Users/pedro.omar/OneDrive/Área de Trabalho/BACKUP/MMA/1 - DADOS/4 - R (Prais-Winsten)/2_faixa_etaria.xlsx")

# Transformando unidade de casos em escala logaritmica
df_idade <- df_idade %>%
  mutate(f1_log = log(f1 + 1),
         f2_log = log(f2 + 1),
         f3_log = log(f3 + 1),
         f4_log = log(f4 + 1),
         f5_log = log(f5 + 1),
         brasil_id_log = log(brasil_id + 1))

# Aplicando a regressão de Prais-Winsten
pw_f1 <- prais_winsten(f1_log ~ ano, data = df_idade, index = "ano")
pw_f2 <- prais_winsten(f2_log ~ ano, data = df_idade, index = "ano")
pw_f3 <- prais_winsten(f3_log ~ ano, data = df_idade, index = "ano")
pw_f4 <- prais_winsten(f4_log ~ ano, data = df_idade, index = "ano")
pw_f5 <- prais_winsten(f5_log ~ ano, data = df_idade, index = "ano")
pw_brasil_id <- prais_winsten(brasil_id_log ~ ano, data = df_idade, index = "ano")

# Extraindo valores preditos (variável dependente = y)
df_idade <- df_idade %>%
  mutate(Predito_f1 = exp(pw_f1$fitted.values) - 1,
         Predito_f2 = exp(pw_f2$fitted.values) - 1,
         Predito_f3 = exp(pw_f3$fitted.values) - 1,
         Predito_f4 = exp(pw_f4$fitted.values) - 1,
         Predito_f5 = exp(pw_f5$fitted.values) - 1,
         Predito_brasil_id = exp(pw_brasil_id$fitted.values) - 1)

# Calculando Coeficiente estimado, erro padrão e p-valor
beta_f1 <- coef(summary(pw_f1))["ano", "Estimate"]
beta_f2 <- coef(summary(pw_f2))["ano", "Estimate"]
beta_f3 <- coef(summary(pw_f3))["ano", "Estimate"]
beta_f4 <- coef(summary(pw_f4))["ano", "Estimate"]
beta_f5 <- coef(summary(pw_f5))["ano", "Estimate"]
beta_brasil_id <- coef(summary(pw_brasil_id))["ano", "Estimate"]

ErroPadrao_f1 <- coef(summary(pw_f1))["ano", "Std. Error"]
ErroPadrao_f2 <- coef(summary(pw_f2))["ano", "Std. Error"]
ErroPadrao_f3 <- coef(summary(pw_f3))["ano", "Std. Error"]
ErroPadrao_f4 <- coef(summary(pw_f4))["ano", "Std. Error"]
ErroPadrao_f5 <- coef(summary(pw_f5))["ano", "Std. Error"]
ErroPadrao_brasil_id <- coef(summary(pw_brasil_id))["ano", "Std. Error"]

pval_f1 <- summary(pw_f1)$coefficients["ano", "Pr(>|t|)"]
pval_f2 <- summary(pw_f2)$coefficients["ano", "Pr(>|t|)"]
pval_f3 <- summary(pw_f3)$coefficients["ano", "Pr(>|t|)"]
pval_f4 <- summary(pw_f4)$coefficients["ano", "Pr(>|t|)"]
pval_f5 <- summary(pw_f5)$coefficients["ano", "Pr(>|t|)"]
pval_brasil_id <- summary(pw_brasil_id)$coefficients["ano", "Pr(>|t|)"]

# Arbitrando Valor crítico t para IC95%
t_value <- 1.96

# Calculando Variação Percentual Anual (VPA)
VPA_total_f1 <- round((-1 + 10^(beta_f1))*100, 2)
VPA_total_f2 <- round((-1 + 10^(beta_f2))*100, 2)
VPA_total_f3 <- round((-1 + 10^(beta_f3))*100, 2)
VPA_total_f4 <- round((-1 + 10^(beta_f4))*100, 2)
VPA_total_f5 <- round((-1 + 10^(beta_f5))*100, 2)
VPA_total_brasil_id <- round((-1 + 10^(beta_brasil_id))*100, 2)

================================================================================
# 3. Análise estado civil (solteiro, casado, viuvo, separado e Brasil)

# Carregando pacotes necessários
library(dplyr)     # Pacote para manuseio de dados de df
library(openxlsx)  # Pacote para abrir arquivos de excel
library(prais)     # Pacote para análise de regressão de prais-winsten
library(readxl)    # Pacote para importar arquivos de excel

# Download de arquivo excel
df_ec <- read.xlsx("C:/Users/pedro.omar/OneDrive/Área de Trabalho/BACKUP/MMA/1 - DADOS/4 - R (Prais-Winsten)/3_estado_civil.xlsx")

# Transformando unidade de casos em escala logaritmica
df_ec <- df_ec %>%
  mutate(solteiro_log = log(solteiro + 1),
         casado_log = log(casado + 1),
         viuvo_log = log(viuvo + 1),
         separado_log = log(separado + 1),
         brasil_ec_log = log(brasil_ec + 1))

# Aplicando a regressão de Prais-Winsten
pw_solteiro <- prais_winsten(solteiro_log ~ ano, data = df_ec, index = "ano")
pw_casado <- prais_winsten(casado_log ~ ano, data = df_ec, index = "ano")
pw_viuvo <- prais_winsten(viuvo_log ~ ano, data = df_ec, index = "ano")
pw_separado <- prais_winsten(separado_log ~ ano, data = df_ec, index = "ano")
pw_brasil_ec <- prais_winsten(brasil_ec_log ~ ano, data = df_ec, index = "ano")

# Extraindo valores preditos (variável dependente = y)
df_ec <- df_ec %>%
  mutate(Predito_solteiro = exp(pw_solteiro$fitted.values) - 1,
         Predito_casado = exp(pw_casado$fitted.values) - 1,
         Predito_viuvo = exp(pw_viuvo$fitted.values) - 1,
         Predito_separado = exp(pw_separado$fitted.values) - 1,
         Predito_brasil_ec = exp(pw_brasil_ec$fitted.values) - 1)

# Calculando Coeficiente estimado, erro padrão e p-valor
beta_solteiro <- coef(summary(pw_solteiro))["ano", "Estimate"]
beta_casado <- coef(summary(pw_casado))["ano", "Estimate"]
beta_viuvo <- coef(summary(pw_viuvo))["ano", "Estimate"]
beta_separado <- coef(summary(pw_separado))["ano", "Estimate"]
beta_brasil_ec <- coef(summary(pw_brasil_ec))["ano", "Estimate"]

ErroPadrao_solteiro <- coef(summary(pw_solteiro))["ano", "Std. Error"]
ErroPadrao_casado <- coef(summary(pw_casado))["ano", "Std. Error"]
ErroPadrao_viuvo <- coef(summary(pw_viuvo))["ano", "Std. Error"]
ErroPadrao_separado <- coef(summary(pw_separado))["ano", "Std. Error"]
ErroPadrao_brasil_ec <- coef(summary(pw_brasil_ec))["ano", "Std. Error"]

pval_solteiro <- summary(pw_solteiro)$coefficients["ano", "Pr(>|t|)"]
pval_casado <- summary(pw_casado)$coefficients["ano", "Pr(>|t|)"]
pval_viuvo <- summary(pw_viuvo)$coefficients["ano", "Pr(>|t|)"]
pval_separado <- summary(pw_separado)$coefficients["ano", "Pr(>|t|)"]
pval_brasil_ec <- summary(pw_brasil_ec)$coefficients["ano", "Pr(>|t|)"]

# Arbitrando Valor crítico t para IC95%
t_value <- 1.96

# Calculando Variação Percentual Anual (VPA)
VPA_total_solteiro <- round((-1 + 10^(beta_solteiro))*100, 2)
VPA_total_casado <- round((-1 + 10^(beta_casado))*100, 2)
VPA_total_viuvo <- round((-1 + 10^(beta_viuvo))*100, 2)
VPA_total_separado <- round((-1 + 10^(beta_separado))*100, 2)
VPA_total_brasil_ec <- round((-1 + 10^(beta_brasil_ec))*100, 2)

================================================================================
# 4. Análise cor/raça (branca, preta, amarela, parda, indígena e Brasil)
  
# Carregando pacotes necessários
library(dplyr)     # Pacote para manuseio de dados de df
library(openxlsx)  # Pacote para abrir arquivos de excel
library(prais)     # Pacote para análise de regressão de prais-winsten
library(readxl)    # Pacote para importar arquivos de excel

# Download de arquivo excel
df_cor <- read.xlsx("C:/Users/pedro.omar/OneDrive/Área de Trabalho/BACKUP/MMA/1 - DADOS/4 - R (Prais-Winsten)/4_cor.xlsx")

# Transformando unidade de casos em escala logaritmica
df_cor <- df_cor %>%
  mutate(branca_log = log(branca + 1),
         preta_log = log(preta + 1),
         amarela_log = log(amarela + 1),
         parda_log = log(parda + 1),
         indigena_log = log(indigena + 1),
         brasil_cor_log = log(brasil_cor + 1))

# Aplicando a regressão de Prais-Winsten
pw_branca <- prais_winsten(branca_log ~ ano, data = df_cor, index = "ano")
pw_preta <- prais_winsten(preta_log ~ ano, data = df_cor, index = "ano")
pw_amarela <- prais_winsten(amarela_log ~ ano, data = df_cor, index = "ano")
pw_parda <- prais_winsten(parda_log ~ ano, data = df_cor, index = "ano")
pw_indigena <- prais_winsten(indigena_log ~ ano, data = df_cor, index = "ano")
pw_brasil_cor <- prais_winsten(brasil_cor_log ~ ano, data = df_cor, index = "ano")

# Extraindo valores preditos (variável dependente = y)
df_cor <- df_cor %>%
  mutate(Predito_branca = exp(pw_branca$fitted.values) - 1,
         Predito_preta = exp(pw_preta$fitted.values) - 1,
         Predito_amarela = exp(pw_amarela$fitted.values) - 1,
         Predito_parda = exp(pw_parda$fitted.values) - 1,
         Predito_indigena = exp(pw_indigena$fitted.values) - 1,
         Predito_brasil_cor = exp(pw_brasil_cor$fitted.values) - 1)

# Calculando Coeficiente estimado, erro padrão e p-valor
beta_branca <- coef(summary(pw_branca))["ano", "Estimate"]
beta_preta <- coef(summary(pw_preta))["ano", "Estimate"]
beta_amarela <- coef(summary(pw_amarela))["ano", "Estimate"]
beta_parda <- coef(summary(pw_parda))["ano", "Estimate"]
beta_indigena <- coef(summary(pw_indigena))["ano", "Estimate"]
beta_brasil_cor <- coef(summary(pw_brasil_cor))["ano", "Estimate"]

ErroPadrao_branca <- coef(summary(pw_branca))["ano", "Std. Error"]
ErroPadrao_preta <- coef(summary(pw_preta))["ano", "Std. Error"]
ErroPadrao_amarela <- coef(summary(pw_amarela))["ano", "Std. Error"]
ErroPadrao_parda <- coef(summary(pw_parda))["ano", "Std. Error"]
ErroPadrao_indigena <- coef(summary(pw_indigena))["ano", "Std. Error"]
ErroPadrao_brasil_cor <- coef(summary(pw_brasil_cor))["ano", "Std. Error"]

pval_branca <- summary(pw_branca)$coefficients["ano", "Pr(>|t|)"]
pval_preta <- summary(pw_preta)$coefficients["ano", "Pr(>|t|)"]
pval_amarela <- summary(pw_amarela)$coefficients["ano", "Pr(>|t|)"]
pval_parda <- summary(pw_parda)$coefficients["ano", "Pr(>|t|)"]
pval_indigena <- summary(pw_indigena)$coefficients["ano", "Pr(>|t|)"]
pval_brasil_cor <- summary(pw_brasil_cor)$coefficients["ano", "Pr(>|t|)"]

# Arbitrando Valor crítico t para IC95%
t_value <- 1.96

# Calculando Variação Percentual Anual (VPA)
VPA_total_branca <- round((-1 + 10^(beta_branca))*100, 2)
VPA_total_preta <- round((-1 + 10^(beta_preta))*100, 2)
VPA_total_amarela <- round((-1 + 10^(beta_amarela))*100, 2)
VPA_total_parda <- round((-1 + 10^(beta_parda))*100, 2)
VPA_total_indigena <- round((-1 + 10^(beta_indigena))*100, 2)
VPA_total_brasil_cor <- round((-1 + 10^(beta_brasil_cor))*100, 2)

================================================================================
# 5. Análise escolaridade [e1(1-3), e2(4-7), e3(8-11), e4(12 ou mais), nenhuma e
# Brasil]
  
# Carregando pacotes necessários
library(dplyr)     # Pacote para manuseio de dados de df
library(openxlsx)  # Pacote para abrir arquivos de excel
library(prais)     # Pacote para análise de regressão de prais-winsten
library(readxl)    # Pacote para importar arquivos de excel

# Download de arquivo excel
df_esc <- read.xlsx("C:/Users/pedro.omar/OneDrive/Área de Trabalho/BACKUP/MMA/1 - DADOS/4 - R (Prais-Winsten)/5_escolaridade.xlsx")

# Transformando unidade de casos em escala logaritmica
df_esc <- df_esc %>%
  mutate(e1_log = log(e1 + 1),
         e2_log = log(e2 + 1),
         e3_log = log(e3 + 1),
         e4_log = log(e4 + 1),
         nenhuma_log = log(nenhuma + 1),
         brasil_esc_log = log(brasil_esc + 1))

# Aplicando a regressão de Prais-Winsten
pw_e1 <- prais_winsten(e1_log ~ ano, data = df_esc, index = "ano")
pw_e2 <- prais_winsten(e2_log ~ ano, data = df_esc, index = "ano")
pw_e3 <- prais_winsten(e3_log ~ ano, data = df_esc, index = "ano")
pw_e4 <- prais_winsten(e4_log ~ ano, data = df_esc, index = "ano")
pw_nenhuma <- prais_winsten(nenhuma_log ~ ano, data = df_esc, index = "ano")
pw_brasil_esc <- prais_winsten(brasil_esc_log ~ ano, data = df_esc, index = "ano")

# Extraindo valores preditos (variável dependente = y)
df_esc <- df_esc %>%
  mutate(Predito_e1 = exp(pw_e1$fitted.values) - 1,
         Predito_e2 = exp(pw_e2$fitted.values) - 1,
         Predito_e3 = exp(pw_e3$fitted.values) - 1,
         Predito_e4 = exp(pw_e4$fitted.values) - 1,
         Predito_nenhuma = exp(pw_nenhuma$fitted.values) - 1,
         Predito_brasil_esc = exp(pw_brasil_esc$fitted.values) - 1,)

# Calculando Coeficiente estimado, erro padrão e p-valor
beta_e1 <- coef(summary(pw_e1))["ano", "Estimate"]
beta_e2 <- coef(summary(pw_e2))["ano", "Estimate"]
beta_e3 <- coef(summary(pw_e3))["ano", "Estimate"]
beta_e4 <- coef(summary(pw_e4))["ano", "Estimate"]
beta_nenhuma <- coef(summary(pw_nenhuma))["ano", "Estimate"]
beta_brasil_esc <- coef(summary(pw_brasil_esc))["ano", "Estimate"]

ErroPadrao_e1 <- coef(summary(pw_e1))["ano", "Std. Error"]
ErroPadrao_e2 <- coef(summary(pw_e2))["ano", "Std. Error"]
ErroPadrao_e3 <- coef(summary(pw_e3))["ano", "Std. Error"]
ErroPadrao_e4 <- coef(summary(pw_e4))["ano", "Std. Error"]
ErroPadrao_nenhuma <- coef(summary(pw_nenhuma))["ano", "Std. Error"]
ErroPadrao_brasil_esc <- coef(summary(pw_brasil_esc))["ano", "Std. Error"]

pval_e1 <- summary(pw_e1)$coefficients["ano", "Pr(>|t|)"]
pval_e2 <- summary(pw_e2)$coefficients["ano", "Pr(>|t|)"]
pval_e3 <- summary(pw_e3)$coefficients["ano", "Pr(>|t|)"]
pval_e4 <- summary(pw_e4)$coefficients["ano", "Pr(>|t|)"]
pval_nenhuma <- summary(pw_nenhuma)$coefficients["ano", "Pr(>|t|)"]
pval_brasil_esc <- summary(pw_brasil_esc)$coefficients["ano", "Pr(>|t|)"]

# Arbitrando Valor crítico t para IC95%
t_value <- 1.96

# Calculando Variação Percentual Anual (VPA)
VPA_total_e1 <- round((-1 + 10^(beta_e1))*100, 2)
VPA_total_e2 <- round((-1 + 10^(beta_e2))*100, 2)
VPA_total_e3 <- round((-1 + 10^(beta_e3))*100, 2)
VPA_total_e4 <- round((-1 + 10^(beta_e4))*100, 2)
VPA_total_nenhuma <- round((-1 + 10^(beta_nenhuma))*100, 2)
VPA_total_brasil_esc <- round((-1 + 10^(beta_brasil_esc))*100, 2)

================================================================================
# 6. Gráfico regional
  
# Carregando pacotes necessários
library(ggplot2)     # Pacote para manuseio de dados de df





