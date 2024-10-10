  # Manuscrito: Análise temporal, regional e sociodemográfica
  # da Mortalidade Materna por aborto no Brasil (1996-2022)

  # Carregando pacotes necessários
  library(readxl)
  library(openxlsx) 
  library(dplyr)     
  library(prais)     

  ==============================================================================
  # 1. Análise regional (norte, nordeste, sudeste, sul, centro-oeste e Brasil)
  
  # Download de arquivo excel
  df <- read.xlsx("1_regional.xlsx")
  
  # Transformando unidade de casos em escala logarítmica
  df <- df %>%
    mutate(norte_log = log(norte + 1),
           nordeste_log = log(nordeste + 1),
           sudeste_log = log(sudeste + 1),
           sul_log = log(sul + 1),
           centroeste_log = log(centroeste + 1),
           brasil_log = log(brasil_reg + 1))
  
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
  
  # Criando uma lista para armazenar resultados
  resultados_reg <- list()
  
  # Nomes das regiões
  regioes <- c("norte", "nordeste", "sudeste", "sul", "centroeste", "brasil")
  
  # Calculando beta, erro padrão e p-valor para cada região
  for (regiao in regioes) {
    modelo <- get(paste0("pw_", regiao))
    resultados_reg[[regiao]] <- list(
      beta = coef(summary(modelo))["ano", "Estimate"],
      erro_padrao = coef(summary(modelo))["ano", "Std. Error"],
      p_valor = summary(modelo)$coefficients["ano", "Pr(>|t|)"]
    )
  }
  
  # Calculando a VPA para cada região usando os coeficientes beta
  vpa_regiao <- list()
  
  for (regiao in names(resultados_reg)) {
    beta <- resultados_reg[[regiao]]$beta
    vpa_regiao[[regiao]] <- round((-1 + 10^(beta)) * 100, 2)
  }
  
  # Arbitrando valor crítico t para IC95%
  t_value <- 1.96
  
  # Inicializando listas para armazenar os IC95%
  IC95_reg <- list()
  
  # Calculando IC95% superior e inferior para cada região
  for (regiao in names(resultados_reg)) {
    beta <- resultados_reg[[regiao]]$beta
    erro_padrao <- resultados_reg[[regiao]]$erro_padrao
    
    IC95_sup <- round((-1 + 10^(beta + t_value * erro_padrao)) * 100, 2)
    IC95_inf <- round((-1 + 10^(beta - t_value * erro_padrao)) * 100, 2)
    
    IC95_reg[[regiao]] <- list(IC95_sup = IC95_sup, IC95_inf = IC95_inf)
  }
  
  # Função para cálculo R²
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
    
    # Calculando Soma dos Quadrados dos Erros (SSE)
    SSE <- sum((observed - predicted)^2)
    
    # Calculando R²
    R_squared <- 1 - (SSE / SST)
    
    return(R_squared)
  }
  
  # Aplicando função R² em cada região
  R2_reg <- data.frame(
    R2_norte = calculate_r_squared(df$norte, df$Predito_norte),
    R2_nordeste = calculate_r_squared(df$nordeste, df$Predito_nordeste),
    R2_sudeste = calculate_r_squared(df$sudeste, df$Predito_sudeste),
    R2_sul = calculate_r_squared(df$sul, df$Predito_sul),
    R2_centroeste = calculate_r_squared(df$centroeste, df$Predito_centroeste),
    R2_brasil = calculate_r_squared(df$brasil_reg, df$Predito_brasil)
  )
  
  ==============================================================================
  # 2. Análise faixa etária [f1(10-14), f2(15-19), f3(20-29), f4(30-39), f5(40-49)
  # e Brasil]
    
  # Download de arquivo excel
  df_idade <- read.xlsx("2_faixa_etaria.xlsx")
  
  # Transformando unidade de casos em escala logarítmica
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
  
  # Criando uma lista para armazenar os resultados
  resultados_idade <- list()
  
  # Nomes das faixas etárias
  faixas <- c("f1", "f2", "f3", "f4", "f5", "brasil_id")
  
  # Calculando beta, erro padrão e p-valor para cada faixa etária
  for (faixa in faixas) {
    modelo <- get(paste0("pw_", faixa))
    resultados_idade[[faixa]] <- list(
      beta = coef(summary(modelo))["ano", "Estimate"],
      erro_padrao = coef(summary(modelo))["ano", "Std. Error"],
      p_valor = summary(modelo)$coefficients["ano", "Pr(>|t|)"]
    )
  }
  
  # Calculando a VPA para cada faixa etária usando os coeficientes beta
  vpa_idade <- list()
  
  for (faixas in names(resultados_idade)) {
    beta <- resultados_idade[[faixas]]$beta
    vpa_idade[[faixas]] <- round((-1 + 10^(beta)) * 100, 2)
  }
  
  # Inicializando listas para armazenar os IC95%
  IC95_idade <- list()
  
  # Calculando IC95% superior e inferior para cada faixa etária
  for (idade in names(resultados_idade)) {
    beta <- resultados_idade[[idade]]$beta
    erro_padrao <- resultados_idade[[idade]]$erro_padrao
    
    IC95_sup <- round((-1 + 10^(beta + t_value * erro_padrao)) * 100, 2)
    IC95_inf <- round((-1 + 10^(beta - t_value * erro_padrao)) * 100, 2)
    
    IC95_idade[[idade]] <- list(IC95_sup = IC95_sup, IC95_inf = IC95_inf)
  }
  
  # Aplicando função R² em cada faixa etária
  R2_idade <- data.frame(
    R2_f1 = calculate_r_squared(df_idade$f1, df_idade$Predito_f1),
    R2_f2 = calculate_r_squared(df_idade$f2, df_idade$Predito_f2),
    R2_f3 = calculate_r_squared(df_idade$f3, df_idade$Predito_f3),
    R2_f4 = calculate_r_squared(df_idade$f4, df_idade$Predito_f4),
    R2_f5 = calculate_r_squared(df_idade$f5, df_idade$Predito_f5),
    R2_brasil_id = calculate_r_squared(df_idade$brasil_id, df_idade$Predito_brasil_id)  # Use a coluna correta
  )
  
  ==============================================================================
  # 3. Análise estado civil (solteiro, casado, viúvo, separado e Brasil)
    
  # Download de arquivo excel
  df_ec <- read.xlsx("3_estado_civil.xlsx")
  
  # Transformando unidade de casos em escala logarítmica
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
  
  # Criando uma lista para armazenar os resultados
  resultados_ec <- list()
  
  # Nomes dos estados civis
  civil <- c("solteiro", "casado", "viuvo", "separado", "brasil_ec")
  
  # Calculando beta, erro padrão e p-valor para cada estado civil
  for (ec in civil) {
    modelo <- get(paste0("pw_", ec))
    resultados_ec[[ec]] <- list(
      beta = coef(summary(modelo))["ano", "Estimate"],
      erro_padrao = coef(summary(modelo))["ano", "Std. Error"],
      p_valor = summary(modelo)$coefficients["ano", "Pr(>|t|)"]
    )
  }
  
  # Calculando a VPA para cada estado civil usando os coeficientes beta
  vpa_ec <- list()
  
  for (civil in names(resultados_ec)) {
    beta <- resultados_ec[[civil]]$beta
    vpa_ec[[civil]] <- round((-1 + 10^(beta)) * 100, 2)
  }
  
  # Inicializando listas para armazenar os IC95%
  IC95_ec <- list()
  
  # Calculando IC95% superior e inferior para cada estado civil
  for (civil in names(resultados_ec)) {
    beta <- resultados_ec[[civil]]$beta
    erro_padrao <- resultados_ec[[civil]]$erro_padrao
    
    IC95_sup <- round((-1 + 10^(beta + t_value * erro_padrao)) * 100, 2)
    IC95_inf <- round((-1 + 10^(beta - t_value * erro_padrao)) * 100, 2)
    
    IC95_ec[[civil]] <- list(IC95_sup = IC95_sup, IC95_inf = IC95_inf)
  }
  
  # Aplicando função R² em cada estado civil
  R2_ec <- data.frame(
    R2_solteiro = calculate_r_squared(df_ec$solteiro, df_ec$Predito_solteiro),
    R2_casado = calculate_r_squared(df_ec$casado, df_ec$Predito_casado),
    R2_viuvo = calculate_r_squared(df_ec$viuvo, df_ec$Predito_viuvo), # Corrigido de 'casado' para 'viuvo'
    R2_separado = calculate_r_squared(df_ec$separado, df_ec$Predito_separado),
    R2_brasil_ec = calculate_r_squared(df_ec$brasil_ec, df_ec$Predito_brasil_ec)
  )
  
  ==============================================================================
  # 4. Análise cor/raça (branca, preta, amarela, parda, indígena e Brasil)
  
  # Download de arquivo excel
  df_cor <- read.xlsx("4_cor.xlsx")
  
  # Transformando unidade de casos em escala logarítmica
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
  
  
  # Criando uma lista para armazenar os resultados
  resultados_cor <- list()
  
  # Nomes das faixas de cor/raça
  cor <- c("branca", "preta", "amarela", "parda", "indigena", "brasil_cor")
  
  # Calculando beta, erro padrão e p-valor para cada raça/cor
  for (etnia in cor) {
    modelo <- get(paste0("pw_", etnia))  # Obtendo o modelo correspondente
    resultados_cor[[etnia]] <- list(
      beta = coef(summary(modelo))["ano", "Estimate"],
      erro_padrao = coef(summary(modelo))["ano", "Std. Error"],
      p_valor = summary(modelo)$coefficients["ano", "Pr(>|t|)"]
    )
  }
  
  # Calculando a VPA para cada raça/cor usando os coeficientes beta
  vpa_cor <- list()
  
  for (cor in names(resultados_cor)) {
    beta <- resultados_cor[[cor]]$beta
    vpa_cor[[cor]] <- round((-1 + 10^(beta)) * 100, 2)
  }

  # Inicializando listas para armazenar os IC95%
  IC95_cor <- list()
  
  # Calculando IC95% superior e inferior para cada raça/cor
  for (cor in names(resultados_cor)) {
    beta <- resultados_cor[[cor]]$beta
    erro_padrao <- resultados_cor[[cor]]$erro_padrao
    
    IC95_sup <- round((-1 + 10^(beta + t_value * erro_padrao)) * 100, 2)
    IC95_inf <- round((-1 + 10^(beta - t_value * erro_padrao)) * 100, 2)
    
    IC95_cor[[cor]] <- list(IC95_sup = IC95_sup, IC95_inf = IC95_inf)
  }
  
  # Aplicando função R² em cada raça/cor
  R2_cor <- data.frame(
    R2_branca = calculate_r_squared(df_cor$branca, df_cor$Predito_branca),
    R2_preta = calculate_r_squared(df_cor$preta, df_cor$Predito_preta),
    R2_amarela = calculate_r_squared(df_cor$amarela, df_cor$Predito_amarela),
    R2_parda = calculate_r_squared(df_cor$parda, df_cor$Predito_parda),
    R2_indigena = calculate_r_squared(df_cor$indigena, df_cor$Predito_indigena),
    R2_brasil_cor = calculate_r_squared(df_cor$brasil_cor, df_cor$Predito_brasil_cor)
  )
  
  ==============================================================================
  # 5. Análise escolaridade [e1(1-3), e2(4-7), e3(8-11), e4(12 ou mais), nenhuma e
  # Brasil]
 
  # Download de arquivo excel
  df_esc <- read.xlsx("5_escolaridade.xlsx")
  
  # Transformando unidade de casos em escala logarítmica
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
  
  # Criando uma lista para armazenar os resultados
  resultados_esc <- list()
  
  # Nomes das faixas de escolaridade
  escola <- c("e1", "e2", "e3", "e4", "nenhuma", "brasil_esc")
  
  # Calculando beta, erro padrão e p-valor para cada escolaridade
  for (estudo in escola) {
    modelo <- get(paste0("pw_", estudo))  # Obtendo o modelo correspondente
    resultados_esc[[estudo]] <- list(
      beta = coef(summary(modelo))["ano", "Estimate"],
      erro_padrao = coef(summary(modelo))["ano", "Std. Error"],
      p_valor = summary(modelo)$coefficients["ano", "Pr(>|t|)"]
    )
  }
  
  # Calculando a VPA para cada escolaridade usando os coeficientes beta
  vpa_esc <- list()
  
  for (escola in names(resultados_esc)) {
    beta <- resultados_esc[[escola]]$beta
    vpa_esc[[escola]] <- round((-1 + 10^(beta)) * 100, 2)
  }
  
  # Inicializando listas para armazenar os IC95%
  IC95_esc <- list()
  
  # Calculando IC95% superior e inferior para cada escolaridade
  for (escola in names(resultados_esc)) {
    beta <- resultados_esc[[escola]]$beta
    erro_padrao <- resultados_esc[[escola]]$erro_padrao
    
    IC95_sup <- round((-1 + 10^(beta + t_value * erro_padrao)) * 100, 2)
    IC95_inf <- round((-1 + 10^(beta - t_value * erro_padrao)) * 100, 2)
    
    IC95_esc[[escola]] <- list(IC95_sup = IC95_sup, IC95_inf = IC95_inf)
  }
  
  # Aplicando função R² em cada escolaridade
  R2_esc <- data.frame(
    R2_e1 = calculate_r_squared(df_esc$e1, df_esc$Predito_e1),
    R2_e2 = calculate_r_squared(df_esc$e2, df_esc$Predito_e2),
    R2_e3 = calculate_r_squared(df_esc$e3, df_esc$Predito_e3),
    R2_e4 = calculate_r_squared(df_esc$e4, df_esc$Predito_e4),
    R2_nenhuma = calculate_r_squared(df_esc$nenhuma, df_esc$Predito_nenhuma),
    R2_brasil_esc = calculate_r_squared(df_esc$brasil_esc, df_esc$Predito_brasil_esc)
  )
  
  ==============================================================================
  # 6. Gráfico regional
    
  # Carregando pacotes necessários
  library(ggplot2)
  library(tidyr)
  library(scales)
  
  # Dados transformados
  df_long <- df %>%
    pivot_longer(cols = c(norte, nordeste, sudeste, sul, centroeste, brasil_reg,
                          Predito_norte, Predito_nordeste, Predito_sudeste, Predito_sul, Predito_centroeste, Predito_brasil),
                 names_to = "tipo",
                 values_to = "valor") %>%
    mutate(tipo = factor(tipo, levels = c("norte", "nordeste", "sudeste", "sul", "centroeste", "brasil_reg",
                                          "Predito_norte", "Predito_nordeste", "Predito_sudeste", "Predito_sul", "Predito_centroeste", "Predito_brasil"),
                         labels = c("Norte - Observado", "Nordeste - Observado", "Sudeste - Observado", "Sul - Observado", "Centro-Oeste - Observado", "Brasil - Observado",
                                    "Norte - Predito", "Nordeste - Predito", "Sudeste - Predito", "Sul - Predito", "Centro-Oeste - Predito", "Brasil - Predito")))
  
  # Filtrando os dados para exibir apenas observados e o predito do Brasil
  df_long_filtrado <- df_long %>%
    filter(grepl("Observado", tipo) | tipo == "Brasil - Predito")
  
  # Definindo as cores e estilos
  cores <- c("Norte - Observado" = "#5B9BD5",
             "Nordeste - Observado" = "#A5A5A5",
             "Sudeste - Observado" = "#4472C4",
             "Sul - Observado" = "#255E91",
             "Centro-Oeste - Observado" = "#636363",
             "Brasil - Observado" = "#000000",
             "Brasil - Predito" = "#000000")
  
  estilos <- c("Norte - Observado" = "solid",
               "Nordeste - Observado" = "solid",
               "Sudeste - Observado" = "solid",
               "Sul - Observado" = "solid",
               "Centro-Oeste - Observado" = "solid",
               "Brasil - Observado" = "solid",
               "Brasil - Predito" = "dashed")
  
 # Obtendo o intercepto da regressão de Prais-Winsten para o Brasil
 intercepto_brasil <- coef(summary(pw_brasil))["(Intercept)", "Estimate"]

 # Centralizando a variável ano em torno de 1996
 df$ano_centralizado <- df$ano - 1996

 # Aplicando a regressão de Prais-Winsten ao Brasil com o ano centralizado
 pw_brasil <- prais_winsten(brasil_log ~ ano_centralizado, data = df, index = "ano_centralizado")

 # Extraindo os coeficientes da regressão ajustada
 beta_brasil <- coef(summary(pw_brasil))["ano_centralizado", "Estimate"]
 intercepto_brasil <- coef(summary(pw_brasil))["(Intercept)", "Estimate"]

 # Verificando a equação da regressão
 paste0("Brasil: y = ", round(beta_brasil, 4), " * ano_centralizado + ", round(intercepto_brasil, 4))

 # Atualizando os valores preditos após centralização
 df <- df %>%
  mutate(Predito_brasil = exp(pw_brasil$fitted.values) - 1)

# Atualizando o gráfico para refletir o ajuste da regressão
df_long <- df %>%
  pivot_longer(cols = c(norte, nordeste, sudeste, sul, centroeste, brasil_reg,
                        Predito_norte, Predito_nordeste, Predito_sudeste, Predito_sul, Predito_centroeste, Predito_brasil),
               names_to = "tipo",
               values_to = "valor") %>%
  mutate(tipo = factor(tipo, levels = c("norte", "nordeste", "sudeste", "sul", "centroeste", "brasil_reg",
                                        "Predito_norte", "Predito_nordeste", "Predito_sudeste", "Predito_sul", "Predito_centroeste", "Predito_brasil"),
                       labels = c("Norte - Observado", "Nordeste - Observado", "Sudeste - Observado", "Sul - Observado", "Centro-Oeste - Observado", "Brasil - Observado",
                                  "Norte - Predito", "Nordeste - Predito", "Sudeste - Predito", "Sul - Predito", "Centro-Oeste - Predito", "Brasil - Predito")))

 # Filtrando os dados para exibir apenas observados e o predito do Brasil
 df_long_filtrado <- df_long %>%
  filter(grepl("Observado", tipo) | tipo == "Brasil - Predito")

 # Criando o gráfico atualizado com títulos e legendas em inglês
 grafico_regional <- ggplot(df_long_filtrado, aes(x = ano, y = valor, color = tipo, linetype = tipo)) +
   geom_line(size = 1) +
   geom_line(data = df_long_filtrado %>% filter(tipo == "Brasil - Predito"), aes(x = ano, y = valor), linetype = "dashed", size = 1) +
   geom_point(data = df_long_filtrado %>% filter(tipo == "Brasil - Predito"), size = 2) +
   scale_color_manual(values = cores,
                      labels = c("North - Observed", "Northeast - Observed", "Southeast - Observed", "South - Observed", "Central-West - Observed", "Brazil - Observed", "Brazil - Predicted")) +
   scale_linetype_manual(values = estilos) +
   labs(
     x = NULL,  # Retirando título do eixo x
     y = "Rates per million",  # Título do eixo y em inglês
     color = "Legend"
   ) +
   guides(linetype = "none") +
   scale_x_continuous(breaks = seq(1996, 2022, by = 2)) +
   scale_y_continuous(labels = label_number(accuracy = 0.01)) +
   theme_minimal() +
   theme(
     panel.background = element_rect(fill = "white"),
     plot.background = element_rect(fill = "white"),
     legend.background = element_rect(fill = "white"),
     legend.position = "bottom",
     panel.grid.major = element_blank(),  # Remove linhas de grade maiores
     panel.grid.minor = element_blank()   # Remove linhas de grade menores
   )
 
 # Exibindo e baixando o gráfico atualizado
 print(grafico_regional)
 ggsave("grafico_regional.png", plot = grafico_regional, width = 10, height = 6, dpi = 300)
 
 # Exibindo a equação da regressão para o Brasil
 equacao_brasil <- paste0("Brasil: y = ", round(beta_brasil, 4), " * (ano - 1996) + ", round(intercepto_brasil, 4))
 predito_1996 <- exp(intercepto_brasil) - 1
 print(equacao_brasil)
 
================================================================================
 # Citação de pacotes
 citation("readxl")
 citation ("openxlsx") 
 citation("dplyr")     
 citation("prais")
 citation("ggplot2")
 citation("tidyr")
 citation("scales")
 citation()
 

