# ========================== #
#### AV. 1: TÓPICO 1 ####
# ========================== #

# Tópico 1 – MQO Simples e Múltiplo com PNADc
# Samuel Maia - 2025-05-00
# Econometria I - Professora Ana Hermeto

# A numeração entre parênteses ao longo do script corresponde às seções e subseções do relatório principal (e.g., 2.3.1 refere-se à subseção "2.3.1 Modelo 1: Regressão simples com anos de estudo").
# ========================== #

# ========================== #
##### Preparando ambiente #####
# ========================== #

# Pacotes necessários
pacman::p_load(tidyverse, haven, car, lmtest, sandwich, modelsummary, stargazer, ggplot2, extrafont, Cairo, devtools)

# Define o diretório de trabalho (ajuste conforme a pasta relevante)
setwd("/Users/samuelmaia/Desktop/2025-1/econometrics/avaliacao1")

# ========================== #

# ========================== #
##### Load & Clean #####
# ========================== #

# Importa base PNADc (versão capitais, .dta)
pnad <- read_dta("dados/pnadc_2023_5_capitais.dta")

# Transforma variáveis
pnad <- pnad %>%
    mutate(
        log_rend = log(rendtrabprinc + 1),
        idade2 = idade^2
    )

# Filtra base: missings, outliers, consistência
pnad_clean <- pnad %>%
    filter(
        !is.na(log_rend),
        !is.na(anosest),
        !is.na(idade),
        rendtrabprinc >= 0,
        idade >= 14 & idade < 100,
        anosest < 25
    ) %>%
    mutate(
        sexo = factor(sexo, levels = c(0, 1), labels = c("Mulher", "Homem"))
    )

# Checar colunas `log_rend` e `idade2`
glimpse(pnad_clean)
glimpse(pnad_clean$log_rend)
glimpse(pnad_clean$anosest)
glimpse(pnad_clean$idade)
glimpse(pnad_clean$idade2)

# ========================== #

# ========================== #
##### (2.2) Descritivas #####
# ========================== #

# Estatísticas descritivas e exportação
datasummary_skim(
    pnad_clean[, c("log_rend", "anosest", "idade", "idade2")],
    output = "latex"
)

# Histogramas

# Criar pasta para figuras
dir.create("figs", showWarnings = FALSE)

# Gerar histograma
plot_histogram_latex <- function(data, var, filename, binwidth = NULL, width = 5, height = 3.2) {
    var_sym <- rlang::ensym(var)
    
    p <- ggplot(data, aes(x = !!var_sym)) +
        geom_histogram(binwidth = binwidth, fill = "gray70", color = "black") +
        labs(x = rlang::as_label(var_sym), y = "Frequência") +
        theme_minimal(base_family = "STIX Two Text", base_size = 11) +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"), axis.title = element_text(face = "bold"), axis.text = element_text(size = 10), panel.grid = element_blank(), panel.border = element_rect(color = "black", fill = NA))
    
    ggsave(filename, plot = p, width = width, height = height)
}

# Para anos de estudo
plot_histogram_latex(pnad_clean, anosest, "figs/hist_anosest.pdf", binwidth = 1)

# ========================== #

# ========================== #
##### (2.3) Modelos #####
# ========================== #

###### (2.3.1) Modelo 1: Regressão simples ####
modelo1 <- lm(log_rend ~ anosest, data = pnad_clean)
print(modelo1)
summary(modelo1)

# ========================== #

###### (2.3.2) Modelo 2: + idade e idade² ####
modelo2 <- lm(log_rend ~ anosest + idade + idade2, data = pnad_clean)
print(modelo2)
summary(modelo2)

###### (2.3.3) Modelo 3: + dummy sexo ####
modelo3 <- lm(log_rend ~ anosest + idade + idade2 + sexo, data = pnad_clean)
print(modelo3)
summary(modelo3)

###### (2.3.4) Modelo 4: + interação anosest:sexo ####
modelo4 <- lm(log_rend ~ anosest * sexo + idade + idade2, data = pnad_clean)
print(modelo4)
summary(modelo4)

# ========================== #
# ========================== #


# ========================== #
# ========================== #

###### (2.4.1) Estimação manual do Modelo 1 ####

x <- pnad_clean$anosest
y <- pnad_clean$log_rend

mean(x)        # média de anos de estudo
mean(y)        # média de log da renda
var(x)         # variância de anos de estudo
cov(x, y)      # covariância entre anos de estudo e log da renda

beta1_hat <- cov(x, y) / var(x)
beta0_hat <- mean(y) - beta1_hat * mean(x)

cat("Beta1 manual:", beta1_hat, "\nBeta0 manual:", beta0_hat)

# ========================== #
# ========================== #



# ========================== #
#### AV.: TÓPICO 2 ####
# ========================== #

# ========================== #
##### (3.3) Diagnóstico de pressupostos ##### 
# ========================== #

# 1. Multicolinearidade: VIF
vif(modelo2); vif(modelo3); vif(modelo4)

# 2. Heterocedasticidade: teste de Breusch-Pagan
bptest(modelo1); bptest(modelo2); bptest(modelo3); bptest(modelo4)

# Estimação com erros robustos (tipo White)
modelsummary(
    list(Simples = modelo1, Múltiplo = modelo2, ComDummy = modelo3, ComInter = modelo4),
    vcov = "HC0",
    output = "tabela_robusta.tex",
    fmt = 3,
    stars = c('*' = .1, '**' = .05, '***' = .01),
    gof_omit = "AIC|BIC|Log.Lik|RMSE"
)

# ========================== #

# ========================== #
##### (3.5) Comparando Modelos ####
# ========================== #

# Comparação de Modelos com AIC, BIC, R² ajustado e erros robustos
modelsummary(
    list("Simples" = modelo1, "Múltiplo" = modelo2, "Com Dummy" = modelo3, "Com Interação" = modelo4),
    stars = TRUE,
    output = "latex",  # ou "markdown" para preview
    statistic = "({std.error})",
    gof_omit = "IC|Log|AIC|BIC|F|RMSE"  # para deixar só R2 e N
)
# ========================== #

# ========================== #
##### Gráficos de Diagnóstico ####
# ========================== #

# Resíduos x Ajustados (Modelo 2)
res_df <- data.frame(
    ajustados = fitted(modelo2),
    residuos  = resid(modelo2)
)

ggplot(res_df, aes(x = ajustados, y = residuos)) +
    geom_point(color = "#4e79a7", alpha = 0.15, size = 0.4) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(x = "Valores Ajustados", y = "Resíduos") +
    theme_minimal(base_family = "STIX Two Text", base_size = 11) +
    theme(axis.title = element_text(), axis.text  = element_text(size = 10), panel.grid = element_blank(), panel.border = element_rect(color = "gray", fill = NA))

ggsave("figs/residuos_modelo2.pdf", width = 5, height = 3.2, device = cairo_pdf)

# ========================== #

# ========================== #
###### Curva de predição idade–renda (Modelo 2) ####
# ========================== #

idade_seq <- 18:70
grid <- data.frame(
    idade = idade_seq,
    idade2 = idade_seq^2,
    anosest = mean(pnad_clean$anosest)
)
grid$yhat <- predict(modelo2, newdata = grid)

ggplot(grid, aes(x = idade, y = yhat)) +
    geom_line(color = "blue", linewidth = 0.8) +
    labs(x = "Idade", y = "Logaritmo da Renda Predita") +
    theme_minimal(base_family = "STIX Two Text", base_size = 11) +
    theme(axis.title = element_text(), axis.text  = element_text(size = 10), panel.grid = element_blank(), panel.border = element_rect(color = "black", fill = NA))

ggsave("figs/curva_idade_renda.pdf", width = 5, height = 3.2, device = cairo_pdf)

# ========================== #

# ========================== #
###### Interação Anos de Estudo × Sexo (Modelo 4) ####
# ========================== #

grid2 <- expand.grid(
    anosest = seq(min(pnad_clean$anosest), max(pnad_clean$anosest)),
    sexo    = levels(pnad_clean$sexo)
)
grid2$idade   <- mean(pnad_clean$idade)
grid2$idade2  <- grid2$idade^2
grid2$predito <- predict(modelo4, newdata = grid2)

ggplot(pnad_clean, aes(x = anosest, y = log_rend, color = sexo)) +
    geom_jitter(alpha = 0.15, size = 0.4, width = 0.3) +
    geom_line(data = grid2, aes(x = anosest, y = predito, group = sexo), color = "black", linewidth = 1.5) +
    geom_line(data = grid2, aes(x = anosest, y = predito, color = sexo), linewidth = 0.8) +
    labs(title  = "Interação: Anos de Estudo × Sexo", x      = "Anos de Estudo", y      = "Logaritmo da Renda do Trabalho", color  = "Sexo") +
    scale_color_manual(values = c("Mulher" = "#4575b4", "Homem" = "#d73027")) +
    theme_minimal(base_family = "STIX Two Text", base_size = 11) +
    theme(plot.title = element_text(hjust = 0.5), axis.title = element_text(), axis.text  = element_text(size = 10), panel.grid = element_blank(), panel.border = element_rect(color = "black", fill = NA))

ggsave("figs/interacao_anosest_sexo.pdf", width = 5, height = 3.2, device = cairo_pdf)

writeLines(capture.output(devtools::session_info()), "session-info.txt")


# ========================== #
#### FIM ####
# ========================== #
