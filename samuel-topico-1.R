# ========================== #
#### Av. 1: TÓPICO I ####
# ========================== #

# Tópico 1 – MQO Simples e Múltiplo com PNADc
# Samuel Maia - 2025-05-00
# Econometria I - Professora Ana Hermeto

# Objetivo: Estimar modelos MQO e analisar pressupostos seguindo o roteiro
# ========================== #


# ========================== #
##### Preparando ambiente #####

# Pacotes necessários
pacman::p_load(tidyverse, haven, car, lmtest, sandwich, modelsummary, stargazer, ggplot2, extrafont, car, lmtest, sandwich, Cairo)
# comment: carregamos `tidyverse` para manipulação, `haven` para importar `.dta`, ggplot2, e demais pacotes para diagnóstico e tabelas de regressão (`car` para VIF; `lmtest` para heterocedasticidade, `sandwich` para erros robustos)


# Importar fonte para figuras
# extrafont::font_import(prompt = FALSE)  # Pode demorar
# extrafont::loadfonts(device = "pdf")

# fonts()

# Define o diretório de trabalho (ajuste conforme a pasta relevante)
setwd("/Users/samuelmaia/Desktop/2025-1/econometrics/avaliacao1")

# ========================== #



# ========================== #
##### Load & Clean #####
# ========================== #

# Importa base PNADc (versão capitais, .dta)
pnad <- read_dta("dados/pnadc_2023_5_capitais.dta")
# Importa base (PNADc - Capitais)
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
glimpse(pnad_clean$idade2)
glimpse(pnad_clean$sexo)

# ========================== #


# ========================== #
##### 2.3 Descritivas #####
# ========================== #


# Estatísticas descritivas e exportação
datasummary_skim(
    pnad_clean[, c("log_rend", "anosest", "idade", "idade2")],
    output = "latex"
)

datasummary(
    N ~ sexo, data = pnad_clean, output = "latex", title = "Distribuição por Sexo"
)


# Output: tabela exportável para ser .tex: `tabela_descr_tarefa1.tex`


# Histogramas

# Criar pasta para figuras
dir.create("figs", showWarnings = FALSE)


# Gerar histograma
plot_histogram_latex <- function(data, var, filename, binwidth = NULL, width = 5, height = 3.2) {
    # Nome da variável para eixos/título
    var_sym <- rlang::ensym(var)
    
    # Gera gráfico
    p <- ggplot(data, aes(x = !!var_sym)) +
        geom_histogram(binwidth = binwidth, fill = "gray70", color = "black") +
        labs(
            # title = sprintf("Distribuição de %s", rlang::as_label(var_sym)),
            x = rlang::as_label(var_sym),
            y = "Frequência"
        ) +
        theme_minimal(base_family = "STIX Two Text", base_size = 11) +
        theme(
            plot.title = element_text(hjust = 0.5, face = "bold"),
            axis.title = element_text(face = "bold"),
            axis.text = element_text(size = 10),
            panel.grid = element_blank(),
            panel.border = element_rect(color = "black", fill = NA)
        )
    
    # Salva como PDF
    ggsave(filename, plot = p, width = width, height = height)
}


# Para anos de estudo
plot_histogram_latex(pnad_clean, anosest, "figs/hist_anosest.pdf", binwidth = 1)

# Para log da renda
plot_histogram_latex(pnad_clean, log_rend, "figs/hist_log_rend.pdf", binwidth = 0.2)

# Para idade
plot_histogram_latex(pnad_clean, idade, "figs/hist_idade.pdf", binwidth = 2)

# Para idade ao quadrado
plot_histogram_latex(pnad_clean, idade2, "figs/hist_idade2.pdf", binwidth = 200)


# ========================== #



# ========================== #
##### 2.4 Modelos #####
# ========================== #


###### 2.4.1 Modelo 1: Regressão simples ####
modelo1 <- lm(log_rend ~ anosest, data = pnad_clean)

print(modelo1)
summary(modelo1)

modelsummary(
    modelo1,
    output = "tabela_regsimples.tex"
)

# 2.5.1 Estimativa manual #
x <- pnad_clean$anosest
y <- pnad_clean$log_rend
beta1_hat <- cov(x, y) / var(x)
beta0_hat <- mean(y) - beta1_hat * mean(x)
cat("Beta1 manual:", beta1_hat, "\nBeta0 manual:", beta0_hat)



###### 2.4.2 Modelo 2: + idade e idade² ####
modelo2 <- lm(log_rend ~ anosest + idade + idade2, data = pnad_clean)

print(modelo2)
summary(modelo2)



###### 2.4.3 Modelo 3: + dummy sexo ####
modelo3 <- lm(log_rend ~ anosest + idade + idade2 + sexo, data = pnad_clean)

print(modelo3)
summary(modelo3)

###### 2.4.4 Modelo 4: + interação anosest:sexo ####
modelo4 <- lm(log_rend ~ anosest * sexo + idade + idade2, data = pnad_clean)

print(modelo4)
summary(modelo4)

# ========================== #



# ========================== #
##### 2.6 Comparação dos modelos #####
# ========================== #

# Tabela de comparação dos modelos (coef., erros padrão, R², N)
modelsummary(
    list("Simples" = modelo1, "Múltiplo" = modelo2, "Com Dummy" = modelo3, "Com Interação" = modelo4),
    stars = TRUE,
    output = "latex",  # ou "markdown" para preview
    statistic = "({std.error})",
    gof_omit = "IC|Log|AIC|BIC|F|RMSE"  # para deixar só R2 e N
)
# ========================== #


# ========================== #
##### 2.5/2.6 Diagnóstico de pressupostos ##### 
# ========================== #

# Multicolinearidade
vif(modelo2); vif(modelo3); vif(modelo4)

# Heterocedasticidade
bptest(modelo1); bptest(modelo2); bptest(modelo3); bptest(modelo4)
# ========================== #

# Estimação com erros robustos (correção tipo White)
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
##### Plots #####
# ========================== #

###### 1. Dispersão + reta MQO (Modelo 1) ####
ggplot(pnad_clean, aes(x = anosest, y = log_rend)) +
    geom_jitter(color = "#4e79a7", alpha = 0.15, size = 0.4, width = 0.3) +
    geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 0.8) +
    labs(
        x = "Anos de Estudo",
        y = "Logaritmo da Renda do Trabalho"
    ) +
    theme_minimal(base_family = "STIX Two Text", base_size = 11) +
    theme(
        axis.title = element_text(),
        axis.text  = element_text(size = 10),
        panel.grid = element_blank(),
        panel.border = element_rect(color = "gray", fill = NA)
    )
ggsave("figs/disp_anosest_logrend.pdf", width = 5, height = 3.2, device = cairo_pdf)


###### 2. Resíduos × Ajustados (Modelo 2) ####
res_df <- data.frame(
    ajustados = fitted(modelo2),
    residuos  = resid(modelo2)
)

ggplot(res_df, aes(x = ajustados, y = residuos)) +
    geom_point(color = "#4e79a7", alpha = 0.15, size = 0.4) +  # <- ajuste aqui
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(
        x = "Valores Ajustados",
        y = "Resíduos"
    ) +
    theme_minimal(base_family = "STIX Two Text", base_size = 11) +
    theme(
        axis.title = element_text(),
        axis.text  = element_text(size = 10),
        panel.grid = element_blank(),
        panel.border = element_rect(color = "gray", fill = NA)
    )
ggsave("figs/residuos_modelo2.pdf", width = 5, height = 3.2, device = cairo_pdf)



###### 3. Curva de predição idade–renda (Modelo 2) ####
idade_seq <- 18:70
grid <- data.frame(
    idade = idade_seq,
    idade2 = idade_seq^2,
    anosest = mean(pnad_clean$anosest)
)
grid$yhat <- predict(modelo2, newdata = grid)

ggplot(grid, aes(x = idade, y = yhat)) +
    geom_line(color = "blue", linewidth = 0.8) +
    labs(
        x = "Idade",
        y = "Logaritmo da Renda Predita"
    ) +
    theme_minimal(base_family = "STIX Two Text", base_size = 11) +
    theme(
        axis.title = element_text(),
        axis.text  = element_text(size = 10),
        panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill = NA)
    )
ggsave("figs/curva_idade_renda.pdf", width = 5, height = 3.2, device = cairo_pdf)


###### 4. Interação Anos de Estudo × Sexo (Modelo 4) ####
grid2 <- expand.grid(
    anosest = seq(min(pnad_clean$anosest), max(pnad_clean$anosest)),
    sexo    = levels(pnad_clean$sexo)
)
grid2$idade   <- mean(pnad_clean$idade)
grid2$idade2  <- grid2$idade^2
grid2$predito <- predict(modelo4, newdata = grid2)

ggplot(pnad_clean, aes(x = anosest, y = log_rend, color = sexo)) +
    geom_jitter(alpha = 0.15, size = 0.4, width = 0.3) +
    
    # Contorno preto
    geom_line(data = grid2, aes(x = anosest, y = predito, group = sexo),
        color = "black", linewidth = 1.5) +
    
    # Linha colorida por cima
    geom_line(data = grid2, aes(x = anosest, y = predito, color = sexo),
        linewidth = 0.8) +
    
    labs(
        title  = "Interação: Anos de Estudo × Sexo",
        x      = "Anos de Estudo",
        y      = "Logaritmo da Renda do Trabalho",
        color  = "Sexo"
    ) +
    scale_color_manual(values = c("Mulher" = "#4575b4", "Homem" = "#d73027")) +
    theme_minimal(base_family = "STIX Two Text", base_size = 11) +
    theme(
        plot.title = element_text(hjust = 0.5),
        axis.title = element_text(),
        axis.text  = element_text(size = 10),
        panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill = NA)
    )

ggsave("figs/interacao_anosest_sexo.pdf", width = 5, height = 3.2, device = cairo_pdf)
# ========================== #



# ========================== #
#### FIM ####
# ========================== #