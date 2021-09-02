# Instalando os pacotes no seu computador
install.packages("readxl") # ler arquivo de excel
install.packages("lme4") # fazer as análises
install.packages("ggplot2") # fazer os gráficos
install.packages("ggeffects") # fazer os gráficos
install.packages("stargazer") # tabela de resultados
install.packages("sjPlot") # tabela de resultados como imagem
install.packages("report") # escrita dos resultados

# Carregando os Pacotes do seu computador
library(readxl)
library(lme4)
library(ggplot2)
library(ggeffects)
library(stargazer)
library(sjPlot)
library(report)

#Lendo os dados
dados <- readxl::read_excel(file.choose())
View(dados)

########## Crossed random effects| Efeitos Aleatórios Cruzados ##########

# Os efeitos cruzados são considerados independentes uns dos outros 
# e são especificados como termos aleatórios separados na fórmula do modelo

#########################################################################

########## MODELO 1: ATRS; grupo como efeito fixo; ID e tempo como efeito aleatório##########

# Vamos testar o modelo onde interceptos e inclinações variam
modelo1 <- lmer(atrs ~ grupo + (grupo|numero) + (grupo|tempo), REML = FALSE, data = dados)

#isSingular Avalia se um modelo misto ajustado é (quase / quase) singular
# ou seja, os parâmetros estão no limite do espaço de parâmetros viável:
# as variâncias de uma ou mais combinações lineares de efeitos são (perto de) zero.
## Se "TRUE", então é singular. Se "FALSE", então não é singular
isSingular(modelo1, tol = 1e-4)

# Sendo assim, testaremos o modelo onde os interceptos variam mas a inclinação é fixa
modelo1 <- lmer(atrs ~ grupo + (1|numero) + (1|tempo), REML = FALSE, data = dados)

#modelo1 <- lmer(atrs ~ grupo + tempo + (1|numero) , REML = FALSE, data = dados)
summary(modelo1, correlation = FALSE)

#Alocando os resultados para calcular intervalo de confiança
resultados <- profile(modelo1, which = NULL, alphamax = 0.01,
                      maxpts = 100, delta = NULL,
                      delta.cutoff = 1/8, verbose = 0, devtol = 1e-09,
                      maxmult = 10, startmethod = "prev", optimizer = NULL,
                      control=NULL, signames = TRUE,
                      parallel = c("no", "multicore", "snow"),
                      ncpus = getOption("profile.ncpus", 1L), cl = NULL,
                      prof.scale = c("sdcor","varcov"))

#Calcular intervalos de confiança para parametros do ajuste do modelo de lmer
confint(resultados,
        parm = "beta_",
        level = 0.95,
        method = "boot",
        nsim = 500,
        boot.type = "norm",
        FUN = NULL,
        oldNames = FALSE)

#Avalia se um modelo misto ajustado é (quase / quase) singular
#ou seja, os parâmetros estão no limite do espaço de parâmetros viável:
#as variâncias de uma ou mais combinações lineares de efeitos são (perto de) zero.
isSingular(modelo1, tol = 1e-4)

#Normalidade dos Resíduos
plot(modelo1)

#QQ-Plot
qqnorm(resid(modelo1))
qqline(resid(modelo1))  

# Imagens
mm_plot <- ggplot(dados, aes(x = grupo, y = atrs, colour = numero)) +
    facet_wrap(~tempo, nrow = 1) +   # a panel for each tempo range
    geom_point(alpha = 0.5) +
    theme_classic() +
    geom_line(data = cbind(dados, pred = predict(modelo1)), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
    theme(legend.position = "none",
          panel.spacing = unit(2, "lines"))  # adding space between panels
plot(mm_plot)

ggpredict(modelo1, terms = c("grupo", "tempo"), type = "re") |> 
  plot() +
  labs(x = "Grupo", y = "ATRS", title = "Diferença na ATRS entre Tempos de coleta nos Grupos") + 
  theme_minimal()

# Tabela com Resultados
stargazer(modelo1, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

sjPlot::tab_model(modelo1)

## Resultados escritos
resultados1 <- report::report(modelo1)
print(resultados1)

########## MODELO 1: ATRS; grupo e tempo como efeito fixo; ID como efeito aleatório##########

# Vamos testar o modelo onde interceptos e inclinações variam
modelo1_2 <- lmer(atrs ~ grupo + tempo + (grupo + tempo|numero) , REML = FALSE, data = dados)

#isSingular Avalia se um modelo misto ajustado é (quase / quase) singular
# ou seja, os parâmetros estão no limite do espaço de parâmetros viável:
# as variâncias de uma ou mais combinações lineares de efeitos são (perto de) zero.
## Se "TRUE", então é singular. Se "FALSE", então não é singular
isSingular(modelo1_2, tol = 1e-4)

# Sendo assim, testaremos o modelo onde os interceptos variam mas a inclinação é fixa

modelo1_2 <- lmer(atrs ~ grupo + tempo + (1|numero) , REML = FALSE, data = dados)
summary(modelo1_2)

#Alocando os resultados para calcular intervalo de confiança
resultados <- profile(modelo1_2, which = NULL, alphamax = 0.01,
                      maxpts = 100, delta = NULL,
                      delta.cutoff = 1/8, verbose = 0, devtol = 1e-09,
                      maxmult = 10, startmethod = "prev", optimizer = NULL,
                      control=NULL, signames = TRUE,
                      parallel = c("no", "multicore", "snow"),
                      ncpus = getOption("profile.ncpus", 1L), cl = NULL,
                      prof.scale = c("sdcor","varcov"))

#Calcular intervalos de confiança para parametros do ajuste do modelo de lmer
confint(resultados,
        parm = "beta_",
        level = 0.95,
        method = "boot",
        nsim = 500,
        boot.type = "norm",
        FUN = NULL,
        oldNames = FALSE)

#Avalia se um modelo misto ajustado é (quase / quase) singular
#ou seja, os parâmetros estão no limite do espaço de parâmetros viável:
#as variâncias de uma ou mais combinações lineares de efeitos são (perto de) zero.
isSingular(modelo1_2, tol = 1e-4)

#Normalidade dos Resíduos
plot(modelo1_2)

#QQ-Plot
qqnorm(resid(modelo1_2))
qqline(resid(modelo1_2))  

# Tabela com Resultados
stargazer(modelo1_2, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

sjPlot::tab_model(modelo1_2)

## Resultados escritos
resultados1_2 <- report::report(modelo1_2)
print(resultados1_2)

########## Transformando variáveis em numéricas para as próximas análises ########## 
dados$eva_repouso <- as.numeric(dados$eva_repouso)
dados$eva_esforc <- as.numeric(dados$eva_esforc)
dados$forca <- as.numeric(dados$forca)

########## MODELO 2: EVA REPOUSO ##########

# Vamos testar o modelo onde interceptos e inclinações variam
modelo2 <- lmer(eva_repouso ~ grupo + (grupo|numero) + (grupo|tempo), data = dados)

#isSingular Avalia se um modelo misto ajustado é (quase / quase) singular
# ou seja, os parâmetros estão no limite do espaço de parâmetros viável:
# as variâncias de uma ou mais combinações lineares de efeitos são (perto de) zero.
## Se "TRUE", então é singular. Se "FALSE", então não é singular
isSingular(modelo2, tol = 1e-4)

# Sendo assim, testaremos o modelo onde os interceptos variam mas a inclinação é fixa
modelo2 <- lmer(eva_repouso ~ grupo + (1|numero) + (1|tempo), data = dados)
summary(modelo2, correlation = FALSE)

#Alocando os resultados para calcular intervalo de confiança
resultados <- profile(modelo2, which = NULL, alphamax = 0.01,
                      maxpts = 100, delta = NULL,
                      delta.cutoff = 1/8, verbose = 0, devtol = 1e-09,
                      maxmult = 10, startmethod = "prev", optimizer = NULL,
                      control=NULL, signames = TRUE,
                      parallel = c("no", "multicore", "snow"),
                      ncpus = getOption("profile.ncpus", 1L), cl = NULL,
                      prof.scale = c("sdcor","varcov"))

#Calcular intervalos de confiança para parametros do ajuste do modelo de lmer
confint(resultados,
        parm = "beta_",
        level = 0.95,
        method = "boot",
        nsim = 500,
        boot.type = "norm",
        FUN = NULL,
        oldNames = FALSE)

#Avalia se um modelo misto ajustado é (quase / quase) singular
#ou seja, os parâmetros estão no limite do espaço de parâmetros viável:
#as variâncias de uma ou mais combinações lineares de efeitos são (perto de) zero.
isSingular(modelo2, tol = 1e-4)

#Normalidade dos Resíduos
plot(modelo2)

#QQ-Plot
qqnorm(resid(modelo2))
qqline(resid(modelo2))  

# Imagens
mm_plot <- ggplot(dados, aes(x = grupo, y = atrs, colour = numero)) +
  facet_wrap(~tempo, nrow = 1) +   # a panel for each tempo range
  geom_point(alpha = 0.5) +
  theme_classic() +
  geom_line(data = cbind(dados, pred = predict(modelo2)), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
  theme(legend.position = "none",
        panel.spacing = unit(2, "lines"))  # adding space between panels
plot(mm_plot)

ggpredict(modelo2, terms = c("grupo", "tempo"), type = "re") |> 
  plot() +
  labs(x = "Grupo", y = "ATRS", title = "Diferença na ATRS entre Tempos de coleta nos Grupos") + 
  theme_minimal()

# Tabela com Resultados
stargazer(modelo2, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

sjPlot::tab_model(modelo2)

## Resultados escritos
resultados2 <- report::report(modelo2)
print(resultados2)

########## MODELO 3: EVA Esforço ##########

# Vamos testar o modelo onde interceptos e inclinações variam
modelo3 <- lmer(eva_esforc ~ grupo + (grupo|numero) + (grupo|tempo), data = dados)

#isSingular Avalia se um modelo misto ajustado é (quase / quase) singular
# ou seja, os parâmetros estão no limite do espaço de parâmetros viável:
# as variâncias de uma ou mais combinações lineares de efeitos são (perto de) zero.
## Se "TRUE", então é singular. Se "FALSE", então não é singular
isSingular(modelo3, tol = 1e-4)

# Sendo assim, testaremos o modelo onde os interceptos variam mas a inclinação é fixa
modelo3 <- lmer(eva_esforc ~ grupo + (1|numero) + (1|tempo), data = dados)
summary(modelo3, correlation = FALSE)

#Alocando os resultados para calcular intervalo de confiança
resultados <- profile(modelo3, which = NULL, alphamax = 0.01,
                      maxpts = 100, delta = NULL,
                      delta.cutoff = 1/8, verbose = 0, devtol = 1e-09,
                      maxmult = 10, startmethod = "prev", optimizer = NULL,
                      control=NULL, signames = TRUE,
                      parallel = c("no", "multicore", "snow"),
                      ncpus = getOption("profile.ncpus", 1L), cl = NULL,
                      prof.scale = c("sdcor","varcov"))

#Calcular intervalos de confiança para parametros do ajuste do modelo de lmer
confint(resultados,
        parm = "beta_",
        level = 0.95,
        method = "boot",
        nsim = 500,
        boot.type = "norm",
        FUN = NULL,
        oldNames = FALSE)

#Avalia se um modelo misto ajustado é (quase / quase) singular
#ou seja, os parâmetros estão no limite do espaço de parâmetros viável:
#as variâncias de uma ou mais combinações lineares de efeitos são (perto de) zero.
isSingular(modelo3, tol = 1e-4)

#Normalidade dos Resíduos
plot(modelo3)

#QQ-Plot
qqnorm(resid(modelo3))
qqline(resid(modelo3))  

# Imagens
mm_plot <- ggplot(dados, aes(x = grupo, y = atrs, colour = numero)) +
  facet_wrap(~tempo, nrow = 1) +   # a panel for each tempo range
  geom_point(alpha = 0.5) +
  theme_classic() +
  geom_line(data = cbind(dados, pred = predict(modelo3)), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
  theme(legend.position = "none",
        panel.spacing = unit(2, "lines"))  # adding space between panels
plot(mm_plot)

ggpredict(modelo3, terms = c("grupo", "tempo"), type = "re") |> 
  plot() +
  labs(x = "Grupo", y = "ATRS", title = "Diferença na ATRS entre Tempos de coleta nos Grupos") + 
  theme_minimal()

# Tabela com Resultados
stargazer(modelo3, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

sjPlot::tab_model(modelo3)

## Resultados escritos
resultados3 <- report::report(modelo3)
print(resultados3)

########## MODELO 4: Força ##########

# Vamos testar o modelo onde interceptos e inclinações variam
modelo4 <- lmer(forca ~ grupo + (grupo|numero) + (grupo|tempo), data = dados)

#isSingular Avalia se um modelo misto ajustado é (quase / quase) singular
# ou seja, os parâmetros estão no limite do espaço de parâmetros viável:
# as variâncias de uma ou mais combinações lineares de efeitos são (perto de) zero.
## Se "TRUE", então é singular. Se "FALSE", então não é singular
isSingular(modelo4, tol = 1e-4)

# Sendo assim, testaremos o modelo onde os interceptos variam mas a inclinação é fixa
modelo4 <- lmer(forca ~ grupo + (1|numero) + (1|tempo), data = dados)
summary(modelo4, correlation = FALSE)

#Alocando os resultados para calcular intervalo de confiança
resultados <- profile(modelo4, which = NULL, alphamax = 0.01,
                      maxpts = 100, delta = NULL,
                      delta.cutoff = 1/8, verbose = 0, devtol = 1e-09,
                      maxmult = 10, startmethod = "prev", optimizer = NULL,
                      control=NULL, signames = TRUE,
                      parallel = c("no", "multicore", "snow"),
                      ncpus = getOption("profile.ncpus", 1L), cl = NULL,
                      prof.scale = c("sdcor","varcov"))

#Calcular intervalos de confiança para parametros do ajuste do modelo de lmer
confint(resultados,
        parm = "beta_",
        level = 0.95,
        method = "boot",
        nsim = 500,
        boot.type = "norm",
        FUN = NULL,
        oldNames = FALSE)

#Avalia se um modelo misto ajustado é (quase / quase) singular
#ou seja, os parâmetros estão no limite do espaço de parâmetros viável:
#as variâncias de uma ou mais combinações lineares de efeitos são (perto de) zero.
isSingular(modelo4, tol = 1e-4)

#Normalidade dos Resíduos
plot(modelo4)

#QQ-Plot
qqnorm(resid(modelo4))
qqline(resid(modelo4))  

# Imagens
mm_plot <- ggplot(dados, aes(x = grupo, y = atrs, colour = numero)) +
  facet_wrap(~tempo, nrow = 1) +   # a panel for each tempo range
  geom_point(alpha = 0.5) +
  theme_classic() +
  geom_line(data = cbind(dados, pred = predict(modelo4)), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
  theme(legend.position = "none",
        panel.spacing = unit(2, "lines"))  # adding space between panels
plot(mm_plot)

ggpredict(modelo4, terms = c("grupo", "tempo"), type = "re") |> 
  plot() +
  labs(x = "Grupo", y = "ATRS", title = "Diferença na ATRS entre Tempos de coleta nos Grupos") + 
  theme_minimal()

# Tabela com Resultados
stargazer(modelo4, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

sjPlot::tab_model(modelo4)

## Resultados escritos
resultados4 <- report::report(modelo4)
print(resultados4)
