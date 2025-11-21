# Carregar pacotes necessários

# Este script verifica se os pacotes estão instalados. Se não (!require), instala e carrega.
# O argumento 'quiet = TRUE' remove as mensagens de download e instalação.

# ------------------------------------------------------------------------------
# A. Importação de Dados
# ------------------------------------------------------------------------------
if(!require("gsheet")) install.packages("gsheet", quiet = TRUE)
require("gsheet")     # Permite ler planilhas do Google Sheets diretamente pela URL

# ------------------------------------------------------------------------------
# B. Manipulação e Visualização de Dados
# ------------------------------------------------------------------------------
if(!require("tidyverse")) install.packages("tidyverse", quiet = TRUE)
require("tidyverse")  # Metapacote essencial: carrega dplyr, ggplot2, tidyr, etc.

if(!require("ggrepel")) install.packages("ggrepel", quiet = TRUE)
require("ggrepel")    # Evita que textos/rótulos se sobreponham nos gráficos

if(!require("corrplot")) install.packages("corrplot", quiet = TRUE)
require("corrplot")   # Ferramenta visual para criar matrizes de correlação

# ------------------------------------------------------------------------------
# C. Modelagem Estatística
# ------------------------------------------------------------------------------
if(!require("nlme")) install.packages("nlme", quiet = TRUE)
require("nlme")       # Ajuste de Modelos Lineares de Efeitos Mistos e GLS
# Fundamental para definir estruturas de correlação (AR1, CS)

if(!require("broom")) install.packages("broom", quiet = TRUE)
require("broom")      # Transforma resultados de modelos (summary) em tabelas limpas

# ------------------------------------------------------------------------------
# D. Apresentação de Resultados
# ------------------------------------------------------------------------------
if(!require("DT")) install.packages("DT", quiet = TRUE)
require("DT")         # Cria tabelas interativas e paginadas para relatórios HTML

if(!require("reactable")) install.packages("reactable", quiet = TRUE)
require("reactable")  # Outra opção excelente para tabelas interativas


# 2. Importação dos Dados
# URL pública da planilha contendo os dados do experimento
url_stroke <- "https://docs.google.com/spreadsheets/d/1n394dzsNn1QITgigbHJ21uz6hi5TDCHs/edit?gid=712419062#gid=712419062"

# Leitura dos dados brutos transformando diretamente em tibble (dataframe moderno)
bd_stroke <- gsheet::gsheet2tbl(url_stroke)

# 3. Manipulação e Limpeza de Dados (Data Wrangling)
# A transformação para formato LONGO é obrigatória para funções como lme() ou lmer().

bd_stroke_long <- bd_stroke %>%
  
  # Renomeia as variáveis identificadoras para facilitar a interpretação no R.
  dplyr::rename(
    Sujeito = Subject,  # ID do paciente (define o nível de agrupamento/efeito aleatório)
    Grupo   = Group     # Fator fixo de interesse (Tratamento A vs B)
  ) %>%
  
  # Etapa B: Reestruturação de Dados (Wide -> Long)
  # O R precisa que cada linha seja uma observação única no tempo.
  tidyr::pivot_longer(
    cols = week_1:week_8,    # Vetor de colunas que contêm as medidas repetidas
    names_to = "Semana",     # Nova variável categórica indicando o tempo
    values_to = "Valor"      # Nova variável contínua com a resposta (outcome)
  ) %>%
  
  # Etapa C: Engenharia de Variáveis (Feature Engineering)
  dplyr::mutate(
    # Necessário para tratar o tempo como covariável contínua (tendência linear/quadrática).
    Semana = as.numeric(stringr::str_remove(Semana, "week_")),
    # Centralização do Tempo (Time Centering)
    # CRUCIAL: Ao subtrair 1, transformamos a escala de [1, 8] para [0, 7].
    # facilitando a interpretação clínica dos parâmetros do modelo.
    Semana_recentralizada = Semana - 1
  )

dplyr::glimpse(bd_stroke)

# 4. VISUALIZAÇÃO EXPLORATÓRIA (AED)

# ------------------------------------------------------------------------------
# Gráfico A: Trajetórias Individuais (Spaghetti Plot)
# ------------------------------------------------------------------------------
# Diagnóstico Visual:
# - Linhas paralelas? -> Intercepto Aleatório basta.
# - Linhas se cruzam/abrem leque? -> Precisa de Inclinação Aleatória (Random Slope).

grafico_trajetorias <- ggplot2::ggplot(
  data = bd_stroke_long,
  mapping = ggplot2::aes(
    x = Semana, 
    y = Valor, 
    group = Sujeito, # Mapeia a estrutura intra-sujeito
    color = Grupo
  )
) +
  # Adiciona as linhas dos indivíduos (alpha baixo para visualizar densidade)
  # Nota: 'linewidth' substitui 'size' nas versões novas do ggplot2
  ggplot2::geom_line(alpha = 0.6, linewidth = 0.5) +
  
  # Marca os pontos observados
  ggplot2::geom_point(size = 1.2, alpha = 0.8) +
  
  # Adiciona a tendência média suavizada (LOESS) por Grupo
  # Removemos 'group=Sujeito' aqui para o R calcular a média do grupo inteiro
  ggplot2::geom_smooth(
    mapping = ggplot2::aes(group = Grupo),
    method = "loess",
    se = FALSE,        # Remove intervalo de confiança para limpar o visual
    color = "black",   # Destaque em preto
    linetype = "dashed",
    linewidth = 1
  ) +
  
  # Separação por painéis para facilitar comparação
  ggplot2::facet_wrap(~Grupo) +
  
  # Estética acadêmica (fundo branco)
  ggplot2::theme_bw() +
  ggplot2::labs(
    title = "Trajetórias Individuais de Recuperação (Spaghetti Plot)",
    subtitle = "Linhas coloridas: Pacientes | Linha tracejada: Tendência média",
    x = "Semana de Tratamento",
    y = "Medida de Recuperação",
    color = "Grupo Experimental"
  )

print(grafico_trajetorias)

# Gráfico B: Boxplots no Tempo

# Diagnóstico Visual: Heterocedasticidade.
# Se a variância (tamanho da caixa) muda muito entre as semanas, o modelo precisará
# de ajuste de pesos (weights = varIdent).

grafico_boxplot <- ggplot2::ggplot(
  data = bd_stroke_long,
  mapping = ggplot2::aes(
    x = factor(Semana), # Converte para fator para eixo discreto
    y = Valor, 
    fill = Grupo
  )
) +
  ggplot2::geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  # Jitter adiciona os pontos reais deslocados para ver a dispersão
  ggplot2::geom_jitter(
    position = ggplot2::position_jitterdodge(jitter.width = 0.2),
    alpha = 0.4, 
    size = 1
  ) +
  ggplot2::theme_bw() +
  ggplot2::labs(
    title = "Distribuição e Variabilidade por Semana",
    x = "Semana",
    y = "Valor da Medida"
  )

print(grafico_boxplot)

# Diagnóstico C: Estrutura de Correlação (Correlograma)
# Objetivo: Decidir a estrutura da matriz R (Independent, AR1, Unstructured).

matriz_correlacao <- bd_stroke %>% 
  dplyr::select(dplyr::starts_with("week")) %>%   # 1. Seleciona colunas originais (week_1...)
  dplyr::rename_with(                             # 2. TRADUZ: Troca "week_" por "Semana "
    ~ stringr::str_replace(., "week_", "Semana ") 
  ) %>%
  stats::cor(                                     # 3. Calcula a correlação
    use = "complete.obs", 
    method = "pearson"
  )

# Visualização da Matriz (Agora com rótulos em Português)
corrplot::corrplot(
  corr = matriz_correlacao,
  method = "color",       # Cores indicam intensidade
  type = "upper",         # Apenas triângulo superior (evita redundância)
  addCoef.col = "black",  # Mostra o valor numérico da correlação
  tl.col = "black",       # Texto preto
  tl.srt = 90,            # Rotação do texto
  diag = FALSE,           # Remove a diagonal principal (r=1)
  title = "Matriz de Correlação Temporal (Decaimento sugere AR1)",
  mar = c(0,0,2,0)
)

# 5. Resumo Numérico (Tabela Descritiva)
tabela_descritiva <- bd_stroke_long %>%
  dplyr::group_by(Grupo, Semana) %>%
  dplyr::summarise(
    Media = mean(Valor, na.rm = TRUE),
    Desvio_Padrao = sd(Valor, na.rm = TRUE),
    N_Pacientes = dplyr::n(),
    Erro_Padrao = Desvio_Padrao / sqrt(N_Pacientes),
    .groups = "drop"
  )



# 6. MODELAGEM ESTATÍSTICA (GLS)

# Variável Y: Valor
# Variáveis X: Grupo, Semana_recentralizada
# Agrupamento: Sujeito

# Modelo CS (Simetria Composta) - Correlação constante
mod_cs <- nlme::gls(
  model = Valor ~ Grupo * Semana_recentralizada,
  data = bd_stroke_long,
  correlation = nlme::corCompSymm(form = ~ 1 | Sujeito),
  method = "REML"
)

# Modelo AR1 (Autorregressivo) - Correlação decai com o tempo
mod_ar1 <- nlme::gls(
  model = Valor ~ Grupo * Semana_recentralizada,
  data = bd_stroke_long,
  correlation = nlme::corAR1(form = ~ Semana_recentralizada | Sujeito),
  method = "REML"
)

# Modelo SYMM (Não Estruturado) - Correlação livre (Cuidado: muitos parâmetros!)
# Se der erro de convergência, é porque temos poucos pacientes para 8 semanas.
mod_symm <- nlme::gls(
  model = Valor ~ Grupo * Semana_recentralizada,
  data = bd_stroke_long,
  correlation = nlme::corSymm(form = ~ 1 | Sujeito),
  method = "REML"
)

# 7. Comparação e Seleção de Modelos

# Critério AIC e BIC (O menor valor vence)
print(stats::AIC(mod_cs, mod_ar1, mod_symm))
print(stats::BIC(mod_cs, mod_ar1, mod_symm))


summary(mod_cs) ; summary(mod_ar1) ; summary(mod_symm)


# Teste de Razão de Verossimilhança (LRT)
# H0: Modelos restrito (poucos parametros ) | H1: Modelo saturado  (muitos parametros)
print(stats::anova(mod_cs, mod_ar1))
print(stats::anova(mod_ar1, mod_symm))
print(stats::anova(mod_cs, mod_symm))


# ------------------------------------------------------------------------------
# 8. DIAGNÓSTICO DO MODELO (AR1)
# ------------------------------------------------------------------------------

# 1. Inspeção Visual Básica (Homocedasticidade e Normalidade)
# ------------------------------------------------------------------------------
# Configura a área de plotagem para 2 gráficos lado a lado
graphics::par(mfrow = c(1, 2))

# Gráfico 1: Resíduos vs Ajustados (Homocedasticidade)
# Usamos type = "normalized" para descontar a correlação temporal
graphics::plot(
  stats::resid(mod_ar1, type = "normalized") ~ stats::fitted(mod_ar1),
  main = "Homocedasticidade (Resíduos vs Preditos)",
  xlab = "Valores Preditos",
  ylab = "Resíduos Normalizados",
  pch = 19,
  col = "#2c3e50" # Azul escuro
)
graphics::abline(h = 0, col = "red", lty = 2, lwd = 2)

# Gráfico 2: QQ-Plot (Normalidade)
stats::qqnorm(
  stats::resid(mod_ar1, type = "normalized"),
  main = "Normalidade (QQ-Plot Normalizados)",
  pch = 19,
  col = "#2c3e50"
)
stats::qqline(
  stats::resid(mod_ar1, type = "normalized"),
  col = "red",
  lwd = 2
)

# Retorna a configuração gráfica ao normal (1 gráfico por vez)
graphics::par(mfrow = c(1, 1))

# ------------------------------------------------------------------------------
# 2. Teste Formal de Normalidade (Shapiro-Wilk)
# ------------------------------------------------------------------------------
# IMPORTANTE: Testamos os resíduos NORMALIZADOS.
# Os brutos falhariam pois contêm a autocorrelação temporal.

residuos_norm <- stats::resid(mod_ar1, type = "normalized")
teste_shapiro <- stats::shapiro.test(residuos_norm)
print(teste_shapiro)

# ------------------------------------------------------------------------------
# 3. Investigação Detalhada (Histograma e Comparação)
# ------------------------------------------------------------------------------

# Histograma com Curva Normal
graphics::hist(
  residuos_norm,
  prob = TRUE,
  main = "Histograma dos Resíduos Normalizados",
  xlab = "Resíduos",
  col = "lightblue",
  border = "white"
)
graphics::curve(
  stats::dnorm(x, mean = mean(residuos_norm), sd = stats::sd(residuos_norm)),
  add = TRUE,
  col = "red",
  lwd = 2
)

# Comparação Educativa: Brutos vs Normalizados
# Mostra por que o teste falhou antes (Brutos têm caudas pesadas)
graphics::par(mfrow = c(1, 2))


res
# A. Resíduos Brutos (Incorreto para teste de normalidade)
stats::qqnorm(stats::resid(mod_ar1), main = "Resíduos Brutos (Com Autocorrelação)", col = "red")
stats::qqline(stats::resid(mod_ar1))

# B. Resíduos Normalizados (Correto - Autocorrelação removida)
stats::qqnorm(residuos_norm, main = "Resíduos Normalizados (Limpos)", col = "blue")
stats::qqline(residuos_norm)

graphics::par(mfrow = c(1, 1))

# Extrai os resíduos que já descontaram a correlação AR1
residuos_limpos <- resid(mod_ar1, type = "normalized")

# Roda o teste formal
shapiro.test(residuos_limpos)


# ------------------------------------------------------------------------------
# ANÁLISE DE RESÍDUOS PADRONIZADOS (PEARSON)
# ------------------------------------------------------------------------------

# Extração dos Resíduos Padronizados
residuos_padronizados <- resid(mod_ar1, type = "pearson")

# Visualização
par(mfrow = c(1, 2))

# 1. QQ-Plot dos Padronizados
# Note: Se houver correlação forte, eles ainda podem fugir da reta
qqnorm(residuos_padronizados, main = "QQ-Plot: Padronizados (Pearson)", col = "darkgreen", pch = 19)
qqline(residuos_padronizados, col = "red", lwd = 2)

# 2. Histograma
hist(residuos_padronizados, prob = TRUE, main = "Hist: Padronizados", xlab = "Resíduos", col = "lightgreen")
curve(dnorm(x, mean = mean(residuos_padronizados), sd = sd(residuos_padronizados)), add = TRUE, col = "red", lwd = 2)

par(mfrow = c(1, 1))

# Teste de Normalidade (Cuidado: pode falhar devido à autocorrelação mantida)
shapiro.test(residuos_padronizados)

# ------------------------------------------------------------------------------
# DICA EXTRA: Tratamento de Heterocedasticidade
# ------------------------------------------------------------------------------
# Se o gráfico de resíduos (Gráfico 1) tiver formato de "funil" (<),
# rode o modelo abaixo permitindo variâncias diferentes por tempo:
#
# mod_ar1_het <- nlme::update(
#   mod_ar1,
#   weights = nlme::varIdent(form = ~ 1 | Semana_recentralizada)
# )
# stats::anova(mod_ar1, mod_ar1_het) # Se p < 0.05, o modelo heterocedástico é melhor.

# DICA EXTRA: Caso você veja "funil" nos resíduos, use este modelo heterocedástico:
# mod_ar1_het <- update(mod_ar1, weights = varIdent(form = ~ 1 | Semana_recentralizada))
# anova(mod_ar1, mod_ar1_het)
# 


