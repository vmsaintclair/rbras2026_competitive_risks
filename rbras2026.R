####################################################################
#####      UFV/PPESTBIO - Riscos Competitivos - RBRAS 2026     #####
#####                Veronica Manhaes Saint Clair              #####
#####                 Metodo: nao-parametrico                  #####
####################################################################

rm(list = ls())

### Bibliotecas
library(survival)
library(cmprsk)
library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)
library(showtext)
library(sysfonts)

### Fonte do texto
font_add_google("Montserrat", "Montserrat")
showtext_auto()
showtext_opts(dpi = 300)


### Dados
data <- read_excel("data.xlsx")

data$transgenia <- as.factor(data$transgenia)
data$titular <- as.factor(data$titular)
data$grupo <- as.factor(data$grupo)

str(data)


### Graficos
grafico_comparativo <- function(fia, km, evento) {
  
  # Dados da FIA
  dados_fia <- data.frame(
    tempo = fia[[evento]]$time,
    probabilidade = fia[[evento]]$est,
    metodo = "FIA"
  )
  
  # Dados do complemento do Kaplan-Meier
  dados_km <- data.frame(
    tempo = km$time,
    probabilidade = 1 - km$surv,
    metodo = "1 - Kaplan-Meier"
  )
  
  # Grafico
  dados_grafico <- bind_rows(dados_fia, dados_km)
  
  dados_grafico$metodo <- factor(
    dados_grafico$metodo,
    levels = c("FIA", "1 - Kaplan-Meier")
  )
  
  p <- ggplot(
    dados_grafico,
    aes(
      x = tempo,
      y = probabilidade,
      color = metodo
    )
  ) +
    
    geom_step(linewidth = 1.3) +
    
    scale_color_manual(
      values = c(
        "FIA" = "gray55",
        "1 - Kaplan-Meier" = "#073A70"
      )
    ) +
    
    scale_x_continuous(
      breaks = c(50, 150, 250, 350)
    ) +
    
    scale_y_continuous(
      breaks = seq(0, 0.4, by = 0.1)
    ) +
    
    coord_cartesian(
      xlim = c(50, 350),
      ylim = c(0, 0.4)
    ) +
    
    labs(
      x = "Tempo (em meses)",
      y = "Probabilidade"
    ) +
    
    theme_minimal(
      base_size = 20,
      base_family = "Montserrat"
    ) +
    
    theme(
      
      legend.position = "none",
      
      axis.title.x = element_text(
        size = 17,
        family = "Montserrat",
        margin = margin(t = 12)
      ),
      
      axis.title.y = element_text(
        size = 17,
        family = "Montserrat"
      ),
      
      axis.text = element_text(
        size = 15,
        family = "Montserrat"
      ),
      
      panel.grid = element_blank(),
      
      panel.border = element_rect(
        colour = "gray50",
        fill = NA,
        linewidth = 0.6
      ),
      
      plot.margin = margin(
        t = 15,
        r = 15,
        b = 15,
        l = 15
      )
    )
  
  return(p)
}


### Analise com extratificacao do ciclo: anual
data_anual <- data %>%
  filter(ciclo == "anual")

table(data_anual$status, data_anual$grupo)

data_anual <- data_anual %>%
  filter(!grupo %in% c(1, 3, 6))

data_anual$grupo <- factor(data_anual$grupo)

table(data_anual$status, data_anual$grupo)

str(data_anual)


## Evento: expiracao por prazo
# FIA
fia1 <- with(
  data_anual,
  cuminc(tempo, status, cencode = "0")
)

fia1

prazo1 <- list(
  fia1$`1 1`$time,
  fia1$`1 1`$est
)

# Kaplan-Meier
km1 <- survfit(
  Surv(tempo, status == 1) ~ 1,
  data = data_anual
)

# Grafico: ciclo anual
p_anual_prazo <- grafico_comparativo(
  fia = fia1,
  km = km1,
  evento = "1 1"
)


## Evento: renuncia
# FIA
ren1 <- list(
  fia1$`1 2`$time,
  fia1$`1 2`$est
)

# Kaplan-Meier
km2 <- survfit(
  Surv(tempo, status == 2) ~ 1,
  data = data_anual
)

# Grafico: ciclo anual
p_anual_renuncia <- grafico_comparativo(
  fia = fia1,
  km = km2,
  evento = "1 2"
)


## Evento: cancelamento e anulacao
# FIA
canc1 <- list(
  fia1$`1 3`$time,
  fia1$`1 3`$est
)

# Kaplan-Meier
km3 <- survfit(
  Surv(tempo, status == 3) ~ 1,
  data = data_anual
)

# Grafico: ciclo anual
p_anual_cancelamento <- grafico_comparativo(
  fia = fia1,
  km = km3,
  evento = "1 3"
)


### Analise com extratificacao do ciclo: perene
data_perene <- data %>%
  filter(ciclo == "perene")

table(data_perene$status, data_perene$grupo)

data_perene <- data_perene %>%
  filter(!grupo %in% 6)

data_perene$grupo <- factor(data_perene$grupo)

table(data_perene$status, data_perene$grupo)

table(data_perene$status, data_perene$titular)

str(data_perene)


## Evento: expiracao por prazo
# FIA
fia2 <- with(
  data_perene,
  cuminc(tempo, status, cencode = "0")
)

fia2

prazo2 <- list(
  fia2$`1 1`$time,
  fia2$`1 1`$est
)

# Kaplan-Meier
km4 <- survfit(
  Surv(tempo, status == 1) ~ 1,
  data = data_perene
)

# Grafico: ciclo perene
p_perene_prazo <- grafico_comparativo(
  fia = fia2,
  km = km4,
  evento = "1 1"
)


## Evento: renuncia
# FIA
ren2 <- list(
  fia2$`1 2`$time,
  fia2$`1 2`$est
)

# Kaplan-Meier
km5 <- survfit(
  Surv(tempo, status == 2) ~ 1,
  data = data_perene
)

# Grafico: ciclo perene
p_perene_renuncia <- grafico_comparativo(
  fia = fia2,
  km = km5,
  evento = "1 2"
)


## Evento: cancelamento e anulacao
# FIA
canc2 <- list(
  fia2$`1 3`$time,
  fia2$`1 3`$est
)

# Kaplan-Meier
km6 <- survfit(
  Surv(tempo, status == 3) ~ 1,
  data = data_perene
)

# Grafico: ciclo perene
p_perene_cancelamento <- grafico_comparativo(
  fia = fia2,
  km = km6,
  evento = "1 3"
)


### Combinacao dos graficos em figuras
## FIGURA 1 - EXPIRACAO POR PRAZO
# (a) Ciclo anual
# (b) Ciclo perene

figura_prazo <- (
  p_anual_prazo +
    p_perene_prazo
) +
  plot_layout(
    ncol = 2,
    nrow = 1
  ) +
  plot_annotation(
    tag_levels = "a",
    tag_prefix = "(",
    tag_suffix = ")"
  ) &
  
  theme(
    plot.tag = element_text(
      size = 17,
      face = "plain",
      family = "Montserrat"
    ),
    
    # Indices no canto superior direito
    plot.tag.position = c(0.96, 0.96)
  )


## FIGURA 2 - RENUNCIA
# (a) Ciclo anual
# (b) Ciclo perene

figura_renuncia <- (
  p_anual_renuncia +
    p_perene_renuncia
) +
  plot_layout(
    ncol = 2,
    nrow = 1
  ) +
  plot_annotation(
    tag_levels = "a",
    tag_prefix = "(",
    tag_suffix = ")"
  ) &
  
  theme(
    plot.tag = element_text(
      size = 17,
      face = "plain",
      family = "Montserrat"
    ),
    
    plot.tag.position = c(0.96, 0.96)
  )


## FIGURA 3 - CANCELAMENTO E ANULACAO
# (a) Ciclo anual
# (b) Ciclo perene

figura_cancelamento <- (
  p_anual_cancelamento +
    p_perene_cancelamento
) +
  plot_layout(
    ncol = 2,
    nrow = 1
  ) +
  plot_annotation(
    tag_levels = "a",
    tag_prefix = "(",
    tag_suffix = ")"
  ) &
  
  theme(
    plot.tag = element_text(
      size = 17,
      face = "plain",
      family = "Montserrat"
    ),
    
    plot.tag.position = c(0.96, 0.96)
  )


### Exportacao das figuras
# Figura 1 - Expiracao por prazo
ggsave(
  filename = "figura_expiracao_prazo.png",
  plot = figura_prazo,
  width = 35,
  height = 15,
  units = "cm",
  dpi = 300,
  bg = "white"
)


# Figura 2 - Renuncia
ggsave(
  filename = "figura_renuncia.png",
  plot = figura_renuncia,
  width = 35,
  height = 15,
  units = "cm",
  dpi = 300,
  bg = "white"
)


# Figura 3 - Cancelamento e anulacao
ggsave(
  filename = "figura_cancelamento_anulacao.png",
  plot = figura_cancelamento,
  width = 35,
  height = 15,
  units = "cm",
  dpi = 300,
  bg = "white"
)


