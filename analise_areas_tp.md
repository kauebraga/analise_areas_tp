---
title: "Análise em Áreas do Transporte Público de Fortaeza"
output:
  html_document:
    keep_md: true
    df_print: paged
    fig_caption: yes
    number_sections: yes
    theme: journal
    toc: yes
    toc_float:
      collapsed: no
      smooth_scroll: no
  prettydoc::html_pretty:
    theme: architect
    highlight: github
editor_options:
  chunk_output_type: console
---

Esse documento detalha o desenvolvimento de um trabalho da disciplina de Estatística Espacial do Programa de Pós-Graduação em Engenharia de Transportes (PETRAN) da Universidade Federal do Ceará (UFC), focando principalmente na apresentação do código em R utilizado para o desenvolvimento do trabalho.

O trabalho tem como objetivo geral fazer um diagnóstico estratégico da baixa acessibilidade por transporte público em Fortaleza com a utilização de ferramentas de estatística espacial, e foi dividido em duas fases. A primeira teve dois objetivos específicos: aplicar método de caracterização da problemática da acessibilidade através de ferramentas de análise de dados em área e aplicar método de diagnóstico das relações de causa e efeito do problema da acessibilidade através de regressão clássica. 

A segunda etapa teve como objetivo aprimorar o modelo de regressão clássica executado na primeira etapa, adicionando a espacialidade ao modelo. Teve como objetivos específicos analisar a estacionaridade do fenômeno da baixa acessibilidade relacionada com variáveis de transportes, analisar globalmente como variáveis de transporte afetam a acessibilidade por transporte público, e comparar modelos globais com a identificação de qual deles melhor se adequa ao fenômeno.

A elaboração e apresentação de slides foi o requisito estabelecido para o trabalho. Os slides da primeira etapa podem ser encontrados [aqui](https://drive.google.com/file/d/15X2WEEjIjDiQ7nJhTiqAlPp9aA79IRp2/view?usp=sharing). A segunda etapa está disponível [aqui](https://drive.google.com/file/d/1CL0SE_RU6LI1-8oV46XTz1Rb8h9OizZA/view?usp=sharing). Como já ressaltado, o segundo trabalho foi uma continuação do primeiro.

Os códigos em R aqui apresentados focam no cálculo dos indicadores e na calibração de modelos de regressão linear e geo-ponderada. Os indicadores calculados foram exportados para o software Geoda para a utilização de ferramentas de autocorrelação espacial global e local e para a produção de mapas de quartis. 


```r
knitr::opts_chunk$set(echo = TRUE, warning=FALSE, message=FALSE, error=FALSE, cache = TRUE, eval = FALSE)

library(dplyr)
library(sf) #pra importar os dados espaciais e tal
library(purrr)
library(data.table)
library(readr)
library(tidyr)
library(ggplot2)
library(forcats)
library(hrbrthemes)
library(mapview)
library(RColorBrewer)
library(ggthemes)
library(stringr)
library(ggthemes)
library(lubridate)
library(broom)

# library(rgdal)
library(spgwr)
# library(dfrtopics)

library(patchwork)

source("R/funcoes_tratamento.R")
```

# Cálculo dos indicadores

Os indicadores a serem calculados são: acessibilidade, frequencia, cobertura e vitalidade.


```r
zonas <- st_read("data/zonas_trafego_novo/Zonas.shp") %>%
  st_transform(crs = 4326) %>%
  filter(municipio == "FORTALEZA") %>%
  arrange(subzonas) %>%
  select(subzonas)
  
linhas <- st_read("data/linhas/linhas.shp", crs = 4326) %>%
  mutate(id = 1:n())


# abrir bilhetagem, e identificar de qual zona pertence cada validacao

bilhetagem <- read_csv("data/bilhetagem_integrado/bilhetagemintegrado_2015-03-04.csv") %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_join(zonas)
```

Indicador de acessibilidade:


```r
# abrir o resultado do otp

vai <- readr::read_csv("data/matriz_impedancia_tp.csv") %>%
  mutate(tempo = travel_time/60) %>%
  filter(origin != destination) #deletar quando origem igual ao destino

# abrir empregos

empregos <- read_delim("data/empregos", delim = "\t") %>%
  mutate(origin = as.integer(COD))

# abrir centroides

centroides <- read_csv("centroides_lara.csv") %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326)

# criar coluna com os empregos totais
empregos$total_empregos <- apply(empregos[,4:7], 1, sum)

# agrupar a matriz de tempo pelas origens e tirar a media do tempo de viagem ponderado para as demais zonas -----------------------

vai_grup <- vai %>%
  left_join(select(empregos, origin, total_empregos), by = c("destination" = "origin")) %>% #o total de empregos tem que ser relacionado ao destinoo!
  group_by(origin) %>%
  summarise(tempo_medio = sum(tempo*total_empregos)/sum(total_empregos)) %>%
  left_join(zonas, by = c("origin" = "subzonas")) %>%
  st_as_sf()
```

Cada validação tem sua zona identificada. Então, para identificar quantos veículos de cada linha passa por cada zona por hora, é feito o seguinte procedimento.


```r
# criar hora, agrupar por linha, carro e hora, em cada zona

bilhetagem1 <- bilhetagem %>%
  as.data.frame() %>%
  select(-geometry) %>%
  mutate(hora_iso = hour(hora)) %>%
  count(hora_iso, linha, prefixo_carro, subzonas) %>%
  arrange(subzonas, linha)

#calcular quantos CARROS da mesma linha passa nas zonas em determinada hora

bilhetagem2 <- bilhetagem1 %>%
  count(subzonas, hora_iso, linha) %>%
  arrange(desc(nn))

# calcular o TOTAL de onibus que passam por hora em cada zona

bilhetagem3 <- bilhetagem2 %>%
  group_by(subzonas, hora_iso) %>%
  summarise(freq = sum(nn))

# calcular a media geral

bilhetagem4 <- bilhetagem3 %>%
  group_by(subzonas) %>%
  summarise(freq_media_hora = mean(freq)) %>%
  arrange(desc(freq_media_hora))


# espacializar
# ESSE EH O INDICADOR

freq_linhas_hora <- zonas %>%
  select(subzonas) %>%
  left_join(bilhetagem4)

# salvar

# freq_linhas_hora %>%
#   st_write("freq_linhas_hora/freq_linhas_hora.shp")


ggplot()+
  geom_sf(data = freq_linhas_hora, aes(fill = freq_media_hora))+
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(n = 8, name = 'BuPu'), na.value = "blue", guide = "colourbar")+
  theme_void()+
  theme(legend.position = "bottom")+
  labs(title = "Frequência média horária")+
  theme(panel.grid.major = element_line(colour = 'transparent'))
  # text = element_text(size=20),
  # legend.key.size =  unit(0.5, "in")) #aumentar tamanho da legenda
```

Agora, parte-se para o cálculo da cobertura de transporte público, que é o percentual da zona coberta por um raio de 400 metros das paradas de ônibus, dividido pela quantidade de empregos e domicílios da zona.


```r
#abrir paradas

stops <- read.delim("data/gtfs/stops.txt", sep=",", header = T) %>% 
  select(id_stop = stop_id, stop_name, lon = stop_lon, lat = stop_lat) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(32724) #transformar para UTM para poder fazer o buffer em metros

# criar buffer de 400m em relação às paradas de ônibus

stops1 <- stops %>%
  st_buffer(400) %>%
  mutate(cidade = "fortaleza") %>%
  count(cidade) #unir todas as geometrias em uma so

ggplot()+
  geom_sf(data = zonas, alpha = 0.3)+
  geom_sf(data = stops1, alpha = 1, fill = "tomato")+
  theme_void()+
  theme(panel.grid.major = element_line(colour = 'transparent'))

# criar outro dataframe com o crs 4326

stops2 <- stops1 %>%
  st_transform(crs = 4326) 

# qual a área da zona que é abrangida pela cobertura das paradas?
# ESSE EH O INDICADOR

int <- st_intersection(stops2, zonas) %>%
  mutate(area_int = st_area(.)) %>%
  mutate(area_zona = st_area(zonas$geometry)) %>%
  mutate(cobertura = area_int / area_zona)

ggplot()+
  geom_sf(data = int, fill = "tomato")+
  geom_sf(data = zonas, alpha = 0.3)+
  theme_void()+
  theme(panel.grid.major = element_line(colour = 'transparent'))
```

Um novo indicador de cobertura é proposto, que é a quantidade de domicílios por parada em cada zona:


```r
#abrir paradas

stops <- read.delim("data/gtfs/stops.txt", sep=",", header = T) %>% 
  select(id_stop = stop_id, stop_name, lon = stop_lon, lat = stop_lat) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# QUANTAS PARADAS HÁ EM CADA ZONA? --------------------------------------------

#ver quais paradas estão em cada zonas
paradas_por_zona <- zonas %>%
  st_intersects(stops)

#nomear cada elemento da lista
names(paradas_por_zona) <- 1:241

#trazer a lista para um dataframe, em seguida fazer a contagem
paradas_por_zona.1 <- data.frame(row.id = rep(seq_along(paradas_por_zona), lengths(paradas_por_zona)), col.id = unlist(paradas_por_zona)) %>%
  count(row.id) %>%
  rename(subzonas = row.id)

# QUANTOS DOMICÍLIOS HÁ EM CADA ZONA? --------------------------------------------

domic <- read_csv("data/domic_2015.csv")

zonas_domic <- zonas %>%
  left_join(domic) %>%
  left_join(paradas_por_zona.1) %>%
  mutate(domic_por_parada = baixa_renda/n) %>%
  mutate(domic_por_parada = ifelse(domic_por_parada == 0, NA, domic_por_parada))

ggplot() + 
  geom_sf(data = zonas_domic, aes(fill = domic_por_parada))+
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(n = 8, name = 'OrRd'), na.value = "blue")+
  labs(title = "Domicílios por parada")+
  theme_void()+
  # ggthemes::theme_map()
  theme(panel.grid.major = element_line(colour = 'transparent'))
```

Indicador de vitalidade:


```r
empregos <- read_delim("data/empregos", delim = "\t") %>%
  mutate(soma_empregos = rowSums(.[,4:7])) %>%
  rename(subzonas = COD) %>%
  mutate(subzonas = as.integer(subzonas))

zonas_empregos <- zonas_domic %>%
  left_join(empregos) %>%
  mutate(domic_empregos = soma/soma_empregos)

ggplot() + 
  geom_sf(data = zonas_empregos, aes(fill = domic_empregos))+
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(n = 8, name = 'OrRd'), na.value = "blue")+
  labs(title = "Razão Domicílios e Empregos")+
  theme_void()+
  # ggthemes::theme_map()
  theme(panel.grid.major = element_line(colour = 'transparent'))
```

Juntar tudo:


```r
access <- vai_grup %>% as.data.frame() %>% select(subzonas = origin, tempo_medio)

freq_linhas_hora.df <- freq_linhas_hora %>% as.data.frame() %>% select(-geometry)

zonas_domic.df <- zonas_domic %>% as.data.frame() %>% select(subzonas, domic_por_parada)

vitalidade.df <- zonas_empregos %>% as.data.frame() %>% select(subzonas, domic_por_empregos = domic_empregos)

#juntar tudo

indicadores_final <- zonas %>%
  left_join(access) %>%
  left_join(freq_linhas_hora.df) %>%
  left_join(zonas_domic.df) %>%
  left_join(vitalidade.df) %>%
  mutate(domic_por_empregos = ifelse(domic_por_empregos == 0, NA, domic_por_empregos))
```


```r
indicadores_final %>%
  st_write("indicadores_area/indicadores_area.shp")
```

A partir dos indicadores calculados, são calculados os índices de autocorrelação global e local:



# Regressão linear


```r
library(broom)

indicadores_final.rl <- indicadores_final %>%
  as.data.frame() %>%
  select(-geometry)

# acessibilidade com frequencia media

access_freq <- indicadores_final.rl %>%
  select(subzonas, tempo_medio, freq_media_hora) %>%
  do(augment(lm(tempo_medio ~ freq_media_hora, data = indicadores_final.rl))) %>%
  select(subzonas = .rownames, resid_freq = .resid)

access_freq.lm <- lm(tempo_medio ~ freq_media_hora, data = indicadores_final.rl)

summary(access_freq.lm)

# acessibilidade com cobertura

access_cobertura <- indicadores_final.rl %>%
  select(subzonas, tempo_medio, domic_por_parada) %>%
  do(augment(lm(tempo_medio ~ domic_por_parada, data = indicadores_final.rl))) %>%
  select(subzonas = .rownames, resid_cobertura = .resid)

access_cobertura.lm <- lm(tempo_medio ~ domic_por_parada, data = indicadores_final.rl)

summary(access_cobertura.lm)

# acessibilidade com vitalidade

access_vitalidade <- indicadores_final.rl %>%
  select(subzonas, tempo_medio, domic_por_empregos) %>%
  do(augment(lm(tempo_medio ~ domic_por_empregos, data = indicadores_final.rl))) %>%
  select(subzonas = .rownames, resid_vitalidade = .resid)

access_vitalidade.lm <- lm(tempo_medio ~ domic_por_empregos, data = indicadores_final.rl)

summary(access_vitalidade.lm)

# zonas %>%
#   mutate(subzonas = as.character(subzonas)) %>%
#   left_join(access_freq) %>%
#   left_join(access_cobertura) %>%
#   left_join(access_vitalidade) %>%
#   st_write("resid_area/resid_area.shp")

# acessibilidade com TUDO

access_freq <- indicadores_final.rl %>%
  select(subzonas, tempo_medio, freq_media_hora) %>%
  do(augment(lm(tempo_medio ~ freq_media_hora, data = indicadores_final.rl))) %>%
  select(subzonas = .rownames, resid_freq = .resid)

access_freq.lm <- lm(tempo_medio ~ freq_media_hora, data = indicadores_final.rl)

summary(access_freq.lm)
```

# Regressão Geoponderada


```r
# abrir

zonas <- st_read("data/zonas_trafego_novo/Zonas.shp") %>%
  #st_transform(crs = 4326) %>%
  filter(municipio == "FORTALEZA") %>%
  arrange(subzonas) %>%
  select(subzonas)

indic <- st_read("indicadores_area/indicadores_area.shp") %>%
  st_transform(crs = 32724) %>%
  na.omit() %>%
  st_centroid() %>%
  as("Spatial")

# GWR COM FREQUENCIA ----------------------------------------------------------

gbwG_fq <- ggwr.sel(temp_md ~ frq_md_, data = indic, gweight = gwr.Gauss)

ggwrG_fq <- ggwr(temp_md ~ frq_md_, data = indic, gweight = gwr.Gauss, bandwidth = gbwG_fq)

# ggwrG_fq$lm

#transformar em dataframe

ggwr_df_fq <- as.data.frame(ggwrG_fq$SDF) %>%
  mutate(subzonas = 1:n())

# os pontos são referentes a quais zonas?
cors <- st_intersects(zonas, ggwr_df_fq)

vazios <- c(2, 20, 24, 39, 55, 71, 77, 79:82, 97, 104, 109, 112, 119, 232)

zonas_corrigido <- zonas %>%
  slice(-vazios) %>%
  mutate(subzonas = 1:n())

zonas_corrigido %>%
  left_join(ggwr_df_fq) %>%
  slice(-33) %>%
  ggplot()+
  geom_sf(aes(fill = frq_md_))+
  #scale_fill_gradientn(colours = brewer.pal(n = 8, name = 'OrRd'))+
  scale_fill_gradient2(low = "red", high = "blue", midpoint = 0)+
  theme_void()+
  theme(panel.grid.major = element_line(colour = 'transparent'))

# GWR COM COBERTURA ----------------------------------------------------------

gbwG.cob <- ggwr.sel(temp_md ~ dmc_pr_p, data = indic, gweight = gwr.Gauss)

ggwrG.cob <- ggwr(temp_md ~ dmc_pr_p, data = indic, gweight = gwr.Gauss, bandwidth = gbwG.cob)

ggwrG.cob$lm

#transformar em dataframe

ggwr_df.cob <- as.data.frame(ggwrG.cob$SDF) %>%
  mutate(subzonas = 1:n())

cors <- st_intersects(zonas, ggwr_df)

vazios <- c(2, 20, 24, 39, 55, 71, 77, 79:82, 97, 104, 109, 112, 119, 232)

zonas_corrigido <- zonas %>%
  slice(-vazios) %>%
  mutate(subzonas = 1:n())

zonas_corrigido %>%
  left_join(ggwr_df.cob) %>%
  slice(-33) %>%
  ggplot()+
  geom_sf(aes(fill = dmc_pr_p))+
  #scale_fill_gradientn(colours = brewer.pal(n = 8, name = 'OrRd'))+
  scale_fill_gradient2(low = "red", high = "blue", midpoint = 0)+
  theme_void()+
  theme(panel.grid.major = element_line(colour = 'transparent'))


# GWR COM OS DOIS ------------------------------------------------------

gbwG <- ggwr.sel(temp_md ~ frq_md_ + dmc_pr_p, data = indic, gweight = gwr.Gauss)

ggwrG <- gwr(temp_md ~ frq_md_ + dmc_pr_p, data = indic, gweight = gwr.Gauss, bandwidth = gbwG, hatmatrix = TRUE)

ggwrG

#transformar em dataframe

ggwr_df <- as.data.frame(ggwrG$SDF) %>%
  mutate(subzonas = 1:n())

cors <- st_intersects(zonas, ggwr_df)

# identificar vazios urbanos
vazios <- c(2, 20, 24, 39, 55, 71, 77, 79:82, 97, 104, 109, 112, 119, 232)

zonas_corrigido <- zonas %>%
  slice(-vazios) %>%
  mutate(subzonas = 1:n())

zonas_corrigido %>%
  left_join(ggwr_df) %>%
  slice(-33) %>%
  ggplot()+
  geom_sf(aes(fill = frq_md_))+
  #scale_fill_gradientn(colours = brewer.pal(n = 8, name = 'OrRd'))+
  scale_fill_gradient2(low = "red", high = "blue", midpoint = 0, na.value = "black", mid = "white")+
  theme_void()+
  theme(panel.grid.major = element_line(colour = 'transparent'),
        plot.title = element_text(size=22))

# zonas_corrigido %>%
#   left_join(ggwr_df) %>%
#   slice(-33) %>%
#   mapview(zcol = "frq_md_")

zonas_corrigido %>%
  left_join(ggwr_df) %>%
  slice(-33) %>%
  ggplot()+
  geom_sf(aes(fill = dmc_pr_p))+
  #scale_fill_gradientn(colours = brewer.pal(n = 8, name = 'OrRd'))+
  scale_fill_gradient2(low = "red", high = "blue", midpoint = 0, mid = "white")+
  theme_void()+
  theme(panel.grid.major = element_line(colour = 'transparent'))
  #labs(title = "Distribuição dos coeficientes para a variável Cobertura")


# boxplot com coeficientes

ggplot()+
  geom_boxplot(data = ggwr_df, aes(x = "",  y = frq_md_))+
  theme_ipsum()+
  #labs(title = "Distribuição dos coeficientes para a frequência")+
  geom_abline(slope = 0, intercept = 0)+
  coord_flip()

ggplot()+
  geom_boxplot(data = ggwr_df, aes(x = "",  y = dmc_pr_p))+
  theme_ipsum()+
  labs(title = "Distribuição dos coeficientes para a cobertura")
```

