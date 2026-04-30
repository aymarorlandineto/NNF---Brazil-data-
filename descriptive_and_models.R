library(dplyr)
library(ggplot2)
library(readxl)
library(lme4)
library(performance)
library(MuMIn)
library(blmeco)
library(parameters)
library(emmeans)
library(patchwork)
library(MASS)
library(sjPlot)
library(RColorBrewer)


conflicted::conflict_scout()
conflicted::conflicts_prefer(dplyr::filter)
conflicted::conflicts_prefer(dplyr::select)

tema_padrao <- theme_bw() +
  theme(    text = element_text(size = 14),  
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    legend.position = "right")

## Dados

file_path <- "C:/Aymar Backup/aymar/Aymar/Pós/Artigo_Inv_Func/data_available.xlsx"
dados_bacias <- read_excel(file_path)
str(dados_bacias)

# explorando
# riqueza e distribuicao
riqueza_bacias <- dados_bacias %>%
  group_by(basin, origin) %>%
  summarise(riqueza = n_distinct(validnames)) %>%
  ungroup()
print(riqueza_bacias,n=100)

unique(dados_bacias$validnames)

dados_bacias %>%
  filter(origin == "exotic") %>%
  pull(validnames) %>%
  unique()

dados_bacias %>%
  filter(origin == "translocated") %>%
  pull(validnames) %>%
  unique()

n_bacias<-dados_bacias %>%
  group_by(validnames, origin) %>%
  summarise(n_bacias = n_distinct(basin)) %>%
  ungroup() 
print(n_bacias,n=352)

top_species <- n_bacias %>%
  top_n(20, n_bacias) %>%
  ungroup() %>%
  arrange(origin, desc(n_bacias))
print(top_species,n=25)

# juntado info para os modelos
dados_bacias <- left_join(dados_bacias, 
                          n_bacias, by = c("validnames","origin"))

dados_bacias$RepGuild1<-as.factor(dados_bacias$repGuild1)
dados_bacias$RepGuild1<-factor(dados_bacias$RepGuild1, order = TRUE, 
                               levels = c("nonguarders", "guarders", "bearers"))

dados_bacias$order<-as.factor(dados_bacias$order)
dados_bacias$order <- relevel(dados_bacias$order, ref = "Characiformes")

dados_bacias$origin<-as.factor(dados_bacias$origin)

names(dados_bacias)
glimpse(dados_bacias)

####
windows()
####

highlight_species <- dados_bacias %>%
  filter(climatch_hist < 6) %>%
  group_by(validnames, origin, basin) %>%
  summarise(climatch_hist = mean(climatch_hist), climatch_fut = mean(climatch_fut), .groups = 'drop') %>%
  mutate(label = paste0(validnames, " (", basin, ")"))

sc_clim<-ggplot(dados_bacias, aes(x = climatch_hist, y = climatch_fut, color = origin)) +
  geom_abline(slope = 1, intercept = 0, color = "black", linewidth = 0.3, linetype = "solid") +
  geom_hline(yintercept = 6, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 6, linetype = "dashed", color = "red") +
  geom_point(alpha = 0.6, size = 2) +
  ggrepel::geom_text_repel(data = highlight_species,
                           aes(label = label),  size = 3, box.padding = 0.5,max.overlaps = 10, segment.alpha = 0.3) +
  scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) +
  scale_color_manual(values = c("exotic" = "#7F171F", "translocated" = "#81A9F0")) +
  labs( x = "Climatch Historical", y = "Climatch Future (ssp245) - 2070",
        color = "Origin" )  +tema_padrao+theme(legend.position = "none")
sc_clim


#### modelos associados ao uso humano
#### especies como replicas
dados_bacias<-dados_bacias %>% distinct(validnames,.keep_all = T)
# modelo global
modelo_global<-glmer(cbind(n_bacias, 12 - n_bacias)~ 
                       UsedforAquaculture+
                       UsedasBait+
                       Aquarium+ 
                       GameFish+
                       Importance +(1|order),
                     family=binomial(link="logit"),
                     data=dados_bacias,
                     na.action = na.omit, control = glmerControl(optimizer="bobyqa"))
summary(modelo_global)
multicollinearity(modelo_global)
r.squaredGLMM(modelo_global)
dispersion_glmer(modelo_global)
standardize_parameters(modelo_global)

#### so para exoticos
modelo_global_exo<-glmer(cbind(n_bacias, 12 - n_bacias)~ 
                           UsedforAquaculture+
                           UsedasBait+
                           Aquarium+ 
                           GameFish+
                           Importance +(1|order),
                         family=binomial(link="logit"),
                         data=dados_bacias %>%
                           filter(origin=="exotic")%>%
                           droplevels(.),
                         na.action = na.omit, control = glmerControl(optimizer="bobyqa"))
summary(modelo_global_exo)
multicollinearity(modelo_global_exo)
r.squaredGLMM(modelo_global_exo)
dispersion_glmer(modelo_global_exo)
standardize_parameters(modelo_global_exo)

m_exo_plot<-plot_model(modelo_global_exo, 
                       terms = c("Importance","UsedforAquaculture", 
                                 "UsedasBait", "Aquarium","GameFish"),
                       colors = "#7F171F",    type = "std",     
                       transform = NULL,     
                       show.values = TRUE,   value.size=3,   
                       value.offset = 0.2) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme_minimal()+tema_padrao
m_exo_plot


####### so para translocados
modelo_global_trans<-glmer(cbind(n_bacias, 12 - n_bacias)~ 
                             UsedforAquaculture+
                             UsedasBait+
                             Aquarium+ 
                             GameFish+
                             Importance +(1|order),
                           family=binomial(link="logit"),
                           data=dados_bacias %>%
                             filter(origin=="translocated")%>%
                             droplevels(.),
                           na.action = na.omit, control = glmerControl(optimizer="bobyqa"))
summary(modelo_global_trans)
multicollinearity(modelo_global_trans)
r.squaredGLMM(modelo_global_trans)
dispersion_glmer(modelo_global_trans)
standardize_parameters(modelo_global_trans)


m_trans_plot<-plot_model(modelo_global_trans, 
                         terms = c("Importance","UsedforAquaculture", 
                                   "UsedasBait", "Aquarium","GameFish"),
                         colors = "#81A9F0",    type = "std",     
                         transform = NULL,     
                         show.values = TRUE,   value.size=3,   
                         value.offset = 0.2) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme_minimal()+tema_padrao
m_trans_plot

human_plot<-m_exo_plot|m_trans_plot+ plot_layout(heights = c(1))
human_plot

### modelos associados a traços ecologicos
dados_bacias <- dados_bacias %>%
  mutate(
    length_s = as.numeric(scale(length)),
    maxBio5_s = as.numeric(scale(maxBio5)),
    minBio6_s = as.numeric(scale(minBio6))
  )
# global
modelo_global<-glmer(cbind(n_bacias, 12 - n_bacias)~ 
                       length_s + RepGuild1+maxBio5_s  + 
                       minBio6_s  +(1|order),
                     family=binomial(link="logit"),
                     data=dados_bacias,
                     na.action = na.omit, control = glmerControl(optimizer="bobyqa"))
summary(modelo_global)
multicollinearity(modelo_global)
r.squaredGLMM(modelo_global)
standardize_parameters(modelo_global)
dispersion_glmer(modelo_global)

### só exoticos
modelo_exo_glmm <- glmer(cbind(n_bacias, 12 - n_bacias)~ 
                           length_s + RepGuild1+maxBio5_s  + 
                           minBio6_s  +(1|order),
                         family=binomial(link="logit"),
                         data=dados_bacias %>%
                           filter(origin=="exotic")%>%
                           droplevels(.),
                         na.action = na.omit, control = glmerControl(optimizer="bobyqa"))
summary(modelo_exo_glmm) 
dispersion_glmer(modelo_exo_glmm)
r.squaredGLMM(modelo_exo_glmm)
multicollinearity(modelo_exo_glmm)
standardize_parameters(modelo_exo_glmm)

### so translocados
modelo_trans_glmm <-  glmer(cbind(n_bacias, 12 - n_bacias) ~ 
                              length_s + RepGuild1+maxBio5_s  + 
                              minBio6_s  + (1|order),
                            family=binomial(link="logit"),
                            data=dados_bacias %>%
                              filter(origin=="translocated")%>%
                              droplevels(.),
                            na.action = na.omit, control = glmerControl(optimizer = "bobyqa"))
summary(modelo_trans_glmm) 
r.squaredGLMM(modelo_trans_glmm)
multicollinearity(modelo_trans_glmm)
dispersion_glmer(modelo_trans_glmm)
standardize_parameters(modelo_trans_glmm)

# pareados (rep guild)
emmeans_exo <- emmeans(modelo_exo_glmm, pairwise ~ 
                         RepGuild1, type="response", 
                       adjust="none")$emmeans %>% 
  multcomp::cld(adjust = "none", Letters = letters) %>% 
  as.data.frame() %>% 
  mutate(Origin = "Exotic") 
colnames(emmeans_exo)[2] <- "response" 
emmeans_trans <- emmeans(modelo_trans_glmm, pairwise ~ RepGuild1,
                         type="response", adjust="none")$emmeans %>% 
  multcomp::cld(adjust = "none", Letters = letters) %>% 
  as.data.frame() %>% 
  mutate(Origin = "Translocated") 
colnames(emmeans_trans)[2] <- "response" 
emmeans_guild <- bind_rows(emmeans_exo, emmeans_trans)%>% 
  mutate(fit = response * 12, lower = asymp.LCL * 12, upper = asymp.UCL * 12) 


# graficos
eco_exo_plot<-plot_model(modelo_exo_glmm, 
                         terms = c("length_s","RepGuild1.L", "RepGuild1.Q", "maxBio5_s","minBio6_s"), 
                         colors = "#7F171F", transform = NULL, type = "std",
                         show.values = TRUE, value.size=3, value.offset = 0.2) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme_minimal()+
  tema_padrao 
eco_exo_plot 

eco_trans_plot<-plot_model(modelo_trans_glmm, 
                           terms = c("length_s","RepGuild1.L", "RepGuild1.Q", "maxBio5_s","minBio6_s"), 
                           colors = "#81A9F0", transform = NULL, type = "std", show.values = TRUE, value.size=3, 
                           value.offset = 0.2) + geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme_minimal()+tema_padrao 
eco_trans_plot

human_plot<-m_exo_plot|m_trans_plot+ plot_layout(heights = c(1))
eco_plot <-eco_exo_plot|eco_trans_plot+ plot_layout(heights = c(1))
plotfinal<-human_plot/eco_plot + 
  plot_layout(guides = "collect") 

plotfinal

### Mapas
#shapefile bacias hidro ibge
shapefile_dir_ibge <- "C:/Aymar Backup/aymar/Global dataset/SNIRH_RegioesHidrograficas_2020"
shapefile_path_ibge <- file.path(shapefile_dir_ibge, "SNIRH_RegioesHidrograficas_2020.shp")
bacias_br<- st_read(shapefile_path_ibge)
bacias_br<-sf::st_set_crs(bacias_br,4326)
bacias_br<-st_transform(bacias_br,4326)
bacias_br<-st_make_valid(bacias_br)
bacias_br$rhi_nm
bacia_names_translation <- c(
  "AMAZÔNICA" = "Amazon",
  "TOCANTINS-ARAGUAIA" = "Tocantins-Araguaia",
  "ATLÂNTICO NORDESTE OCIDENTAL" = "Western Northeast Atlantic",
  "ATLÂNTICO NORDESTE ORIENTAL" = "Eastern Northeast Atlantic",
  "ATLÂNTICO LESTE" = "Eastern Atlantic",
  "ATLÂNTICO SUDESTE" = "Southeastern Atlantic",
  "ATLÂNTICO SUL" = "Southern Atlantic",
  "PARAGUAI" = "Paraguay",
  "PARANÁ" = "Paraná",
  "URUGUAI" = "Uruguay",
  "PARNAÍBA" = "Parnaíba",
  "SÃO FRANCISCO" = "São Francisco")
bacias_br <- bacias_br %>%
  mutate(rhi_nm = recode(rhi_nm, !!!bacia_names_translation))
colnames(bacias_br)[4]<-"Basin"
bacias_br

bacias_sf_trans <-  bacias_br %>%
  left_join(riqueza_bacias, by = c("Basin"="basin"))

paleta_bacias <- RColorBrewer::brewer.pal(12, "Set3")

mapa <- ggplot(bacias_sf_trans) +
  geom_sf(aes(fill = Basin), color = "black", size = 0.2) +  # Contorno preto fino
  geom_sf_text(aes(label = riqueza), size = 3, color = "black") +  # Texto com riqueza
  scale_fill_manual(values = brewer.pal(12, "Set3")) +  # Paleta Set3
  facet_wrap(~origin) +  # Facet por origem
  labs(fill = "Basin") +   theme_bw()+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),  # Borda preta sem fundo
        legend.position = "bottom",
        legend.direction = "horizontal") +
  guides(fill = guide_legend(nrow = 2, override.aes = list(size = 3)))  # Ajuste da lege
mapa

### Reinos/Bacias doadoras

dados_bacias$native_range <- gsub(",\\s*$", "", dados_bacias$native_range)
dados_bacias <- dados_bacias[!grepl(",", dados_bacias$native_range), ]
levels(as.factor(dados_bacias$native_range))


bacias_doadoras <- dados_bacias %>%
  filter(origin == "translocated" & native_range != "unknown") %>%
  mutate(native_range = str_trim(native_range)) %>%
  separate_rows(native_range, sep = ",\\s*") %>%
  filter(native_range != "") %>%
  group_by(basin, validnames) %>%
  mutate(peso = 1 / n()) %>%
  ungroup() %>%
  group_by(basin, native_range) %>%
  summarise(Contribuicao = sum(peso), .groups = "drop") %>%
  group_by(basin) %>%
  mutate(Percentual = (Contribuicao / sum(Contribuicao)) * 100) %>% 
  select(-Contribuicao) %>%
  ungroup()
bacias_doadoras

bacias_doadoras_geral <- dados_bacias %>%
  filter(origin == "translocated" & native_range != "unknown") %>%
  mutate(native_range = str_trim(native_range)) %>%
  separate_rows(native_range, sep = ",\\s*") %>%
  filter(native_range != "") %>%
  group_by(basin, validnames) %>%
  mutate(peso = 1 / n()) %>%
  ungroup() %>%
  group_by(native_range) %>%
  summarise(Contribuicao = sum(peso), .groups = "drop") %>%
  mutate(Percentual = (Contribuicao / sum(Contribuicao)) * 100) %>%
  select(-Contribuicao) %>%
  ungroup()
bacias_doadoras_geral


## o mesmo para exoticas
bacias_exoticas <- dados_bacias %>%
  filter(origin == "exotic" & native_range != "unknown") %>%
  mutate(native_range = str_trim(native_range)) %>%
  separate_rows(native_range, sep = ",\\s*") %>%
  filter(native_range != "") %>%
  group_by(basin, validnames) %>%
  mutate(peso = 1 / n()) %>%
  ungroup() %>%
  group_by(basin, native_range) %>%
  summarise(Contribuicao = sum(peso), .groups = "drop") %>%
  group_by(basin) %>%
  mutate(Percentual = (Contribuicao / sum(Contribuicao)) * 100) %>%
  select(-Contribuicao) %>%
  ungroup()
bacias_exoticas

bacias_exoticas_geral <- dados_bacias %>%
  filter(origin == "exotic" & native_range != "unknown") %>%
  mutate(native_range = str_trim(native_range)) %>%
  separate_rows(native_range, sep = ",\\s*") %>%
  filter(native_range != "") %>%
  group_by(basin, validnames) %>%
  mutate(peso = 1 / n()) %>%
  ungroup() %>%
  group_by(native_range) %>%
  summarise(Contribuicao = sum(peso), .groups = "drop") %>%
  mutate(Percentual = (Contribuicao / sum(Contribuicao)) * 100) %>%
  select(-Contribuicao) %>%
  ungroup()
bacias_exoticas_geral

colnames(bacias_exoticas)[2]<-"Origin_realm"
colnames(bacias_doadoras)[2]<-"Origin_basin"
colnames(bacias_exoticas)[1]<-"Recipient_basin"
colnames(bacias_doadoras)[1]<-"Recipient_basin"

print(bacias_exoticas, n=50)
print(bacias_doadoras, n=50)

