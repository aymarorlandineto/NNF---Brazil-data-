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
library(pbkrtest)
conflicted::conflict_scout()
conflicted::conflicts_prefer(dplyr::filter)
conflicted::conflicts_prefer(dplyr::select)

tema_padrao <- theme_bw() +
  theme(text = element_text(size = 14, color = "black"),
        axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black"),  
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    legend.position = "right")

### Mapas
library(tidyverse)
library(sf)
library(patchwork)
library(ggplot2)
library(broom.mixed)
shapefile_dir_ibge <- "C:/Aymar Backup/aymar/Global dataset/SNIRH_RegioesHidrograficas_2020"
shapefile_path_ibge <- file.path(shapefile_dir_ibge, "SNIRH_RegioesHidrograficas_2020.shp")
bacias_br<- st_read(shapefile_path_ibge)
bacias_br<-sf::st_set_crs(bacias_br,4326)
bacias_br<-st_transform(bacias_br,4326)
bacias_sf<-st_make_valid(bacias_br)

## Dados
file_path <- "C:/Aymar Backup/aymar/Aymar/Pós/Artigo_Inv_Func/data_available.xlsx"
dados_bacias <- read_excel(file_path)
str(dados_bacias)
dados_bacias
dados_bacias$RepGuild1<-as.factor(dados_bacias$repGuild1)
dados_bacias$RepGuild1<-factor(dados_bacias$RepGuild1, order = TRUE, 
                               levels = c("nonguarders", "guarders", "bearers"))

dados_bacias$order<-as.factor(dados_bacias$order)
dados_bacias$order <- relevel(dados_bacias$order, ref = "Characiformes")

dados_bacias$origin<-as.factor(dados_bacias$origin)

names(dados_bacias)
glimpse(dados_bacias)

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

### Reinos/Bacias doadoras
library(tidyr)
library(stringr)
conflicted::conflict_scout()

dados_bacias$native_range <- gsub(",\\s*$", "", dados_bacias$native_range)
#dados_bacias <- dados_bacias[!grepl(",", dados_bacias$native_range), ]
levels(as.factor(dados_bacias$native_range))
dados_bacias %>%
  filter(str_detect(native_range, ","))

bacias_doadoras <- dados_bacias %>%
  filter(origin == "translocated",
         native_range != "unknown") %>%
  mutate(native_range = str_trim(native_range)) %>%
  separate_rows(native_range, sep = ",\\s*") %>%
  filter(native_range != "") %>%
  group_by(basin, validnames) %>%
  mutate(peso = 1 / n()) %>%
  ungroup() %>%
  group_by(basin, native_range) %>%
  summarise(Contribuicao = sum(peso), .groups = "drop") %>%
  group_by(basin) %>%
  mutate(Percentual = 100 * Contribuicao / sum(Contribuicao)) %>%
  select(-Contribuicao) %>%
  ungroup()

bacias_doadoras_geral <- dados_bacias %>%
  filter(origin == "translocated",
         native_range != "unknown") %>%
  mutate(native_range = str_trim(native_range)) %>%
  separate_rows(native_range, sep = ",\\s*") %>%
  filter(native_range != "") %>%
  group_by(validnames) %>%
  mutate(peso = 1 / n()) %>%
  ungroup() %>%
  group_by(native_range) %>%
  summarise(Contribuicao = sum(peso), .groups = "drop") %>%
  mutate(Percentual = 100 * Contribuicao / sum(Contribuicao)) %>%
  select(-Contribuicao)


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

################
library(ggalluvial)
windows()
bacias_exoticas_f <- bacias_exoticas %>%
  mutate(Basin = factor(as.character(Recipient_basin),
levels = sort(unique(as.character(Recipient_basin)))),
    Origin_realm = factor(as.character(Origin_realm),
levels = sort(unique(as.character(Origin_realm)))))

bacias_doadoras_f <- bacias_doadoras %>%
  mutate(Basin = factor(as.character(Recipient_basin),
levels = sort(unique(as.character(Recipient_basin)))),
    Origin_basin = factor(as.character(Origin_basin),
levels = sort(unique(as.character(Origin_basin)))))
bacias_doadoras_f <- bacias_doadoras %>%
  mutate( Basin = factor(recode(as.character(Recipient_basin),
             "Western Northeast Atlantic" = "WNA",
             "Eastern Northeast Atlantic" = "ENA")),
    Origin_basin = factor(recode(as.character(Origin_basin),
             "Western Northeast Atlantic" = "WNA",
             "Eastern Northeast Atlantic" = "ENA")))

############################
#alturas 
############################

ref_alturas <- full_join(bacias_exoticas_f %>%
    group_by(Basin) %>%
      summarise(total_exo = sum(Percentual, na.rm = TRUE),
        .groups = "drop"),
  bacias_doadoras_f %>%
    group_by(Basin) %>%
    summarise(total_doa = sum(Percentual, na.rm = TRUE),
      .groups = "drop"),
  by = "Basin") %>%
  mutate(total_ref = pmax(total_exo, total_doa, na.rm = TRUE)) %>%
  select(Basin, total_ref)

############################
# normalização   
############################
bacias_exoticas_f2 <- bacias_exoticas_f %>%
  filter(Percentual >= 10) %>%
  left_join(ref_alturas, by = "Basin") %>%
  group_by(Basin) %>%
  mutate(
    Percentual_pad = Percentual / sum(Percentual) * total_ref
  ) %>%
  ungroup()

bacias_doadoras_f2 <- bacias_doadoras_f %>%
  filter(Percentual >= 10) %>%
  left_join(ref_alturas, by = "Basin") %>%
  group_by(Basin) %>%
  mutate( Percentual_pad = Percentual / sum(Percentual) * total_ref) %>%
  ungroup()

cores_realms <- c("Neotropic" = "#8DD3C7",    
  "Palearctic" = "#B3DE69",   
  "Nearctic" = "#FB8072",    
  "Afrotropic" = "#FDB462",   
  "Indo-Malay" = "#00316E")

cores_bacias <- c("Amazon" = "#6A3D9A",      
  "Eastern Atlantic" = "#1F78B4", 
  "Paraguay" = "#33A02C",     
  "Paraná" = "#E31A1C",       
  "Parnaíba" = "#A6CEE3",     
  "Southeastern Atlantic" = "#FFDF00", 
  "São Francisco" = "#B2AF8A",  
  "Tocantins-Araguaia" = "#E78AC3",  
  "Uruguay" = "#666666",      
  "WNA" = "#CAB2D6", 
  "ENA" = "#FF7F00",  
  "Southern Atlantic" = "#B15928" )

cores_total <- c(cores_realms, cores_bacias)

p1 <- ggplot( bacias_exoticas_f2,
  aes(  axis1 = Origin_realm,
    axis2 = Basin,  y = Percentual_pad)) +
  geom_alluvium(aes(fill = Origin_realm), alpha = 0.5) +
  geom_stratum(width = 0.25) +
  geom_text(  stat = "stratum",
    aes(label = after_stat(stratum)),
    size = 3.5) +
  scale_fill_manual(values = cores_realms) +
  scale_x_discrete(    limits = c("Origin_realm", "Basin"),
    expand = c(0, 0.10) ) +
  theme_void() +
  labs(title = "Influx to basins") +
  theme(legend.position = "left",text = element_text(size = 18))


p2 <- ggplot( bacias_doadoras_f2,
  aes(axis1 = Basin,
    axis2 = Origin_basin,
    y = Percentual_pad)) +
  geom_alluvium(aes(fill = Origin_basin), alpha = 0.5) +
  geom_stratum(width = 0.25) +
  geom_text(stat = "stratum",
    data = ~ subset(.x, Origin_basin != "Eastern Atlantic"),
    aes(label = after_stat(stratum)),
    size = 3.5 ) +
  scale_fill_manual(values = cores_bacias) +
  scale_x_discrete(limits = c("Basin", "Origin_basin"),
    expand = c(0, 0.10)) +
  theme_void() +
  labs(title = "Outflux from basins") +
  theme(legend.position = "right",text = element_text(size = 18))+
  theme(legend.position = "right",
    text = element_text(size = 18),
    plot.margin = margin(5.5, 60, 5.5, 0))

p3 <- (p1 | p2) 
p3

#########

# explorando
# riqueza e distribuicao
riqueza_bacias <- dados_bacias %>%
  group_by(basin, origin) %>%
  summarise(riqueza = n_distinct(validnames)) %>%
  ungroup()
print(riqueza_bacias,n=100)

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
dados_bacias <- left_join(dados_bacias, n_bacias, by = c("validnames","origin"))

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


dados_bacias<-dados_bacias %>% distinct(validnames,.keep_all = T)

table(dados_bacias$order, dados_bacias$validnames)
unique(dados_bacias$order)
unique(dados_bacias$validnames)

n_order<- dados_bacias %>%
  group_by(order) %>%
  summarise(qtd_validnames = n_distinct(validnames))
print(n_order, n=23)

dados_bacias$validnames<- as.factor(dados_bacias$validnames)
dados_bacias <- dados_bacias %>%
  mutate(length_s = as.numeric(scale(length)),
    maxBio5_s = as.numeric(scale(maxBio5)),
    minBio6_s = as.numeric(scale(minBio6)))

dados_bacias<- dados_bacias %>%
  rename(Aquaculture = UsedforAquaculture,
         Bait = UsedasBait,
         TL = length_s,
         RP = RepGuild1,
        MaxT = maxBio5_s,
        MinT = minBio6_s)

dados_bacias<-dados_bacias %>% mutate(n_native = case_when( origin == "translocated" ~ sapply(strsplit(dados_bacias$native_range, ", "),length),
  TRUE ~ 0)) %>% relocate(origin,validnames,basin,native_range,n_bacias,n_native)

dados_bacias <- dados_bacias %>% mutate(n_native = if_else(
  origin == "translocated",  stringr::str_count(native_range, ",") + 1, 0))

dados_bacias$n_possiveis <- 12 - dados_bacias$n_native

##### Modelo Global (TODAS AS SP E DRIVERS)
dados_modelo_global <- dados_bacias %>%
  select(validnames, origin, n_bacias,n_possiveis,Aquaculture, 
         Bait, Aquarium, GameFish, Importance,
         TL, RP, MaxT, MinT, order) %>%
  na.omit() %>%
  droplevels()

modelo_global<-glmer(cbind(n_bacias, n_possiveis - n_bacias) ~ 
    Aquaculture + 
    Bait +
    Aquarium + 
    GameFish +
    Importance +TL+
     RP + MaxT + MinT + (1|order),
  family = binomial(link = "logit"),
  data = dados_modelo_global,
  na.action = na.omit)
modelo_global
anovax( modelo_global)# Bootstrap likelihood-ratio tests
performance(modelo_global)
parameters(modelo_global)
isSingular(modelo_global)
multicollinearity(modelo_global)
standardize_parameters(modelo_global)
dispersion_glmer(modelo_global)
 
#grafico CI WALD
#global_plot<-plot_model(modelo_global, 
#                        colors = "black", transform = NULL, type = "std2",test = "LRT",
#                        show.values = TRUE, value.size=3, value.offset = 0.2) + 
#  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
#  theme_minimal()+
#  tema_padrao 


##### Drivers Separados
dados_modelo1 <- dados_bacias %>%
  select(validnames,origin, n_bacias,n_possiveis, Aquaculture, Bait, Aquarium, GameFish, Importance,
         order) %>% na.omit() %>% droplevels()

#### so para exoticos
modelo_human_exo<-glmer(cbind(n_bacias, n_possiveis - n_bacias)~ 
                           Aquaculture+
                           Bait+
                           Aquarium+ 
                           GameFish+
                           Importance +(1|order),
                         family=binomial(link="logit"),
                         data=dados_modelo1 %>%
                           filter(origin=="exotic")%>%
                           droplevels(.),
                         na.action = na.omit)
modelo_human_exo
pbkrtest::anovax(modelo_human_exo)
performance(modelo_human_exo)
parameters(modelo_human_exo)
isSingular(modelo_human_exo)
multicollinearity(modelo_human_exo)
standardize_parameters(modelo_human_exo)
dispersion_glmer(modelo_human_exo)

####### so para translocados
modelo_human_tran<-glmer(cbind(n_bacias, n_possiveis - n_bacias)~ 
                           Aquaculture+
                           Aquarium+ 
                           GameFish+
                           Importance +(1|order),
                         family=binomial(link="logit"),
                         data=dados_modelo1 %>%
                           filter(origin=="translocated")%>%
                           droplevels(.),
                         na.action = na.omit)
modelo_human_tran
pbkrtest::anovax(modelo_human_tran)
performance(modelo_human_tran)
parameters(modelo_human_tran)
isSingular(modelo_human_tran)
multicollinearity(modelo_human_tran)
standardize_parameters(modelo_human_tran)
dispersion_glmer(modelo_human_tran)

### modelos associados a traços ecologicos
dados_modelo <- dados_bacias %>%
  select(validnames,origin, n_bacias,n_possiveis, TL, RP, MaxT, MinT, order) %>%
  na.omit() %>%
  droplevels()

### só exoticos
modelo_exo_trat <- glmer(cbind(n_bacias, n_possiveis - n_bacias)~ 
                           TL + RP+MaxT  + 
                           MinT  +(1|order),
                         family=binomial(link="logit"),
                         data=dados_modelo %>%
                           filter(origin=="exotic")%>%
                           droplevels(.),
                         na.action = na.omit)
modelo_exo_trat
summary(modelo_exo_trat)
pbkrtest::anovax(modelo_exo_trat)
performance(modelo_exo_trat)
parameters(modelo_exo_trat)
isSingular(modelo_exo_trat)
multicollinearity(modelo_exo_trat)
standardize_parameters(modelo_exo_trat)
dispersion_glmer(modelo_exo_trat)

### so translocados
modelo_trans_trat <-  glmer(cbind(n_bacias, n_possiveis - n_bacias) ~ 
                              TL + RP+MaxT  + 
                              MinT + (1|order),
                            family=binomial(link="logit"),
                            data=dados_modelo%>%
                              filter(origin=="translocated")%>%
                              droplevels(.),
                            na.action = na.omit)
modelo_trans_trat
anovax(modelo_exo_trat)
performance(modelo_trans_trat)
parameters(modelo_trans_trat)
isSingular(modelo_trans_trat)
multicollinearity(modelo_trans_trat)
standardize_parameters(modelo_trans_trat)
dispersion_glmer(modelo_trans_trat)

# pareados (rep guild)
emmeans_exo <- emmeans(modelo_exo_trat, pairwise ~ 
                         RP, type="response", 
                       adjust="none")$emmeans %>% 
  multcomp::cld(adjust = "none", Letters = letters) %>% 
  as.data.frame() %>% 
  mutate(Origin = "Exotic") 
colnames(emmeans_exo)[2] <- "response" 
emmeans_trans <- emmeans(modelo_trans_trat, pairwise ~ RP,
                         type="response", adjust="none")$emmeans %>% 
  multcomp::cld(adjust = "none", Letters = letters) %>% 
  as.data.frame() %>% 
  mutate(Origin = "Translocated") 
colnames(emmeans_trans)[2] <- "response" 

emmeans_guild <- bind_rows(emmeans_exo, emmeans_trans)%>% 
  mutate(fit = response * 12, lower = asymp.LCL * 12, upper = asymp.UCL * 12) 

# graficos
library(parallel)

cl <- makeCluster(detectCores() - 1)

boot_table <- function(model) {
  
  set.seed(123)
  
  boot_res <- bootMer(
    model,
    FUN = fixef,
    nsim = 1000,
    type = "parametric",
    parallel = "snow",
    cl = cl
  )
  
  boot_ci <- apply(
    boot_res$t, 2,
    quantile,
    probs = c(0.025, 0.975),
    na.rm = TRUE
  )
  
  boot_ci <- as.data.frame(t(boot_ci))
  colnames(boot_ci) <- c("lwr", "upr")
  
  coefs <- data.frame(
    term = names(fixef(model)),
    estimate = as.numeric(fixef(model))
  )
  
  coefs$lwr <- boot_ci$lwr
  coefs$upr <- boot_ci$upr
  
  coefs <- subset(coefs, term != "(Intercept)")
  
  return(coefs)
}
coefs_modelo_global  <- boot_table(modelo_global)
coefs_human_exo  <- boot_table(modelo_human_exo)
coefs_human_tran <- boot_table(modelo_human_tran)
coefs_exo_trat   <- boot_table(modelo_exo_trat)
coefs_trans_trat  <- boot_table(modelo_trans_trat)
coefs_todos <- bind_rows(
  global      = coefs_modelo_global,
  human_exo   = coefs_human_exo,
  human_tran  = coefs_human_tran,
  exo_trat    = coefs_exo_trat,
  trans_trat  = coefs_trans_trat,
  .id = "modelo")

coefs_todos$term <- factor(coefs_todos$term,
  levels = rev(unique(coefs_todos$term)))

coefs_todos$signif <- ifelse(coefs_todos$lwr > 0 | coefs_todos$upr < 0, "*","")

coefs_todos <- coefs_todos %>%
  dplyr::bind_rows(data.frame(modelo = "human_tran",
      term = "Bait",estimate = NA,
      lwr = NA,upr = NA,signif = ""))

#writexl::write_xlsx(coefs_todos , "coefs_todos.xlsx")

global_plot<-ggplot(coefs_todos %>% filter(modelo == "global"), aes(x = term,
                               y = estimate)) +
  geom_pointrange(aes(ymin = lwr, ymax = upr),size = 0.7) +
  coord_flip() +
  geom_text(aes(label = signif),
    hjust = -1,
    vjust = .05,
    size = 5) +
  geom_hline( yintercept = 0,linetype = "dashed",color="black") +
  labs(x = NULL, y = "Standardized effect size") +#(logit scale, bootstrap 95% CI)
  theme_minimal(base_size = 14)+
  tema_padrao
global_plot<- global_plot +
  labs( title = "Global model",
        subtitle = "R² = 49%, Observations = 152")

exo_human_plot <- ggplot(coefs_todos %>% filter(modelo == "human_exo"),
  aes(x = term, y = estimate)) + geom_pointrange(
    aes(ymin = lwr, ymax = upr),
    size = 0.7,
    color = "#7F171F")+
 coord_flip() +
  geom_text(aes(label = signif),
    hjust = -1,
    vjust = .05,
    size = 5) +
  geom_hline( yintercept = 0,linetype = "dashed",color="black") +
  labs(x = NULL, y = "Standardized effect size") +#(logit scale, bootstrap 95% CI)
  theme_minimal(base_size = 14)+
  tema_padrao
exo_human_plot<- exo_human_plot +
  labs( title = "Exotic - Human use",
        subtitle = "R² = 55%, Observations = 117")

exo_eco_plot <- ggplot(coefs_todos %>% filter(modelo == "exo_trat"),
                         aes(x = term, y = estimate)) + geom_pointrange(
                           aes(ymin = lwr, ymax = upr),
                           size = 0.7,
                           color = "#7F171F")+
  coord_flip() +
  geom_text(aes(label = signif),
            hjust = -1,
            vjust = .05,
            size = 5) +
  geom_hline( yintercept = 0,linetype = "dashed",color="black") +
  labs(x = NULL, y = "Standardized effect size") +#(logit scale, bootstrap 95% CI)
  theme_minimal(base_size = 14)+
  tema_padrao
exo_eco_plot <- exo_eco_plot +
  labs( title = "Exotic - Ecological",
        subtitle = "R² = 56%, Observations = 64")


tran_human_plot <- ggplot(coefs_todos %>% filter(modelo == "human_tran"),
aes(x = term, y = estimate)) + geom_pointrange(aes(ymin = lwr, ymax = upr),
size = 0.7,color = "#81A9F0")+
  coord_flip() +
  geom_text(aes(label = signif),
            hjust = -1,
            vjust = .05,
            size = 5) +
  geom_hline( yintercept = 0,linetype = "dashed",color="black") +
  labs(x = NULL, y = "Standardized effect size") +#(logit scale, bootstrap 95% CI)
  theme_minimal(base_size = 14)+
  tema_padrao
tran_human_plot<- tran_human_plot +
  labs( title = "Translocated - Human use",
        subtitle = "R² = 22%, Observations = 234") 

tran_eco_plot <- ggplot(coefs_todos %>% filter(modelo == "trans_trat"),
                       aes(x = term, y = estimate)) + geom_pointrange(
                         aes(ymin = lwr, ymax = upr),
                         size = 0.7,
                         color = "#81A9F0")+
  coord_flip() +
  geom_text(aes(label = signif),
            hjust = -1,
            vjust = .05,
            size = 5) +
  geom_hline( yintercept = 0,linetype = "dashed",color="black") +
  labs(x = NULL, y = "Standardized effect size") +#(logit scale, bootstrap 95% CI)
  theme_minimal(base_size = 14)+
  tema_padrao
tran_eco_plot <- tran_eco_plot +
  labs( title = "Translocated - Ecological",
    subtitle = "R² = 23%, Observations = 88") 

exo_plot<-exo_human_plot|exo_eco_plot+ plot_layout(heights = c(1))
tran_plot <-tran_human_plot|tran_eco_plot+ plot_layout(heights = c(1))
plotfinal <- (global_plot | (exo_plot / tran_plot)) +
  plot_layout(widths = c(1, 2))
plotfinal<- plotfinal+ plot_annotation(tag_levels = 'A') & 
  theme(plot.tag.position = c(0, 1),
        plot.tag = element_text(size = 12, hjust = 0, vjust = 0))
plotfinal
ggsave("final_plot_new5.pdf", plot =plotfinal , dpi = 600, width = 11, height = 7)


