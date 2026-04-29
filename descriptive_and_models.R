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
dados_bacias %>%
  group_by(basin, origin) %>%
  summarise(riqueza = n_distinct(validnames)) %>%
  ungroup()

n_bacias<-dados_bacias %>%
  group_by(validnames, origin) %>%
  summarise(n_bacias = n_distinct(basin)) %>%
  ungroup() 

unique(n_bacias$validnames)

n_bacias %>%
  filter(origin == "exotic") %>%
  pull(validnames) %>%
  unique()
n_bacias %>%
  filter(origin == "translocated") %>%
  pull(validnames) %>%
  unique()

top_species <- n_bacias %>%
  top_n(20, n_bacias) %>%
  ungroup() %>%
  arrange(origin, desc(n_bacias))
print(top_species,n=20)

# juntado info para os modelos
dados_bacias <- left_join(dados_bacias, 
                          n_bacias, by = c("validnames","origin"))

dados_bacias<-dados_bacias %>% distinct(validnames,.keep_all = T)

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

#### modelos associados ao uso humano
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
