load(file = "Dados/POCV.RData")

level1 <- paste("Município de",LocRef$Localidade)
tabela1 <- ComercioRAIS %>% 
  filter(!is.na(Vagas) & Localidade == LocRef$Localidade & Ano >= 2011 & Vagas > 0) %>% 
  select(Localidade, Ano, Subsetor, Vagas) %>%  
  mutate(Referencia = level1) %>% 
  select(Referencia,Ano,Subsetor,Vagas)

level2 <- paste("Região de Planejamento",LocRef$RPSEGPLAN)
tabela2 <- ComercioRAIS %>% 
  merge(RegioesGoias,by = c("Localidade")) %>% 
  filter(!is.na(Vagas) & RPSEGPLAN == LocRef$RPSEGPLAN & Ano >= 2011 & Vagas > 0) %>% 
  select(RPSEGPLAN,Ano, Subsetor,Vagas) %>% 
  group_by(Ano,Subsetor) %>% 
  summarise(Vagas = sum(Vagas)) %>% 
  mutate(Referencia = level2) %>% 
  select(Referencia,Ano,Subsetor,Vagas)

level3 <- paste("Goiás")
tabela3 <- ComercioRAIS %>% 
  filter(!is.na(Vagas) & Ano >= 2011) %>% 
  select(Ano, Subsetor,Vagas) %>% 
  group_by(Ano,Subsetor) %>% 
  summarise(Vagas = sum(Vagas)) %>% 
  mutate(Referencia = level3) %>% 
  select(Referencia,Ano,Subsetor,Vagas)

dados <- merge(tabela2,tabela1,all = TRUE) %>% 
  merge(tabela3,all = TRUE)

dados$Referencia <- factor(dados$Referencia,ordered = T,levels = c(level1,level2,level3))

grafico <- dados %>% 
  ggplot(aes(x = Ano, y = Vagas)) +
  geom_bar(aes(fill = Subsetor), stat="identity") +
  scale_fill_brewer(palette="Set1") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90),
        legend.title = element_blank(),
        legend.direction = "horizontal") +
  scale_x_continuous(breaks = unique(dados$Ano)) +
  labs(y = "Postos de Trabalho", x=NULL) +
  facet_wrap(~Referencia,ncol = 1,scales = "free_y")