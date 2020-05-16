load(file = "Dados/POCV.RData")

level1 <- paste("Município de",LocRef$Localidade)
tabela1 <- ServicosRAIS %>% 
  filter(!is.na(Vagas) & Localidade == LocRef$Localidade & Vagas > 0) %>% 
  select(Localidade, Ano, Subsetor, Vagas) %>%  
  mutate(Referencia = level1) %>% 
  select(Referencia,Ano,Subsetor,Vagas)

level2 <- paste("Região de Planejamento",LocRef$RPSEGPLAN)
tabela2 <- ServicosRAIS %>% 
  merge(RegioesGoias,by = c("Localidade")) %>% 
  filter(!is.na(Vagas) & RPSEGPLAN == LocRef$RPSEGPLAN & Vagas > 0) %>% 
  select(RPSEGPLAN,Ano, Subsetor,Vagas) %>% 
  group_by(Ano,Subsetor) %>% 
  summarise(Vagas = sum(Vagas)) %>% 
  mutate(Referencia = level2) %>% 
  select(Referencia,Ano,Subsetor,Vagas)

level3 <- paste("Goiás")
tabela3 <- ServicosRAIS %>% 
  filter(!is.na(Vagas)) %>% 
  select(Ano, Subsetor,Vagas) %>% 
  group_by(Ano,Subsetor) %>% 
  summarise(Vagas = sum(Vagas)) %>% 
  mutate(Referencia = level3) %>% 
  select(Referencia,Ano,Subsetor,Vagas)

dados <- merge(tabela2,tabela1,all = TRUE) %>% 
  merge(tabela3,all = TRUE) %>% 
  filter(Ano >= "2010-01-01")

dados$Referencia <- factor(dados$Referencia,ordered = T,levels = c(level1,level2,level3))

grafico <- dados %>% 
  ggplot(aes(x = format(Ano,format="%Y"), y = Vagas)) +
  geom_bar(aes(fill = Subsetor), stat = "identity") +
  scale_fill_manual(values = mypallete) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90),
        legend.title = element_blank(),
        legend.direction = "vertical",legend.text = element_text(size = 6)) +
  labs(y = "Postos de Trabalho", x = NULL) +
  facet_wrap(~Referencia,ncol = 1,scales = "free_y")