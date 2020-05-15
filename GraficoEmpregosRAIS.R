load(file = "Dados/POCV.RData")

level1 <- paste("Município de",LocRef$Localidade)
tabela1 <- Emprego %>% 
  filter(!is.na(Empregos) & Localidade == LocRef$Localidade & Ano >= 2011 & Empregos > 0) %>% 
  select(Localidade,Ano, Setor, Empregos) %>%  
  mutate(Referencia = level1) %>% 
  select(Referencia,Ano,Setor,Empregos)

level2 <- paste("Região de Planejamento",LocRef$RPSEGPLAN)
tabela2 <- Emprego %>% 
  filter(!is.na(Empregos) & RPSEGPLAN == LocRef$RPSEGPLAN & Ano >= 2011) %>% 
  select(RPSEGPLAN,Ano, Setor,Empregos) %>% 
  group_by(Ano,Setor) %>% 
  summarise(Empregos = sum(Empregos)) %>% 
  mutate(Referencia = level2) %>% 
  select(Referencia,Ano,Setor,Empregos)

level3 <- paste("Estado de Goiás")
tabela3 <- Emprego %>% 
  filter(!is.na(Empregos) & Ano >= 2011) %>% 
  select(RPSEGPLAN,Ano, Setor,Empregos) %>% 
  group_by(Ano,Setor) %>% 
  summarise(Empregos = sum(Empregos)) %>% 
  mutate(Referencia = level3) %>% 
  select(Referencia,Ano,Setor,Empregos)

dados <- merge(tabela2,tabela1,all = TRUE) %>% 
  merge(tabela3,all = TRUE)

dados$Referencia <- factor(dados$Referencia,ordered = T,levels = c(level1,level2,level3))

grafico <- dados %>% 
  ggplot(aes(x = Ano, y = Empregos)) +
  geom_bar(aes(fill = Setor), stat="identity") +
  scale_fill_brewer(palette="Set1") +
  theme_bw() +
  theme(legend.position="bottom",
        axis.text.x = element_text(angle = 90),
        strip.text = element_text(size = 6),
        legend.title = element_blank(),
        legend.direction = "vertical") +
  scale_x_continuous(breaks = unique(dados$Ano)) +
  labs(y = "Postos de Trabalho", x=NULL) +
  facet_wrap(~Referencia,ncol = 1,scales = "free_y")
