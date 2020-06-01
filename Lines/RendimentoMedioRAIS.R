level1 <- paste("Município de",LocRef$Localidade)
tabela1 <- Emprego %>% 
  filter(!is.na(`Rendimento Médio`) & Localidade == LocRef$Localidade & Ano >= max(EmpregoRAIS$Ano)-10) %>% 
  select(Localidade,Ano, Setor,`Rendimento Médio`) %>%  
  mutate(Localidade = level1)

level2 <- paste("RP",LocRef$RPSEGPLAN)
tabela2 <- Emprego %>% 
  filter(!is.na(`Rendimento Médio`) & !is.na(Empregos) & RPSEGPLAN == LocRef$RPSEGPLAN & Ano >= max(EmpregoRAIS$Ano)-10) %>% 
  select(RPSEGPLAN,Ano, Setor,`Rendimento Médio`,Empregos) %>% 
  mutate(RendaTotal=`Rendimento Médio`*Empregos) %>% 
  select(RPSEGPLAN,Ano, Setor,Empregos,RendaTotal) %>% 
  group_by(Ano,Setor) %>% 
  summarise(Empregos = sum(Empregos), RendaTotal = sum(RendaTotal)) %>% 
  mutate(RendimentoMedio = RendaTotal/Empregos,Referencia = level2) %>% 
  select(Referencia,Ano, Setor,RendimentoMedio)

level3 <- paste("Goiás")
tabela3 <- Emprego %>% 
  filter(!is.na(`Rendimento Médio`) & !is.na(Empregos) & Ano >= max(EmpregoRAIS$Ano)-10) %>% 
  select(Ano, Setor,`Rendimento Médio`,Empregos) %>% 
  mutate(RendaTotal=`Rendimento Médio`*Empregos) %>% 
  select(Ano, Setor,Empregos,RendaTotal) %>% 
  group_by(Ano,Setor) %>% 
  summarise(Empregos = sum(Empregos), RendaTotal = sum(RendaTotal)) %>% 
  mutate(RendimentoMedio = RendaTotal/Empregos) %>% 
  select(Ano,Setor,RendimentoMedio) %>% 
  mutate(Referencia = level3) %>% 
  select(Referencia,Ano, Setor,RendimentoMedio)

colnames(tabela1) <- colnames(tabela2) <- colnames(tabela3)

dados <- merge(tabela2,tabela1,all = TRUE) %>% 
  merge(tabela3,all = TRUE)

dados$Referencia <- factor(dados$Referencia,ordered = T,levels = c(level1,level2,level3))

LineRendimentoMedioRAIS <- dados %>% 
  ggplot(aes(x = Ano, y = RendimentoMedio)) +
  geom_line(aes(color = Referencia), stat="identity",size = 1) +
  scale_color_manual(values = mypallete) +
  theme_bw() +
  theme(legend.position="bottom",
        axis.text.x = element_text(angle = 90),
        strip.text = element_text(size = 6),
        legend.title = element_blank(),
        legend.direction = "horizontal") +
  scale_x_continuous(breaks = unique(dados$Ano)) +
  labs(y = "Rendimento Médio", x=NULL,
       caption = "Fonte: Elaborado pelo núcleo de base do OMT/GYN a partir de dados do BDE/IMB, com acesso em 19/03/2020.") +
  facet_wrap(~Setor, ncol = 2,scales = "free_y")