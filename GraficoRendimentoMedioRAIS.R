load(file = "Dados/POCV.RData")

tabela1 <- Emprego %>% 
  filter(!is.na(`Rendimento Médio`) & Localidade == LocRef$Localidade & Ano >= max(EmpregoRAIS$Ano)-10) %>% 
  select(Localidade,Ano, Setor,`Rendimento Médio`) %>% 
  data.table()

tabela2 <- Emprego %>% 
  filter(!is.na(`Rendimento Médio`) & !is.na(Empregos) & RPSEGPLAN == LocRef$RPSEGPLAN & Ano >= max(EmpregoRAIS$Ano)-10) %>% 
  select(RPSEGPLAN,Ano, Setor,`Rendimento Médio`,Empregos) %>% 
  mutate(RendaTotal=`Rendimento Médio`*Empregos) %>% 
  select(RPSEGPLAN,Ano, Setor,Empregos,RendaTotal) %>% 
  group_by(RPSEGPLAN,Ano,Setor) %>% 
  summarise(Empregos = sum(Empregos), RendaTotal = sum(RendaTotal)) %>% 
  mutate(RendimentoMedio = RendaTotal/Empregos) %>% 
  select(RPSEGPLAN,Ano, Setor,RendimentoMedio)

tabela3 <- Emprego %>% 
  filter(!is.na(`Rendimento Médio`) & !is.na(Empregos) & Ano >= max(EmpregoRAIS$Ano)-10) %>% 
  select(Ano, Setor,`Rendimento Médio`,Empregos) %>% 
  mutate(RendaTotal=`Rendimento Médio`*Empregos) %>% 
  select(Ano, Setor,Empregos,RendaTotal) %>% 
  group_by(Ano,Setor) %>% 
  summarise(Empregos = sum(Empregos), RendaTotal = sum(RendaTotal)) %>% 
  mutate(RendimentoMedio = RendaTotal/Empregos) %>% 
  select(Ano,Setor,RendimentoMedio) %>% 
  mutate(Referencia = "Goiás") %>% 
  select(Referencia,Ano, Setor,RendimentoMedio)

colnames(tabela1) <- colnames(tabela2) <- colnames(tabela3)

dados <- merge(tabela2,tabela1,all = TRUE) %>% 
  merge(tabela3,all = TRUE)

dados$Referencia <- factor(dados$Referencia,ordered = T,levels = c("Goiânia","Metropolitana de Goiânia","Goiás"))
levels(dados$Referencia)

grafico <- dados %>% 
  ggplot(aes(x = Ano, y = RendimentoMedio)) +
  geom_line(aes(color = Referencia), stat="identity") +
  scale_color_brewer(palette="Set2") +
  theme_bw() +
  theme(legend.position="bottom", axis.text.x = element_text(angle = 90), strip.text = element_text(size = 6)) +
  scale_x_continuous(breaks = unique(dados$Ano)) +
  labs(y = "Rendimento Médio", x="Ano") +
  facet_wrap(~Setor, ncol = 2)
grafico