load(file = "Dados/POCV.RData")

tabela <- Emprego %>% 
  filter(!is.na(`Rendimento Médio`) & !is.na(Empregos) & Ano >= max(EmpregoRAIS$Ano)-4) %>% 
  select(Ano, Setor,`Rendimento Médio`,Empregos) %>% 
  mutate(RendaTotal=`Rendimento Médio`*Empregos) %>% 
  select(Ano, Setor,Empregos,RendaTotal) %>% 
  group_by(Ano,Setor) %>% 
  summarise(Empregos = sum(Empregos), RendaTotal = sum(RendaTotal)) %>% 
  mutate(RendimentoMedio = RendaTotal/Empregos) %>% 
  reshape2::dcast(formula = Setor~Ano,value.var = "RendimentoMedio")