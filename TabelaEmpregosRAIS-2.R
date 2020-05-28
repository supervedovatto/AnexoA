tab1 <- Emprego %>% 
  filter(!is.na(`Rendimento Médio`) & Localidade == LocRef$Localidade & Ano >= max(EmpregoRAIS$Ano)-4) %>% 
  select(Ano, Setor,`Rendimento Médio`) %>% 
  reshape2::dcast(formula = Setor~Ano,fun.aggregate = mean,value.var = "Rendimento Médio")

tab2 <- Emprego %>% 
  filter(!is.na(`Rendimento Médio`) & !is.na(Empregos) & RPSEGPLAN == LocRef$RPSEGPLAN & Ano >= max(EmpregoRAIS$Ano)-4) %>% 
  select(Ano, Setor,`Rendimento Médio`,Empregos) %>% 
  mutate(RendaTotal=`Rendimento Médio`*Empregos) %>% 
  select(Ano, Setor,Empregos,RendaTotal) %>% 
  group_by(Ano,Setor) %>% 
  summarise(Empregos = sum(Empregos), RendaTotal = sum(RendaTotal)) %>% 
  mutate(RendimentoMedio = round(RendaTotal/Empregos,digits = 2)) %>% 
  reshape2::dcast(formula = Setor~Ano,value.var = "RendimentoMedio")

tab3 <- Emprego %>% 
  filter(!is.na(`Rendimento Médio`) & !is.na(Empregos) & Ano >= max(EmpregoRAIS$Ano)-4) %>% 
  select(Ano, Setor,`Rendimento Médio`,Empregos) %>% 
  mutate(RendaTotal=`Rendimento Médio`*Empregos) %>% 
  select(Ano, Setor,Empregos,RendaTotal) %>% 
  group_by(Ano,Setor) %>% 
  summarise(Empregos = sum(Empregos), RendaTotal = sum(RendaTotal)) %>% 
  mutate(RendimentoMedio = round(RendaTotal/Empregos,digits = 2)) %>% 
  reshape2::dcast(formula = Setor~Ano,value.var = "RendimentoMedio")

tabela <- bind_rows(tab1,tab2,tab3)
rm(tab1,tab2,tab3)