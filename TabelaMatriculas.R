level1 <- paste("Município de",LocRef$Localidade)
tabela1 <- Matriculas %>% 
  filter(Localidade == LocRef$Localidade & Ano == max(Ano)) %>% 
  select(Modalidade,Rede,Total) %>% 
  mutate(Rede = fct_relevel(Rede,"Federal","Estadual","Municipal","Particular","Comunitária, Confessional ou Filantrópica"),
         Referencia = factor(level1))

level2 <- paste("RP",LocRef$RPSEGPLAN)
tabela2 <- Matriculas %>% 
  merge(RegioesGoias) %>% 
  filter(RPSEGPLAN == LocRef$RPSEGPLAN & Ano == max(Ano)) %>% 
  mutate(Rede = fct_relevel(Rede,"Federal","Estadual","Municipal","Particular","Comunitária, Confessional ou Filantrópica")) %>% 
  group_by(Modalidade,Rede) %>% 
  summarise(Total = sum(Total)) %>% 
  mutate(Referencia = factor(level2)) %>% 
  as.data.frame()

level3 <- "Estado de Goiás"
tabela3 <- Matriculas %>% 
  merge(RegioesGoias) %>% 
  filter(Ano == max(Ano)) %>% 
  mutate(Rede = fct_relevel(Rede,"Federal","Estadual","Municipal","Particular","Comunitária, Confessional ou Filantrópica")) %>% 
  group_by(Modalidade,Rede) %>% 
  summarise(Total = sum(Total)) %>% 
  mutate(Referencia = factor(level3)) %>% 
  as.data.frame()

tabela <- rbind(tabela1,tabela2,tabela3) %>% 
  select(Referencia,Modalidade,Rede,Total) %>% 
  spread(Rede,Total)

rm(tabela1,tabela2,tabela3,level1,level2,level3)