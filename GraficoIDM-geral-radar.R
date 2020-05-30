level1 <- paste("Mun. de",LocRef$Localidade)
tabela1 <- IDM %>% 
  filter(!is.na(Valor) & Localidade == LocRef$Localidade & Ano == max(Ano)) %>% 
  group_by(IDM) %>% 
  summarise(Valor = mean(Valor)) %>% 
  mutate(Referencia = level1)

level2 <- paste("RP",LocRef$RPSEGPLAN)
tabela2 <- IDM %>% 
  merge(RegioesGoias) %>% 
  filter(!is.na(Valor) & RPSEGPLAN == LocRef$RPSEGPLAN & Ano == max(Ano)) %>% 
  group_by(Localidade,IDM) %>% 
  summarise(Valor = mean(Valor)) %>% 
  group_by(IDM) %>% 
  summarise(Valor = mean(Valor)) %>% 
  mutate(Referencia = level2)
  
level3 <- "Estado de Goiás"
tabela3 <- IDM %>% 
  filter(!is.na(Valor) & Ano == max(Ano)) %>% 
  group_by(Localidade,IDM) %>% 
  summarise(Valor = mean(Valor)) %>% 
  group_by(IDM) %>% 
  summarise(Valor = mean(Valor)) %>% 
  mutate(Referencia = level3)

grafico <- rbind(tabela1,tabela2,tabela3) %>% 
  mutate(Referencia = factor(Referencia,ordered = T,levels = c(level1,level2,level3))) %>% 
  dcast(Referencia~IDM,value.var = "Valor") %>% 
  ggradar(grid.max = 10,  grid.mid = 5, grid.min = 0,
          grid.label.size = 5,
          values.radar = c(0,5,10),
          label.gridline.mid = TRUE,
          legend.position = "bottom",
          legend.text.size = 8,
          axis.label.offset = 1.1,
          group.line.width = 1,
          group.point.size = 2,
          axis.label.size = 2.5) +
  theme(plot.caption = element_text(size = 8)) +
  labs(caption = "Fonte: Elaborado pelo núcleo de base do OMT/GYN a partir de dados do BDE/IMB, com acesso em 19/03/2020.")