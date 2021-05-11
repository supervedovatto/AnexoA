level2 <- paste("RP",LocRef$RPSEGPLAN)
tabela2 <- Emprego %>% 
  filter(!is.na(`Rendimento Médio`) & !is.na(Empregos) & RPSEGPLAN == LocRef$RPSEGPLAN & Ano == max(EmpregoRAIS$Ano)) %>% 
  select(RPSEGPLAN,Ano, Setor,`Rendimento Médio`,Empregos) %>% 
  mutate(RendaTotal=`Rendimento Médio`*Empregos) %>% 
  select(RPSEGPLAN,Ano, Setor,Empregos,RendaTotal) %>% 
  group_by(Ano,Setor) %>% 
  summarise(Empregos = sum(Empregos), RendaTotal = sum(RendaTotal)) %>% 
  mutate(RendimentoMedio = RendaTotal/Empregos,Referencia = level2) %>% 
  select(Referencia,Ano, Setor,RendimentoMedio)

MapRendimentoMedio <- MapaGoias %>% 
  merge(Emprego) %>% 
  filter(Ano == max(Ano) & RPSEGPLAN == LocRef$RPSEGPLAN) %>%
  filter(Setor != "Atividade não especificada ou classificada") %>% 
  mutate(Setor = fct_recode(Setor, "Agricultura, Criação de Animais, ..." = "Agricultura, Silvicultura, Criação de Animais, Extração Vegetal e Pesca")) %>% 
  ggplot() +
  facet_wrap(.~Setor,ncol = 2) +
  geom_sf(aes(fill = `Rendimento Médio`), size = 0.1) +
  geom_sf_label(aes(label = Localidade),
                label.padding = unit(0.5, "mm"),
                size = 1) +
  theme_bw() +
  scale_fill_viridis_c(option = "plasma") +
  theme(
    title = element_text(size = 8),
    legend.position = "right",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  ) +
  labs(
    caption = "Fonte: Elaborado pelo OMT/GYN com dados do BDE/IMB. Acesso: 19/03/2020.",
    subtitle = paste("Rendimento Salarial nos Municípios da RP ",LocRef$RPSEGPLAN," (",max(Emprego$Ano),")",sep=""),
    fill = "Rendimento Médio"
  )