MapsMicrorregioes <- MapaGoias[MapaGoias$Mesorregiao == LocRef$Mesorregiao,] %>% 
  ggplot() + 
  theme_bw() +
  scale_fill_manual(values = mypallete) +
  theme(title = element_text(size=5),
        axis.text = element_text(size=5),
        axis.title = element_blank()) +
  geom_sf(aes(fill= Microrregiao), size = 0.1) +
  labs(caption = "Fonte: Elaborado pelo OMT/GYN com dados do BDE/IMB. Acesso: 19/03/2020.",
       title = paste("Microrregiões do",LocRef$Mesorregiao),
       fill = "Microrregiões")