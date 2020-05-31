MapaSEGPLAN <- MapaGoias %>% 
  ggplot() + 
    geom_sf(aes(fill= RPSEGPLAN), size = 0.1) +
    theme_bw() +
    scale_fill_manual(values = mypallete) +
    theme(title = element_text(size=8),
          legend.position="bottom",
          axis.text = element_text(size=5),
          axis.title = element_blank(),
          legend.title = element_blank())+
    labs(caption = "Fonte: IBGE, com acesso em 19/03/2020.",
         fill = "Regiões de Planejamento")