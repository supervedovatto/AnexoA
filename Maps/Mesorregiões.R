MapaGoias %>% 
  ggplot() + 
    theme_bw() +
    scale_fill_manual(values = mypallete) +
    geom_sf(aes(fill= Mesorregiao), size = 0.1,) +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.text = element_text(size=5),
          axis.title = element_blank())+
    labs(caption = "Fonte: IBGE, com acesso em 19/03/2020.",
         fill = "Mesorregiões")

