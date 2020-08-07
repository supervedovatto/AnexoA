MapaGoias %>% 
  merge(IDH) %>% 
  filter(Ano == max(Ano) & RPSEGPLAN == LocRef$RPSEGPLAN) %>%
  ggplot() +
  facet_wrap(.~IDHM) +
  geom_sf(aes(fill = Valor), size = 0.1) +
  geom_sf_label(aes(label = Localidade),
                label.padding = unit(0.5, "mm"),
                size = 1) +
  theme_bw() +
  scale_fill_viridis_c(guide = guide_legend(nrow = 1,label.position = "bottom")) +
  theme(
    title = element_text(size = 8),
    legend.position = "bottom",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  ) +
  labs(
    caption = "Fonte: Elaborado pelo OMT/GYN com dados do BDE/IMB. Acesso: 19/03/2020.",
    subtitle = paste("IDH nos Municípios da RP ",LocRef$RPSEGPLAN," (",year(max(IDH$Ano)),")",sep=""),
    fill = "IDH"
  )