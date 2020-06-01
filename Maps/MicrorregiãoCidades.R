MapaGoias$CityRef_id <- "Outras Cidades"
MapaGoias[MapaGoias$Localidade == LocRef$Localidade, ]$CityRef_id <-
  as.character(LocRef$Localidade)

MicrorregiaoCidades <- MapaGoias[MapaGoias$Microrregiao == LocRef$Microrregiao, ] %>%
  ggplot() +
  theme_bw() +
  scale_fill_manual(values = mypallete) +
  theme(
    title = element_text(size = 8),
    legend.position = "none",
    axis.text = element_text(size = 5),
    axis.title = element_blank()
  ) +
  geom_sf(aes(fill = CityRef_id), size = 0.1) +
  geom_sf_label(aes(label = Localidade),
                label.padding = unit(0.5, "mm"),
                size = 1) +
  labs(
    x = "",
    y = "",
    caption = "Fonte: Elaborado pelo OMT/GYN com dados do BDE/IMB. Acesso: 19/03/2020.",
    subtitle = paste("Microrregião de", LocRef$Microrregiao)
  )