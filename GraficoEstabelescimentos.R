load(file = "Dados/POCV.RData")

dados <- EstabelecimentosESalas %>% 
  filter(Localidade == LocRef$Localidade) %>%
  select(-starts_with("Salas"),-starts_with("Localidade")) %>% 
  filter(!is.na(Estabelecimentos))

grafico <- dados %>% 
    ggplot(aes(x = Ano, y = Estabelecimentos)) + 
    geom_line(aes(color = Rede), size = 1) +
    scale_color_manual(values = mypallete) +
    scale_x_continuous(breaks = unique(dados$Ano)) +
    theme_minimal() +
    theme(legend.position="bottom",axis.text.x = element_text(angle = 90)) +
    labs(caption = "Fonte: Elaborado pelo núcleo de base do OMT/GYN a partir de dados do BDE/IMB, com acesso em 19/03/2020.")
