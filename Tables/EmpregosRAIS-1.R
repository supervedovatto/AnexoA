TableEmpregoRais1 <- Emprego %>%
  filter(
    !is.na(Empregos) &
      Setor != "Atividade não especificada ou classificada" &
      Localidade == LocRef$Localidade &
      Ano >= max(EmpregoRAIS$Ano) - 4
  ) %>%
  select(Ano, Setor, Empregos) %>%
  reshape2::dcast(formula = Setor ~ Ano,
                  fun.aggregate = sum,
                  value.var = "Empregos") %>%
  mutate_all(linebreak) %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    caption = paste(
      "Total de empregos por setor, entre",
      max(Emprego$Ano) - 4,
      "e",
      max(Emprego$Ano),
      "no Município de",
      LocRef$Localidade,
      "segundo a RAIS."
    )
  ) %>%
  kable_styling(latex_options = c("striped", "hold_position")) %>%
  column_spec(1, width = "20em") %>%
  footnote(
    general_title = "Fonte:",
    general = "Elaborado pelo núcleo de base do OMT/GYN a partir de dados do BDE/IMB, com acesso em 19/03/2020.",
    footnote_as_chunk = T,
    threeparttable = T
  )