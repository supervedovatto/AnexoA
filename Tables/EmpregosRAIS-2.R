tab1 <- Emprego %>%
  filter(
    !is.na(`Rendimento Médio`) &
      Localidade == LocRef$Localidade &
      Ano >= max(EmpregoRAIS$Ano) - 4
  ) %>%
  select(Ano, Setor, `Rendimento Médio`) %>%
  reshape2::dcast(formula = Setor ~ Ano,
                  fun.aggregate = mean,
                  value.var = "Rendimento Médio")

tab2 <- Emprego %>%
  filter(
    !is.na(`Rendimento Médio`) &
      !is.na(Empregos) &
      RPSEGPLAN == LocRef$RPSEGPLAN &
      Ano >= max(EmpregoRAIS$Ano) - 4
  ) %>%
  select(Ano, Setor, `Rendimento Médio`, Empregos) %>%
  mutate(RendaTotal = `Rendimento Médio` * Empregos) %>%
  select(Ano, Setor, Empregos, RendaTotal) %>%
  group_by(Ano, Setor) %>%
  summarise(Empregos = sum(Empregos),
            RendaTotal = sum(RendaTotal)) %>%
  mutate(RendimentoMedio = round(RendaTotal / Empregos, digits = 2)) %>%
  reshape2::dcast(formula = Setor ~ Ano, value.var = "RendimentoMedio")

tab3 <- Emprego %>%
  filter(!is.na(`Rendimento Médio`) &
           !is.na(Empregos) & Ano >= max(EmpregoRAIS$Ano) - 4) %>%
  select(Ano, Setor, `Rendimento Médio`, Empregos) %>%
  mutate(RendaTotal = `Rendimento Médio` * Empregos) %>%
  select(Ano, Setor, Empregos, RendaTotal) %>%
  group_by(Ano, Setor) %>%
  summarise(Empregos = sum(Empregos),
            RendaTotal = sum(RendaTotal)) %>%
  mutate(RendimentoMedio = round(RendaTotal / Empregos, digits = 2)) %>%
  reshape2::dcast(formula = Setor ~ Ano, value.var = "RendimentoMedio")

TableEmpregoRais2 <- bind_rows(tab1, tab2, tab3) %>%
  mutate_all(linebreak) %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    caption = paste(
      "Rendimento salarial médio (em reais) dos empregos por setor, entre",
      max(Emprego$Ano) - 4,
      "e",
      max(Emprego$Ano),
      "no Município de",
      LocRef$Localidade,
      "segundo a RAIS."
    )
  ) %>%
  kable_styling("striped", full_width = F) %>%
  pack_rows(paste("Município de", LocRef$Localidade), 1, 8) %>%
  pack_rows(paste("Região de Planejamento", LocRef$RPSEGPLAN), 9, 16) %>%
  pack_rows("Estado de Goiás", 17, 24) %>%
  column_spec(1, width = "20em") %>%
  footnote(
    general_title = "Fonte:",
    general = "Elaborado pelo núcleo de base do OMT/GYN a partir de dados do BDE/IMB, com acesso em 19/03/2020.",
    footnote_as_chunk = T,
    threeparttable = T
  )

rm(tab1, tab2, tab3)