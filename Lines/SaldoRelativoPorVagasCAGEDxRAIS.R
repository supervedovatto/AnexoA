level1 <- paste("Município de", LocRef$Localidade)
tabela1 <- Emprego %>%
  mutate(Saldo = Admitidos - Desligados, Taxa = Saldo / Empregos) %>%
  filter(!is.na(Taxa) &
           Empregos > 0 & Localidade == LocRef$Localidade & Ano >= 2011) %>%
  select(Localidade, Ano, Setor, Taxa) %>%
  mutate(Localidade = level1)

level2 <- paste("RP", LocRef$RPSEGPLAN)
tabela2 <- Emprego %>%
  mutate(Saldo = Admitidos - Desligados) %>%
  filter(!is.na(Saldo) &
           Empregos > 0 & RPSEGPLAN == LocRef$RPSEGPLAN & Ano >= 2011) %>%
  group_by(Setor, Ano) %>%
  summarize(Saldo = sum(Saldo, na.rm = TRUE),
            Empregos = sum(Empregos, na.rm = TRUE)) %>%
  mutate(Taxa = Saldo / Empregos, Referencia = level2) %>%
  select(Referencia, Ano, Setor, Taxa)

level3 <- paste("Goiás")
tabela3 <- Emprego %>%
  mutate(Saldo = Admitidos - Desligados) %>%
  filter(!is.na(Saldo) & Empregos > 0 & Ano >= 2011) %>%
  group_by(Setor, Ano) %>%
  summarize(Saldo = sum(Saldo, na.rm = TRUE),
            Empregos = sum(Empregos, na.rm = TRUE)) %>%
  mutate(Taxa = Saldo / Empregos, Referencia = level3) %>%
  select(Referencia, Ano, Setor, Taxa)

colnames(tabela1) <- colnames(tabela2) <- colnames(tabela3)

dados <- merge(tabela2, tabela1, all = TRUE) %>%
  merge(tabela3, all = TRUE)

dados$Referencia <-
  factor(dados$Referencia,
         ordered = T,
         levels = c(level1, level2, level3))

# As taxas da Administração Pública Direta e Indireta são muito baixas devido à estabilidade do serviço público
# dados <- dados %>%  filter(Setor != "Administração Pública Direta e Indireta")

LineSaldoRelaticoPorVagas <- dados %>%
  ggplot(aes(x = Ano, y = Taxa)) +
  geom_line(aes(color = Referencia), stat = "identity", size = 1) +
  scale_color_manual(values = mypallete) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90),
    strip.text = element_text(size = 6),
    legend.title = element_blank(),
    legend.direction = "horizontal"
  ) +
  scale_x_continuous(breaks = unique(dados$Ano)) +
  labs(y = "Taxa de Aumento de Postos de Trabalho", x = NULL,
       caption = "Fonte: Elaborado pelo núcleo de base do OMT/GYN a partir de dados do BDE/IMB, com acesso em 19/03/2020.") +
  facet_wrap( ~ Setor, ncol = 2, scales = "free_y")